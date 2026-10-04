let ( let* ) = Result.bind
let field name = function `Assoc xs -> Option.value ~default:`Null (List.assoc_opt name xs) | _ -> `Null
let str = function `String s -> s | _ -> invalid_arg "expected string"
let int = function `Int n -> n | _ -> invalid_arg "expected integer"
let num = function `Float n -> n | `Int n -> float_of_int n | `Null -> 0. | _ -> invalid_arg "expected time"
let bool = function `Bool b -> b | _ -> invalid_arg "expected boolean"
let list = function `List xs -> xs | _ -> invalid_arg "expected list"
let s k j = str (field k j)
let n k j = num (field k j)
let i k j = int (field k j)
let xs k j = list (field k j)
let put k v = function `Assoc pairs -> `Assoc ((k,v) :: List.remove_assoc k pairs) | _ -> invalid_arg "expected object"
let puts fields j = List.fold_left (fun j (k,v) -> put k v j) j fields
let hash s = Digest.to_hex (Digest.string s)
let now () = `Float (Unix.gettimeofday ())
let take n xs = List.filteri (fun index _ -> index < n) xs
let parse f = try Ok (f ()) with
  | Invalid_argument message -> Error ("invalid context data: " ^ message)
  | Yojson.Json_error _ -> Error "invalid context JSON"
  | Yojson.Safe.Util.Type_error _ -> Error "invalid context JSON structure"
let bounded_string max = function
  | `String s when String.length s > 0 && String.length s <= max -> s
  | _ -> invalid_arg "missing or oversized string"
let text k max j = bounded_string max (field k j)
let empty = `Assoc ["schema",`Int 1; "generation",`Int 0; "connections",`List [];
                    "observations",`List []; "findings",`List []]
let root home =
  let home = match State_path.canonicalize home with Ok path -> path | Error message -> invalid_arg message in
  Filename.concat home ".monty/context"
let path home = Filename.concat (root home) "state.json"
let safe_path home =
  let target = path home in
  let* canonical = State_path.canonicalize target in
  if canonical <> target then Error "unsafe context state path (symlink)"
  else let* st = State_store.lstat target in
    match st with None | Some {Unix.st_kind=Unix.S_REG;_} -> Ok target
    | _ -> Error "context state is not a regular file"
let load ~home =
  let* path = safe_path home in
  let* stored = State_store.read_json ~path in
  match stored with None -> Ok empty | Some state ->
    parse (fun () ->
        if i "schema" state <> 1 then invalid_arg "unsupported state schema";
        List.iter (fun key -> ignore (xs key state)) ["connections";"observations";"findings"];
        ignore (i "generation" state); state)
let save ~home state =
  let* path = safe_path home in
  let* () = State_store.ensure_real_directory ~label:"context directory" ~mode:0o700 (root home) in
  State_store.write_json_atomic ~path state
let mutate ~home f = State_store.with_lock ~home (fun () ->
    let* state = load ~home in
    let* state, value = f state in
    let* () = save ~home state in Ok value)
let connection state id = match List.find_opt (fun c -> s "id" c = id) (xs "connections" state) with
  | Some c -> Ok c | None -> Error ("unknown context connection: " ^ id)
let replace_connection state c = put "connections" (`List (List.map
    (fun old -> if s "id" old = s "id" c then c else old) (xs "connections" state))) state
let defaults = ["cadence_seconds",300; "lookback_hours",24; "retention_days",30;
                "max_pages",20; "max_observations",10000; "max_findings",10000; "max_threads",1000;
                "analysis_messages",100; "timeout_seconds",120]
let validate_config config = parse (fun () ->
    let id = text "id" 80 config in
    (match State_path.safe_component ~label:"connection id" id with Ok _ -> () | Error _ -> invalid_arg "unsafe connection id");
    if not (List.mem (s "source" config) ["slack";"zulip"]) then invalid_arg "source must be slack or zulip";
    ignore (text "account" 300 config);
    ignore (bool (field "include_dms" config));
    let channels = xs "channels" config in
    if List.length channels > 100 then invalid_arg "at most 100 selected channels";
    List.iter (fun v -> ignore (bounded_string 100 v)) channels;
    if channels = [] && not (bool (field "include_dms" config)) then invalid_arg "select channels or DMs";
    let credential = text "credential_file" 4096 config in
    if Filename.is_relative credential then invalid_arg "credential_file must be absolute";
    let context = match field "relevance" config with `Null -> "Prioritize mentions and direct requests." | `String v when String.length v <= 8000 -> v | _ -> invalid_arg "relevance is too large" in
    let config = puts ["relevance",`String context] config in
    let config = List.fold_left (fun config (key,default) ->
        let value = match field key config with `Null -> default | v -> int v in
        let min,max = match key with
          | "cadence_seconds" -> 60,86400 | "retention_days" -> 1,365
          | "lookback_hours" -> 1,8760 | "max_pages" -> 1,100
          | "max_observations" | "max_findings" -> 100,100000 | "max_threads" -> 1,10000
          | "analysis_messages" -> 1,500 | _ -> 5,600 in
        if value < min || value > max then invalid_arg (key ^ " out of range");
        put key (`Int value) config) config defaults in
    List.iter (fun key -> match field key config with
        | `Null -> () | v -> let p = bounded_string 4096 v in
            if Filename.is_relative p then invalid_arg (key ^ " must be an absolute executable path")) ["python";"slack_cli"];
    (* Only persist the public contract, never arbitrary input/secret fields. *)
    let keys = ["id";"source";"account";"channels";"include_dms";"credential_file";
                "relevance";"projects";"groups";"python";"slack_cli"] @ List.map fst defaults in
    List.iter (fun key -> match field key config with `Null -> () | v ->
        List.iter (fun item -> ignore (bounded_string 100 item)) (list v)) ["projects";"groups"];
    `Assoc (List.map (fun key -> key,field key config) keys))
let configure ~home file =
  let* config = parse (fun () -> Yojson.Safe.from_file file) in
  let* config = validate_config config in
  mutate ~home (fun state ->
      let id = s "id" config in
      let old = List.find_opt (fun c -> s "id" c = id) (xs "connections" state) in
      let* () = match old with
        | Some c when List.exists (fun key -> field key (field "config" c) <> field key config) ["source";"account"] ->
            Error "connection identity is immutable; use a new id for a different source/account"
        | _ -> Ok () in
      let same_scope = match old with None -> false | Some old ->
        List.for_all (fun k -> field k (field "config" old) = field k config)
          ["source";"account";"channels";"include_dms"] in
      let c = match old with Some c when same_scope -> c | _ ->
        `Assoc ["id",`String id;"checkpoint",`Null;"coverage",`String "setup required: run context check";
                "last_collection",`Null;"last_analysis",`Null] in
      let c = puts ["config",config;"generation",`Int (i "generation" state + 1);
                    "next_due",(match old with Some old -> field "next_due" old | None -> `Int 0)] c in
      let observations = xs "observations" state |> List.filter_map (fun o ->
          if s "connection" o <> id then Some o else if same_scope then Some (put "analyzed" `Null o) else None) in
      let state = puts ["generation",`Int (i "generation" state + 1);
                         "connections",`List (c :: List.filter (fun c -> s "id" c <> id) (xs "connections" state));
                         "observations",`List observations] state in
      Ok (state,config))
let pending o = field "analyzed" o <> field "revision" o
let backlog state id = xs "observations" state |> List.filter (fun o -> s "connection" o = id && pending o) |> List.length
let observation ~connection ~fetched o =
  let native = text "native_id" 300 o in
  let conversation = text "conversation" 400 o in
  ignore (text "revision" 200 o);
  ignore (text "author" 300 o);
  let content = match field "content" o with `String s when String.length s <= 16000 -> s | _ -> invalid_arg "oversized content" in
  let url = text "url" 2000 o in
  if not (String.starts_with ~prefix:"https://" url) then invalid_arg "evidence URL must use https";
  let timestamp = n "source_time" o in
  if not (Float.is_finite timestamp) || timestamp < 0. then invalid_arg "invalid source time";
  ignore (bool (field "deleted" o));
  puts ["id",`String (hash (connection ^ ":" ^ native ^ ":" ^ s "revision" o));"connection",`String connection;
        "conversation",`String conversation;"content",`String content;"fetched_at",fetched;"analyzed",`Null] o
let ingest ~home ~id ~generation result =
  let* () = if Sys.getenv_opt "MONTY_FAULT_INJECT" = Some "context-before-ingest" then
      Error "fault injected before durable context ingestion" else Ok () in
  let* items, checkpoint, coverage, retry = parse (fun () ->
      if i "schema" result <> 1 then invalid_arg "adapter schema";
      let items = xs "observations" result in
      if List.length items > 10000 then invalid_arg "adapter batch too large";
      let items = List.map (observation ~connection:id ~fetched:(now ())) items in
      let coverage = text "coverage" 4000 result in
      let retry = n "retry_after" result in
      if not (Float.is_finite retry) || retry < 0. then invalid_arg "invalid retry delay";
      items,field "checkpoint" result,coverage,retry) in
  mutate ~home (fun state ->
      let* c = connection state id in
      let* settings = Settings.load ~home in
      if not settings.context_enabled || i "generation" c <> generation then Ok (state,()) else
      let config = field "config" c in
      let cutoff = Unix.gettimeofday () -. float_of_int (i "retention_days" config * 86400) in
      let kept = xs "observations" state |> List.filter (fun o ->
          s "connection" o <> id || pending o || n "source_time" o >= cutoff) in
      let table = Hashtbl.create (List.length kept) in
      List.iter (fun o -> Hashtbl.replace table (s "id" o) o) kept;
      List.iter (fun o -> match Hashtbl.find_opt table (s "id" o) with
          | Some old when field "revision" old = field "revision" o -> ()
          | _ -> Hashtbl.replace table (s "id" o) o) items;
      let all = Hashtbl.fold (fun _ o acc -> o::acc) table [] |> List.sort (fun a b -> String.compare (s "id" a) (s "id" b)) in
      if List.length (List.filter (fun o -> s "connection" o = id) all) > i "max_observations" config then
        Error "observation capacity reached; checkpoint preserved, analyze backlog or increase max_observations"
      else
        let c = puts ["checkpoint",checkpoint;"coverage",`String coverage;"last_attempt",now ();
                      "last_collection",(if i "successful_pages" result > 0 then now () else field "last_collection" c);
                      "user",field "user" result;
                      "collection_error",field "error" result;
                      "next_due",`Float (Unix.gettimeofday () +. max retry (float_of_int (i "cadence_seconds" config)))] c in
        Ok (put "observations" (`List all) (replace_connection state c),()))
let update_health ~home ~id ~generation fields = mutate ~home (fun state ->
    let* c = connection state id in
    if i "generation" c <> generation then Ok (state,()) else
    Ok (replace_connection state (puts fields c),()))
let finding_ref f = s "id" f ^ "@" ^ string_of_int (i "revision" f)
let find_finding state reference =
  match String.split_on_char '@' reference with
  | [id;revision] -> (match int_of_string_opt revision with
      | Some revision -> (match List.find_opt (fun f -> s "id" f = id) (xs "findings" state) with
          | Some f when i "revision" f = revision -> Ok f
          | Some _ -> Error "finding changed; retrieve and present the current revision first"
          | None -> Error "finding not found")
      | _ -> Error "expected finding-id@revision")
  | _ -> Error "use the exact displayed finding-id@revision"
let mark ~home ~reference ~resolve = mutate ~home (fun state ->
    let* f = find_finding state reference in
    let updated = if resolve then put "resolved" (`Bool true) f else put "presented_revision" (field "revision" f) f in
    Ok (put "findings" (`List (List.map (fun old -> if field "id" old = field "id" f then updated else old) (xs "findings" state))) state,()))
