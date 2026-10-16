(* Owned history is independent of native runtime and task lifecycle. All writers
   enter Monty's home lock; a manifest is the only commit point for batch files. *)
open Json_fields
let now () = `Float (Unix.gettimeofday ())
let take n values = List.filteri (fun index _ -> index < n) values
exception Failure of int * string
let fail code message = raise (Failure (code, message))
let get = function Ok x -> x | Error e -> fail 500 e
let required k j = try text k 8192 j with _ -> fail 400 ("missing/invalid " ^ k)
let array k j = match field k j with `Null -> [] | `List xs -> xs | _ -> fail 400 ("invalid " ^ k)
let obj xs = `Assoc xs
let string x = `String x
let sha value = Digestif.SHA256.(to_hex (digest_string value))
let random_id () =
  let ic = open_in_bin "/dev/urandom" in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
      really_input_string ic 24 |> String.to_seq |> List.of_seq
      |> List.map (fun c -> Printf.sprintf "%02x" (Char.code c)) |> String.concat "")
let component value =
  if String.length value = 0 || String.length value > 160 ||
     not (String.for_all (function 'a'..'z'|'A'..'Z'|'0'..'9'|'-'|'_' -> true | _ -> false) value)
  then fail 400 "invalid identifier";
  value
let root home = Filename.concat home ".monty"
let safe path =
  let canonical = get (State_path.canonicalize path) in
  if canonical <> path then fail 500 "unsafe archive path (symlink)";
  path
let file home parts = safe (List.fold_left Filename.concat (root home) parts)
let read path = match get (State_store.read_json ~path:(safe path)) with
  | Some j -> j | None -> fail 404 "record not found"
let write path j = get (State_store.write_json_atomic ~path:(safe path) j)
let locked home f = get (State_store.with_lock ~home (fun () -> Ok (f ())))
let path home id = file home ["conversations"; component id; "conversation.json"]
let load home id = read (path home id)
let save home j = write (path home (s "id" j)) j
let list home =
  let dir = file home ["conversations"] in
  if not (Sys.file_exists dir) then [] else
  Sys.readdir dir |> Array.to_list |> List.sort String.compare
  |> List.filter_map (fun id -> let p = path home id in
      if Sys.file_exists p then Some (read p) else None)
let session c id = match List.find_opt (fun x -> s "id" x = id) (xs "sessions" c) with
  | Some x -> x | None -> fail 404 "native session not found"
let update_session c sess =
  puts ["revision",`Int (i "revision" c + 1); "sessions",`List (List.map
    (fun old -> if s "id" old = s "id" sess then sess else old) (xs "sessions" c))] c
let tombstone home ~harness ~scope native = file home ["conversation-tombstones"; sha (harness ^ "\000" ^ scope ^ "\000" ^ native) ^ ".json"]
let ensure_not_purged home ~harness ~scope native =
  if Sys.file_exists (tombstone home ~harness ~scope native) then fail 409 "native session was explicitly purged"
let existing home ~harness ~scope native =
  list home |> List.find_opt (fun c -> List.exists (fun sess ->
    field "harness" sess = string harness && field "scope" sess = string scope && s "native_id" sess = native) (xs "sessions" c))
let create home ~id ~title ~task_keys ~session:sess = locked home (fun () ->
  let harness=s "harness" sess and scope=s "scope" sess and native=s "native_id" sess in
  ensure_not_purged home ~harness ~scope native;
  match existing home ~harness ~scope native with
  | Some c -> c
  | None -> let c = obj ["schema",`Int 1;"id",string id;"title",string title;
      "revision",`Int 1;"task_keys",`List task_keys;"created_at",now ();
      "archived",`Bool false;"selected_session",field "id" sess;
      "sessions",`List [sess];"transfers",`List []] in save home c; c)
let new_session ~profile ~harness ~scope ~native_id ~context =
  obj ["id",string (random_id ());"profile",string profile;"harness",string harness;"scope",string scope;
    "native_id",string native_id;"context",context;"provider",`Null;"model",`Null;
    "batches",`List [];"effective",`List [];"checkpoint",`Int 0;
    "coverage",obj ["state",string "not_synced";"gaps",`List [string "History before first capture may be unavailable."]]]
let batch_path home cid sid bid = file home ["conversations";component cid;"sessions";component sid;"batches";component bid ^ ".json"]
let batches home cid sess = xs "batches" sess |> List.map (fun b -> read (batch_path home cid (s "id" sess) (str b)))
let items home cid sess = batches home cid sess |> List.concat_map (fun b -> array "items" b)
let history ?(offset=0) home cid =
  if offset<0 then fail 400 "negative history offset";
  let c = load home cid in
  if field "purged" c = `Bool true then fail 410 "conversation purged";
  (* Eight bounded batches per session. Clients page independently of the
     source, including while it is offline. *)
  put "sessions" (`List (List.map (fun sess ->
      let refs=xs "batches" sess in
      let page=refs |> List.filteri (fun n _ -> n>=offset && n<offset+8) in
      puts ["records",`List (List.map (fun b -> read (batch_path home cid (s "id" sess) (str b))) page);
        "next_offset",(if offset+8<List.length refs then `Int (offset+8) else `Null)] sess) (xs "sessions" c))) c

let rec redact = function
  | `Assoc fields -> `Assoc (List.map (fun (k,v) -> k,
      if List.mem (String.lowercase_ascii k) ["authorization";"api_key";"apikey";"access_token";"accesstoken";"refresh_token";"refreshtoken";"id_token";"idtoken";"password";"secret"]
      then string "[redacted]" else redact v) fields)
  | `List xs -> `List (List.map redact xs) | x -> x
let rec public = function
  | `Assoc fields as value -> `Assoc(List.filter_map(fun (key,item)->
      if key="endpoint" && field "native_id" value<>`Null then None else Some(key,public item)) fields)
  | `List values->`List(List.map public values)
  | value->value
let within root path = path = root || String.starts_with ~prefix:(root ^ "/") path
let store_blob home ~source ~media_type bytes =
  if String.length bytes>8_388_608 then fail 413 "attachment exceeds 8 MiB";
  let hash=sha bytes in
  let path=file home ["conversation-blobs";hash] in
  if not(Sys.file_exists path) then get(State_store.write_file_atomic ~path ~perm:0o600 bytes);
  obj ["source",string source;"available",`Bool true;"sha256",string hash;
    "size",`Int (String.length bytes);"media_type",media_type]
let attachment home roots source =
  let unavailable why = obj ["source",string source;"available",`Bool false;"reason",string why] in
  try
    if Filename.is_relative source then fail 400 "relative attachment";
    let path = Unix.realpath source in
    if not (List.exists (fun root -> within root path) roots) then fail 400 "outside workspace";
    let st = Unix.lstat source in
    if st.Unix.st_kind <> Unix.S_REG || st.st_size > 8_388_608 then fail 400 "not a bounded regular file";
    let ic = open_in_bin source in
    let bytes = Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
      if not (State_store.same_file st (Unix.fstat (Unix.descr_of_in_channel ic))) then fail 400 "attachment changed";
      really_input_string ic st.st_size) in
    store_blob home ~source ~media_type:(string "application/octet-stream") bytes
  with _ -> unavailable "source unavailable, unsafe, or exceeds 8 MiB"
let rec attachments home roots j =
  let here = match field "type" j with
    | `String ("localImage"|"imageGeneration") -> (match field "path" j with
        | `String p -> [attachment home roots p] | _ -> [])
    | `String "image" when field "type" (field "source" j)=string "base64" ->
        let source=field "source" j in
        [ (try store_blob home ~source:"embedded image" ~media_type:(field "media_type" source) (Base64.decode_exn(bounded_string 11_184_812 (field "data" source)))
           with _ -> obj ["source",string "embedded image";"available",`Bool false;"reason",string "invalid or oversized image"]) ]
    | `String "image" -> (match field "data" j,field "url" j with
        | `String encoded,_ -> [
            (try
              if String.length encoded>11_184_812 then fail 413 "image exceeds 8 MiB";
              store_blob home ~source:"embedded image" ~media_type:(field "mimeType" j) (Base64.decode_exn encoded)
            with _ -> obj ["source",string "embedded image";"available",`Bool false;"reason",string "invalid base64 or exceeds 8 MiB"])]
        | _,`String url -> [obj ["source",string url;"available",`Bool false;"reason",string "remote bytes not fetched"]] | _ -> [])
    | _ -> [] in
  here @ (match j with `Assoc fields -> List.concat_map (fun (_,v) -> attachments home roots v) fields
       | `List xs -> List.concat_map (attachments home roots) xs | _ -> [])
let normalized sess snapshot =
  (if s "harness" sess = "pi" then
    let turns=Hashtbl.create 64 in
    array "entries" snapshot |> List.mapi (fun order e ->
      let message=field "message" e in
      let turn=if field "role" message=string "user" then field "id" e else
        Option.value ~default:`Null (Hashtbl.find_opt turns (Yojson.Safe.to_string (field "parentId" e))) in
      Hashtbl.replace turns (Yojson.Safe.to_string (field "id" e)) turn;
      let kind=match s "type" e,field "role" message with
        | "message",`String "user"->"userMessage" | "message",`String "assistant"->"agentMessage"
        | "message",`String "toolResult"->"toolResult" | "message",`String "system"->"systemMessage"
        | "message",_->"message" | other,_->other in
      obj ["source_id",field "id" e;"turn_id",turn;"order",`Int order;"kind",string kind;
        "parent",field "parentId" e;"content",e;"timestamp",field "timestamp" e;
        "provider",field "provider" message;"model",field "model" message;"trust",string "historical_data"])
  else if s "harness" sess = "claude" then
    array "entries" snapshot |> List.mapi (fun order e ->
      obj ["source_id",field "uuid" e;"turn_id",field "uuid" e;"order",`Int order;
        "kind",string (if s "type" e="user" then "userMessage" else "agentMessage");
        "content",field "message" e;"parent",field "parent_tool_use_id" e;
        "model",field "model" (field "message" e);"provider",string "anthropic";"trust",string "historical_data"])
  else if s "harness" sess = "fixture" then
    array "entries" snapshot |> List.mapi (fun order e ->
      obj ["source_id",field "uuid" e;"turn_id",field "exchange" e;"order",`Int order;
        "kind",field "kind" e;"content",field "payload" e;"parent",field "parent" e;
        "provider",`Null;"model",`Null;"trust",string "historical_data"])
  else array "turns" (field "thread" snapshot) |> List.concat_map (fun turn ->
      array "items" turn |> List.mapi (fun order item ->
        obj ["source_id",field "id" item;"turn_id",field "id" turn;"order",`Int order;
          "kind",field "type" item;"content",item;"status",field "status" turn;
          "provider",field "modelProvider" turn;"model",field "model" turn;
          "trust",string "historical_data"])))
  |> List.map (fun item ->
    if field "kind" item=string "userMessage" && field "transfer_prompt" sess<>`Null &&
      (let content=if s "harness" sess="pi" then field "content" (field "message" (field "content" item)) else field "content" (field "content" item) in
       content=field "transfer_prompt" sess || match content with
       | `List parts -> List.exists (fun part -> field "text" part=field "transfer_prompt" sess) parts | _ -> false)
    then puts ["kind",string "contextTransfer";"content",obj ["transfer",field "transfer" sess]] item
    else item)
let key item = Yojson.Safe.to_string (`List [field "turn_id" item;field "source_id" item])
let effective_keys sess snapshot incoming =
  if s "harness" sess<>"pi" then List.map (fun x -> string(key x)) incoming else
  let by_id=Hashtbl.create 64 in
  List.iter (fun item -> Hashtbl.replace by_id (field "source_id" item) item) incoming;
  let seen=Hashtbl.create 64 in
  let rec walk id acc = if id=`Null then acc else (
    if Hashtbl.mem seen id then fail 422 "cycle in Pi session lineage";
    Hashtbl.add seen id ();
    match Hashtbl.find_opt by_id id with None -> fail 422 "missing ancestor in Pi session lineage"
    | Some item -> walk (field "parent" item) (string(key item)::acc)) in
  walk (field "leafId" snapshot) []
let fault name = if Sys.getenv_opt "MONTY_FAULT_INJECT" = Some name then fail 500 ("fault injected: " ^ name)
(* Batches are immutable once referenced by the manifest. Keep only the latest
   digest/revision in memory, and read appended batches once, including after a
   failed write or an externally replaced manifest.
   ponytail: retain indexes until purge/restart; evict idle sessions if catalog
   size makes these compact indexes expensive. *)
let revision_cache : (string * string * string,
  Yojson.Safe.t list * (string,Yojson.Safe.t) Hashtbl.t) Hashtbl.t = Hashtbl.create 32
let previous_revisions home cid sess =
  let identity=home,cid,s "id" sess in
  let ids=xs "batches" sess in
  let cached,previous=match Hashtbl.find_opt revision_cache identity with
    | Some (cached,previous) when take (List.length cached) ids=cached -> cached,previous
    | _ -> [],Hashtbl.create 64 in
  let count=List.length cached in
  ids |> List.iteri (fun index bid -> if index>=count then
    read (batch_path home cid (s "id" sess) (str bid)) |> array "items" |> List.iter (fun item ->
      Hashtbl.replace previous (key item)
        (obj ["digest",field "digest" item;"revision",field "revision" item])));
  Hashtbl.replace revision_cache identity (ids,previous);
  previous
let revisions home cid sess incoming =
  if incoming=[] then [] else
  let previous = previous_revisions home cid sess in
  let roots = array "workspaces" (field "context" sess) |> List.map str in
  incoming |> List.filter_map (fun item ->
    if field "source_id" item = `Null then fail 502 "native item has no stable identity";
    let prior = Hashtbl.find_opt previous (key item) in
    let digest = sha (Yojson.Safe.to_string item) in
    if Option.fold ~none:false ~some:(fun x -> field "digest" x = string digest) prior then None
    else Some (puts ["digest",string digest;"revision",`Int (Option.fold ~none:1 ~some:(fun x -> i "revision" x+1) prior);
      "attachments",`List (attachments home roots (field "content" item))] item))
let append_batch home cid sess ~provenance fields =
  let bid = random_id () in
  write (batch_path home cid (s "id" sess) bid)
    (obj (["schema",`Int 1;"normalizer",`Int 1;"retrieved_at",now ();"provenance",string provenance] @ fields));
  fault "conversation-after-batch";
  puts ["batches",`List (xs "batches" sess @ [string bid]);"checkpoint",`Int (i "checkpoint" sess+1)] sess
let ingest ?(preserve_effective=false) home cid sid ~snapshot ~provenance = locked home (fun () ->
  let c = load home cid in
  if field "purged" c = `Bool true then fail 410 "conversation purged";
  let sess = session c sid in
  let raw = redact snapshot in
  if String.length (Yojson.Safe.to_string raw) > 16_777_216 then fail 413 "native snapshot exceeds 16 MiB";
  let incoming = normalized sess raw in
  let changed = revisions home cid sess incoming in
  let effective = effective_keys sess raw incoming in
  let effective = if preserve_effective then effective @ List.filter (fun k -> not(List.mem k effective)) (xs "effective" sess) else effective in
  let fingerprint = sha (Yojson.Safe.to_string raw) in
  let checkpoint = i "checkpoint" sess in
  let unchanged = field "snapshot_hash" sess = string fingerprint in
  let sess = if unchanged && changed=[] && effective=xs "effective" sess then sess else
    let captured=if s "harness" sess="pi" then
      (* Pi entries are immutable native records. Retain only new/revised entries,
         plus the source header/leaf; repeated file copies do not duplicate logs. *)
      let ids=Hashtbl.create 64 in
      List.iter (fun item -> Hashtbl.replace ids (field "source_id" item) ()) changed;
      puts ["capture",string "entry_delta";"entries",`List (array "entries" raw |> List.filter (fun e ->
        Hashtbl.mem ids (field "id" e)))] raw else raw in
    append_batch home cid sess ~provenance
      ["raw",(if unchanged then `Null else captured);"raw_sha256",string fingerprint;"items",`List changed]
    |> puts ["snapshot_hash",string fingerprint;"effective",`List effective] in
  let gaps=[string "Only exposed source history is covered; content lost before capture cannot be recovered."] @
    (if s "harness" sess="pi" then [string "File mirroring covers persisted entries; unsaved native activity and branch navigation require an owned RPC runtime."] else []) in
  let sess = puts ["coverage",obj ["state",string (if field "partial_tail" raw=`Bool true then "partial" else "available");"last_success",now ();
    "gaps",`List gaps;"partial_tail",field "partial_tail" raw;"leaf_source",field "leaf_source" raw;
    "unsupported",`List (incoming |> List.filter (fun x -> not (List.mem (s "kind" x)
      ["userMessage";"agentMessage";"commandExecution";"fileChange";"mcpToolCall";"dynamicToolCall";"contextCompaction";"contextTransfer";"message";"tool";
       "toolResult";"systemMessage";"model_change";"thinking_level_change";"usage";"compaction";"context_edit";"branch_summary";"label";"session_info";"custom";"custom_message"])) |> List.map (field "kind"))]] sess in
  let updated=update_session c sess in
  let updated=if checkpoint=i "checkpoint" sess then put "revision" (field "revision" c) updated else updated in
  save home updated)
let source_error home cid sid message = locked home (fun () ->
  let c = load home cid in
  if field "purged" c <> `Bool true then (
    let sess = session c sid in
    let coverage = puts ["state",string "unavailable";"error",string message;"checked_at",now ()] (field "coverage" sess) in
    let updated=update_session c (put "coverage" coverage sess) in
    save home (put "revision" (field "revision" c) updated)))
let receipt_path home id = file home ["conversation-commands";component id ^ ".json"]
let receipt home id = read (receipt_path home id)
let reserve home id request = locked home (fun () ->
  let path = receipt_path home id in
  let fingerprint = sha (Yojson.Safe.to_string request) in
  if Sys.file_exists path then (
    let r = read path in
    if s "fingerprint" r <> fingerprint then fail 409 "request ID already used for a different command";
    false,r)
  else let r = obj ["id",string id;"fingerprint",string fingerprint;"status",string "accepted";
      "created_at",now ();"request",redact (if field "op" request=string "respond" then
        put "response" (string "[response omitted]") request else if field "op" request=string "terminal_input" then
        put "data" (string "[terminal input omitted]") request else request)] in write path r;true,r)
let finish home id status result = locked home (fun () ->
  let r = receipt home id |> puts ["status",string status;"result",redact result;"updated_at",now ()] in
  write (receipt_path home id) r; r)
let recover home = locked home (fun () ->
  let dir = file home ["conversation-commands"] in
  if Sys.file_exists dir then Sys.readdir dir |> Array.iter (fun name ->
    let p = safe (Filename.concat dir name) in let r = read p in
    if List.mem (s "status" r) ["running";"accepted"] then write p (puts ["status",string "outcome_unknown";
      "result",obj ["reason",string "service restarted before a durable outcome; never automatically resubmitted"]] r));
  (* A batch not reachable from the manifest was never committed. It is safe to
     remove on restart; retry will reconstruct it from the source snapshot. *)
  list home |> List.iter (fun c -> xs "sessions" c |> List.iter (fun sess ->
    let dir = Filename.dirname (batch_path home (s "id" c) (s "id" sess) "unused") in
    if Sys.file_exists dir then Sys.readdir dir |> Array.iter (fun name ->
      if Filename.check_suffix name ".json" && not (List.mem (string (Filename.chop_suffix name ".json")) (xs "batches" sess))
      then Unix.unlink (safe (Filename.concat dir name))))))
let archive home cid = locked home (fun () -> let c = load home cid in
  let c = puts ["archived",`Bool true;"revision",`Int (i "revision" c+1)] c in save home c;c)
let purge home cid = locked home (fun () ->
  let c = load home cid in
  (* Publish all source tombstones before deleting any archive data. Retry is safe. *)
  xs "sessions" c |> List.iter (fun sess -> write (tombstone home ~harness:(s "harness" sess) ~scope:(s "scope" sess) (s "native_id" sess))
    (obj ["conversation",string cid;"purged_at",now ()]));
  xs "sessions" c |> List.iter (fun sess -> Hashtbl.remove revision_cache (home,cid,s "id" sess));
  let receipts=file home ["conversation-commands"] in
  if Sys.file_exists receipts then Sys.readdir receipts |> Array.iter (fun name ->
    let path=safe (Filename.concat receipts name) in let r=read path in
    if field "conversation" (field "request" r)=string cid || field "id" (field "result" r)=string cid then
      write path (puts ["request",obj ["purged_conversation",string cid];"result",obj ["purged_conversation",string cid]] r));
  let c = puts ["purged",`Bool true;"sessions",`List [];"transfers",`List [];"selected_session",`Null;
    "title",string "Purged conversation";"task_keys",`List []] c in save home c;
  let rec remove dir = if Sys.file_exists dir then (
    Sys.readdir (safe dir) |> Array.iter (fun name -> let p = safe (Filename.concat dir name) in
      if Sys.is_directory p then remove p else Unix.unlink p); Unix.rmdir dir) in
  remove (file home ["conversations";component cid;"sessions"]);
  (* Shared blob GC uses only committed, unpurged archives. *)
  let retained = Hashtbl.create 32 in
  list home |> List.iter (fun c -> xs "sessions" c |> List.iter (fun sess ->
    batches home (s "id" c) sess |> List.iter (fun batch ->
      (array "attachments" batch @ List.concat_map (array "attachments") (array "items" batch))
      |> List.iter (fun a -> match field "sha256" a with `String h -> Hashtbl.replace retained h () | _ -> ()))));
  let dir = file home ["conversation-blobs"] in
  if Sys.file_exists dir then Sys.readdir dir |> Array.iter (fun name ->
    if not (Hashtbl.mem retained name) then Unix.unlink (safe (Filename.concat dir name)));
  c)

let capture_event home cid sid json = locked home (fun () ->
  let c=load home cid in
  if field "purged" c<>`Bool true then (
    let sess=session c sid in
    let raw=redact json in
    let encoded=Yojson.Safe.to_string raw in
    if String.length encoded>16_777_216 then fail 413 "native event too large";
    let roots=array "workspaces" (field "context" sess) |> List.map str in
    let params=field "params" raw in
    let completed=field "method" raw=string "item/completed" && field "turnId" params<>`Null && field "id" (field "item" params)<>`Null in
    let incoming=if completed then normalized sess
      (obj ["thread",obj ["turns",`List [obj ["id",field "turnId" params;
        "items",`List [field "item" params]]]]]) else [] in
    let incoming=List.map (put "provisional_order" (`Bool true)) incoming |> revisions home cid sess in
    let effective=xs "effective" sess in
    let effective=effective @ (List.map (fun x -> string(key x)) incoming |> List.filter (fun k -> not(List.mem k effective))) in
    let sess=append_batch home cid sess ~provenance:"live native event"
      ["raw",raw;"items",`List incoming;"attachments",`List (attachments home roots raw)]
      |> put "effective" (`List effective) in
    save home (update_session c sess)))
