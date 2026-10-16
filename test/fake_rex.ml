open Test_support

let endpoint = "unix:///fake/rex/server.sock"
let app = "client:01a1-app"
let other_app = "client:01a1-other-app"

let present key = Option.fold ~none:false ~some:((<>) "") (Sys.getenv_opt key)
let fields = function `Assoc values -> values | _ -> failwith "fixture expected object"
let put key value json = `Assoc ((key, value) :: List.remove_assoc key (fields json))
let set key value json = json := put key value !json
let entries key state = fields (field key state)
let fail ?(code = 1) message = prerr_endline message; exit code
let not_found kind target = fail ~code:3 (Printf.sprintf "Error: %s target %S not found" kind target)
let option args key fallback =
  let rec find = function
    | name :: value :: _ when name = key -> value
    | _ :: rest -> find rest
    | [] -> fallback
  in
  find args
let tail args =
  let rec find = function "--" :: rest -> rest | _ :: rest -> find rest | [] -> [] in
  find args
let positionals args =
  let rec find = function
    | [] | "--" :: _ -> []
    | ("-s" | "-b" | "-w" | "-c" | "--label" | "--window" | "--shell"
      | "--split" | "--args" | "--ratio") :: _value :: rest -> find rest
    | token :: rest when String.starts_with ~prefix:"-" token -> find rest
    | token :: rest -> token :: find rest
  in
  find (List.tl args)

let run original_args =
  (match Sys.getenv_opt "REX_FAKE_LOG" with
  | Some path when path <> "" ->
      let channel = open_out_gen [ Open_creat; Open_append; Open_text ] 0o600 path in
      Fun.protect ~finally:(fun () -> close_out channel) (fun () ->
          output_string channel (Yojson.Safe.to_string (`List (List.map string original_args)) ^ "\n"))
  | _ -> ());
  let globals = ref [] in
  let rec parse = function
    | ("-S" | "-C" | "--timeout" as key) :: value :: rest ->
        globals := (key, value) :: !globals; parse rest
    | token :: rest when String.starts_with ~prefix:"--autostart" token
                         || String.starts_with ~prefix:"--color" token -> parse rest
    | token :: rest -> token :: parse rest
    | [] -> []
  in
  let args = parse original_args in
  let command = match args with name :: _ -> name | [] -> "" in
  let sub = match args with _ :: name :: _ -> name | _ -> "" in
  let diagnostic_bytes =
    Option.value ~default:0 (Option.bind (Sys.getenv_opt "REX_FAKE_STDERR_BYTES") int_of_string_opt)
  in
  if diagnostic_bytes > 0 then (
    for _ = 1 to Stdlib.(diagnostic_bytes / 11) do output_string stderr "diagnostic " done;
    output_char stderr '\n'; flush stderr);
  if present "REX_FAKE_DOWN" then fail ("Error: connect to " ^ endpoint ^ ": connection refused");
  if Sys.getenv_opt "REX_FAKE_FAIL" = Some command then
    fail ~code:7 ("Error: fake " ^ command ^ " failure");
  let state_path = Sys.getenv "REX_FAKE_STATE" in
  let state = ref
      (if Sys.file_exists state_path then Yojson.Safe.from_file state_path
       else obj [ "sessions", obj []; "blocks", obj []; "windows", obj [];
                  "counter", `Int 0; "selected", `List [] ])
  in
  let save () = write state_path (Yojson.Safe.to_string !state) in
  let update collection key value = set collection (put key value (field collection !state)) state in
  let fresh kind =
    let count = i "counter" !state + 1 in
    set "counter" (`Int count) state;
    Printf.sprintf "%s:%08x-fake-%04d" kind (0x1a14100 + count) count
  in
  let resolve_session target =
    let sessions = entries "sessions" !state in
    if List.mem_assoc target sessions then target
    else
      let rec choose = function
        | [] -> not_found "session" target
        | predicate :: rest ->
            (match List.filter (fun (id, value) -> predicate id value) sessions with
            | [ (id, _) ] -> id
            | [] -> choose rest
            | _ -> fail ~code:3 (Printf.sprintf "Error: session target %S is ambiguous" target))
      in
      choose [ (fun _ value -> s "label" value = target);
               (fun id _ -> String.ends_with ~suffix:target id) ]
  in
  let session_or_die target = List.assoc (resolve_session target) (entries "sessions" !state) in
  let block_or_die session id =
    match List.assoc_opt id (entries "blocks" !state) with
    | Some value when s "session" value = session -> value
    | _ -> not_found "block" id
  in
  let append key value json = put key (`List (xs key json @ [ value ])) json in
  let new_window session label =
    let id = fresh "window" in
    update "windows" id (obj [ "session", string session; "label", string label; "blocks", `List [] ]);
    update "sessions" session (append "windows" (string id) (List.assoc session (entries "sessions" !state)));
    id
  in
  let new_block session window label cwd cmd =
    let id = fresh "block" in
    update "blocks" id
      (obj [ "session", string session; "window", string window; "label", string label;
             "cwd", cwd; "command", `List (List.map string cmd);
             "child", obj [ "pid", `Int 4242; "user_id", `Int 501; "name", string "zsh" ];
             "last_exit", `Null; "placement", string "placed";
             "status", `List [ obj [ "state", string "working"; "app", string "claude-code";
                                      "updated_at", string "now" ] ] ]);
    update "windows" window (append "blocks" (string id) (List.assoc window (entries "windows" !state)));
    id
  in
  let output value =
    print_endline (if Sys.getenv_opt "REX_FAKE_INVALID" = Some command then "{not json"
                   else Yojson.Safe.to_string value)
  in
  let opt key fallback = option args key fallback in
  let positional index = List.nth (positionals args) index in
  match command, sub with
  | "whoami", _ -> output (obj [ "endpoint", string endpoint; "account", string "tester";
                                  "session_count", `Int (List.length (entries "sessions" !state)) ])
  | "ls", _ -> output (obj [ "sessions", `List (List.map (fun (id, value) ->
      obj [ "session_id", string id; "label", field "label" value ]) (entries "sessions" !state)) ])
  | "session", "inspect" ->
      let id = resolve_session (positional 1) in
      let session = session_or_die id in
      output (obj [ "session_id", string id; "label", field "label" session;
                    "windows", `List (List.map (fun window ->
                      let id = match window with `String id -> id | _ -> assert false in
                      obj [ "window_id", window;
                            "label", field "label" (List.assoc id (entries "windows" !state)) ]) (xs "windows" session)) ])
  | "block", "inspect" ->
      let session = opt "-s" "" and id = positional 1 in
      let entry = block_or_die session id in
      output (obj [ "session_id", string session;
                    "block", obj [ "block_id", string id; "label", field "label" entry;
                                   "placement", field "placement" entry; "window_id", field "window" entry ] ])
  | "block", "call" ->
      let entry = block_or_die (opt "-s" "") (opt "-b" "") in
      (match positional 1 with
      | "process" -> output (obj [ "last_exit", field "last_exit" entry; "child", field "child" entry;
                                   "home", string "/fake/home" ])
      | "program_status" -> output (obj [ "records", field "status" entry ])
      | method_ -> fail ~code:2 ("Error: unknown method " ^ method_))
  | "client", "ls" ->
      let clients =
        [ obj [ "client_id", string "client:01a1-peer" ];
          obj [ "client_id", string "client:01a1-null"; "info", `Null ];
          obj [ "client_id", string "client:01a1-unregistered"; "info", string "unknown" ];
          obj [ "client_id", string "client:01a1-cli"; "info", obj [ "kind", string "cli" ] ] ]
      in
      let client id label = obj [ "client_id", string id; "info", obj [ "kind", string "app"; "label", string label ] ] in
      let clients = clients @ (if present "REX_FAKE_NO_APP" then [] else [ client app "Fake Mac" ])
                            @ (if present "REX_FAKE_TWO_APPS" then [ client other_app "Other Mac" ] else []) in
      output (obj [ "clients", `List clients ])
  | "actions", _ ->
      let actions = [ obj [ "name", string "client.window.new" ]; obj [ "name", string "client.tab.goto" ] ] in
      output (obj [ "actions", `List (actions @ if present "REX_FAKE_NO_SELECT" then [] else
        [ obj [ "name", string "session.select";
                "args", obj [ "properties", obj [ "session_id", obj []; "window_id", obj [] ] ] ] ]) ])
  | "do", _ ->
      let payload = Yojson.Safe.from_string (opt "--args" "{}") in
      ignore (session_or_die (s "session_id" payload));
      let client = Option.fold ~none:`Null ~some:string (List.assoc_opt "-C" !globals) in
      set "selected" (`List (xs "selected" !state @ [ put "client" client payload ])) state;
      save (); print_endline "null"
  | "new", _ ->
      let label = match positionals args with value :: _ -> value | [] -> "generated-label" in
      let session = fresh "session" in
      update "sessions" session (obj [ "label", string label; "windows", `List [] ]);
      let window = new_window session (opt "--window" "Window") in
      let cwd = match opt "-c" "" with "" -> `Null | path -> string path in
      let block = new_block session window (opt "--window" "shell") cwd (tail args) in
      save ();
      output (obj [ "session_id", string session;
                    "initial_windows", `List [ obj [ "window_id", string window;
                      "block_ids", `List [ string block ]; "revision", `Int 1 ] ] ])
  | "run", _ ->
      let session = opt "-s" "" in ignore (session_or_die session);
      let window = new_window session (opt "--label" "Window") in
      let cwd = match opt "-c" "" with "" -> `Null | path -> string path in
      let block = new_block session window (opt "--label" "") cwd (tail args) in
      save (); output (obj [ "window_id", string window; "block_ids", `List [ string block ]; "revision", `Int 2 ])
  | "split", _ ->
      let session = opt "-s" "" and anchor = opt "-b" "" in
      ignore (session_or_die session);
      let window = s "window" (block_or_die session anchor) in
      let cwd = match opt "-c" "" with "" -> `Null | path -> string path in
      let block = new_block session window (opt "--label" "") cwd (tail args) in
      save (); output (obj [ "anchor_block_id", string anchor; "block_ids", `List [ string block ];
                            "split_ids", `List [ string "split:fake" ]; "revision", `Int 3 ])
  | "focus", _ ->
      let session = opt "-s" "" and id = positional 0 in ignore (block_or_die session id);
      let focused = match field "focused" !state with `List values -> values | _ -> [] in
      set "focused" (`List (focused @ [ string id ])) state; save ()
  | _ -> fail ~code:2 ("Error: unsupported fake rex command: " ^ String.concat " " args)
