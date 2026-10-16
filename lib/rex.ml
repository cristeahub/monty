(* Native Rex terminal backend. Every tab, split and session is created with the
   installed rex CLI; Monty records the returned full IDs beside the worker's
   durable state and never guesses a target from labels or UI focus. *)

let ( let* ) = Result.bind

(* Rex sets these in every terminal it opens. REX_CLIENT is an optional default
   for -C. They describe where the caller runs, never a worker identity. *)
let environment_names = [ "REX_SESSION"; "REX_BLOCK"; "REX_SERVER"; "REX_CLIENT" ]

let id_prefix = function
  | "REX_SESSION" -> Some "session:"
  | "REX_BLOCK" -> Some "block:"
  | "REX_CLIENT" -> Some "client:"
  | _ -> None

let validate_binding name value =
  if not (List.mem name environment_names) then
    Error (Printf.sprintf "%s is not a Rex context variable" name)
  else if
    value = "" || String.length value > 1024
    || not (String.for_all (fun c -> Char.code c > 0x20 && Char.code c < 0x7f) value)
  then Error (Printf.sprintf "%s must be a printable value without spaces" name)
  else
    match id_prefix name with
    | Some prefix when not (String.starts_with ~prefix value) ->
        Error (Printf.sprintf "%s must start with %s" name prefix)
    | _ -> Ok ()

type context = {
  server : string option;
  session : string option;
  block : string option;
  client : string option;
}

(* Malformed values are ignored rather than fatal: a stale or hand-edited shell
   must not block ordinary Monty commands. *)
let context_of_env ?(getenv = Sys.getenv_opt) () =
  let get name =
    match getenv name with
    | Some value when validate_binding name value = Ok () -> Some value
    | _ -> None
  in
  { server = get "REX_SERVER"; session = get "REX_SESSION";
    block = get "REX_BLOCK"; client = get "REX_CLIENT" }

let bindings_of_context context =
  [ ("REX_SERVER", context.server); ("REX_SESSION", context.session);
    ("REX_BLOCK", context.block); ("REX_CLIENT", context.client) ]
  |> List.filter_map (fun (name, value) -> Option.map (fun value -> (name, value)) value)

let inside_rex context = context.session <> None

(* ---- CLI invocation ------------------------------------------------------ *)

type failure = Not_found of string | Failed of string

let failure_message = function Not_found message | Failed message -> message

let argv ?server args =
  [ "rex"; "--autostart=false" ]
  @ (match server with Some server -> [ "-S"; server ] | None -> [])
  @ args

let render args = args |> List.map Shell.quote |> String.concat " "

let read_all channel = try In_channel.input_all channel with Sys_error _ -> ""

(* stdout carries JSON, stderr carries diagnostics; they must stay separate.
   rex exits 3 with "... not found" for a missing session, block or client. *)
let invoke ?server args =
  let args = argv ?server args in
  try
    let stdout, stdin, stderr =
      Unix.open_process_args_full (List.hd args) (Array.of_list args) (Unix.environment ())
    in
    close_out_noerr stdin;
    let out = read_all stdout in
    let err = read_all stderr in
    match Unix.close_process_full (stdout, stdin, stderr) with
    | Unix.WEXITED 0 -> Ok out
    | status ->
        let detail = String.trim (err ^ out) in
        let message =
          Printf.sprintf "%s failed (%s): %s" (render args)
            (match status with
            | Unix.WEXITED code -> Printf.sprintf "exit %d" code
            | Unix.WSIGNALED signal -> Printf.sprintf "signal %d" signal
            | Unix.WSTOPPED signal -> Printf.sprintf "stopped %d" signal)
            detail
        in
        if status = Unix.WEXITED 3 && Home.contains_substring (String.lowercase_ascii detail) "not found"
        then Error (Not_found message)
        else Error (Failed message)
  with
  | Unix.Unix_error (error, fn, arg) ->
      Error (Failed (Printf.sprintf "%s: %s(%s): %s" (render args) fn arg (Unix.error_message error)))
  | Sys_error message -> Error (Failed (Printf.sprintf "%s: %s" (render args) message))

let invoke_json ?server args =
  let* out = invoke ?server args in
  try Ok (Yojson.Safe.from_string out)
  with Yojson.Json_error _ ->
    Error (Failed (Printf.sprintf "%s returned invalid JSON: %s" (render (argv ?server args)) (String.trim out)))

let member = Yojson.Safe.Util.member

let string_member name json =
  match member name json with `String value when value <> "" -> Some value | _ -> None

let first_block json =
  match member "block_ids" json with
  | `List (`String id :: _) when String.starts_with ~prefix:"block:" id -> Some id
  | _ -> None

(* ---- Read-only inspection ------------------------------------------------- *)

let endpoint ?server () =
  match invoke_json ?server [ "whoami"; "--json" ] with
  | Ok json -> (
      match string_member "endpoint" json with
      | Some endpoint -> Ok endpoint
      | None -> Error "rex whoami did not report the server endpoint")
  | Error failure ->
      Error
        (Printf.sprintf
           "the Rex server is not reachable: %s. Open the Rex app (its server runs with it) or point REX_SERVER at a running server."
           (failure_message failure))

let session_live ?server session =
  match invoke_json ?server [ "session"; "inspect"; session; "--json" ] with
  | Ok _ -> Ok true
  | Error (Not_found _) -> Ok false
  | Error (Failed message) -> Error message

(* A usable split anchor is a placed block; its window is recorded with the worker. *)
let block_window ?server ~session block =
  match invoke_json ?server [ "block"; "inspect"; "-s"; session; block; "--json" ] with
  | Ok json ->
      let block = member "block" json in
      if string_member "placement" block = Some "placed" then
        Ok (string_member "window_id" block)
      else Error (Printf.sprintf "Rex block %s is not placed in a window" (Option.value ~default:"?" (string_member "block_id" block)))
  | Error failure -> Error (failure_message failure)

(* The app that will display Monty sessions: REX_CLIENT, else the only connected app. *)
let app_client ?server hint =
  let* json =
    invoke_json ?server [ "client"; "ls"; "--json"; "--all" ]
    |> Result.map_error failure_message
  in
  let clients = match member "clients" json with `List clients -> clients | _ -> [] in
  let id client = string_member "client_id" client in
  let is_app client = match member "info" client with
    | `Assoc _ as info -> string_member "kind" info = Some "app"
    | _ -> false in
  match hint with
  | Some wanted ->
      if List.exists (fun client -> id client = Some wanted) clients then Ok wanted
      else Error (Printf.sprintf "REX_CLIENT %s is not connected to the Rex server" wanted)
  | None -> (
      match List.filter is_app clients |> List.filter_map id with
      | [ only ] -> Ok only
      | [] ->
          Error
            "no Rex app is connected to the server, so Monty cannot show a new session; open the Rex app with Remote Control enabled, or launch from inside a Rex terminal with --target tab or split"
      | _ ->
          Error
            "several Rex apps are connected; set REX_CLIENT to the client ID (rex client ls) that should show Monty sessions")

let client_supports ?server client action =
  let* json =
    invoke_json ?server [ "-C"; client; "actions"; "--json" ]
    |> Result.map_error failure_message
  in
  let actions = match member "actions" json with `List actions -> actions | _ -> [] in
  if List.exists (fun entry -> string_member "name" entry = Some action) actions then Ok ()
  else
    Error
      (Printf.sprintf
         "the connected Rex app %s does not offer the %s action; enable Remote Control in the Rex app or launch from inside a Rex terminal with --target tab or split"
         client action)

let select ?server ~client ~session ~window () =
  let args =
    `Assoc ([ ("session_id", `String session) ]
      @ match window with Some window -> [ ("window_id", `String window) ] | None -> [])
  in
  invoke ?server [ "-C"; client; "do"; "session.select"; "--args"; Yojson.Safe.to_string args ]
  |> Result.map ignore |> Result.map_error failure_message

(* ---- Monty-owned session ---------------------------------------------------- *)

let home_session_path home = Filename.concat (Filename.concat home ".monty") "rex-session.json"

let read_json_regular path =
  let* existing = State_store.lstat path in
  match existing with
  | None -> Ok None
  | Some { Unix.st_kind = Unix.S_REG; _ } -> State_store.read_json ~path
  | Some _ -> Error (Printf.sprintf "%s must be a regular file" path)

let label_limit = 120

(* Labels are display text only; long titles are cut on a UTF-8 boundary. *)
let label_of_title title =
  let title = String.trim title in
  let title =
    if String.length title <= label_limit then title
    else
      let rec boundary index =
        if index > 0 && Char.code title.[index] land 0xC0 = 0x80 then boundary (index - 1) else index
      in
      String.sub title 0 (boundary label_limit)
  in
  if title = "" then "Monty worker" else title

(* Launches without a caller session land in one Monty-owned session on the
   same server, recorded under the home and verified live before reuse. The
   launch that follows shows the new window in the app. *)
let ensure_home_session ~home ?server ~endpoint () =
  let path = home_session_path home in
  let* recorded = read_json_regular path in
  let* reusable =
    match recorded with
    | Some json when string_member "server" json = Some endpoint -> (
        match string_member "session_id" json with
        | Some session ->
            let* live = session_live ?server session in
            Ok (if live then Some session else None)
        | None -> Ok None)
    | _ -> Ok None
  in
  match reusable with
  | Some session -> Ok session
  | None ->
      let* json =
        invoke_json ?server [ "new"; "monty"; "--json" ] |> Result.map_error failure_message
      in
      let* session =
        match string_member "session_id" json with
        | Some session -> Ok session
        | None -> Error "rex new returned no session ID; inspect rex ls before retrying"
      in
      let* () =
        State_store.with_lock ~home (fun () ->
            State_store.write_json_atomic ~path
              (`Assoc
                [ ("schema", `String "monty:rex-session:v1"); ("server", `String endpoint);
                  ("session_id", `String session);
                  ("created_at", `String (Worker_memory.now_utc ())) ]))
      in
      Ok session

(* ---- Placement plan --------------------------------------------------------- *)

type placement =
  | Tab of { session : string; show : string option }  (* show: app client to select *)
  | Split of { session : string; block : string; window : string option }
  | Session of { client : string }  (* one Rex session per worker, shown in the app *)

type plan = { server : string option; endpoint : string; placement : placement }

(* Runs before any worker reservation and makes no Rex mutation except the
   Monty-owned session a context-less tab launch needs. *)
let preflight ~home ~target =
  let context = context_of_env () in
  let server = context.server in
  let* endpoint = endpoint ?server () in
  let need_client () =
    let* client = app_client ?server context.client in
    let* () = client_supports ?server client "session.select" in
    Ok client
  in
  let* placement =
    match target with
    | Terminal.Window ->
        let* client = need_client () in
        Ok (Session { client })
    | Terminal.Split -> (
        match (context.session, context.block) with
        | Some session, Some block ->
            let* window = block_window ?server ~session block in
            Ok (Split { session; block; window })
        | _ ->
            Error
              "a Rex split needs the caller's own pane: run from inside a Rex terminal (REX_SESSION and REX_BLOCK set), or use --target tab or window")
    | Terminal.Tab -> (
        match context.session with
        | Some session ->
            let* live = session_live ?server session in
            if live then Ok (Tab { session; show = None })
            else Error (Printf.sprintf "REX_SESSION %s is not a live session on %s; unset it or run from a live Rex terminal" session endpoint)
        | None ->
            let* client = need_client () in
            let* session = ensure_home_session ~home ?server ~endpoint () in
            Ok (Tab { session; show = Some client }))
  in
  Ok { server; endpoint; placement }

(* ---- Durable terminal reference --------------------------------------------- *)

let schema = "monty:rex-terminal:v1"
let reference_file = "rex-terminal.json"
let reference_path worker_dir = Filename.concat worker_dir reference_file

type reference = {
  ref_server : string;
  session_id : string;
  window_id : string option;
  block_id : string;
  client_id : string option;
  target : string;
  launch_script : string;
  requested_at : string;
}

let reference_json r =
  let optional name = function None -> (name, `Null) | Some value -> (name, `String value) in
  `Assoc
    [ ("schema", `String schema); ("server", `String r.ref_server);
      ("session_id", `String r.session_id); optional "window_id" r.window_id;
      ("block_id", `String r.block_id); optional "client_id" r.client_id;
      ("target", `String r.target); ("launch_script", `String r.launch_script);
      ("requested_at", `String r.requested_at) ]

let read_reference worker_dir =
  let path = reference_path worker_dir in
  let* json = read_json_regular path in
  match json with
  | None -> Ok None
  | Some json -> (
      let required name =
        match string_member name json with
        | Some value -> Ok value
        | None -> Error (Printf.sprintf "%s lacks %s" path name)
      in
      match string_member "schema" json with
      | Some value when value <> schema ->
          Error (Printf.sprintf "%s has unsupported schema %s" path value)
      | _ ->
          let* ref_server = required "server" in
          let* session_id = required "session_id" in
          let* block_id = required "block_id" in
          let* target = required "target" in
          let* launch_script = required "launch_script" in
          let* requested_at = required "requested_at" in
          Ok
            (Some
               { ref_server; session_id; window_id = string_member "window_id" json;
                 block_id; client_id = string_member "client_id" json; target;
                 launch_script; requested_at }))

(* Recorded after the rex call succeeded, under the home lock, and only while the
   worker is still in the launch-requested state this attempt produced. *)
let write_reference ~home ~worker_dir reference =
  State_store.with_lock ~home (fun () ->
      let* record = Job_store.parse_job_file ~home (Worker_memory.job_file worker_dir) in
      if record.Job_store.status <> "launch-requested" then
        Error
          (Printf.sprintf
             "worker %s changed to %S while its Rex terminal was being recorded; the terminal exists as %s"
             record.id record.status reference.block_id)
      else State_store.write_json_atomic ~path:(reference_path worker_dir) (reference_json reference))

(* ---- Launch ----------------------------------------------------------------- *)

let launch ~home ~worker_dir ~plan ~target ~workdir ~script_path ~label =
  let server = plan.server in
  let label = label_of_title label in
  let command = [ "--"; "/bin/zsh"; "-l"; script_path ] in
  let common = [ "--json"; "-c"; workdir; "--keep-open"; "--shell"; "none" ] in
  let missing command what = Printf.sprintf "rex %s returned no %s ID; inspect the session before requesting another launch" command what in
  let* session_id, window_id, block_id, client_id =
    match plan.placement with
    | Tab { session; show } ->
        let* json =
          invoke_json ?server ([ "run"; "-s"; session; "--label"; label ] @ common @ command)
          |> Result.map_error failure_message
        in
        let* block = Option.to_result ~none:(missing "run" "block") (first_block json) in
        Ok (session, string_member "window_id" json, block, show)
    | Split { session; block = anchor; window } ->
        let* json =
          invoke_json ?server
            ([ "split"; "-s"; session; "-b"; anchor; "--split"; "right"; "--label"; label ] @ common @ command)
          |> Result.map_error failure_message
        in
        let* block = Option.to_result ~none:(missing "split" "block") (first_block json) in
        Ok (session, window, block, None)
    | Session { client } ->
        let* json =
          invoke_json ?server ([ "new"; "monty: " ^ label; "--window"; label ] @ common @ command)
          |> Result.map_error failure_message
        in
        let* session = Option.to_result ~none:(missing "new" "session") (string_member "session_id" json) in
        let first = match member "initial_windows" json with `List (first :: _) -> first | _ -> `Null in
        let* block = Option.to_result ~none:(missing "new" "block") (first_block first) in
        Ok (session, string_member "window_id" first, block, Some client)
  in
  (* Record the terminal before showing it, so a display failure leaves a
     reference that resume can focus instead of a second tab. *)
  let* () =
    write_reference ~home ~worker_dir
      { ref_server = plan.endpoint; session_id; window_id; block_id; client_id;
        target = Terminal.target_to_string target; launch_script = script_path;
        requested_at = Worker_memory.now_utc () }
  in
  match client_id with
  | Some client ->
      select ?server ~client ~session:session_id ~window:window_id ()
      |> Result.map_error (fun message ->
             Printf.sprintf "Rex created %s but could not show it: %s; monty resume will focus it" block_id message)
  | None -> Ok ()

(* ---- Reconnect ---------------------------------------------------------------- *)

type liveness = Live of string | Exited of int option | Gone

(* Reconnect always asks the recorded server, never the caller's. *)
let liveness reference =
  match
    invoke_json ~server:reference.ref_server
      [ "block"; "call"; "-s"; reference.session_id; "-b"; reference.block_id; "process" ]
  with
  | Ok json -> (
      match member "child" json with
      | `Assoc _ as child ->
          Ok (Live (Option.value ~default:"process" (string_member "name" child)))
      | _ -> (
          match member "exit_code" (member "last_exit" json) with
          | `Int code -> Ok (Exited (Some code))
          | _ -> Ok (Exited None)))
  | Error (Not_found _) -> Ok Gone
  | Error (Failed message) ->
      Error
        (Printf.sprintf
           "cannot verify the recorded Rex terminal %s: %s; restore Rex connectivity before requesting another launch"
           reference.block_id message)

let focus reference =
  let server = reference.ref_server in
  let* () =
    invoke ~server [ "focus"; "-s"; reference.session_id; reference.block_id ]
    |> Result.map ignore |> Result.map_error failure_message
  in
  match reference.client_id with
  | Some client ->
      select ~server ~client ~session:reference.session_id ~window:reference.window_id ()
  | None -> Ok ()

let program_status reference =
  invoke_json ~server:reference.ref_server
    [ "block"; "call"; "-s"; reference.session_id; "-b"; reference.block_id; "program_status" ]
  |> Result.map_error failure_message

let describe reference =
  String.concat "\n"
    ([ "server: " ^ reference.ref_server; "session: " ^ reference.session_id ]
    @ (match reference.window_id with Some w -> [ "window: " ^ w ] | None -> [])
    @ [ "block: " ^ reference.block_id ]
    @ (match reference.client_id with Some c -> [ "client: " ^ c ] | None -> [])
    @ [ "target: " ^ reference.target; "requested: " ^ reference.requested_at ])

(* ---- Program status (OSC 7501) --------------------------------------------- *)

let status_text value =
  let clean = Buffer.create (String.length value) in
  String.iter (fun c -> if Char.code c >= 0x20 && Char.code c <> 0x7f then Buffer.add_char clean c) value;
  let text = Buffer.contents clean in
  Base64.encode_exn (if String.length text > 512 then String.sub text 0 512 else text)

let status_report ?(id = "monty") ?kind ?title ?msg state =
  let pairs =
    [ "state=" ^ state; "id=" ^ id; "app=monty" ]
    @ (match kind with Some kind -> [ "kind=" ^ kind ] | None -> [])
    @ (match title with Some title -> [ "title=" ^ status_text title ] | None -> [])
    @ match msg with Some msg -> [ "msg=" ^ status_text msg ] | None -> []
  in
  "\027]7501;" ^ String.concat ":" pairs ^ "\027\\"
