open Lwt.Infix
open Context_store
open Conversation_store
let request ~home meth path body =
  let metadata_path=file home ["client-service.json"] in
  if not(Sys.file_exists metadata_path) then fail 503 "Monty client service is not running; run monty service start --config FILE";
  let state=read metadata_path in
  let base=s "url" state in
  let uri=Uri.of_string base in
  if Uri.scheme uri<>Some "http" || Uri.host uri<>Some "127.0.0.1" then fail 500 "unsafe service address";
  let token=String.trim (Shell.read_file (file home ["client-service-token"])) in
  let headers=Cohttp.Header.of_list ["authorization","Bearer " ^ token;"content-type","application/json"] in
  let body=Cohttp_lwt.Body.of_string (Yojson.Safe.to_string body) in
  Lwt_unix.with_timeout 35. (fun () ->
    Cohttp_lwt_unix.Client.call ~headers ~body meth (Uri.of_string (base ^ path)) >>= fun (response,body) ->
    Cohttp_lwt.Body.to_string body >|= fun text ->
    let json=Yojson.Safe.from_string text in
    let code=Cohttp.Code.code_of_status (Cohttp.Response.status response) in
    if code>=400 then fail code (Yojson.Safe.to_string json);json)
let health ~home = Lwt.catch (fun () ->
    Lwt_unix.with_timeout 1. (fun () -> request ~home `GET "/v1/health" `Null) >|= Option.some)
    (function
      | Failure(503,_) | Lwt_unix.Timeout
      | Unix.Unix_error((Unix.ECONNREFUSED|Unix.ECONNRESET|Unix.ETIMEDOUT),_,_) -> Lwt.return_none
      | error -> Lwt.fail error)
let ensure_service ?config_path ?port ~home () =
  let config_path=Option.map Unix.realpath config_path in
  let metadata=file home ["client-service.json"] in
  let saved () = if Sys.file_exists metadata then read metadata else `Null in
  let ready result =
    let state=saved () in
    if Option.fold ~none:false ~some:(fun path -> path<>s "config_path" state) config_path ||
       Option.fold ~none:false ~some:(fun port -> port<>0 && port<>i "port" state) port then
      fail 409 "service is running with different startup settings; run monty service stop first";
    Lwt.return result in
  health ~home >>= function
  | Some result -> ready result
  | None ->
      let state=saved () in
      let path=match config_path,field "config_path" state with
        | Some path,_ | None,`String path when path<>"" -> path
        | _ -> fail 503 "Monty client service is not running and no startup configuration is saved; run monty service start --config FILE" in
      let port=match port,field "port" state with Some port,_->port|None,`Null->0|None,_->i "port" state in
      Client_service.start_background ~home ~config_path:path ~port;
      let deadline=Unix.gettimeofday ()+.15. in
      let rec wait () = health ~home >>= function
        | Some result -> ready result
        | None when Unix.gettimeofday ()<deadline -> Lwt_unix.sleep 0.1 >>= wait
        | None -> fail 503 ("Monty client service did not become ready; inspect " ^ file home ["client-service.log"]) in
      wait ()
let service_running ~home =
  let path=file home ["client-service.lock"] in
  if not(Sys.file_exists path) then false else
  let fd=get(State_store.open_lock_file path) in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    try Unix.lockf fd Unix.F_TLOCK 0;false with
    | Unix.Unix_error((Unix.EAGAIN|Unix.EACCES),_,_) -> true)
let stop_service ~home =
  if not(service_running ~home) then Lwt.return_false else
  let epoch=field "epoch" (read (file home ["client-service.json"])) in
  let stopped () = not(service_running ~home) || field "epoch" (read (file home ["client-service.json"]))<>epoch in
  let deadline=Unix.gettimeofday ()+.15. in
  let rec wait () =
    if stopped () then Lwt.return_true else
    if Unix.gettimeofday ()>=deadline then fail 503 "Monty service did not stop; inspect .monty/client-service.log" else
    Lwt_unix.sleep 0.1 >>= wait in
  health ~home >>= function
  | None -> wait ()
  | Some state when field "epoch" state<>epoch -> Lwt.return_true
  | Some state ->
      Lwt.catch (fun () -> Lwt_unix.with_timeout 1. (fun () ->
        request ~home `POST "/v1/shutdown" (obj ["epoch",field "epoch" state])) >|= fun _ -> ())
        (fun error -> if stopped () then Lwt.return_unit else
          match error with
          | Failure _ -> Lwt.fail error
          | _ -> health ~home >>= function None -> Lwt.return_unit | Some _ -> Lwt.fail error) >>= wait
let command ~home id json =
  request ~home `POST ("/v1/commands/" ^ component id) json >>= fun _ ->
  let rec poll () = request ~home `GET ("/v1/commands/" ^ id) `Null >>= fun receipt ->
    match s "status" receipt with
    | "finished" -> Lwt.return (field "result" receipt)
    | "accepted"|"running" -> Lwt_unix.sleep 0.2 >>= poll
    | _ -> Lwt.fail (Failure (409,"command " ^ id ^ ": " ^ Yojson.Safe.to_string receipt)) in poll ()
let launch ~home ~profile ~codex_command ~codex_yolo ~mode ~session_id ~prompt ~instructions ~context ~request_id ~worker =
  if not (List.mem mode ["fresh";"picker";"last";"exact"]) || (mode="exact" && session_id=None) then
    fail 400 "choose fresh, picker, last, or exact with --session";
  let expect = if Unix.isatty Unix.stdin && Unix.isatty Unix.stdout then
      Some (get (Process.command_exists_with_arguments "expect")) else None in
  let home=Unix.realpath home in
  let id=Option.value ~default:(random_id ()) request_id in
  let workspaces =
    let rec collect n acc = match Sys.getenv_opt (Printf.sprintf "MONTY_WORKSPACE_%d" n) with
      | Some path -> collect (n+1) (acc @ [string (Unix.realpath path)]) | None -> acc in
    match (if worker then collect 1 [] else []) with [] -> [string (Unix.realpath (Sys.getcwd ()))] | xs -> xs in
  let read_input = function None->"" | Some path ->
    let st=Unix.stat path in if st.st_size>65536 then fail 413 "instruction/context file too large";
    Shell.read_file path in
  let instruction_text=String.concat "\n\n" [read_input instructions;read_input context] in
  let environment=Unix.environment () |> Array.to_list |> List.filter_map (fun binding ->
    match String.index_opt binding '=' with None->None|Some pos ->
      let key=String.sub binding 0 pos in
      if worker && String.starts_with ~prefix:"MONTY_" key && not(List.mem key ["MONTY_API_TOKEN";"MONTY_FAULT_INJECT"])
      then Some(key,string(String.sub binding (pos+1) (String.length binding-pos-1))) else None) in
  let context=obj ["workspaces",`List workspaces;"instructions",string instruction_text;"environment",`Assoc environment] in
  let flow () =
    ensure_service ~home () >>= fun health ->
    let native_home=Option.value ~default:(Filename.concat (Sys.getenv "HOME") ".codex") (Sys.getenv_opt "CODEX_HOME") |> Unix.realpath in
    let matches p = field "harness" p=string "codex" && field "scope" p=string(sha native_home) in
    let profiles=xs "profiles" health in
    let p=match profile with
      | Some id -> (match List.find_opt (fun p -> s "id" p=id) profiles with
          | Some p when matches p -> p
          | Some _ -> fail 409 "terminal CODEX_HOME does not match service profile"
          | None -> fail 400 "profile not configured in service")
      | None -> (match List.filter matches profiles with
          | [p] -> p
          | [] -> fail 409 ("No Codex service profile matches CODEX_HOME " ^ native_home ^
              "; add this account home to the service's profile configuration, then run monty service stop and monty service start")
          | _ -> fail 409 "Multiple Codex service profiles match CODEX_HOME; select one with --profile or MONTY_API_PROFILE") in
    get(Skill.ensure_at ~home ~harness:Harness.Codex ~directory:native_home ~monty_command:(Unix.realpath Sys.executable_name));
    let profile=s "id" p in
    (if mode="fresh" then
      command ~home id (obj ["op",string "start";"profile",string profile;"context",context;"codex_yolo",`Bool codex_yolo;
        "title",string (Option.value ~default:"Monty conversation" (if worker then Sys.getenv_opt "MONTY_JOB_TITLE" else None));
        "task_keys",`List (match (if worker then Sys.getenv_opt "MONTY_TASK_KEY" else None) with Some k when k<>""->[string k]|_->[])])
    else request ~home `GET "/v1/conversations" `Null >>= fun catalog ->
      let candidates=Context_store.list catalog |> List.filter (fun c -> field "purged" c<>`Bool true &&
        List.exists (fun sess -> s "profile" sess=profile &&
          match session_id with None->field "id" sess=field "selected_session" c|Some id->s "native_id" sess=id) (xs "sessions" c)) in
      let candidates=List.sort (fun a b -> compare (n "created_at" b) (n "created_at" a)) candidates in
      let c=match candidates with
        | [] -> fail 404 "No registered conversation. Explicitly import/attach the selected native thread through the API first."
        | [c] -> c | c::_ when mode="last" || session_id<>None -> c
        | cs ->
            List.iteri (fun n c -> Printf.eprintf "%d. %s (%s)\n" (n+1) (s "title" c) (s "id" c)) cs;
            Printf.eprintf "Select conversation: %!";
            let choice=try int_of_string (read_line ()) with _ -> fail 400 "invalid selection" in
            (try List.nth cs (choice-1) with _ -> fail 400 "invalid selection") in
      let sess=List.find (fun sess -> s "profile" sess=profile &&
        match session_id with None -> field "id" sess=field "selected_session" c | Some id->s "native_id" sess=id) (xs "sessions" c) in
      if field "observed" sess<>`Bool true then fail 409 "Stored history requires explicit detached resume through the API";
      Lwt.return (put "selected_session" (field "id" sess) c)) >>= fun c ->
    let sess=session c (s "selected_session" c) in
    if field "harness" sess<>string "codex" || field "scope" sess<>field "scope" p then
      fail 409 "stored terminal session does not match the configured Codex account scope";
    (* A terminal always targets the endpoint registered by the service, never a
       native default which might create an unobserved standalone runtime. *)
    let endpoint=required "endpoint" sess in
    if not(String.starts_with ~prefix:"unix://" endpoint) then fail 500 "only local Unix Codex endpoints are supported";
    let native=s "native_id" sess in
    (* Remote Codex also reads the account defaults when opening another session.
       Keep Vim enabled there as well as in this terminal's CLI overrides. *)
    Codex_rpc.connect (String.sub endpoint 7 (String.length endpoint-7)) >>= fun rpc ->
    Codex_rpc.call rpc "skills/list" (obj ["cwds",`List workspaces;"forceReload",`Bool true]) >>= fun _ ->
    Codex_rpc.call rpc "config/value/write" (obj ["keyPath",string "tui.vim_mode_default";
      "value",`Bool true;"mergeStrategy",string "replace"]) >>= fun _ ->
    Lwt_io.close rpc.oc >>= fun () ->
    (if worker then
      get(Codex_session.capture ~home (Yojson.Safe.to_string(obj ["hook_event_name",string "SessionStart";
        "source",string "resume";"session_id",string native]))));
    Lwt.return (endpoint,native) in
  Printf.eprintf "Monty command receipt: %s\n%!" id;
  let endpoint,native=try Lwt_main.run (flow ()) with
    | Failure _ as e -> raise e | _ -> fail 503 ("service unavailable; command receipt " ^ id ^ " may need inspection; no standalone fallback") in
  let command=codex_command ^ " resume --remote " ^ Shell.quote endpoint ^ " -C " ^ Shell.quote (Sys.getcwd ())
    ^ " -c " ^ Shell.quote "tui.vim_mode_default=true" ^ " -- " ^ Shell.quote native ^ Option.fold ~none:"" ~some:(fun p -> " " ^ Shell.quote p) prompt in
  match expect with
  | None -> Unix.execv "/bin/sh" [|"/bin/sh";"-c";"exec " ^ command|]
  | Some expect ->
      (* ponytail: match the native Vim footer once; remove this bridge when Codex
         honors vim_mode_default at startup instead of forcing Insert mode. *)
      Unix.putenv "MONTY_CODEX_TUI_COMMAND" ("exec " ^ command);
      Unix.execv expect [|expect;"-N";"-n";"-c";{|
set command $env(MONTY_CODEX_TUI_COMMAND)
unset env(MONTY_CODEX_TUI_COMMAND)
spawn -noecho /bin/sh -c $command
proc resize {} {
    global spawn_out
    stty rows [stty rows < /dev/fd/0] columns [stty columns < /dev/fd/0] < $spawn_out(slave,name)
}
resize
trap resize WINCH
set initialized 0
interact -o -nobuffer -re {Vim: (Insert|Normal)} {
    if {!$initialized} {
        set initialized 1
        if {$interact_out(1,string) eq "Insert"} {send -- "\033"}
    }
}
set result [wait]
if {[lindex $result 2] != 0 || [llength $result] > 4} {exit 1}
exit [lindex $result 3]
|}|]
