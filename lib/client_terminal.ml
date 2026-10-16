open Lwt.Infix
open Json_fields
open Conversation_store
let request=Client_http.request
let health ~home = Lwt.catch (fun () ->
    Cohttp_lwt_unix.Private.IO.catch (fun () ->
      Lwt_unix.with_timeout 1. (fun () -> request ~home `GET "/v1/health" `Null)) >>= function
    | Ok result -> Lwt.return_some result
    | Error error -> Lwt.fail error)
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
  let deadline=Unix.gettimeofday ()+.25. in
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
let profile_for ~home harness =
  ensure_service ~home () >|= fun health ->
  let directory=get(Skill.agent_directory harness) |> Unix.realpath in
  let candidates=xs "profiles" health |> List.filter(fun p -> field "harness" p=string(Harness.to_string harness) && field "scope" p=string(sha directory)) in
  match Sys.getenv_opt "MONTY_API_PROFILE",candidates with
  | Some id,_ -> (match List.find_opt(fun p->s "id" p=id) candidates with Some p->s "id" p|None->fail 409 "selected service profile does not match the account scope")
  | None,[p]->s "id" p|_->fail 409 "configure/select one server profile matching the native account directory"
let headless_async ~wait ~resume (options:Launcher.options) worker =
  let home=Unix.realpath options.home in
  let options={options with Launcher.home=home} in
  match Headless.preflight_worker ~explicit_resume:resume options worker with Error message->Lwt.return(Error message)|Ok (plan,_) ->
  let options=plan.Headless.options in
  Lwt.catch (fun () ->
    profile_for ~home:options.home options.harness >>= fun profile ->
    let id=random_id() in
    request ~home:options.home `POST ("/v1/commands/" ^ id)
      (obj["op",string "headless_start";"profile",string profile;"command",string options.harness_command;"worker",string worker;"resume",`Bool resume;
        "codex_yolo",`Bool options.codex_yolo;"wt_command",string options.wt_command;
        "environment",`Assoc(Unix.environment () |> Array.to_list |> List.filter_map(fun binding->
          let pos=String.index binding '=' in let key=String.sub binding 0 pos in
          if Worker_environment.allowed key then
            Some(key,string(String.sub binding (pos+1)(String.length binding-pos-1))) else None))]) >>= fun _ ->
    let rec poll () = request ~home:options.home `GET ("/v1/commands/" ^ id) `Null >>= fun receipt ->
      match s "status" receipt with
      | "running" when not wait && field "run_status" (field "result" receipt)=string "started" -> Lwt.return(field "result" receipt)
      | "finished" -> let result=field "result" receipt in
          if field "exit_code" result<>`Int 0 then fail 409 (s "stderr" result);
          (try Lwt.return(Yojson.Safe.from_string(s "stdout" result)) with _->fail 502 "headless supervisor returned no valid final result")
      | "accepted"|"running" -> Lwt_unix.sleep 0.1 >>= poll
      | _ -> fail 409 (Yojson.Safe.to_string receipt) in poll () >|= fun result->Ok result)
    (function Failure(_,message)->Lwt.return(Error message)|_->Lwt.return(Error "Monty server unavailable; inspect receipts before retrying; no independent headless fallback"))
let headless ~wait ~resume options worker = Lwt_main.run(headless_async ~wait ~resume options worker)
(* The newest run handoff this worker published, for native status only. *)
let handoff_outcome ~home =
  match Sys.getenv_opt "MONTY_RUN_DIR",Sys.getenv_opt "MONTY_JOB_ID" with
  | Some run_dir,Some id when id<>"" ->
      let dir=Filename.concat (Filename.concat (Run_handoff.handoff_root home) (Filename.basename run_dir)) id in
      let outcomes=try Sys.readdir dir |> Array.to_list |> List.filter (String.ends_with ~suffix:".json") |> List.filter_map (fun name ->
          try let json=Yojson.Safe.from_file (Filename.concat dir name) in Some (s "finished_at" json,s "outcome" json) with _->None)
        with Sys_error _ -> [] in
      (match List.sort compare outcomes |> List.rev with (_,outcome)::_ -> Some outcome | [] -> None)
  | _ -> None
let launch ~home ~profile ~harness ~command_override ~fork ~mode ~session_id ~prompt ~instructions ~context ~request_id ~worker ~title ~codex_yolo =
  if not(List.mem mode ["fresh";"picker";"last";"exact"]) || (mode="exact" && session_id=None) then fail 400 "choose fresh, picker, last, or exact with --session";
  let home=Unix.realpath home in
  (* The caller's own Rex placement travels with each attachment, never with the durable session. *)
  let rex=Rex.context_of_env () in
  let terminal_environment=`Assoc (List.map (fun (key,value)->key,string value) (Rex.bindings_of_context rex)) in
  let workspaces=if worker then (
    let id=match Sys.getenv_opt "MONTY_JOB_ID" with Some id when id<>""->id|_->fail 400 "worker terminal requires its durable worker identity" in
    let record=get(Job_store.find ~home ~scope:Job_store.Active id) in
    record.workspaces |> List.map(fun (workspace:Job_store.workspace_state)->string(Unix.realpath(Option.value ~default:workspace.repo workspace.worktree))))
    else [string(Unix.realpath(Sys.getcwd()))] in
  let contents = function None->""|Some path->
    if (Unix.stat path).st_size>65536 then fail 413 "instruction/context file too large";Shell.read_file path in
  let environment=Unix.environment () |> Array.to_list |> List.filter_map (fun binding ->
    let pos=String.index binding '=' in let key=String.sub binding 0 pos in
    if worker && Worker_environment.allowed key && key<>"MONTY_JOB_WORKTREE" && not(String.starts_with ~prefix:"MONTY_WORKSPACE_" key) then
      Some(key,string(String.sub binding (pos+1) (String.length binding-pos-1))) else None) in
  let environment=environment @ (if worker then ("MONTY_JOB_WORKTREE",List.hd workspaces)::List.mapi(fun i path->Printf.sprintf "MONTY_WORKSPACE_%d" (i+1),path) workspaces else []) in
  let context=obj["workspaces",`List workspaces;"instructions",string(String.concat "\n\n" [contents instructions;contents context]);"environment",`Assoc environment] in
  let flow () =
    ensure_service ~home () >>= fun health ->
    let directory=get(Skill.agent_directory harness) |> Unix.realpath in
    let matches p=field "harness" p=string(Harness.to_string harness) && field "scope" p=string(sha directory) in
    let profiles=xs "profiles" health in
    let p=match profile with
      | Some id -> (match List.find_opt(fun p->s "id" p=id && matches p) profiles with Some p->p|_->fail 409 "terminal account scope does not match its configured service profile")
      | None -> (match List.filter matches profiles with [p]->p|[]->fail 409 "no service profile matches the selected native account directory; configure its account and roots before launching"|_->fail 409 "multiple service profiles match; select --profile") in
    let pid=s "id" p in
    (if mode="fresh" then command ~home (Option.value ~default:(random_id()) request_id)
       (obj ["op",string "start";"profile",string pid;"command",Option.fold ~none:`Null ~some:string command_override;"context",context;"terminal",`Bool true;"fork",Option.fold ~none:`Null ~some:string fork;"codex_yolo",`Bool codex_yolo;"terminal_environment",terminal_environment;
         "title",string title;"task_keys",`List(match if worker then Sys.getenv_opt "MONTY_TASK_KEY" else None with Some key when key<>""->[string key]|_->[])])
     else request ~home `GET "/v1/conversations" `Null >|= fun catalog ->
       let candidates=Json_fields.list catalog |> List.filter(fun c -> field "purged" c<>`Bool true &&
         List.exists(fun sess->matches sess &&
           (match session_id with None->field "id" sess=field "selected_session" c|Some id->s "native_id" sess=id)) (xs "sessions" c))
         |> List.sort(fun a b->compare(n "created_at" b)(n "created_at" a)) in
       match candidates with
       | []->fail 404 "no registered session; import the chosen native session through Monty first, or explicitly use fresh"
       | [c]->c|c::_ when mode="last" || session_id<>None->c
       | cs->List.iteri(fun index c->Printf.eprintf "%d. %s (%s)\n" (index+1) (s "title" c) (s "id" c)) cs;
         Printf.eprintf "Select conversation: %!";
         let choice=try int_of_string(read_line()) with _->fail 400 "invalid selection" in
         try List.nth cs (choice-1) with _->fail 400 "invalid selection") >>= fun c ->
    (match session_id with None->Lwt.return c|Some native->
      let chosen=match List.find_opt(fun sess->s "native_id" sess=native && matches sess)(xs "sessions" c) with Some sess->sess|None->fail 404 "chosen exact native session was not registered" in
      if field "selected_session" c=field "id" chosen then Lwt.return c else
      command ~home(random_id())(obj["op",string "select_session";"conversation",field "id" c;"revision",field "revision" c;"session",field "id" chosen])) >>= fun c ->
    let sess=session c (s "selected_session" c) in
    if not(matches sess) || Option.fold ~none:false ~some:(fun id->s "native_id" sess<>id) session_id then fail 409 "chosen native session is not the selected conversation";
    let rows,cols=Terminal_size.get Unix.stdin in
    command ~home (random_id()) (obj ["op",string "terminal_open";"rows",`Int rows;"cols",`Int cols;"command",Option.fold ~none:`Null ~some:string command_override;"conversation",field "id" c;"selected_session",field "id" sess;
      "revision",field "revision" c;"fresh",`Bool(mode="fresh");"fork",Option.fold ~none:`Null ~some:string fork;"prompt",Option.fold ~none:`Null ~some:string prompt;
      "term",string(Option.value ~default:"xterm-256color" (Sys.getenv_opt "TERM"));"terminal_environment",terminal_environment]) >|= fun sess ->
    s "id" c,s "id" sess in
  let cid,sid=try Lwt_main.run(flow()) with Failure _ as e->raise e|_->fail 503 "Monty service unavailable; inspect receipts before retrying; no independent harness fallback" in
  let original=if Unix.isatty Unix.stdin then Some(Unix.tcgetattr Unix.stdin) else None in
  Fun.protect ~finally:(fun () -> Option.iter(fun attrs->Unix.tcsetattr Unix.stdin Unix.TCSANOW attrs) original) (fun () ->
    Option.iter(fun attrs->Unix.tcsetattr Unix.stdin Unix.TCSANOW {attrs with
      c_icanon=false;c_echo=false;c_isig=false;c_ixon=false;c_icrnl=false;c_opost=false;c_vmin=1;c_vtime=0}) original;
    let offset=ref 0 and resized=ref true in
    let prior=Sys.signal (Terminal_size.winch ()) (Sys.Signal_handle(fun _->resized:=true)) in
    Fun.protect ~finally:(fun () -> ignore(Sys.signal (Terminal_size.winch ()) prior)) (fun () ->
      let route op extra=command ~home (random_id()) (obj(["op",string op;"conversation",string cid;"selected_session",string sid] @ extra)) >|= ignore in
      (* OSC 7501 lifecycle records under id=monty, only inside Rex; the harness keeps
         the root record. A done record or zero exit never completes the Monty task. *)
      let inside_rex=Rex.inside_rex rex in
      let emit text=if inside_rex then Lwt_io.write Lwt_io.stdout text >>= fun () -> Lwt_io.flush Lwt_io.stdout else Lwt.return_unit in
      let handoff=ref None and polls=ref 0 in
      let report ?exit () =
        let state,kind,msg=match !handoff,exit with
          | Some "ready-for-review",_ -> "done",None,"handoff: ready for review"
          | Some "needs-attention",_ -> "blocked",Some "question","handoff: needs attention"
          | Some "failed",_ -> "error",None,"handoff: failed"
          | _,Some 0 -> "done",None,"worker"
          | _,Some _ -> "error",None,"worker"
          | _,None -> "working",None,"worker terminal attached" in
        let msg=match exit with Some code -> Printf.sprintf "%s · terminal exited (%d)" msg code | None -> msg in
        emit (Rex.status_report ~title ?kind ~msg state) in
      let poll_handoff () =
        incr polls;
        if not (worker && inside_rex) || !polls mod 50<>1 then Lwt.return_unit else
        let current=handoff_outcome ~home in
        if current = !handoff then Lwt.return_unit else (handoff:=current;report ()) in
      let rec output () = request ~home `GET ("/v1/terminals/" ^ sid ^ "?offset=" ^ string_of_int !offset) `Null >>= fun value ->
        let bytes=Base64.decode_exn(s "data" value) in
        Lwt_io.write Lwt_io.stdout bytes >>= fun () -> Lwt_io.flush Lwt_io.stdout >>= fun () ->
        offset:=i "offset" value;
        (if !resized && field "running" value=`Bool true then (resized:=false;let rows,cols=Terminal_size.get Unix.stdin in
          route "terminal_resize" ["rows",`Int rows;"cols",`Int cols]) else Lwt.return_unit) >>= fun () ->
        poll_handoff () >>= fun () ->
        if field "running" value=`Bool false && bytes="" then (match field "code" value with `Int code->Lwt.return (`Exited code)|_->fail 503 "native terminal outcome is unknown; inspect retained output") else Lwt_unix.sleep 0.04 >>= output in
      let rec input () = Lwt_io.read ~count:4096 Lwt_io.stdin >>= fun bytes ->
        if bytes="" then Lwt.return `Detached else route "terminal_input" ["data",string(Base64.encode_exn bytes)] >>= input in
      Lwt_main.run(report () >>= fun () -> Lwt.pick [input();output()] >>= function
        | `Detached -> emit (Rex.status_report "clear") >|= fun () -> 0
        | `Exited code -> report ~exit:code () >|= fun () -> code)))
