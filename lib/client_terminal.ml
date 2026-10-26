open Lwt.Infix
open Json_fields
open Conversation_store
let request=Client_http.request
(* Is a service answering? Only an HTTP answer proves one is there: a missing
   metadata file (503), a refused, reset or timed-out connection, a connection
   closed mid-response (cohttp wraps that read error in its own exception) and
   a cancelled probe all mean no usable service. *)
let health ~home = Lwt.catch (fun () ->
    Lwt_unix.with_timeout 1. (fun () -> request ~home `GET "/v1/health" `Null) >|= Option.some)
    (function
      | Failure(503,_) -> Lwt.return_none
      | Failure _ as error -> Lwt.fail error
      | _ -> Lwt.return_none)
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
      (* A saved hand-maintained configuration is reused as is; Monty's own
         generated file is rebuilt so newly registered projects are allowed. *)
      let path=match config_path,field "config_path" state with
        | Some path,_ -> path
        | None,`String path when path<>"" && path<>Service_profiles.path ~home -> path
        | _ -> Service_profiles.generate ~home in
      let port=match port,field "port" state with Some port,_->port|None,`Null->0|None,_->i "port" state in
      Client_service.start_background ~home ~config_path:path ~port;
      let deadline=Unix.gettimeofday ()+.15. in
      let rec wait () = health ~home >>= function
        | Some result -> ready result
        | None when Unix.gettimeofday ()<deadline -> Lwt_unix.sleep 0.1 >>= wait
        | None -> fail 503 ("Monty client service did not become ready; inspect " ^ file home ["client-service.log"]) in
      wait ()
(* The service holds its lock for its whole life, even when stopped or asleep. Testing,
   never taking, the lock keeps a probing client from blocking a service that is starting. *)
let service_running ~home =
  let path=file home ["client-service.lock"] in
  if not(Sys.file_exists path) then false else
  let fd=get(State_store.open_lock_file path) in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    try Unix.lockf fd Unix.F_TEST 0;false with
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
(* The epoch of the service that requests currently go to. *)
let saved_epoch ~home =
  let path=file home ["client-service.json"] in
  if Sys.file_exists path then field "epoch" (read path) else `Null
(* Long-lived clients outlast a slow, sleeping or briefly stopped service. [f] is a request
   that is safe to resend (a command keeps its ID, a terminal read its offset); it is
   repeated until the service attached at [epoch] answers. Only evidence ends the wait:
   another epoch in the saved metadata or a health answer (replaced), no answer while the
   service lock is free (stopped), or no answer to ten request timeouts' worth of checks a
   second apart while its process still holds the lock. [waiting] runs on each request
   left unanswered. *)
let retrying ?(waiting=ignore) ~home ~epoch f =
  let lost reason = Lwt.fail (Failure (503,"lost the Monty service: " ^ reason)) in
  let checks=int_of_float (10. *. Client_http.timeout ()) in
  let rec attempt () =
    if saved_epoch ~home<>epoch then lost "it was restarted or replaced" else
    Lwt.catch f (function
      | Failure(code,_) as answer when code<>408 -> Lwt.fail answer
      | Lwt.Canceled as error -> Lwt.fail error
      | _ -> waiting (); check 0)
  and check silent =
    Lwt.both (health ~home) (Lwt_unix.sleep 1.) >>= function
    | Some state,() when field "epoch" state<>epoch -> lost "it was restarted or replaced"
    | Some _,() -> attempt ()
    | None,() when not (service_running ~home) -> lost "it stopped"
    | None,() when silent>=checks -> lost "it stopped answering"
    | None,() -> check (silent+1) in
  attempt ()
let command ?waiting ~home ~epoch id json =
  let call meth body=retrying ?waiting ~home ~epoch (fun () -> request ~home meth ("/v1/commands/" ^ component id) body) in
  call `POST json >>= fun _ ->
  let rec poll () = call `GET `Null >>= fun receipt ->
    match s "status" receipt with
    | "finished" -> Lwt.return (field "result" receipt)
    | "accepted"|"running" -> Lwt_unix.sleep 0.2 >>= poll
    | _ -> Lwt.fail (Failure (409,"command " ^ id ^ ": " ^ Yojson.Safe.to_string receipt)) in poll ()
(* The caller's native account directory, canonical when it exists. A missing
   directory simply matches no profile instead of failing the lookup. *)
let account_directory harness =
  let directory=get(Skill.agent_directory harness) in
  try Unix.realpath directory with Unix.Unix_error _ -> directory
let profile_for ~home harness =
  ensure_service ~home () >|= fun health ->
  let directory=account_directory harness in
  let candidates=xs "profiles" health |> List.filter(fun p -> field "harness" p=string(Harness.to_string harness) && field "scope" p=string(sha directory)) in
  let id=match Sys.getenv_opt "MONTY_API_PROFILE",candidates with
    | Some id,_ -> (match List.find_opt(fun p->s "id" p=id) candidates with Some p->s "id" p|None->fail 409 "selected service profile does not match the account scope")
    | None,[p]->s "id" p|_->fail 409 "configure/select one server profile matching the native account directory" in
  id,field "epoch" health
let headless_async ~wait ~resume (options:Launcher.options) worker =
  let home=Unix.realpath options.home in
  let options={options with Launcher.home=home} in
  match Headless.preflight_worker ~explicit_resume:resume options worker with Error message->Lwt.return(Error message)|Ok (plan,_) ->
  let options=plan.Headless.options in
  Lwt.catch (fun () ->
    profile_for ~home:options.home options.harness >>= fun (profile,epoch) ->
    let id=random_id() in
    (* The supervisor runs in the service, so waiting for its receipt outlasts a stalled service. *)
    let call meth body=retrying ~home:options.home ~epoch (fun () -> request ~home:options.home meth ("/v1/commands/" ^ id) body) in
    call `POST
      (obj["op",string "headless_start";"profile",string profile;"command",string options.harness_command;"worker",string worker;"resume",`Bool resume;
        "codex_yolo",`Bool options.codex_yolo;"wt_command",string options.wt_command;
        "environment",`Assoc(Unix.environment () |> Array.to_list |> List.filter_map(fun binding->
          let pos=String.index binding '=' in let key=String.sub binding 0 pos in
          if Worker_environment.allowed key then
            Some(key,string(String.sub binding (pos+1)(String.length binding-pos-1))) else None))]) >>= fun _ ->
    let rec poll () = call `GET `Null >>= fun receipt ->
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
(* Monty's own program status record (OSC 7501, id=monty) carries only worker lifecycle
   facts: the run handoff outcome and how the native process ended. Whether the program is
   working, idle or blocked right now is the harness's own root record, which Rex reads from
   the proxied terminal bytes untouched, so merely attaching a terminal reports nothing;
   a working claim here would outlive the harness going idle. *)
let lifecycle_status ~title ~handoff ~exit =
  let exited=Option.map (Printf.sprintf "terminal exited (%d)") exit in
  let record ?kind state msg=Some (Rex.status_report ~title ?kind ~msg:(String.concat " · " (msg @ Option.to_list exited)) state) in
  match handoff,exit with
  | Some "ready-for-review",_ -> record "done" ["handoff: ready for review"]
  | Some "needs-attention",_ -> record ~kind:"question" "blocked" ["handoff: needs attention"]
  | Some "failed",_ -> record "error" ["handoff: failed"]
  | _,Some 0 -> record "done" []
  | _,Some _ -> record "error" []
  | _,None -> None
(* A worker's sessions record its ordered workspaces: each worktree, else its repo. *)
let worker_workspaces (record:Job_store.record) =
  record.workspaces |> List.map(fun (workspace:Job_store.workspace_state)->string(Unix.realpath(Option.value ~default:workspace.repo workspace.worktree)))
let worker_session (record:Job_store.record) sess =
  let saved=field "context" sess in
  let environment=field "environment" saved in
  field "MONTY_JOB_ID" environment=string record.id &&
  field "MONTY_WORKER_DIR" environment=string record.worker_dir &&
  field "MONTY_JOB_FILE" environment=string record.path &&
  field "workspaces" saved=`List (worker_workspaces record)
(* Whether the service owns a live native terminal of exactly this worker and, when given,
   this native session. That runtime then holds the worker lock, and resuming it only
   attaches another client. Read-only: it never starts the service. *)
let live_worker_terminal ?native ~home record =
  let home=Unix.realpath home in
  let mine sess=field "terminal" sess=`Bool true && worker_session record sess &&
    Option.fold ~none:true ~some:(fun id->s "native_id" sess=id) native in
  try Lwt_main.run (health ~home >>= function
    | None -> Lwt.return_false
    | Some _ -> request ~home `GET "/v1/conversations" `Null >>= fun catalog ->
        Json_fields.list catalog |> List.filter(fun c->field "purged" c<>`Bool true)
        |> List.concat_map (xs "sessions") |> List.filter mine
        |> Lwt_list.exists_s (fun sess -> request ~home `GET ("/v1/terminals/" ^ s "id" sess) `Null >|= fun output ->
          field "running" output=`Bool true))
  with _ -> false
let launch ~home ~profile ~harness ~command_override ~fork ~mode ~session_id ~prompt ~instructions ~context ~request_id ~worker ~title ~codex_yolo =
  if not(List.mem mode ["fresh";"picker";"last";"exact"]) || (mode="exact" && session_id=None) then fail 400 "choose fresh, picker, last, or exact with --session";
  let home=Unix.realpath home in
  (* The caller's own Rex placement travels with each attachment, never with the durable session. *)
  let rex=Rex.context_of_env () in
  let terminal_environment=`Assoc (List.map (fun (key,value)->key,string value) (Rex.bindings_of_context rex)) in
  let worker_record=if worker then (
    let id=match Sys.getenv_opt "MONTY_JOB_ID" with Some id when id<>""->id|_->fail 400 "worker terminal requires its durable worker identity" in
    Some(get(Job_store.find ~home ~scope:Job_store.Active id))) else None in
  let workspaces=match worker_record with
    | Some record -> worker_workspaces record
    | None -> [string(Unix.realpath(Sys.getcwd()))] in
  let matches_context sess =
    match worker_record with
    | Some record -> worker_session record sess
    | None ->
        (match field "MONTY_JOB_ID" (field "environment" (field "context" sess)) with `Null|`String ""->true|_->false) &&
        (session_id<>None || field "workspaces" (field "context" sess)=`List workspaces) in
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
    (* This attachment stays with the service it found; a replacement no longer owns its terminal. *)
    let epoch=field "epoch" health in
    let command=command ~home ~epoch in
    let directory=account_directory harness in
    let matches p=field "harness" p=string(Harness.to_string harness) && field "scope" p=string(sha directory) in
    let profiles=xs "profiles" health in
    let p=match profile with
      | Some id -> (match List.find_opt(fun p->s "id" p=id && matches p) profiles with Some p->p|_->fail 409 "terminal account scope does not match its configured service profile")
      | None -> (match List.filter matches profiles with [p]->p|[]->fail 409 "no service profile matches the selected native account directory; configure its account and roots before launching"|_->fail 409 "multiple service profiles match; select --profile") in
    let pid=s "id" p in
    (if mode="fresh" then command (Option.value ~default:(random_id()) request_id)
       (obj ["op",string "start";"profile",string pid;"command",Option.fold ~none:`Null ~some:string command_override;"context",context;"terminal",`Bool true;"fork",Option.fold ~none:`Null ~some:string fork;"codex_yolo",`Bool codex_yolo;"terminal_environment",terminal_environment;
         "title",string title;"task_keys",`List(match if worker then Sys.getenv_opt "MONTY_TASK_KEY" else None with Some key when key<>""->[string key]|_->[])])
     else request ~home `GET "/v1/conversations" `Null >|= fun catalog ->
       let candidates=Json_fields.list catalog |> List.filter(fun c -> field "purged" c<>`Bool true &&
         List.exists(fun sess->matches sess && matches_context sess &&
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
      let chosen=match List.find_opt(fun sess->s "native_id" sess=native && matches sess && matches_context sess)(xs "sessions" c) with Some sess->sess|None->fail 404 "chosen exact native session was not registered" in
      if field "selected_session" c=field "id" chosen then Lwt.return c else
      command (random_id())(obj["op",string "select_session";"conversation",field "id" c;"revision",field "revision" c;"session",field "id" chosen])) >>= fun c ->
    let sess=session c (s "selected_session" c) in
    if not(matches sess && matches_context sess) || Option.fold ~none:false ~some:(fun id->s "native_id" sess<>id) session_id then fail 409 "chosen native session is not the selected conversation";
    let rows,cols=Terminal_size.get Unix.stdin in
    command (random_id()) (obj ["op",string "terminal_open";"rows",`Int rows;"cols",`Int cols;"command",Option.fold ~none:`Null ~some:string command_override;"conversation",field "id" c;"selected_session",field "id" sess;
      "revision",field "revision" c;"fresh",`Bool(mode="fresh");"fork",Option.fold ~none:`Null ~some:string fork;"prompt",Option.fold ~none:`Null ~some:string prompt;
      "term",string(Option.value ~default:"xterm-256color" (Sys.getenv_opt "TERM"));"terminal_environment",terminal_environment]) >|= fun sess ->
    s "id" c,s "id" sess,s "native_id" sess,epoch in
  let cid,sid,native,epoch=try Lwt_main.run(flow()) with Failure _ as e->raise e|e->
    let url=try s "url" (read (file home ["client-service.json"])) with _->"unknown" in
    fail 503 ("Monty service at " ^ url ^ " unavailable (" ^ Printexc.to_string e ^ "); inspect receipts before retrying; no independent harness fallback") in
  (* Lifecycle records go out only for workers inside Rex; see lifecycle_status. *)
  let reporting=worker && Rex.inside_rex rex in
  let original=if Unix.isatty Unix.stdin then Some(Unix.tcgetattr Unix.stdin) else None in
  let attached () = Fun.protect ~finally:(fun () -> Option.iter(fun attrs->Unix.tcsetattr Unix.stdin Unix.TCSANOW attrs) original) (fun () ->
    Option.iter(fun attrs->Unix.tcsetattr Unix.stdin Unix.TCSANOW {attrs with
      c_icanon=false;c_echo=false;c_isig=false;c_ixon=false;c_icrnl=false;c_opost=false;c_vmin=1;c_vtime=0}) original;
    let offset=ref 0 and resized=ref true and notice=ref false in
    let prior=Sys.signal (Terminal_size.winch ()) (Sys.Signal_handle(fun _->resized:=true)) in
    Fun.protect ~finally:(fun () -> ignore(Sys.signal (Terminal_size.winch ()) prior)) (fun () ->
      (* The notice goes on the bottom row with the cursor saved and restored, so native output
         continues where it was; once output flows again, a size change redraws over it. *)
      let waiting () = if not !notice then (
        notice:=true;
        if Unix.isatty Unix.stderr then
          Printf.eprintf "\0277\027[%d;1H\027[2Kmonty: reconnecting to the Monty service...\0278%!" (fst(Terminal_size.get Unix.stdin))
        else prerr_endline "monty: reconnecting to the Monty service...") in
      let route op extra=command ~waiting ~home ~epoch (random_id()) (obj(["op",string op;"conversation",string cid;"selected_session",string sid] @ extra)) >|= ignore in
      let emit text=if reporting then Lwt_io.write Lwt_io.stdout text >>= fun () -> Lwt_io.flush Lwt_io.stdout else Lwt.return_unit in
      let handoff=ref None and polls=ref 0 in
      let report ?exit () = match lifecycle_status ~title ~handoff:!handoff ~exit with Some text -> emit text | None -> Lwt.return_unit in
      let poll_handoff () =
        incr polls;
        if not reporting || !polls mod 50<>1 then Lwt.return_unit else
        let current=handoff_outcome ~home in
        if current = !handoff then Lwt.return_unit else (handoff:=current;report ()) in
      let rec output () =
        retrying ~waiting ~home ~epoch (fun () -> request ~home `GET ("/v1/terminals/" ^ sid ^ "?offset=" ^ string_of_int !offset) `Null) >>= fun value ->
        let bytes=Base64.decode_exn(s "data" value) in
        Lwt_io.write Lwt_io.stdout bytes >>= fun () -> Lwt_io.flush Lwt_io.stdout >>= fun () ->
        offset:=i "offset" value;
        (if (!resized || !notice) && field "running" value=`Bool true then (resized:=false;let rows,cols=Terminal_size.get Unix.stdin in
          (if !notice then (notice:=false;route "terminal_resize" ["rows",`Int rows;"cols",`Int(if cols>1 then cols-1 else cols+1)]) else Lwt.return_unit) >>= fun () ->
          route "terminal_resize" ["rows",`Int rows;"cols",`Int cols]) else Lwt.return_unit) >>= fun () ->
        poll_handoff () >>= fun () ->
        if field "running" value=`Bool false && bytes="" then (match field "code" value with `Int code->Lwt.return (`Exited code)|_->fail 503 "native terminal outcome is unknown; inspect retained output") else Lwt_unix.sleep 0.04 >>= output in
      let rec input () = Lwt_io.read ~count:4096 Lwt_io.stdin >>= fun bytes ->
        if bytes="" then Lwt.return `Detached else
        Lwt.catch (fun () -> route "terminal_input" ["data",string(Base64.encode_exn bytes)])
          (* Input can race the native exit; the output loop reports that exit first. *)
          (function Lwt.Canceled as error -> Lwt.fail error | error -> Lwt_unix.sleep 1. >>= fun () -> Lwt.fail error) >>= input in
      Lwt_main.run(Lwt.pick [input();output()] >>= function
        | `Detached -> emit (Rex.status_report "clear") >|= fun () -> 0
        | `Exited code -> report ~exit:code () >|= fun () -> code))) in
  match attached () with
  | code -> code
  | exception error ->
      (* The client stops while the native session may still run in the service. *)
      let reason=match error with
        | Failure(_,message) -> message
        | Unix.Unix_error(error,call,_) -> call ^ ": " ^ Unix.error_message error
        | error -> Printexc.to_string error in
      let reattach=match worker_record with
        | Some record -> Printf.sprintf "monty resume %s --harness %s --home %s" (Shell.quote record.id) (Harness.to_string harness) (Shell.quote home)
        | None -> Printf.sprintf "monty continue %s --harness %s --home %s" (Shell.quote native) (Harness.to_string harness) (Shell.quote home) in
      (try
        (* Leave the native's alternate screen, mouse reporting and hidden cursor behind. *)
        if Unix.isatty Unix.stdout then print_string "\027[0m\027[?25h\027[?1000l\027[?1002l\027[?1003l\027[?1006l\027[?2004l\027[?1049l";
        if reporting then print_string (Rex.status_report ~title ~msg:("Monty service lost · " ^ reattach) "error");
        flush stdout;
        Printf.eprintf "\nmonty: %s\nThe native %s session may still be running in the Monty service. Reattach with:\n  %s\n%!"
          reason (Harness.to_string harness) reattach
      with Sys_error _ -> ());
      (* EX_UNAVAILABLE: the terminal client stopped, but its native session may still run. *)
      69
