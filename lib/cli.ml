let exit_code = function
  | Ok () -> 0
  | Error msg ->
      Fmt.epr "monty: %s\n" msg;
      1

type operations = {
  start :
    name:string ->
    home:string ->
    harness:Harness.t ->
    harness_command:string ->
    codex_yolo:bool ->
    (unit, string) result;
  launch_one : Launcher.options -> Job.t -> (unit, string) result;
  doctor :
    home:string ->
    harness:Harness.t ->
    harness_command:string ->
    wt_command:string ->
    backend:Terminal.backend ->
    worktree_mode:Launcher.worktree_mode ->
    (unit, string) result;
}

let default_operations =
  {
    start =
      (fun ~name ~home ~harness ~harness_command ~codex_yolo ->
        Head_butler.start ~home ~harness ~harness_command ~codex_yolo ~name);
    launch_one = Launcher.launch_one;
    doctor =
      (fun ~home ~harness ~harness_command ~wt_command ~backend ~worktree_mode ->
        Doctor.run ~home ~harness ~harness_command ~wt_command ~backend
          ~worktree_mode);
  }

let env_default getenv name default =
  match getenv name with Some value when String.trim value <> "" -> value | _ -> default

let backend_conv =
  Cmdliner.Arg.conv
    ( Terminal.backend_of_string,
      fun ppf value -> Fmt.pf ppf "%s" (Terminal.backend_to_string value) )

let target_conv =
  Cmdliner.Arg.conv
    ( Terminal.target_of_string,
      fun ppf value -> Fmt.pf ppf "%s" (Terminal.target_to_string value) )

let worktree_conv =
  Cmdliner.Arg.conv
    ( Launcher.worktree_mode_of_string,
      fun ppf value -> Fmt.pf ppf "%s" (Launcher.worktree_mode_to_string value) )

type handoff_format = Markdown | Plain | Json

let handoff_format_conv =
  let parse = function
    | "markdown" | "md" -> Ok Markdown
    | "plain" | "text" -> Ok Plain
    | "json" -> Ok Json
    | value -> Error (`Msg (Printf.sprintf "unknown handoff format %S" value))
  in
  let print ppf = function
    | Markdown -> Fmt.pf ppf "markdown"
    | Plain -> Fmt.pf ppf "plain"
    | Json -> Fmt.pf ppf "json"
  in
  Cmdliner.Arg.conv (parse, print)

let harness_conv =
  Cmdliner.Arg.conv
    ( Harness.of_string,
      fun ppf value -> Fmt.pf ppf "%s" (Harness.to_string value) )

let backend_default getenv =
  match Terminal.backend_of_string (env_default getenv "MONTY_TERMINAL" "ghostty") with
  | Ok backend -> backend
  | Error _ -> Terminal.Ghostty

let target_default getenv =
  match Terminal.target_of_string (env_default getenv "MONTY_TARGET" "tab") with
  | Ok target -> target
  | Error _ -> Terminal.Tab

let worktree_default getenv =
  match Launcher.worktree_mode_of_string (env_default getenv "MONTY_WORKTREE" "always") with
  | Ok mode -> mode
  | Error _ -> Launcher.Always

let make_cmd ?(getenv = Sys.getenv_opt) ?(operations = default_operations) () =
let home_arg =
  let doc = "Monty control-room directory. Defaults to MONTY_HOME or the nearest parent dune-project named monty." in
  Cmdliner.Arg.(value & opt string (Home.default_with_getenv getenv) & info [ "home" ] ~docv:"DIR" ~doc)
 in
let pi_command_arg =
  let doc = "Shell command used to start pi. May include fixed arguments." in
  Cmdliner.Arg.(value & opt string (env_default getenv "MONTY_PI_COMMAND" "pi") & info [ "pi-command" ] ~docv:"COMMAND" ~doc)
 in
let codex_command_arg =
  let doc = "Shell command used to start Codex. May include fixed arguments." in
  Cmdliner.Arg.(value & opt string (env_default getenv "MONTY_CODEX_COMMAND" "codex") & info [ "codex-command" ] ~docv:"COMMAND" ~doc)
 in
let harness_arg =
  let doc = "Agent harness: pi or codex. Overrides MONTY_HARNESS and persisted Monty settings." in
  Cmdliner.Arg.(value & opt (some harness_conv) None & info [ "harness" ] ~docv:"HARNESS" ~doc)
 in
let codex_yolo_arg =
  let doc = "Run Codex without approvals or sandboxing. This is dangerous." in
  Cmdliner.Arg.(value & flag & info [ "codex-yolo" ] ~doc)
 in
let wt_command_arg =
  let doc = "Shell command used to invoke the wt CLI." in
  Cmdliner.Arg.(value & opt string (env_default getenv "MONTY_WT_COMMAND" "wt") & info [ "wt-command" ] ~docv:"COMMAND" ~doc)
 in
let backend_arg =
  let doc = "Terminal backend: ghostty or dry-run." in
  Cmdliner.Arg.(value & opt backend_conv (backend_default getenv) & info [ "terminal" ] ~docv:"BACKEND" ~doc)
 in
let target_arg =
  let doc = "Where to launch the worker: tab, window, or split." in
  Cmdliner.Arg.(value & opt target_conv (target_default getenv) & info [ "target" ] ~docv:"TARGET" ~doc)
 in
let worktree_arg =
  let doc = "Worktree mode: always or never." in
  Cmdliner.Arg.(value & opt worktree_conv (worktree_default getenv) & info [ "worktree" ] ~docv:"MODE" ~doc)
 in
let branch_prefix_arg =
  let doc = "Prefix for automatically generated worktree branches. Overrides the persisted setting, MONTY_BRANCH_PREFIX fallback, and monty default." in
  Cmdliner.Arg.(value & opt (some string) None & info [ "branch-prefix" ] ~docv:"PREFIX" ~doc)
 in
let fork_arg =
  let doc = "Optional Pi session id or path to fork. Unsupported by Codex." in
  Cmdliner.Arg.(value & opt (some string) None & info [ "fork" ] ~docv:"SESSION" ~doc)
 in
let script_dir_arg =
  let doc = "Directory where generated worker launch scripts are written." in
  Cmdliner.Arg.(value & opt (some string) None & info [ "script-dir" ] ~docv:"DIR" ~doc)
 in
let monty_command () =
  let executable = Sys.executable_name in
  if Filename.is_relative executable && not (Sys.file_exists executable) then
    match Process.command_exists executable with
    | Some path -> Shell.normalize path
    | None -> Shell.normalize (Shell.abs_path executable)
  else Shell.normalize (Shell.abs_path executable)
 in
let options backend target harness_override codex_yolo_override pi_command codex_command wt_command worktree_mode branch_prefix_override fork home script_dir =
  let ( let* ) = Result.bind in
  let* harness =
    Settings.effective_harness ~getenv ~home harness_override
  in
  let* codex_yolo =
    Settings.effective_codex_yolo ~getenv ~home codex_yolo_override
  in
  let* branch_prefix =
    Settings.effective_branch_prefix ~getenv ~home branch_prefix_override
  in
  let script_dir =
    match script_dir with
    | Some dir -> Shell.normalize (Shell.abs_path dir)
    | None -> Home.runtime_script_dir ~home () |> Shell.normalize
  in
  Ok Launcher.{
    backend;
    target;
    harness;
    harness_command =
      (match harness with Harness.Pi -> pi_command | Harness.Codex -> codex_command);
    codex_yolo;
    wt_command;
    worktree_mode;
    branch_prefix;
    fork;
    home;
    script_dir;
    monty_command = monty_command ();
  }
 in
let options_term =
  Cmdliner.Term.(
    const options $ backend_arg $ target_arg $ harness_arg $ codex_yolo_arg $ pi_command_arg
    $ codex_command_arg $ wt_command_arg $ worktree_arg
    $ branch_prefix_arg $ fork_arg $ home_arg $ script_dir_arg)
 in
let headless_options harness_override codex_yolo_override pi_command codex_command
    wt_command branch_prefix_override fork home script_dir =
  let ( let* ) = Result.bind in
  let* harness =
    Settings.effective_harness ~getenv ~home harness_override
  in
  let* codex_yolo =
    Settings.effective_codex_yolo ~getenv ~home codex_yolo_override
  in
  let* branch_prefix =
    Settings.effective_branch_prefix ~getenv ~home branch_prefix_override
  in
  Ok Launcher.
    { backend = Terminal.Dry_run;
      target = Terminal.Tab;
      harness;
      harness_command =
        (match harness with Harness.Pi -> pi_command | Harness.Codex -> codex_command);
      codex_yolo;
      wt_command;
      worktree_mode = Always;
      branch_prefix;
      fork;
      home;
      script_dir =
        (match script_dir with
        | Some dir -> Shell.normalize (Shell.abs_path dir)
        | None -> Home.runtime_script_dir ~home () |> Shell.normalize);
      monty_command = monty_command () }
 in
let headless_options_term =
  Cmdliner.Term.(
    const headless_options $ harness_arg $ codex_yolo_arg $ pi_command_arg
    $ codex_command_arg $ wt_command_arg $ branch_prefix_arg $ fork_arg
    $ home_arg $ script_dir_arg)
 in
let start name home harness_override codex_yolo_override pi_command codex_command =
  match
    ( Settings.effective_harness ~getenv ~home harness_override,
      Settings.effective_codex_yolo ~getenv ~home codex_yolo_override )
  with
  | Error message, _ | _, Error message -> exit_code (Error message)
  | Ok harness, Ok codex_yolo ->
      let harness_command =
        match harness with Harness.Pi -> pi_command | Harness.Codex -> codex_command
      in
      operations.start ~name ~home ~harness ~harness_command ~codex_yolo
      |> exit_code
 in
let start_term =
  let name_arg =
    let doc = "Session name for the head-butler session when supported by the harness." in
    Cmdliner.Arg.(value & opt string "Monty Head Butler" & info [ "name"; "n" ] ~docv:"NAME" ~doc)
  in
  Cmdliner.Term.(const start $ name_arg $ home_arg $ harness_arg $ codex_yolo_arg $ pi_command_arg $ codex_command_arg)
 in
let launch repo title context branch options =
  match options with
  | Error message -> exit_code (Error message)
  | Ok options ->
      let cwd = Sys.getcwd () in
      let repo = Shell.normalize (Shell.abs_path ~base:cwd repo) in
      let context = Shell.normalize (Shell.abs_path ~base:cwd context) in
      let job = Job.make ?branch ~title ~repo ~context () in
      operations.launch_one options job |> exit_code
 in
let launch_term =
  let repo =
    let doc = "Repository path where wt should create or reuse the worker worktree." in
    Cmdliner.Arg.(required & opt (some string) None & info [ "repo" ] ~docv:"DIR" ~doc)
  in
  let title =
    let doc = "Human-readable worker task title." in
    Cmdliner.Arg.(required & opt (some string) None & info [ "title" ] ~docv:"TITLE" ~doc)
  in
  let context =
    let doc = "Markdown task context file supplied to the selected harness." in
    Cmdliner.Arg.(required & opt (some string) None & info [ "context" ] ~docv:"FILE" ~doc)
  in
  let branch =
    let doc = "Worktree branch name. Defaults to <branch-prefix>/<title-slug>." in
    Cmdliner.Arg.(value & opt (some string) None & info [ "branch" ] ~docv:"BRANCH" ~doc)
  in
  Cmdliner.Term.(const launch $ repo $ title $ context $ branch $ options_term)
 in
let launch_many manifest options =
  match options with
  | Error message -> exit_code (Error message)
  | Ok options ->
      let manifest = Shell.abs_path manifest |> Shell.normalize in
      (match Manifest.load ~home:options.Launcher.home manifest with
      | Error msg -> exit_code (Error msg)
      | Ok jobs ->
          let retry_command = Launcher.retry_launch_many_command options manifest in
          Launcher.launch_many ~retry_command options jobs |> exit_code)
 in
let launch_many_term =
  let manifest =
    let doc = "JSON manifest containing a jobs array." in
    Cmdliner.Arg.(required & opt (some string) None & info [ "manifest" ] ~docv:"FILE" ~doc)
  in
  Cmdliner.Term.(const launch_many $ manifest $ options_term)
 in
let headless_prepare_many manifest dry_run options =
  match options with
  | Error message -> exit_code (Error message)
  | Ok options ->
      let manifest = Shell.abs_path manifest |> Shell.normalize in
      (match Manifest.load ~home:options.Launcher.home manifest with
      | Error msg -> exit_code (Error msg)
      | Ok jobs -> (
          match Headless.prepare_many ~dry_run options jobs with
          | Error msg -> exit_code (Error msg)
          | Ok json ->
              Headless.print_json json;
              0))
 in
let headless_prepare_many_term =
  let manifest =
    let doc = "JSON manifest containing the Monty jobs to prepare." in
    Cmdliner.Arg.(required & opt (some string) None & info [ "manifest" ] ~docv:"FILE" ~doc)
  in
  let dry_run =
    let doc = "Perform complete preflight without reserving workers or creating worktrees." in
    Cmdliner.Arg.(value & flag & info [ "dry-run" ] ~doc)
  in
  Cmdliner.Term.(const headless_prepare_many $ manifest $ dry_run $ headless_options_term)
 in
let print_headless_result = function
  | Error msg -> exit_code (Error msg)
  | Ok json ->
      Headless.print_json json;
      0
 in

let headless_begin worker options =
  match options with
  | Error message -> exit_code (Error message)
  | Ok options ->
      Headless.begin_worker ~explicit_resume:false options worker
      |> print_headless_result
 in

let headless_run worker options =
  match options with
  | Error message -> exit_code (Error message)
  | Ok options ->
      Headless.run_codex_worker ~explicit_resume:false options worker
      |> print_headless_result
 in

let headless_resume worker options =
  match options with
  | Error message -> exit_code (Error message)
  | Ok options -> (
      match options.Launcher.harness with
      | Harness.Pi ->
          Headless.begin_worker ~explicit_resume:true options worker
          |> print_headless_result
      | Harness.Codex ->
          Headless.run_codex_worker ~explicit_resume:true options worker
          |> print_headless_result)
 in
let headless_worker_term command =
  let worker =
    let doc = "Worker id, branch leaf, branch, or title slug." in
    Cmdliner.Arg.(required & pos 0 (some string) None & info [] ~docv:"WORKER" ~doc)
  in
  Cmdliner.Term.(const command $ worker $ headless_options_term)
 in
let headless_run_many manifest options =
  match options with
  | Error message -> exit_code (Error message)
  | Ok options ->
      let manifest = Shell.abs_path manifest |> Shell.normalize in
      (match Manifest.load ~home:options.Launcher.home manifest with
      | Error msg -> exit_code (Error msg)
      | Ok jobs -> (
          match Headless.run_codex_many options jobs with
          | Error msg -> exit_code (Error msg)
          | Ok (json, success) ->
              Headless.print_json json;
              if success then 0 else 1))
 in
let headless_run_many_term =
  let manifest =
    let doc = "JSON manifest containing prepared Monty jobs to run with Codex." in
    Cmdliner.Arg.(required & opt (some string) None & info [ "manifest" ] ~docv:"FILE" ~doc)
  in
  Cmdliner.Term.(const headless_run_many $ manifest $ headless_options_term)
 in
let resolve_handoff_worker worker home =
  let home = Shell.normalize (Shell.abs_path home) in
  match worker with
  | Some worker -> Job_store.find ~home ~scope:Job_store.All worker
  | None -> (
      match getenv "MONTY_WORKER_DIR" with
      | Some worker_dir when String.trim worker_dir <> "" ->
          let path = Filename.concat (Shell.normalize worker_dir) "job.json" in
          if Sys.file_exists path then Job_store.parse_job_file ~home path
          else
            Error
              (Printf.sprintf
                 "MONTY_WORKER_DIR has no job.json: %s; pass a worker explicitly"
                 worker_dir)
      | _ -> (
          match getenv "MONTY_JOB_ID" with
          | Some id when String.trim id <> "" ->
              Job_store.find ~home ~scope:Job_store.All id
          | _ ->
              Error
                "run handoff command needs a worker argument or MONTY_WORKER_DIR in the current session"))
 in
let print_published format (published : Run_handoff.published) =
  (match format with
  | Markdown ->
      Fmt.pr "%s" (Run_handoff.render_markdown published.handoff)
  | Plain ->
      Fmt.pr "%s" (Run_handoff.render_plain published.handoff)
  | Json ->
      Headless.print_json
        (`Assoc
          [ ("handoff", Run_handoff.to_json published.handoff);
            ("delivery", `String "direct") ]));
  0
 in
let handoff_publish worker run_id outcome summary validation accepted fixed rejected
    unresolved review_summary risks last_phase error artifacts format home =
  let result =
    let ( let* ) = Result.bind in
    let* record = resolve_handoff_worker worker home in
    let* outcome = Run_handoff.outcome_of_string outcome in
    Run_handoff.publish ~home ~record ?handoff_id:run_id
      ~source:Run_handoff.Interactive ~outcome ~summary ~validation ~accepted
      ~fixed ~rejected ~unresolved ?review_summary ~risks ?last_phase ?error
      ~artifacts ()
  in
  match result with
  | Error message -> exit_code (Error message)
  | Ok published -> print_published format published
 in
let handoff_publish_term =
  let worker =
    let doc = "Worker id, branch, task key, or title slug. Defaults to the current Monty worker environment." in
    Cmdliner.Arg.(value & pos 0 (some string) None & info [] ~docv:"WORKER" ~doc)
  in
  let run_id =
    let doc = "Stable run id for idempotent publication. Monty generates one when omitted." in
    Cmdliner.Arg.(value & opt (some string) None & info [ "run-id" ] ~docv:"ID" ~doc)
  in
  let outcome =
    let doc = "Run outcome: ready-for-review, needs-attention, or failed." in
    Cmdliner.Arg.(value & opt string "ready-for-review" & info [ "outcome" ] ~docv:"OUTCOME" ~doc)
  in
  let summary =
    let doc = "Dense description of what this run did." in
    Cmdliner.Arg.(required & opt (some string) None & info [ "summary" ] ~docv:"TEXT" ~doc)
  in
  let many name doc =
    Cmdliner.Arg.(value & opt_all string [] & info [ name ] ~docv:"TEXT" ~doc)
  in
  let validation = many "check" "Validation command and result; repeat for multiple checks." in
  let accepted = many "accepted" "Accepted review finding; repeat as needed." in
  let fixed = many "fixed" "Review finding fixed in this run; repeat as needed." in
  let rejected = many "rejected" "Review finding rejected after verification; repeat as needed." in
  let unresolved = many "unresolved" "Unresolved review finding; repeat as needed." in
  let review_summary =
    let doc = "Short overall review result." in
    Cmdliner.Arg.(value & opt (some string) None & info [ "review" ] ~docv:"TEXT" ~doc)
  in
  let risks = many "risk" "Residual risk, blocker, or user decision; repeat as needed." in
  let last_phase =
    let doc = "Last known execution phase, especially for failures." in
    Cmdliner.Arg.(value & opt (some string) None & info [ "last-phase" ] ~docv:"PHASE" ~doc)
  in
  let error =
    let doc = "Useful failure message." in
    Cmdliner.Arg.(value & opt (some string) None & info [ "error" ] ~docv:"MESSAGE" ~doc)
  in
  let artifacts = many "artifact" "Evidence path inside durable worker memory; repeat as needed." in
  let format =
    let doc = "Output format: markdown, plain, or json." in
    Cmdliner.Arg.(value & opt handoff_format_conv Markdown & info [ "format" ] ~docv:"FORMAT" ~doc)
  in
  Cmdliner.Term.(
    const handoff_publish $ worker $ run_id $ outcome $ summary $ validation
    $ accepted $ fixed $ rejected $ unresolved $ review_summary $ risks
    $ last_phase $ error $ artifacts $ format $ home_arg)
 in
let handoff_pending format home =
  let result =
    let ( let* ) = Result.bind in
    let* _recovered = Headless.recover_finished_pi ~home in
    let* _repaired = Run_handoff.recover_orphaned_notices ~home in
    Run_handoff.pending ~home
  in
  match result with
  | Error message -> exit_code (Error message)
  | Ok values ->
      (match format with
      | Json ->
          let entries =
            values
            |> List.map (fun ((notice : Run_handoff.notice), handoff) ->
                   `Assoc
                     [ ("notice", Run_handoff.notice_to_json notice);
                       ("handoff", Run_handoff.to_json handoff) ])
          in
          Headless.print_json
            (`Assoc
              [ ("schema", `String "monty:run-handoff-pending:v1");
                ("pending", `List entries) ])
      | Markdown ->
          List.iter
            (fun ((notice : Run_handoff.notice), handoff) ->
              Fmt.pr "%s"
                (Run_handoff.render_markdown ~notice_id:notice.id handoff))
            values
      | Plain ->
          List.iter
            (fun ((notice : Run_handoff.notice), handoff) ->
              Fmt.pr "%s"
                (Run_handoff.render_plain ~notice_id:notice.id handoff))
            values);
      0
 in
let handoff_format_term =
  let doc = "Output format: markdown, plain, or json." in
  Cmdliner.Arg.(value & opt handoff_format_conv Markdown & info [ "format" ] ~docv:"FORMAT" ~doc)
 in
let handoff_pending_term =
  Cmdliner.Term.(const handoff_pending $ handoff_format_term $ home_arg)
 in
let handoff_acknowledge notice home =
  match Run_handoff.acknowledge ~home notice with
  | Error message -> exit_code (Error message)
  | Ok notice ->
      Fmt.pr "Acknowledged run handoff %s\n" notice.Run_handoff.id;
      0
 in
let handoff_acknowledge_term =
  let notice =
    let doc = "Notice id printed with a pending run handoff." in
    Cmdliner.Arg.(required & pos 0 (some string) None & info [] ~docv:"NOTICE" ~doc)
  in
  Cmdliner.Term.(const handoff_acknowledge $ notice $ home_arg)
 in
let handoff_show worker run_id format home =
  let result =
    let ( let* ) = Result.bind in
    let* record = resolve_handoff_worker (Some worker) home in
    Run_handoff.find_handoff ~home record run_id
  in
  match result with
  | Error message -> exit_code (Error message)
  | Ok handoff ->
      (match format with
      | Markdown -> Fmt.pr "%s" (Run_handoff.render_markdown handoff)
      | Plain -> Fmt.pr "%s" (Run_handoff.render_plain handoff)
      | Json -> Headless.print_json (Run_handoff.to_json handoff));
      0
 in
let handoff_show_term =
  let worker =
    let doc = "Worker whose latest or named run handoff should be shown." in
    Cmdliner.Arg.(required & pos 0 (some string) None & info [] ~docv:"WORKER" ~doc)
  in
  let run_id =
    let doc = "Specific run id. Defaults to the latest handoff." in
    Cmdliner.Arg.(value & opt (some string) None & info [ "run-id" ] ~docv:"ID" ~doc)
  in
  Cmdliner.Term.(const handoff_show $ worker $ run_id $ handoff_format_term $ home_arg)
 in
let handoff_follow_up worker run_id question home =
  let result =
    let ( let* ) = Result.bind in
    let* record = resolve_handoff_worker (Some worker) home in
    let* handoff = Run_handoff.find_handoff ~home record run_id in
    Run_handoff.follow_up_json ~home ~record ~handoff ~question
  in
  match result with
  | Error message -> exit_code (Error message)
  | Ok json ->
      Headless.print_json json;
      0
 in
let handoff_follow_up_term =
  let worker =
    let doc = "Worker whose durable run evidence should answer the question." in
    Cmdliner.Arg.(required & pos 0 (some string) None & info [] ~docv:"WORKER" ~doc)
  in
  let run_id =
    let doc = "Specific run id. Defaults to the latest handoff." in
    Cmdliner.Arg.(value & opt (some string) None & info [ "run-id" ] ~docv:"ID" ~doc)
  in
  let question =
    let doc = "Read-only follow-up question." in
    Cmdliner.Arg.(required & opt (some string) None & info [ "question" ] ~docv:"TEXT" ~doc)
  in
  Cmdliner.Term.(const handoff_follow_up $ worker $ run_id $ question $ home_arg)
 in
let headless_finish worker attempt outcome last_phase error home =
  let success =
    match String.lowercase_ascii outcome with
    | "success" | "succeeded" -> Ok true
    | "failed" | "failure" -> Ok false
    | value ->
        Error
          (Printf.sprintf
             "unknown headless finish outcome %S; expected success or failed" value)
  in
  match success with
  | Error message -> exit_code (Error message)
  | Ok success ->
      Headless.finish_pi_worker ~home ~worker ~attempt_id:attempt ~success
        ?last_phase ?error ()
      |> print_headless_result
 in
let headless_finish_term =
  let worker =
    let doc = "Pi worker whose asynchronous chain returned." in
    Cmdliner.Arg.(required & pos 0 (some string) None & info [] ~docv:"WORKER" ~doc)
  in
  let attempt =
    let doc = "Attempt id from the versioned completion contract." in
    Cmdliner.Arg.(required & opt (some string) None & info [ "attempt" ] ~docv:"ID" ~doc)
  in
  let outcome =
    let doc = "Harness callback outcome: success or failed." in
    Cmdliner.Arg.(required & opt (some string) None & info [ "outcome" ] ~docv:"OUTCOME" ~doc)
  in
  let last_phase =
    let doc = "Last known chain phase for a failure." in
    Cmdliner.Arg.(value & opt (some string) None & info [ "last-phase" ] ~docv:"PHASE" ~doc)
  in
  let error =
    let doc = "Harness failure message." in
    Cmdliner.Arg.(value & opt (some string) None & info [ "error" ] ~docv:"MESSAGE" ~doc)
  in
  Cmdliner.Term.(const headless_finish $ worker $ attempt $ outcome $ last_phase $ error $ home_arg)
 in
let resume archived worker options =
  match options with
  | Error message -> exit_code (Error message)
  | Ok options ->
  let record =
    if archived then Resume.find_reactivatable ~home:options.Launcher.home worker
    else Resume.find_resumable ~home:options.Launcher.home worker
  in
  match record with
  | Error msg -> exit_code (Error msg)
  | Ok record -> (
      let job =
        if archived then
          match options.Launcher.backend with
          | Terminal.Dry_run -> Resume.plan_reactivate ~home:options.Launcher.home record
          | Terminal.Ghostty -> Resume.reactivate ~home:options.Launcher.home record
        else Ok record.Job_store.job
      in
      match job with
      | Error msg -> exit_code (Error msg)
      | Ok job ->
          let validate_open_task =
            (not archived)
            || options.Launcher.backend <> Terminal.Dry_run
          in
          Launcher.resume_job ~validate_open_task
            ~persisted_worktree_mode:record.Job_store.worktree_mode options job
          |> exit_code)
 in
let resume_term =
  let worker =
    let doc = "Worker id, branch leaf, branch, or title slug to resume." in
    Cmdliner.Arg.(required & pos 0 (some string) None & info [] ~docv:"WORKER" ~doc)
  in
  let archived =
    let doc = "Resume an archived job and move it back to active workers." in
    Cmdliner.Arg.(value & flag & info [ "archived" ] ~doc)
  in
  Cmdliner.Term.(const resume $ archived $ worker $ options_term)
 in
let complete worker force home wt_command =
  Done.complete ?worker ~home ~wt_command ~force () |> exit_code
 in
let complete_term =
  let worker =
    let doc = "Worker id, branch leaf, branch, or title slug to mark done. Defaults to MONTY_WORKER_DIR." in
    Cmdliner.Arg.(value & pos 0 (some string) None & info [] ~docv:"WORKER" ~doc)
  in
  let force =
    let doc = "Discard local worktree changes while deleting the worktree and branch." in
    Cmdliner.Arg.(value & flag & info [ "force"; "f" ] ~doc)
  in
  Cmdliner.Term.(const complete $ worker $ force $ home_arg $ wt_command_arg)
 in
let list_jobs archived all run no_sync home =
  let scope = if all then Job_store.All else if archived then Job_store.Archived else Job_store.Active in
  List_jobs.run ~home ~scope ?run ~sync:(not no_sync) () |> exit_code
 in
let list_jobs_term =
  let archived =
    let doc = "List done tasks instead of open tasks." in
    Cmdliner.Arg.(value & flag & info [ "archived" ] ~doc)
  in
  let all =
    let doc = "List open and done tasks." in
    Cmdliner.Arg.(value & flag & info [ "all" ] ~doc)
  in
  let run =
    let doc = "Only list tasks linked to jobs for a run directory name or path." in
    Cmdliner.Arg.(value & opt (some string) None & info [ "run" ] ~docv:"RUN" ~doc)
  in
  let no_sync =
    let doc = "Read inventory without reconciliation writes or external task fetches." in
    Cmdliner.Arg.(value & flag & info [ "no-sync" ] ~doc)
  in
  Cmdliner.Term.(const list_jobs $ archived $ all $ run $ no_sync $ home_arg)
 in
let ensure_worktree repo branch wt_command =
  let repo = Shell.normalize (Shell.abs_path repo) in
  match Wt.create_or_reuse ~wt_command ~repo ~branch with
  | Error msg -> exit_code (Error msg)
  | Ok path ->
      Fmt.pr "%s\n" path;
      0
 in
let ensure_worktree_term =
  let repo =
    let doc = "Repository whose branch should be checked out." in
    Cmdliner.Arg.(required & opt (some string) None & info [ "repo" ] ~docv:"DIR" ~doc)
  in
  let branch =
    let doc = "Branch to check out in the selected repository." in
    Cmdliner.Arg.(required & opt (some string) None & info [ "branch" ] ~docv:"BRANCH" ~doc)
  in
  Cmdliner.Term.(const ensure_worktree $ repo $ branch $ wt_command_arg)
 in
let overview home =
  match Project_overview.overview ~home with
  | Error msg -> exit_code (Error msg)
  | Ok text ->
      Fmt.pr "%s\n" text;
      0
 in
let overview_term = Cmdliner.Term.(const overview $ home_arg)
 in
let projects_list home =
  match Project_overview.load_projects ~home with
  | Error msg -> exit_code (Error msg)
  | Ok projects ->
      Fmt.pr "%s" (Project_overview.render_projects projects);
      0
 in
let projects_list_term = Cmdliner.Term.(const projects_list $ home_arg)
 in
let projects_show project home =
  match Project_overview.load_projects ~home with
  | Error msg -> exit_code (Error msg)
  | Ok projects -> (
      match Project_overview.resolve_project projects project with
      | Error msg -> exit_code (Error msg)
      | Ok project ->
          Fmt.pr "%s\n" (Project_overview.show_project ~home project);
          0)
 in
let projects_show_term =
  let project =
    let doc = "Project id, repo path, or derived repo name." in
    Cmdliner.Arg.(required & pos 0 (some string) None & info [] ~docv:"PROJECT" ~doc)
  in
  Cmdliner.Term.(const projects_show $ project $ home_arg)
 in
let projects_add repo github query home =
  match Project_overview.add_project ~home ~repo ?github ?query () with
  | Error msg -> exit_code (Error msg)
  | Ok project ->
      Fmt.pr "Added project %s\n" project.Project_overview.id;
      Fmt.pr "Memory: %s\n" (Project_overview.project_memory_file ~home project.id);
      0
 in
let projects_add_term =
  let repo =
    let doc = "Repository path for this project." in
    Cmdliner.Arg.(required & opt (some string) None & info [ "repo" ] ~docv:"DIR" ~doc)
  in
  let github =
    let doc = "GitHub OWNER/REPO whose issues are the task source of truth." in
    Cmdliner.Arg.(value & opt (some string) None & info [ "github" ] ~docv:"OWNER/REPO" ~doc)
  in
  let query =
    let doc = "GitHub issue search query. Defaults to gh issue list's open issues." in
    Cmdliner.Arg.(value & opt (some string) None & info [ "query" ] ~docv:"QUERY" ~doc)
  in
  Cmdliner.Term.(const projects_add $ repo $ github $ query $ home_arg)
 in
let print_sync_warnings warnings =
  List.iter (fun warning -> Fmt.epr "monty: warning: %s\n" warning) warnings
 in
let tasks_list project all no_sync home =
  let result =
    let ( let* ) = Result.bind in
    let* sync_warnings =
      if no_sync then Ok []
      else
        Project_overview.sync_jobs_to_local_tasks ~home
        |> Result.map (fun result -> result.Project_overview.warnings)
    in
    let* tasks, inventory_warnings =
      Project_overview.load_tasks_with_warnings ~home ?project ~all ()
    in
    Ok (tasks, List.sort_uniq String.compare (sync_warnings @ inventory_warnings))
  in
  match result with
  | Error msg -> exit_code (Error msg)
  | Ok (tasks, warnings) ->
      print_sync_warnings warnings;
      Fmt.pr "%s" (Project_overview.render_tasks tasks);
      0
 in
let tasks_list_term =
  let project =
    let doc = "Only list tasks for this project." in
    Cmdliner.Arg.(value & opt (some string) None & info [ "project" ] ~docv:"PROJECT" ~doc)
  in
  let all =
    let doc = "Include completed local tasks." in
    Cmdliner.Arg.(value & flag & info [ "all" ] ~doc)
  in
  let no_sync =
    let doc = "Read inventory without reconciliation writes or external task fetches." in
    Cmdliner.Arg.(value & flag & info [ "no-sync" ] ~doc)
  in
  Cmdliner.Term.(const tasks_list $ project $ all $ no_sync $ home_arg)
 in
let tasks_sync home =
  match Project_overview.sync_jobs_to_local_tasks ~home with
  | Error msg -> exit_code (Error msg)
  | Ok result ->
      print_sync_warnings result.Project_overview.warnings;
      Fmt.pr "Synced jobs to local tasks: %d created, %d updated, %d linked jobs\n"
        result.Project_overview.created result.updated result.linked_jobs;
      0
 in
let tasks_sync_term = Cmdliner.Term.(const tasks_sync $ home_arg)
 in
let tasks_repair_worker worker home =
  match Project_overview.repair_legacy_task_link ~home worker with
  | Error msg -> exit_code (Error msg)
  | Ok key ->
      Fmt.pr "Linked legacy worker to %s\n" key;
      0
 in
let tasks_repair_worker_term =
  let worker =
    let doc = "Legacy worker whose title or branch should be matched explicitly." in
    Cmdliner.Arg.(required & pos 0 (some string) None & info [] ~docv:"WORKER" ~doc)
  in
  Cmdliner.Term.(const tasks_repair_worker $ worker $ home_arg)
 in
let task_add project title home =
  match Project_overview.add_local_task ~home ~project ~title () with
  | Error msg -> exit_code (Error msg)
  | Ok task ->
      Fmt.pr "Added local task %s\n" task.Project_overview.id;
      0
 in
let task_add_term =
  let project =
    let doc = "Project for this local task." in
    Cmdliner.Arg.(required & opt (some string) None & info [ "project" ] ~docv:"PROJECT" ~doc)
  in
  let title =
    let doc = "Task title." in
    Cmdliner.Arg.(required & opt (some string) None & info [ "title" ] ~docv:"TITLE" ~doc)
  in
  Cmdliner.Term.(const task_add $ project $ title $ home_arg)
 in
let task_done id home =
  Project_overview.done_local_task ~home id |> exit_code
 in
let task_done_term =
  let id =
    let doc = "Local task id, with or without the local: prefix." in
    Cmdliner.Arg.(required & pos 0 (some string) None & info [] ~docv:"TASK" ~doc)
  in
  Cmdliner.Term.(const task_done $ id $ home_arg)
 in
let task_show id home =
  match Project_overview.show_task ~home id with
  | Error msg -> exit_code (Error msg)
  | Ok output ->
      Fmt.pr "%s" output;
      0
 in
let task_show_term =
  let id =
    let doc = "Local task id, with or without the local: prefix." in
    Cmdliner.Arg.(required & pos 0 (some string) None & info [] ~docv:"TASK" ~doc)
  in
  Cmdliner.Term.(const task_show $ id $ home_arg)
 in
let task_workspace_add id repo branch home =
  match Project_overview.add_task_workspace ~home ~id ~repo ~branch with
  | Error msg -> exit_code (Error msg)
  | Ok _ ->
      Fmt.pr "Added workspace to local task %s: %s | %s\n" id
        (Shell.normalize (Shell.abs_path repo)) branch;
      0
 in
let task_workspace_add_term =
  let id =
    let doc = "Open local task that should own the workspace." in
    Cmdliner.Arg.(required & pos 0 (some string) None & info [] ~docv:"TASK" ~doc)
  in
  let repo =
    let doc = "Absolute registered repository path." in
    Cmdliner.Arg.(required & opt (some string) None & info [ "repo" ] ~docv:"DIR" ~doc)
  in
  let branch =
    let doc = "Planned branch in this repository." in
    Cmdliner.Arg.(required & opt (some string) None & info [ "branch" ] ~docv:"BRANCH" ~doc)
  in
  Cmdliner.Term.(const task_workspace_add $ id $ repo $ branch $ home_arg)
 in
let task_workspace_ensure id repo home wt_command =
  match
    Project_overview.ensure_task_workspaces ~home ~id ?repo ~wt_command ()
  with
  | Error msg -> exit_code (Error msg)
  | Ok workspaces ->
      List.iter
        (fun (workspace : Job_store.workspace_state) ->
          match workspace.worktree with
          | Some path -> Fmt.pr "%s\n" path
          | None -> ())
        workspaces;
      0
 in
let task_workspace_ensure_term =
  let id =
    let doc = "Open linked local task whose workspaces should be materialized." in
    Cmdliner.Arg.(required & pos 0 (some string) None & info [] ~docv:"TASK" ~doc)
  in
  let repo =
    let doc = "Ensure only this absolute repository path." in
    Cmdliner.Arg.(value & opt (some string) None & info [ "repo" ] ~docv:"DIR" ~doc)
  in
  Cmdliner.Term.(const task_workspace_ensure $ id $ repo $ home_arg $ wt_command_arg)
 in
let task_merge source target home =
  match Project_overview.merge_local_tasks ~home ~source ~target with
  | Error msg -> exit_code (Error msg)
  | Ok (source_task, target_task) ->
      Fmt.pr "Merged local:%s into local:%s\n" source_task.Overview_types.id
        target_task.Overview_types.id;
      0
 in
let task_merge_term =
  let source =
    let doc = "Unlaunched open source task." in
    Cmdliner.Arg.(required & pos 0 (some string) None & info [] ~docv:"SOURCE" ~doc)
  in
  let target =
    let doc = "Unlaunched open task that should remain." in
    Cmdliner.Arg.(required & opt (some string) None & info [ "into" ] ~docv:"TARGET" ~doc)
  in
  Cmdliner.Term.(const task_merge $ source $ target $ home_arg)
 in
let doctor home harness_override pi_command codex_command wt_command backend worktree_mode =
  match Settings.effective_harness ~getenv ~home harness_override with
  | Error message -> exit_code (Error message)
  | Ok harness ->
      let harness_command =
        match harness with Harness.Pi -> pi_command | Harness.Codex -> codex_command
      in
      operations.doctor ~home ~harness ~harness_command ~wt_command ~backend
        ~worktree_mode |> exit_code
 in
let doctor_term =
  Cmdliner.Term.(
    const doctor $ home_arg $ harness_arg $ pi_command_arg $ codex_command_arg
    $ wt_command_arg $ backend_arg
    $ worktree_arg)
 in
let settings_show home =
  match Settings.load ~home with
  | Error message -> exit_code (Error message)
  | Ok settings ->
      Fmt.pr "%s" (Settings.render settings);
      0
 in
let settings_show_term = Cmdliner.Term.(const settings_show $ home_arg)
 in
let settings_get key home =
  match Settings.load ~home with
  | Error message -> exit_code (Error message)
  | Ok settings -> (
      match String.lowercase_ascii key with
      | "harness" ->
          Fmt.pr "%s\n"
            (settings.Settings.harness
            |> Option.value ~default:Harness.Pi |> Harness.to_string);
          0
      | "codex-yolo" ->
          Fmt.pr "%s\n" (if settings.Settings.codex_yolo then "true" else "false");
          0
      | "branch-prefix" ->
          Fmt.pr "%s\n"
            (settings.Settings.branch_prefix
            |> Option.value ~default:"monty");
          0
      | _ -> exit_code (Error (Printf.sprintf "unknown setting %S" key)))
 in
let settings_get_term =
  let key =
    let doc = "Setting name." in
    Cmdliner.Arg.(required & pos 0 (some string) None & info [] ~docv:"SETTING" ~doc)
  in
  Cmdliner.Term.(const settings_get $ key $ home_arg)
 in
let settings_set key value home =
  match String.lowercase_ascii key with
  | "harness" -> (
      match Harness.of_string value with
      | Error (`Msg message) -> exit_code (Error message)
      | Ok harness ->
          Settings.set_harness ~home harness
          |> Result.map (fun () -> Fmt.pr "harness = %s\n" (Harness.to_string harness))
          |> exit_code)
  | "codex-yolo" -> (
      match Settings.bool_of_string value with
      | Error message -> exit_code (Error message)
      | Ok enabled ->
          Settings.set_codex_yolo ~home enabled
          |> Result.map (fun () ->
                 Fmt.pr "codex-yolo = %s\n"
                   (if enabled then "true" else "false"))
          |> exit_code)
  | "branch-prefix" ->
      Settings.set_branch_prefix ~home value
      |> Result.map (fun () -> Fmt.pr "branch-prefix = %s\n" value)
      |> exit_code
  | _ -> exit_code (Error (Printf.sprintf "unknown setting %S" key))
 in
let settings_set_term =
  let key =
    let doc = "Setting name." in
    Cmdliner.Arg.(required & pos 0 (some string) None & info [] ~docv:"SETTING" ~doc)
  in
  let value =
    let doc = "New setting value." in
    Cmdliner.Arg.(required & pos 1 (some string) None & info [] ~docv:"VALUE" ~doc)
  in
  Cmdliner.Term.(const settings_set $ key $ value $ home_arg)
 in
let settings_cmd =
  let show_cmd =
    Cmdliner.Cmd.v
      (Cmdliner.Cmd.info "show" ~doc:"Show all persisted Monty settings.")
      settings_show_term
  in
  let get_cmd =
    Cmdliner.Cmd.v
      (Cmdliner.Cmd.info "get" ~doc:"Print one persisted Monty setting.")
      settings_get_term
  in
  let set_cmd =
    Cmdliner.Cmd.v
      (Cmdliner.Cmd.info "set" ~doc:"Set one persisted Monty setting.")
      settings_set_term
  in
  Cmdliner.Cmd.group ~default:settings_show_term
    (Cmdliner.Cmd.info "settings" ~doc:"Show and change persisted Monty settings.")
    [ show_cmd; get_cmd; set_cmd ]
 in
let start_cmd =
  let doc = "Start the head-butler agent session in the Monty control room." in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "start" ~doc) start_term
 in
let launch_cmd =
  let doc = "Launch one worker agent session." in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "launch" ~doc) launch_term
 in
let launch_many_cmd =
  let doc = "Launch multiple worker agent sessions from a JSON manifest." in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "launch-many" ~doc) launch_many_term
 in
let resume_cmd =
  let doc = "Resume a worker agent session from durable Monty memory." in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "resume" ~doc) resume_term
 in
let open_cmd =
  let doc = "Open a worker agent session from durable Monty memory. Alias for resume." in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "open" ~doc) resume_term
 in
let done_cmd =
  let doc = "Mark a worker job done, close its linked local task, delete its worktree and branch, and archive its memory." in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "done" ~doc) complete_term
 in
let list_cmd =
  let doc = "List Monty tasks from the local task source of truth." in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "list" ~doc) list_jobs_term
 in
let ensure_worktree_cmd =
  let doc = "Create or reuse a worktree for a repo and branch, disambiguating by repo." in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "ensure-worktree" ~doc) ensure_worktree_term
 in
let overview_cmd =
  let doc = "Show a cross-project overview of projects, tasks, and active jobs." in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "overview" ~doc) overview_term
 in
let projects_cmd =
  let list_cmd =
    let doc = "List known Monty projects." in
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "list" ~doc) projects_list_term
  in
  let show_cmd =
    let doc = "Show project memory and task sources." in
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "show" ~doc) projects_show_term
  in
  let add_cmd =
    let doc = "Add a project to Monty's overview." in
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "add" ~doc) projects_add_term
  in
  let doc = "Manage Monty project memory." in
  Cmdliner.Cmd.group (Cmdliner.Cmd.info "projects" ~doc) [ list_cmd; show_cmd; add_cmd ]
 in
let tasks_cmd =
  let list_cmd =
    let doc = "List external and local tasks." in
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "list" ~doc) tasks_list_term
  in
  let sync_cmd =
    let doc = "Sync worker jobs into the local task source of truth." in
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "sync" ~doc) tasks_sync_term
  in
  let repair_cmd =
    let doc = "Explicitly link one legacy worker by title or branch, rejecting ambiguity." in
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "repair-worker" ~doc) tasks_repair_worker_term
  in
  let doc = "Read task summaries from sources of truth." in
  Cmdliner.Cmd.group (Cmdliner.Cmd.info "tasks" ~doc) [ list_cmd; sync_cmd; repair_cmd ]
 in
let task_cmd =
  let add_cmd =
    let doc = "Add a Monty-owned local task." in
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "add" ~doc) task_add_term
  in
  let done_cmd =
    let doc = "Mark a Monty-owned local task done." in
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "done" ~doc) task_done_term
  in
  let show_cmd =
    let doc = "Show task workspaces with absolute repo and worktree paths." in
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "show" ~doc) task_show_term
  in
  let workspace_cmd =
    let add_cmd =
      let doc = "Attach planned repo-plus-branch metadata to an open task." in
      Cmdliner.Cmd.v (Cmdliner.Cmd.info "add" ~doc) task_workspace_add_term
    in
    let ensure_cmd =
      let doc = "Materialize and persist one or all linked task workspaces." in
      Cmdliner.Cmd.v (Cmdliner.Cmd.info "ensure" ~doc) task_workspace_ensure_term
    in
    Cmdliner.Cmd.group
      (Cmdliner.Cmd.info "workspace" ~doc:"Manage task workspaces.")
      [ add_cmd; ensure_cmd ]
  in
  let merge_cmd =
    let doc = "Merge one unlaunched open task into another." in
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "merge" ~doc) task_merge_term
  in
  let doc = "Manage Monty-owned local task data." in
  Cmdliner.Cmd.group (Cmdliner.Cmd.info "task" ~doc)
    [ add_cmd; show_cmd; workspace_cmd; merge_cmd; done_cmd ]
 in
let headless_cmd =
  let prepare_cmd =
    let doc = "Reserve jobs and materialize their Monty-owned worktrees for headless dispatch." in
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "prepare-many" ~doc) headless_prepare_many_term
  in
  let begin_cmd =
    let doc = "Claim one prepared worker and emit its harness subagent call." in
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "begin" ~doc)
      (headless_worker_term headless_begin)
  in
  let run_cmd =
    let doc = "Run one prepared worker through the non-interactive Codex review chain." in
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "run" ~doc)
      (headless_worker_term headless_run)
  in
  let run_many_cmd =
    let doc = "Run prepared manifest workers concurrently through Codex review chains." in
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "run-many" ~doc) headless_run_many_term
  in
  let resume_cmd =
    let doc = "Intentionally start or emit a successor headless chain for an open worker." in
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "resume" ~doc)
      (headless_worker_term headless_resume)
  in
  let finish_cmd =
    let doc = "Finalize a returned Pi chain through the shared durable run-handoff contract." in
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "finish" ~doc) headless_finish_term
  in
  let doc = "Prepare and run headless Pi or Codex worker chains." in
  Cmdliner.Cmd.group (Cmdliner.Cmd.info "headless" ~doc)
    [ prepare_cmd; begin_cmd; run_cmd; run_many_cmd; resume_cmd; finish_cmd ]
 in
let handoff_cmd =
  let publish_cmd =
    let doc = "Publish a versioned run handoff without marking the task done." in
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "publish" ~doc) handoff_publish_term
  in
  let pending_cmd =
    let doc = "Show unacknowledged finished-run notices for safe-boundary delivery." in
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "pending" ~doc) handoff_pending_term
  in
  let acknowledge_cmd =
    let doc = "Idempotently acknowledge a displayed finished-run notice." in
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "acknowledge" ~doc) handoff_acknowledge_term
  in
  let show_cmd =
    let doc = "Show the latest or named durable handoff for one worker." in
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "show" ~doc) handoff_show_term
  in
  let follow_up_cmd =
    let doc = "Build a versioned read-only follow-up dispatch from durable run evidence." in
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "follow-up" ~doc) handoff_follow_up_term
  in
  let doc = "Publish, deliver, inspect, and ask follow-ups about worker runs." in
  Cmdliner.Cmd.group (Cmdliner.Cmd.info "handoff" ~doc)
    [ publish_cmd; pending_cmd; acknowledge_cmd; show_cmd; follow_up_cmd ]
 in
let doctor_cmd =
  let doc = "Check Monty launch dependencies." in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "doctor" ~doc) doctor_term
 in
let main_cmd =
  let doc = "Monty, the head butler for Pi and Codex worker sessions." in
  let man =
    [ `S Cmdliner.Manpage.s_description;
      `P "Run monty with no subcommand to start the head-butler agent session in this repo.";
      `P "Use launch or launch-many when the head-butler needs to spin out worker sessions.";
      `P "Use open or resume to reopen an existing worker from durable Monty memory." ]
  in
  Cmdliner.Cmd.group ~default:start_term
    (Cmdliner.Cmd.info "monty" ~version:"dev" ~doc ~man)
    [
      start_cmd;
      launch_cmd;
      launch_many_cmd;
      open_cmd;
      resume_cmd;
      done_cmd;
      list_cmd;
      overview_cmd;
      projects_cmd;
      settings_cmd;
      tasks_cmd;
      task_cmd;
      headless_cmd;
      handoff_cmd;
      ensure_worktree_cmd;
      doctor_cmd;
    ]
 in
main_cmd

let eval ?getenv ?operations argv =
  Cmdliner.Cmd.eval' ~argv (make_cmd ?getenv ?operations ())
