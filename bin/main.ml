open Monty

let exit_code = function
  | Ok () -> 0
  | Error msg ->
      Fmt.epr "monty: %s\n" msg;
      1

let env_default name default =
  match Sys.getenv_opt name with Some value when String.trim value <> "" -> value | _ -> default

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

let backend_default () =
  match Terminal.backend_of_string (Terminal.default_backend ()) with
  | Ok backend -> backend
  | Error _ -> Terminal.Ghostty

let target_default () =
  match Terminal.target_of_string (Terminal.default_target ()) with
  | Ok target -> target
  | Error _ -> Terminal.Tab

let worktree_default () =
  match Launcher.worktree_mode_of_string (env_default "MONTY_WORKTREE" "always") with
  | Ok mode -> mode
  | Error _ -> Launcher.Always

let home_arg =
  let doc = "Monty control-room directory. Defaults to MONTY_HOME or the nearest parent dune-project named monty." in
  Cmdliner.Arg.(value & opt string (Home.default ()) & info [ "home" ] ~docv:"DIR" ~doc)

let pi_command_arg =
  let doc = "Shell command used to start pi. May include fixed arguments." in
  Cmdliner.Arg.(value & opt string (env_default "MONTY_PI_COMMAND" "pi") & info [ "pi-command" ] ~docv:"COMMAND" ~doc)

let wt_command_arg =
  let doc = "Shell command used to invoke the wt CLI." in
  Cmdliner.Arg.(value & opt string (env_default "MONTY_WT_COMMAND" "wt") & info [ "wt-command" ] ~docv:"COMMAND" ~doc)

let backend_arg =
  let doc = "Terminal backend: ghostty or dry-run." in
  Cmdliner.Arg.(value & opt backend_conv (backend_default ()) & info [ "terminal" ] ~docv:"BACKEND" ~doc)

let target_arg =
  let doc = "Where to launch the worker: tab, window, or split." in
  Cmdliner.Arg.(value & opt target_conv (target_default ()) & info [ "target" ] ~docv:"TARGET" ~doc)

let worktree_arg =
  let doc = "Worktree mode: always or never." in
  Cmdliner.Arg.(value & opt worktree_conv (worktree_default ()) & info [ "worktree" ] ~docv:"MODE" ~doc)

let branch_prefix_arg =
  let doc = "Prefix for automatically generated worktree branches. Defaults to MONTY_BRANCH_PREFIX or monty." in
  Cmdliner.Arg.(value & opt string (env_default "MONTY_BRANCH_PREFIX" "monty") & info [ "branch-prefix" ] ~docv:"PREFIX" ~doc)

let fork_arg =
  let doc = "Optional pi session id or path to fork for worker sessions." in
  Cmdliner.Arg.(value & opt (some string) None & info [ "fork" ] ~docv:"SESSION" ~doc)

let script_dir_arg =
  let doc = "Directory where generated worker launch scripts are written." in
  Cmdliner.Arg.(value & opt (some string) None & info [ "script-dir" ] ~docv:"DIR" ~doc)

let options backend target pi_command wt_command worktree_mode branch_prefix fork home script_dir =
  let script_dir =
    match script_dir with
    | Some dir -> Shell.normalize (Shell.abs_path dir)
    | None -> Home.runtime_script_dir ~home () |> Shell.normalize
  in
  Launcher.{ backend; target; pi_command; wt_command; worktree_mode; branch_prefix; fork; home; script_dir }

let options_term =
  Cmdliner.Term.(
    const options $ backend_arg $ target_arg $ pi_command_arg $ wt_command_arg $ worktree_arg
    $ branch_prefix_arg $ fork_arg $ home_arg $ script_dir_arg)

let start name home pi_command = Head_butler.start ~home ~pi_command ~name |> exit_code

let start_term =
  let name_arg =
    let doc = "Session name for the head-butler pi session." in
    Cmdliner.Arg.(value & opt string "Monty Head Butler" & info [ "name"; "n" ] ~docv:"NAME" ~doc)
  in
  Cmdliner.Term.(const start $ name_arg $ home_arg $ pi_command_arg)

let launch repo title context branch options =
  let cwd = Sys.getcwd () in
  let repo = Shell.normalize (Shell.abs_path ~base:cwd repo) in
  let context = Shell.normalize (Shell.abs_path ~base:cwd context) in
  let job = Job.make ?branch ~title ~repo ~context () in
  Launcher.launch_one options job |> exit_code

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
    let doc = "Markdown context file to pass to pi as an @file." in
    Cmdliner.Arg.(required & opt (some string) None & info [ "context" ] ~docv:"FILE" ~doc)
  in
  let branch =
    let doc = "Worktree branch name. Defaults to <branch-prefix>/<title-slug>." in
    Cmdliner.Arg.(value & opt (some string) None & info [ "branch" ] ~docv:"BRANCH" ~doc)
  in
  Cmdliner.Term.(const launch $ repo $ title $ context $ branch $ options_term)

let launch_many manifest options =
  match Manifest.load manifest with
  | Error msg -> exit_code (Error msg)
  | Ok jobs -> Launcher.launch_many options jobs |> exit_code

let launch_many_term =
  let manifest =
    let doc = "JSON manifest containing a jobs array." in
    Cmdliner.Arg.(required & opt (some string) None & info [ "manifest" ] ~docv:"FILE" ~doc)
  in
  Cmdliner.Term.(const launch_many $ manifest $ options_term)

let resume archived worker options =
  let scope = if archived then Job_store.Archived else Job_store.Active in
  match Resume.find_record ~home:options.Launcher.home ~scope worker with
  | Error msg -> exit_code (Error msg)
  | Ok record -> (
      let job =
        if archived then
          match options.Launcher.backend with
          | Terminal.Dry_run -> Ok (Job_store.active_job record)
          | Terminal.Ghostty -> Job_store.reactivate record
        else Ok record.Job_store.job
      in
      match job with
      | Error msg -> exit_code (Error msg)
      | Ok job -> Launcher.launch_one options job |> exit_code)

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

let complete worker force home wt_command =
  Done.complete ?worker ~home ~wt_command ~force () |> exit_code

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

let list_jobs archived all run home =
  let scope = if all then Job_store.All else if archived then Job_store.Archived else Job_store.Active in
  List_jobs.run ~home ~scope ?run () |> exit_code

let list_jobs_term =
  let archived =
    let doc = "List archived jobs instead of active jobs." in
    Cmdliner.Arg.(value & flag & info [ "archived" ] ~doc)
  in
  let all =
    let doc = "List active and archived jobs." in
    Cmdliner.Arg.(value & flag & info [ "all" ] ~doc)
  in
  let run =
    let doc = "Only list jobs for a run directory name or path." in
    Cmdliner.Arg.(value & opt (some string) None & info [ "run" ] ~docv:"RUN" ~doc)
  in
  Cmdliner.Term.(const list_jobs $ archived $ all $ run $ home_arg)

let doctor pi_command wt_command = Doctor.run ~pi_command ~wt_command |> exit_code

let doctor_term = Cmdliner.Term.(const doctor $ pi_command_arg $ wt_command_arg)

let start_cmd =
  let doc = "Start the head-butler pi session in the Monty control room." in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "start" ~doc) start_term

let launch_cmd =
  let doc = "Launch one worker pi session." in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "launch" ~doc) launch_term

let launch_many_cmd =
  let doc = "Launch multiple worker pi sessions from a JSON manifest." in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "launch-many" ~doc) launch_many_term

let resume_cmd =
  let doc = "Resume a worker pi session from durable Monty memory." in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "resume" ~doc) resume_term

let done_cmd =
  let doc = "Mark a worker job done, delete its worktree and branch, and archive its memory." in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "done" ~doc) complete_term

let list_cmd =
  let doc = "List active or archived Monty jobs." in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "list" ~doc) list_jobs_term

let doctor_cmd =
  let doc = "Check Monty launch dependencies." in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "doctor" ~doc) doctor_term

let main_cmd =
  let doc = "Monty, the head butler for pi worker sessions." in
  let man =
    [ `S Cmdliner.Manpage.s_description;
      `P "Run monty with no subcommand to start the head-butler pi session in this repo.";
      `P "Use launch or launch-many when the head-butler needs to spin out worker sessions." ]
  in
  Cmdliner.Cmd.group ~default:start_term
    (Cmdliner.Cmd.info "monty" ~version:"dev" ~doc ~man)
    [ start_cmd; launch_cmd; launch_many_cmd; resume_cmd; done_cmd; list_cmd; doctor_cmd ]

let () = exit (Cmdliner.Cmd.eval' main_cmd)
