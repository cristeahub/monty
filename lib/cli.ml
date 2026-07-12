let exit_code = function
  | Ok () -> 0
  | Error msg ->
      Fmt.epr "monty: %s\n" msg;
      1

type operations = {
  start : name:string -> home:string -> pi_command:string -> (unit, string) result;
  launch_one : Launcher.options -> Job.t -> (unit, string) result;
  doctor :
    home:string ->
    pi_command:string ->
    wt_command:string ->
    backend:Terminal.backend ->
    worktree_mode:Launcher.worktree_mode ->
    (unit, string) result;
}

let default_operations =
  {
    start = (fun ~name ~home ~pi_command -> Head_butler.start ~home ~pi_command ~name);
    launch_one = Launcher.launch_one;
    doctor =
      (fun ~home ~pi_command ~wt_command ~backend ~worktree_mode ->
        Doctor.run ~home ~pi_command ~wt_command ~backend ~worktree_mode);
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
  let doc = "Prefix for automatically generated worktree branches. Defaults to MONTY_BRANCH_PREFIX or monty." in
  Cmdliner.Arg.(value & opt string (env_default getenv "MONTY_BRANCH_PREFIX" "monty") & info [ "branch-prefix" ] ~docv:"PREFIX" ~doc)
 in
let fork_arg =
  let doc = "Optional pi session id or path to fork for worker sessions." in
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
let options backend target pi_command wt_command worktree_mode branch_prefix fork home script_dir =
  let script_dir =
    match script_dir with
    | Some dir -> Shell.normalize (Shell.abs_path dir)
    | None -> Home.runtime_script_dir ~home () |> Shell.normalize
  in
  Launcher.{
    backend;
    target;
    pi_command;
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
    const options $ backend_arg $ target_arg $ pi_command_arg $ wt_command_arg $ worktree_arg
    $ branch_prefix_arg $ fork_arg $ home_arg $ script_dir_arg)
 in
let headless_options pi_command wt_command branch_prefix fork home script_dir =
  options Terminal.Dry_run Terminal.Tab pi_command wt_command Launcher.Always
    branch_prefix fork home script_dir
 in
let headless_options_term =
  Cmdliner.Term.(
    const headless_options $ pi_command_arg $ wt_command_arg $ branch_prefix_arg
    $ fork_arg $ home_arg $ script_dir_arg)
 in
let start name home pi_command = operations.start ~name ~home ~pi_command |> exit_code
 in
let start_term =
  let name_arg =
    let doc = "Session name for the head-butler pi session." in
    Cmdliner.Arg.(value & opt string "Monty Head Butler" & info [ "name"; "n" ] ~docv:"NAME" ~doc)
  in
  Cmdliner.Term.(const start $ name_arg $ home_arg $ pi_command_arg)
 in
let launch repo title context branch options =
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
    let doc = "Markdown context file to pass to pi as an @file." in
    Cmdliner.Arg.(required & opt (some string) None & info [ "context" ] ~docv:"FILE" ~doc)
  in
  let branch =
    let doc = "Worktree branch name. Defaults to <branch-prefix>/<title-slug>." in
    Cmdliner.Arg.(value & opt (some string) None & info [ "branch" ] ~docv:"BRANCH" ~doc)
  in
  Cmdliner.Term.(const launch $ repo $ title $ context $ branch $ options_term)
 in
let launch_many manifest options =
  let manifest = Shell.abs_path manifest |> Shell.normalize in
  match Manifest.load ~home:options.Launcher.home manifest with
  | Error msg -> exit_code (Error msg)
  | Ok jobs ->
      let retry_command = Launcher.retry_launch_many_command options manifest in
      Launcher.launch_many ~retry_command options jobs |> exit_code
 in
let launch_many_term =
  let manifest =
    let doc = "JSON manifest containing a jobs array." in
    Cmdliner.Arg.(required & opt (some string) None & info [ "manifest" ] ~docv:"FILE" ~doc)
  in
  Cmdliner.Term.(const launch_many $ manifest $ options_term)
 in
let headless_prepare_many manifest dry_run options =
  let manifest = Shell.abs_path manifest |> Shell.normalize in
  match Manifest.load ~home:options.Launcher.home manifest with
  | Error msg -> exit_code (Error msg)
  | Ok jobs -> (
      match Headless.prepare_many ~dry_run options jobs with
      | Error msg -> exit_code (Error msg)
      | Ok json ->
          Headless.print_json json;
          0)
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
let headless_begin explicit_resume worker options =
  match Headless.begin_worker ~explicit_resume options worker with
  | Error msg -> exit_code (Error msg)
  | Ok json ->
      Headless.print_json json;
      0
 in
let headless_worker_term explicit_resume =
  let worker =
    let doc = "Worker id, branch leaf, branch, or title slug." in
    Cmdliner.Arg.(required & pos 0 (some string) None & info [] ~docv:"WORKER" ~doc)
  in
  Cmdliner.Term.(const (headless_begin explicit_resume) $ worker $ headless_options_term)
 in
let resume archived worker options =
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
let doctor home pi_command wt_command backend worktree_mode =
  operations.doctor ~home ~pi_command ~wt_command ~backend ~worktree_mode |> exit_code
 in
let doctor_term =
  Cmdliner.Term.(
    const doctor $ home_arg $ pi_command_arg $ wt_command_arg $ backend_arg
    $ worktree_arg)
 in
let start_cmd =
  let doc = "Start the head-butler pi session in the Monty control room." in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "start" ~doc) start_term
 in
let launch_cmd =
  let doc = "Launch one worker pi session." in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "launch" ~doc) launch_term
 in
let launch_many_cmd =
  let doc = "Launch multiple worker pi sessions from a JSON manifest." in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "launch-many" ~doc) launch_many_term
 in
let resume_cmd =
  let doc = "Resume a worker pi session from durable Monty memory." in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "resume" ~doc) resume_term
 in
let open_cmd =
  let doc = "Open a worker pi session from durable Monty memory. Alias for resume." in
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
  let doc = "Manage Monty-owned local task data." in
  Cmdliner.Cmd.group (Cmdliner.Cmd.info "task" ~doc) [ add_cmd; done_cmd ]
 in
let headless_cmd =
  let prepare_cmd =
    let doc = "Reserve jobs and materialize their Monty-owned worktrees for headless dispatch." in
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "prepare-many" ~doc) headless_prepare_many_term
  in
  let begin_cmd =
    let doc = "Claim one prepared worker and emit its harness subagent call." in
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "begin" ~doc) (headless_worker_term false)
  in
  let resume_cmd =
    let doc = "Intentionally emit a successor harness call for an open worker." in
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "resume" ~doc) (headless_worker_term true)
  in
  let doc = "Generate headless calls for the harness subagent tool." in
  Cmdliner.Cmd.group (Cmdliner.Cmd.info "headless" ~doc)
    [ prepare_cmd; begin_cmd; resume_cmd ]
 in
let doctor_cmd =
  let doc = "Check Monty launch dependencies." in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "doctor" ~doc) doctor_term
 in
let main_cmd =
  let doc = "Monty, the head butler for pi worker sessions." in
  let man =
    [ `S Cmdliner.Manpage.s_description;
      `P "Run monty with no subcommand to start the head-butler pi session in this repo.";
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
      tasks_cmd;
      task_cmd;
      headless_cmd;
      ensure_worktree_cmd;
      doctor_cmd;
    ]
 in
main_cmd

let eval ?getenv ?operations argv =
  Cmdliner.Cmd.eval' ~argv (make_cmd ?getenv ?operations ())
