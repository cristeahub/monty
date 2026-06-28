type worktree_mode = Always | Never

type options = {
  backend : Terminal.backend;
  target : Terminal.target;
  pi_command : string;
  wt_command : string;
  worktree_mode : worktree_mode;
  fork : string option;
  script_dir : string;
}

let worktree_mode_of_string = function
  | "always" | "yes" | "true" -> Ok Always
  | "never" | "no" | "false" -> Ok Never
  | value -> Error (`Msg (Printf.sprintf "unknown worktree mode %S" value))

let worktree_mode_to_string = function Always -> "always" | Never -> "never"

let ensure_directory label path =
  if Sys.file_exists path && Sys.is_directory path then Ok ()
  else Error (Printf.sprintf "%s is not an existing directory: %s" label path)

let ensure_file label path =
  if Sys.file_exists path && not (Sys.is_directory path) then Ok ()
  else Error (Printf.sprintf "%s is not an existing file: %s" label path)

let dry_run ~options ~job ~branch ~repo ~context =
  let wt_line, workdir =
    match options.worktree_mode with
    | Never -> (None, repo)
    | Always ->
        ( Some
            (Printf.sprintf "cd %s && %s b %s" (Shell.quote repo)
               options.wt_command (Shell.quote branch)),
          "<worktree printed by wt>" )
  in
  Fmt.pr "[dry-run] job: %s\n" job.Job.title;
  Option.iter (Fmt.pr "[dry-run] worktree: %s\n") wt_line;
  Fmt.pr "[dry-run] workdir: %s\n" workdir;
  Fmt.pr "[dry-run] context: %s\n" context;
  Fmt.pr "[dry-run] terminal: %s %s\n"
    (Terminal.backend_to_string options.backend)
    (Terminal.target_to_string options.target);
  let pi_options =
    Pi_command.{ pi_command = options.pi_command; fork = options.fork; script_dir = options.script_dir }
  in
  Fmt.pr "[dry-run] pi: %s\n" (Pi_command.build_command ~options:pi_options ~job ~context);
  Ok ()

let launch_one ?index options job =
  let repo = Shell.normalize (Shell.abs_path job.Job.repo) in
  let context = Shell.normalize (Shell.abs_path job.Job.context) in
  let branch = Job.branch_or_default ?index job in
  match options.backend with
  | Terminal.Dry_run -> dry_run ~options ~job ~branch ~repo ~context
  | Terminal.Ghostty ->
      let ( let* ) = Result.bind in
      let* () = ensure_directory "repo" repo in
      let* () = ensure_file "context" context in
      let* workdir =
        match options.worktree_mode with
        | Never -> Ok repo
        | Always -> Wt.create_or_reuse ~wt_command:options.wt_command ~repo ~branch
      in
      let pi_options =
        Pi_command.{ pi_command = options.pi_command; fork = options.fork; script_dir = options.script_dir }
      in
      let script_path =
        Pi_command.write_launch_script ~options:pi_options ~job ~branch
          ~source_repo:repo ~workdir ~context
      in
      let* () = Ghostty.launch ~target:options.target ~workdir ~script_path in
      Fmt.pr "Launched %S in %s\n" job.Job.title workdir;
      Ok ()

let launch_many options indexed_jobs =
  let rec loop = function
    | [] -> Ok ()
    | (index, job) :: rest -> (
        match launch_one ~index options job with
        | Ok () -> loop rest
        | Error msg -> Error msg)
  in
  loop indexed_jobs
