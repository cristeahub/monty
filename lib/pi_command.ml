type options = {
  pi_command : string;
  fork : string option;
  script_dir : string;
  branch_prefix : string;
  monty_command : string;
}

let script_filename ~script_dir ~title =
  let slug = Slug.of_title title in
  let stamp = Unix.gettimeofday () |> Int64.of_float |> Int64.to_string in
  Filename.concat script_dir (Printf.sprintf "%s-%d-%s.sh" stamp (Unix.getpid ()) slug)

let build_command ~options ~instructions ~job ~context =
  let name = Shell.quote job.Job.title in
  let instruction_arg =
    match instructions with
    | None -> ""
    | Some path -> " " ^ Shell.quote ("@" ^ path)
  in
  let context_arg = Shell.quote ("@" ^ context) in
  let prompt = Shell.quote (Job.prompt job) in
  let fork =
    match options.fork with
    | None -> ""
    | Some id -> " --fork " ^ Shell.quote id
  in
  Printf.sprintf "exec %s --name %s%s%s %s %s" options.pi_command name fork
    instruction_arg context_arg prompt

let rehydrate_lines ~monty_command ~wt_command ~branch ~source_repo =
  [ "MONTY_JOB_WORKTREE=$("
    ^ Shell.quote monty_command
    ^ " ensure-worktree --repo "
    ^ Shell.quote source_repo ^ " --branch " ^ Shell.quote branch
    ^ " --wt-command " ^ Shell.quote wt_command ^ ")";
    "if [ -z \"$MONTY_JOB_WORKTREE\" ] || [ ! -d \"$MONTY_JOB_WORKTREE\" ]; then";
    "  printf '%s\\n' 'monty did not return an existing worktree path' >&2";
    "  exit 1";
    "fi";
    "cd \"$MONTY_JOB_WORKTREE\"" ]

let write_launch_script ~options ~job ~id ~branch ~source_repo ~initial_workdir
    ~context ~instructions ~worker_dir ~worktree_mode ~wt_command =
  Shell.ensure_dir options.script_dir;
  let path = script_filename ~script_dir:options.script_dir ~title:job.Job.title in
  let command = build_command ~options ~instructions:(Some instructions) ~job ~context in
  let setup_lines =
    match worktree_mode with
    | "always" -> rehydrate_lines ~monty_command:options.monty_command ~wt_command ~branch ~source_repo
    | _ -> [ "cd " ^ Shell.quote initial_workdir; "MONTY_JOB_WORKTREE=" ^ Shell.quote initial_workdir ]
  in
  let contents =
    String.concat "\n"
      ([ "#!/bin/sh";
         "set -eu";
         "printf '\\033]0;%s\\007' " ^ Shell.quote job.Job.title;
         "export MONTY_BRANCH_PREFIX=" ^ Shell.quote options.branch_prefix;
         "export MONTY_RUN_DIR=" ^ Shell.quote (Worker_memory.run_dir_of_worker_dir worker_dir);
         "export MONTY_WORKER_DIR=" ^ Shell.quote worker_dir;
         "export MONTY_JOB_ID=" ^ Shell.quote id;
         "export MONTY_JOB_TITLE=" ^ Shell.quote job.Job.title;
         "export MONTY_JOB_BRANCH=" ^ Shell.quote branch;
         "export MONTY_JOB_REPO=" ^ Shell.quote source_repo;
         "export MONTY_JOB_CONTEXT=" ^ Shell.quote context;
         "export MONTY_WORKTREE_MODE=" ^ Shell.quote worktree_mode ]
      @ setup_lines
      @ [ "export MONTY_JOB_WORKTREE";
          "printf '%s\\n' " ^ Shell.quote ("Monty worker: " ^ job.Job.title);
          "printf '%s\\n' " ^ Shell.quote ("Worker memory: " ^ worker_dir);
          "printf 'Worktree: %s\\n' \"$MONTY_JOB_WORKTREE\"";
          "printf '%s\\n\\n' " ^ Shell.quote ("Context: " ^ context);
          command;
          "" ])
  in
  Shell.write_file path contents;
  Shell.chmod_executable path;
  path
