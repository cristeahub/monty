type options = {
  pi_command : string;
  fork : string option;
  script_dir : string;
  branch_prefix : string;
}

let script_filename ~script_dir ~title =
  let slug = Slug.of_title title in
  let stamp = Unix.gettimeofday () |> Int64.of_float |> Int64.to_string in
  Filename.concat script_dir (Printf.sprintf "%s-%d-%s.sh" stamp (Unix.getpid ()) slug)

let build_command ~options ~job ~context =
  let name = Shell.quote job.Job.title in
  let context_arg = Shell.quote ("@" ^ context) in
  let prompt = Shell.quote (Job.prompt job) in
  let fork =
    match options.fork with
    | None -> ""
    | Some id -> " --fork " ^ Shell.quote id
  in
  Printf.sprintf "exec %s --name %s%s %s %s" options.pi_command name fork context_arg
    prompt

let write_launch_script ~options ~job ~branch ~source_repo ~workdir ~context =
  Shell.ensure_dir options.script_dir;
  let path = script_filename ~script_dir:options.script_dir ~title:job.Job.title in
  let command = build_command ~options ~job ~context in
  let contents =
    String.concat "\n"
      [ "#!/bin/sh";
        "set -eu";
        "cd " ^ Shell.quote workdir;
        "printf '\\033]0;%s\\007' " ^ Shell.quote job.Job.title;
        "export MONTY_JOB_TITLE=" ^ Shell.quote job.Job.title;
        "export MONTY_BRANCH_PREFIX=" ^ Shell.quote options.branch_prefix;
        "export MONTY_JOB_BRANCH=" ^ Shell.quote branch;
        "export MONTY_JOB_REPO=" ^ Shell.quote source_repo;
        "export MONTY_JOB_WORKTREE=" ^ Shell.quote workdir;
        "export MONTY_JOB_CONTEXT=" ^ Shell.quote context;
        "printf '%s\\n' " ^ Shell.quote ("Monty worker: " ^ job.Job.title);
        "printf '%s\\n' " ^ Shell.quote ("Worktree: " ^ workdir);
        "printf '%s\\n\\n' " ^ Shell.quote ("Context: " ^ context);
        command;
        "" ]
  in
  Shell.write_file path contents;
  Shell.chmod_executable path;
  path
