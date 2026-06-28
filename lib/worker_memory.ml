let now_utc () =
  let tm = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec

let default_worker_dir ~home ~id =
  Filename.concat home (Filename.concat ".monty/runs/manual/workers" id)
  |> Shell.normalize

let worker_dir ~home ~id job =
  match job.Job.worker_dir with
  | Some dir -> Shell.normalize (Shell.abs_path dir)
  | None -> default_worker_dir ~home ~id

let run_dir_of_worker_dir worker_dir =
  worker_dir |> Filename.dirname |> Filename.dirname |> Shell.normalize

let file worker_dir name = Filename.concat worker_dir name

let instructions_file worker_dir = file worker_dir "MONTY.md"
let memory_file worker_dir = file worker_dir "memory.md"
let job_file worker_dir = file worker_dir "job.json"
let artifacts_dir worker_dir = Filename.concat worker_dir "artifacts"

let maybe_assoc name = function None -> [] | Some value -> [ (name, `String value) ]

let write_job_json ~worker_dir ~id ~job ~branch ~repo ~context ~worktree_mode
    ~last_known_worktree =
  Shell.ensure_dir worker_dir;
  let json =
    `Assoc
      ([ ("id", `String id);
         ("title", `String job.Job.title);
         ("repo", `String repo);
         ("branch", `String branch);
         ("context", `String context);
         ("worker_dir", `String worker_dir);
         ("run_dir", `String (run_dir_of_worker_dir worker_dir));
         ("worktree_mode", `String worktree_mode);
         ("updated_at", `String (now_utc ())) ]
      @ maybe_assoc "last_known_worktree" last_known_worktree)
  in
  Yojson.Safe.to_file (job_file worker_dir) json

let init_memory ~worker_dir ~title =
  let path = memory_file worker_dir in
  if Sys.file_exists path then ()
  else
    Shell.write_file path
      (String.concat "\n"
         [ "# Worker memory: " ^ title;
           "";
           "Use this file, or create other files in this folder, for anything the head butler should remember.";
           "Keep durable notes here, not only in the worktree, because wt worktrees may be deleted and recreated.";
           "" ])

let write_instructions ~worker_dir ~id ~job ~branch ~repo ~context ~worktree_mode =
  Shell.ensure_dir (artifacts_dir worker_dir);
  init_memory ~worker_dir ~title:job.Job.title;
  let run_dir = run_dir_of_worker_dir worker_dir in
  let text =
    String.concat "\n"
      [ "# Monty worker instructions";
        "";
        "You were spawned by Monty, the head butler.";
        "";
        "## Durable memory";
        "";
        "Your durable Monty worker memory folder is:";
        "";
        "```text";
        worker_dir;
        "```";
        "";
        "Use this folder for anything the head butler should remember.";
        "You control the structure.";
        "At minimum, append important discoveries, blockers, and handoff notes to `memory.md`.";
        "Do not store durable notes only in the wt worktree.";
        "The worktree may be deleted and recreated.";
        "";
        "## Rehydrating the worktree";
        "";
        "The durable code identity for this task is the repo plus branch:";
        "";
        "```text";
        "repo: " ^ repo;
        "branch: " ^ branch;
        "```";
        "";
        "If the worktree disappears, recreate it with:";
        "";
        "```sh";
        "cd " ^ Shell.quote repo;
        "wt b " ^ Shell.quote branch;
        "```";
        "";
        "## Environment";
        "";
        "Monty exports these variables in your session:";
        "";
        "- MONTY_RUN_DIR=" ^ run_dir;
        "- MONTY_WORKER_DIR=" ^ worker_dir;
        "- MONTY_JOB_ID=" ^ id;
        "- MONTY_JOB_TITLE=" ^ job.Job.title;
        "- MONTY_JOB_REPO=" ^ repo;
        "- MONTY_JOB_BRANCH=" ^ branch;
        "- MONTY_JOB_CONTEXT=" ^ context;
        "- MONTY_JOB_WORKTREE, set dynamically after `wt b` runs";
        "- MONTY_WORKTREE_MODE=" ^ worktree_mode;
        "";
        "## Task context";
        "";
        "Read the task context file passed after this instructions file.";
        "Write back important session memory to the durable worker folder above.";
        "" ]
  in
  Shell.write_file (instructions_file worker_dir) text

let ensure ~home ~job ~branch ~repo ~context ~worktree_mode ~last_known_worktree =
  let id = Job.id_or_default ~branch job in
  let worker_dir = worker_dir ~home ~id job in
  write_instructions ~worker_dir ~id ~job ~branch ~repo ~context ~worktree_mode;
  write_job_json ~worker_dir ~id ~job ~branch ~repo ~context ~worktree_mode
    ~last_known_worktree;
  (id, worker_dir, instructions_file worker_dir)
