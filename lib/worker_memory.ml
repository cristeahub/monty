let now_utc () =
  let tm = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec

let default_worker_dir ~home ~id =
  Filename.concat home (Filename.concat ".monty/runs/manual/workers" id)
  |> Shell.normalize

let worker_state ~home ~id job =
  let ( let* ) = Result.bind in
  let* id = State_path.safe_component ~label:"worker id" id in
  match job.Job.worker_dir with
  | Some dir -> State_path.of_worker_dir ~home ~id (Shell.abs_path dir)
  | None -> State_path.active ~home ~run_id:"manual" ~id

let worker_dir_result ~home ~id job =
  worker_state ~home ~id job |> Result.map (fun state -> state.State_path.worker_dir)

let worker_dir ~home ~id job =
  match worker_dir_result ~home ~id job with
  | Ok path -> path
  | Error msg -> invalid_arg msg

let run_dir_of_worker_dir worker_dir =
  worker_dir |> Filename.dirname |> Filename.dirname |> Shell.normalize

let file worker_dir name = Filename.concat worker_dir name

let instructions_file worker_dir = file worker_dir "MONTY.md"
let memory_file worker_dir = file worker_dir "memory.md"
let job_file worker_dir = file worker_dir "job.json"
let artifacts_dir worker_dir = Filename.concat worker_dir "artifacts"

let maybe_assoc name = function None -> [] | Some value -> [ (name, `String value) ]

let job_json ?(status = "active") ?launch_script ?launch_error ~worker_dir ~id
    ~job ~branch ~repo ~context ~worktree_mode ~last_known_worktree () =
  `Assoc
    ([ ("id", `String id);
       ("title", `String job.Job.title);
       ("repo", `String repo);
       ("branch", `String branch);
       ("context", `String context);
       ("worker_dir", `String worker_dir);
       ("run_dir", `String (run_dir_of_worker_dir worker_dir));
       ("worktree_mode", `String worktree_mode);
       ("status", `String status);
       ("updated_at", `String (now_utc ())) ]
    @ maybe_assoc "prompt" job.Job.prompt
    @ maybe_assoc "task_key" job.Job.task_key
    @ maybe_assoc "last_known_worktree" last_known_worktree
    @ maybe_assoc "launch_script" launch_script
    @ maybe_assoc "launch_error" launch_error)

let write_job_json_unlocked ?status ?launch_script ?launch_error ~worker_dir ~id
    ~job ~branch ~repo ~context ~worktree_mode ~last_known_worktree () =
  Shell.ensure_dir worker_dir;
  State_store.write_json_atomic ~path:(job_file worker_dir)
    (job_json ?status ?launch_script ?launch_error ~worker_dir ~id ~job ~branch
       ~repo ~context ~worktree_mode ~last_known_worktree ())

let write_job_json ?status ?launch_script ?launch_error ~home ~worker_dir ~id
    ~job ~branch ~repo ~context ~worktree_mode ~last_known_worktree () =
  State_store.with_lock ~home (fun () ->
      write_job_json_unlocked ?status ?launch_script ?launch_error ~worker_dir
        ~id ~job ~branch ~repo ~context ~worktree_mode ~last_known_worktree ())

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

let write_instructions ?destination_dir ~worker_dir ~id ~job ~branch ~repo
    ~context ~worktree_mode () =
  let destination_dir = Option.value ~default:worker_dir destination_dir in
  Shell.ensure_dir (artifacts_dir destination_dir);
  init_memory ~worker_dir:destination_dir ~title:job.Job.title;
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
        "If the worktree disappears, recreate the correct repo-scoped worktree with:";
        "";
        "```sh";
        "cd \"$(monty ensure-worktree --repo " ^ Shell.quote repo ^ " --branch " ^ Shell.quote branch ^ ")\"";
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
        "- MONTY_TASK_KEY=" ^ Option.value ~default:"" job.Job.task_key;
        "- MONTY_JOB_WORKTREE, set dynamically after `wt b` runs";
        "- MONTY_WORKTREE_MODE=" ^ worktree_mode;
        "";
        "## Task context";
        "";
        "Read the task context file passed after this instructions file.";
        "Write back important session memory to the durable worker folder above.";
        "";
        "## Finishing this job";
        "";
        "When the user says the feature is done, archive the Monty job.";
        "Before completing, update durable notes in `memory.md` and make sure the code worktree is clean.";
        "If the user explicitly wants to discard local changes while archiving, use `monty done --force`.";
        "Then run:";
        "";
        "```sh";
        "monty done";
        "```";
        "";
        "This deletes the worktree and branch, moves this worker folder to the run archive, closes any linked Monty-owned local task, marks the job done, and archives its memory.";
        "After it succeeds, stop working in this session because the worktree was deleted.";
        "" ]
  in
  Shell.write_file (instructions_file destination_dir) text

let ensure_prepared_result ~home ~state ~id ~job ~branch ~repo ~context
    ~worktree_mode ~last_known_worktree =
  let ( let* ) = Result.bind in
  let* id = State_path.safe_component ~label:"worker id" id in
  if not (String.equal state.State_path.id id) then
    Error
      (Printf.sprintf "prepared worker path id %S does not match worker id %S"
         state.State_path.id id)
  else
    let worker_dir = state.State_path.worker_dir in
    let* () =
      State_store.with_lock ~home (fun () ->
          let* () = State_path.ensure_contained_for_mutation state in
          try
            write_instructions ~worker_dir ~id ~job ~branch ~repo ~context
              ~worktree_mode ();
            let* () =
              write_job_json_unlocked ~worker_dir ~id ~job ~branch ~repo ~context
                ~worktree_mode ~last_known_worktree ()
            in
            Ok ()
          with
          | Sys_error msg -> Error msg
          | Unix.Unix_error (err, fn, arg) ->
              Error
                (Printf.sprintf "failed to prepare worker memory via %s(%s): %s" fn arg
                   (Unix.error_message err))
          | Invalid_argument msg -> Error msg)
    in
    Ok (id, worker_dir, instructions_file worker_dir)

let ensure_result ~home ~job ~branch ~repo ~context ~worktree_mode
    ~last_known_worktree =
  let ( let* ) = Result.bind in
  let id = Job.id_or_default ~branch job in
  let* state = worker_state ~home ~id job in
  let* () = State_path.ensure_contained_for_mutation state in
  ensure_prepared_result ~home ~state ~id ~job ~branch ~repo ~context
    ~worktree_mode ~last_known_worktree

let ensure ~home ~job ~branch ~repo ~context ~worktree_mode ~last_known_worktree =
  match
    ensure_result ~home ~job ~branch ~repo ~context ~worktree_mode
      ~last_known_worktree
  with
  | Ok value -> value
  | Error msg -> failwith msg
