let required_branch record =
  match record.Job_store.job.Job.branch with
  | Some branch when String.trim branch <> "" -> Ok branch
  | _ -> Error (Printf.sprintf "Monty job %s has no branch" record.Job_store.id)

let env_worker_dir () =
  match Sys.getenv_opt "MONTY_WORKER_DIR" with
  | Some dir when String.trim dir <> "" -> Some (Shell.normalize dir)
  | _ -> None

let env_job_id () =
  match Sys.getenv_opt "MONTY_JOB_ID" with
  | Some id when String.trim id <> "" -> Some id
  | _ -> None

let resolve_current ~home =
  match env_worker_dir () with
  | Some worker_dir ->
      let path = Filename.concat worker_dir "job.json" in
      if Sys.file_exists path then Job_store.parse_job_file path
      else Error (Printf.sprintf "MONTY_WORKER_DIR has no job.json: %s" worker_dir)
  | None -> (
      match env_job_id () with
      | Some id -> Job_store.find ~home ~scope:Job_store.Active id
      | None ->
          Error
            "monty done needs a worker argument or MONTY_WORKER_DIR in the current session")

let resolve ~home = function
  | Some worker -> Job_store.find ~home ~scope:Job_store.Active worker
  | None -> resolve_current ~home

let ensure_active record =
  if Job_store.is_archived record then
    Error (Printf.sprintf "Monty job is already archived: %s" record.Job_store.id)
  else Ok ()

let existing_dir = function
  | Some path when Sys.file_exists path && Sys.is_directory path -> Some path
  | _ -> None

let locate_worktree ~wt_command ~repo ~branch record =
  match String.lowercase_ascii record.Job_store.worktree_mode with
  | "never" -> Ok None
  | _ -> (
      match existing_dir record.Job_store.last_known_worktree with
      | Some path -> (
          match Wt.validate_worktree ~repo path with
          | Ok path -> Ok (Some (Shell.normalize path))
          | Error _ -> Wt.create_or_reuse ~wt_command ~repo ~branch |> Result.map Option.some)
      | None ->
          Wt.create_or_reuse ~wt_command ~repo ~branch |> Result.map Option.some)

let ensure_archive_target record archive_dir =
  let source_dir = record.Job_store.worker_dir in
  if not (Sys.file_exists source_dir && Sys.is_directory source_dir) then
    Error (Printf.sprintf "worker directory is missing: %s" source_dir)
  else if Sys.file_exists archive_dir then
    Error (Printf.sprintf "archive directory already exists: %s" archive_dir)
  else Ok ()

let archive_record record ~archive_dir ~branch ~worktree ~deleted_branch =
  let source_dir = record.Job_store.worker_dir in
  let now = Worker_memory.now_utc () in
  let target_job = Filename.concat archive_dir "job.json" in
  let updates =
    [ Job_store.string "id" record.Job_store.id;
      Job_store.string "title" record.Job_store.job.Job.title;
      Job_store.string "repo" record.Job_store.job.Job.repo;
      Job_store.string "branch" branch;
      Job_store.string "context" record.Job_store.job.Job.context;
      Job_store.string "worker_dir" archive_dir;
      Job_store.string "run_dir" record.Job_store.run_dir;
      Job_store.string "status" "done";
      Job_store.string "worktree_mode" record.Job_store.worktree_mode;
      Job_store.string "completed_at" now;
      Job_store.string "archived_at" now;
      Job_store.string "updated_at" now ]
    @ Job_store.maybe_string "deleted_branch" (if deleted_branch then Some branch else None)
    @ Job_store.maybe_string "deleted_worktree" worktree
    @ Job_store.maybe_string "last_known_worktree" worktree
  in
  try
    Shell.ensure_dir (Filename.dirname archive_dir);
    Unix.rename source_dir archive_dir;
    Job_store.update_file target_job updates
  with Unix.Unix_error (err, fn, arg) ->
    Error
      (Printf.sprintf "failed to archive %s via %s(%s): %s" record.Job_store.id fn
         arg (Unix.error_message err))

let complete ?worker ~home ~wt_command ~force () =
  let home = Shell.normalize (Shell.abs_path home) in
  let ( let* ) = Result.bind in
  let* record = resolve ~home worker in
  let* () = ensure_active record in
  let* branch = required_branch record in
  let repo = Shell.normalize (Shell.abs_path record.Job_store.job.Job.repo) in
  let archive_dir = Job_store.archive_dir record in
  let* () = ensure_archive_target record archive_dir in
  let* worktree = locate_worktree ~wt_command ~repo ~branch record in
  let* () =
    match worktree with
    | None -> Ok ()
    | Some path ->
        if force then Wt.force_clean ~worktree:path else Wt.ensure_clean ~worktree:path
  in
  let deletes_worktree_and_branch =
    match String.lowercase_ascii record.Job_store.worktree_mode with
    | "never" -> false
    | _ -> true
  in
  let* () =
    if deletes_worktree_and_branch then
      Wt.delete_worktree_and_branch ?worktree ~wt_command ~repo ~branch ~force ()
    else Ok ()
  in
  let* () =
    archive_record record ~archive_dir ~branch ~worktree
      ~deleted_branch:deletes_worktree_and_branch
  in
  Fmt.pr "Archived %S\n" record.Job_store.job.Job.title;
  Fmt.pr "Worker memory: %s\n" archive_dir;
  (match worktree with
  | Some path -> Fmt.pr "Deleted worktree: %s\n" path
  | None -> Fmt.pr "Deleted worktree: <none, worktree mode never>\n");
  if deletes_worktree_and_branch then Fmt.pr "Deleted branch: %s\n" branch
  else Fmt.pr "Deleted branch: <skipped, worktree mode never>\n";
  Ok ()
