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

let prefix text value =
  let text_len = String.length text in
  let value_len = String.length value in
  value_len >= text_len && String.sub value 0 text_len = text

let is_digit = function '0' .. '9' -> true | _ -> false

let legacy_local_task_id_from_worker_id value =
  let length = String.length value in
  if
    length >= 9
    && prefix "local-" value
    && is_digit value.[6] && is_digit value.[7] && is_digit value.[8]
    && (length = 9 || value.[9] = '-' || value.[9] = '_' || value.[9] = '/')
  then Some (String.sub value 0 9)
  else None

let task_key_for_archive record linked_local_task_id =
  match record.Job_store.job.Job.task_key with
  | Some _ as task_key -> task_key
  | None -> Option.map (fun id -> "local:" ^ id) linked_local_task_id

let resolve_linked_local_task_id ~home ~repo:_ record =
  Project_overview.validate_worker_task_link ~home record

let close_linked_local_task ~home record =
  Project_overview.set_worker_task_status ~home record "done"

let resolve_current ~home =
  match env_worker_dir () with
  | Some worker_dir ->
      let path = Filename.concat worker_dir "job.json" in
      if Sys.file_exists path then Job_store.parse_job_file ~home path
      else (
        match env_job_id () with
        | Some id -> Job_store.find ~home ~scope:Job_store.All id
        | None -> Error (Printf.sprintf "MONTY_WORKER_DIR has no job.json: %s" worker_dir))
  | None -> (
      match env_job_id () with
      | Some id -> Job_store.find ~home ~scope:Job_store.All id
      | None ->
          Error
            "monty done needs a worker argument or MONTY_WORKER_DIR in the current session")

let resolve ~home = function
  | Some worker -> Job_store.find ~home ~scope:Job_store.All worker
  | None -> resolve_current ~home

let ensure_completable record =
  match record.Job_store.transition with
  | Some transition when transition.operation = Job_store.Complete -> Ok ()
  | Some transition ->
      Error
        (Printf.sprintf "Monty job %s is already in a %s transition"
           record.Job_store.id (Job_store.operation_name transition.operation))
  | None when Job_store.is_archived record ->
      Error (Printf.sprintf "Monty job is already archived: %s" record.Job_store.id)
  | None
    when List.mem record.Job_store.status
           [ "active"; "prepared"; "launch-failed"; "launch-requested" ] ->
      Ok ()
  | None ->
      Error
        (Printf.sprintf "Monty job %s has status %S and cannot be completed"
           record.Job_store.id record.Job_store.status)

let existing_dir = function
  | Some path when Sys.file_exists path && Sys.is_directory path -> Some path
  | _ -> None

let locate_worktree ~wt_command ~repo ~branch record =
  match String.lowercase_ascii record.Job_store.worktree_mode with
  | "never" -> Ok None
  | _ -> (
      match existing_dir record.Job_store.last_known_worktree with
      | Some path -> Wt.validate_worktree ~repo path |> Result.map Option.some
      | None -> Wt.locate_existing ~wt_command ~repo ~branch)

let fault checkpoint =
  match Sys.getenv_opt "MONTY_FAULT_INJECT" with
  | Some value when String.equal value checkpoint ->
      Error (Printf.sprintf "fault injected at %s" checkpoint)
  | _ -> Ok ()

let task_id_of_key = Job_store.local_task_id_of_key

let ensure_archive_target record archive_dir =
  let source_dir = record.Job_store.worker_dir in
  if not (Sys.file_exists source_dir && Sys.is_directory source_dir) then
    Error (Printf.sprintf "worker directory is missing: %s" source_dir)
  else if Sys.file_exists archive_dir then
    Error (Printf.sprintf "archive directory already exists: %s" archive_dir)
  else Ok ()

let prepare_fresh ~home ~wt_command ~force record =
  let ( let* ) = Result.bind in
  let* branch = required_branch record in
  let repo = Shell.normalize (Shell.abs_path record.Job_store.job.Job.repo) in
  let* physical = State_path.of_job_file ~home record.Job_store.path in
  let* archive_state =
    State_path.archived ~home ~run_id:physical.State_path.run_id
      ~id:physical.State_path.id
  in
  let* linked_task_id = resolve_linked_local_task_id ~home ~repo record in
  let task_key = task_key_for_archive record linked_task_id in
  let* () = ensure_archive_target record archive_state.worker_dir in
  let* worktree = locate_worktree ~wt_command ~repo ~branch record in
  let* () =
    match worktree with
    | None -> Ok ()
    | Some _ when force -> Ok ()
    | Some path -> Wt.ensure_clean ~worktree:path
  in
  let* record = Job_store.prepare_completion record ~task_key ~force in
  Ok (record, worktree)

let complete ?worker ~home ~wt_command ~force () =
  let home = Shell.normalize (Shell.abs_path home) in
  let ( let* ) = Result.bind in
  let* initial = resolve ~home worker in
  let* () = ensure_completable initial in
  let* _linked_task_id =
    Project_overview.validate_worker_task_link ~home initial
  in
  let* record, preflight_worktree =
    match initial.Job_store.transition with
    | Some transition when transition.operation = Job_store.Complete -> Ok (initial, None)
    | Some _ -> assert false
    | None -> prepare_fresh ~home ~wt_command ~force initial
  in
  let* () = fault "complete-after-intent" in
  let* transition =
    match record.Job_store.transition with
    | Some transition when transition.operation = Job_store.Complete -> Ok transition
    | _ -> Error "completion intent was not persisted"
  in
  let* branch = required_branch record in
  let repo = Shell.normalize (Shell.abs_path record.Job_store.job.Job.repo) in
  let deletes_worktree_and_branch =
    not (String.equal (String.lowercase_ascii record.Job_store.worktree_mode) "never")
  in
  let* worktree =
    if deletes_worktree_and_branch then
      match preflight_worktree with
      | Some _ as worktree -> Ok worktree
      | None -> locate_worktree ~wt_command ~repo ~branch record
    else Ok None
  in
  let* () =
    match worktree with
    | None -> Ok ()
    | Some path ->
        if transition.force then Wt.force_clean ~worktree:path
        else Wt.ensure_clean ~worktree:path
  in
  let removal_worktree =
    match worktree with Some _ -> worktree | None -> record.Job_store.last_known_worktree
  in
  let* () =
    if deletes_worktree_and_branch then
      Wt.remove_if_present ?worktree:removal_worktree ~wt_command ~repo ~branch ()
    else Ok ()
  in
  let* () = fault "complete-after-cleanup" in
  let* record = Job_store.relocate_transition record Job_store.Complete in
  let* () = fault "complete-after-move" in
  let* record = Job_store.normalize_transition record Job_store.Complete in
  let* () = fault "complete-after-normalize" in
  let* linked_task_id = task_id_of_key transition.task_key in
  let* () = close_linked_local_task ~home record in
  let* () = fault "complete-after-task" in
  let* () = fault "complete-before-finalize" in
  let* record = Job_store.finalize_transition record Job_store.Complete in
  Fmt.pr "Archived %S\n" record.Job_store.job.Job.title;
  Fmt.pr "Worker memory: %s\n" transition.target;
  (match linked_task_id with
  | Some id -> Fmt.pr "Closed local task: local:%s\n" id
  | None -> ());
  (match worktree with
  | Some path -> Fmt.pr "Deleted worktree: %s\n" path
  | None when deletes_worktree_and_branch ->
      Fmt.pr "Deleted worktree: <already absent>\n"
  | None -> Fmt.pr "Deleted worktree: <none, worktree mode never>\n");
  if deletes_worktree_and_branch then Fmt.pr "Deleted branch: %s\n" branch
  else Fmt.pr "Deleted branch: <skipped, worktree mode never>\n";
  Ok ()
