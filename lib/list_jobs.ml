let run_matches run record =
  match run with
  | None -> true
  | Some value ->
      let value = String.trim value in
      String.equal value ""
      || String.equal value record.Job_store.run_dir
      || String.equal value (Filename.basename record.Job_store.run_dir)

let status_label record = if Job_store.is_archived record then "DONE" else "ACTIVE"

let line record =
  Printf.sprintf "%-16s %-7s %-32s %-24s %s" record.Job_store.id
    (status_label record)
    record.Job_store.job.Job.title
    (Option.value ~default:"<no-branch>" record.Job_store.job.Job.branch)
    record.Job_store.worker_dir

let compare_records left right =
  match String.compare left.Job_store.run_dir right.Job_store.run_dir with
  | 0 -> String.compare left.Job_store.id right.Job_store.id
  | value -> value

let render records =
  let records = List.sort compare_records records in
  let header = Printf.sprintf "%-16s %-7s %-32s %-24s %s" "ID" "STATUS" "TITLE" "BRANCH" "DIR" in
  String.concat "\n" (header :: List.map line records) ^ "\n"

let task_done task = String.equal (String.lowercase_ascii task.Project_overview.status) "done"

let task_in_scope scope task =
  match scope with
  | Job_store.Active -> not (task_done task)
  | Job_store.Archived -> task_done task
  | Job_store.All -> true

let task_keys_for_run ~home run =
  match run with
  | None -> Ok None
  | Some _ -> (
      match Job_store.load ~home ~scope:Job_store.All with
      | Error msg -> Error msg
      | Ok records ->
          let task_keys =
            records
            |> List.filter (run_matches run)
            |> List.filter_map (fun record -> record.Job_store.job.Job.task_key)
          in
          Ok (Some task_keys))

let task_matches_run task_keys task =
  match task_keys with
  | None -> true
  | Some keys -> List.exists (String.equal task.Project_overview.key) keys

let run ~home ~scope ?run () =
  let ( let* ) = Result.bind in
  let* _sync_result = Project_overview.sync_jobs_to_local_tasks ~home in
  let* task_keys = task_keys_for_run ~home run in
  let* tasks = Project_overview.load_tasks ~home ~all:true () in
  let tasks = tasks |> List.filter (task_in_scope scope) |> List.filter (task_matches_run task_keys) in
  Fmt.pr "%s" (Project_overview.render_tasks tasks);
  Ok ()
