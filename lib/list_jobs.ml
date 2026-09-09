let run_matches run record =
  match run with
  | None -> true
  | Some value ->
      let value = String.trim value in
      String.equal value ""
      || String.equal value record.Job_store.run_dir
      || String.equal value (Filename.basename record.Job_store.run_dir)

let task_done task = String.equal (String.lowercase_ascii task.Overview_types.status) "done"

let task_in_scope scope task =
  match scope with
  | Job_store.Active -> not (task_done task)
  | Job_store.Archived -> task_done task
  | Job_store.All -> true

let task_keys_for_run records run =
  match run with
  | None -> None
  | Some _ ->
      records
      |> List.filter (run_matches run)
      |> List.concat_map (fun record ->
             Project_overview.diagnostic_task_key record
             :: Option.to_list record.Job_store.job.Job.task_key)
      |> Option.some

let task_matches_run task_keys task =
  match task_keys with
  | None -> true
  | Some keys -> List.exists (String.equal task.Overview_types.key) keys

let print_warnings warnings =
  List.iter (fun warning -> Fmt.epr "monty: warning: %s\n" warning) warnings

let run ~home ~scope ?run ?project ?group ?(banner = "") ?(sync = true) () =
  let ( let* ) = Result.bind in
  let* () =
    match group with
    | None -> Ok ()
    | Some _ -> Project_storage.list_projects ~home ?group () |> Result.map ignore
  in
  let* sync_warnings =
    if sync then
      Project_overview.sync_jobs_to_local_tasks ~home
      |> Result.map (fun result -> result.Overview_types.warnings)
    else Ok []
  in
  let* scan = Job_store.scan ~home in
  let task_keys = task_keys_for_run scan.records run in
  let* tasks, inventory_warnings =
    Project_overview.load_tasks_with_warnings ~home ?project ?group ~all:true ()
  in
  print_warnings
    (List.sort_uniq String.compare (sync_warnings @ scan.warnings @ inventory_warnings));
  let tasks =
    tasks |> List.filter (task_in_scope scope)
    |> List.filter (task_matches_run task_keys)
  in
  Fmt.pr "%s%s" banner (Project_overview.render_tasks tasks);
  Ok ()
