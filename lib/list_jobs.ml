let mantle_profile = Mantle.profile

let selection ~getenv ?project group all_groups =
  if all_groups && group <> None then Error "--group and --all-groups cannot be combined"
  else
    let profile = mantle_profile getenv in
    let selected =
      if group <> None then group
      else if all_groups || project <> None then None else profile
    in
    let scope =
      if all_groups then "Groups: all (--all-groups)"
      else match group, project, profile with
      | Some group, _, _ -> "Group: " ^ group ^ " (--group)"
      | None, Some project, Some _ -> "Project: " ^ project ^ " (--project)"
      | None, None, Some profile -> "Group: " ^ profile
      | _ -> ""
    in
    let profile = Option.fold ~none:"" ~some:(fun name -> "Profile: " ^ name ^ " (Mantle)") profile in
    let banner = [ profile; scope ] |> List.filter (fun text -> text <> "") |> String.concat " | " in
    Ok (selected, if banner = "" then "" else banner ^ "\n")

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

(* Swarm children are not tasks. With --swarm they are shown indented under
   their driver's row, keyed after it so sorting keeps them together. *)
let swarm_child_rows (scan : Job_store.scan_result) tasks =
  let ( let* ) = Option.bind in
  scan.records
  |> List.filter_map (fun (record : Job_store.record) ->
         match Job.swarm_child record.job with
         | Some (parent, name) when not (Job_store.is_archived record) ->
             let* driver =
               List.find_opt
                 (fun (candidate : Job_store.record) -> String.equal candidate.id parent)
                 scan.records
             in
             let key =
               match driver.job.Job.task_key with
               | Some key -> key
               | None -> Project_overview.diagnostic_task_key driver
             in
             let* task =
               List.find_opt (fun (task : Overview_types.task) -> String.equal task.key key) tasks
             in
             let status =
               match Swarm_ledger.load_opt ~worker_dir:driver.worker_dir with
               | Ok (Some ledger) -> (
                   match Swarm_ledger.find_child ledger name with
                   | Some child -> Swarm_ledger.state_to_string child.state
                   | None -> record.status)
               | Ok None | Error _ -> record.status
             in
             Some
               { Overview_types.key = task.key ^ "/" ^ name;
                 display_id = "  " ^ record.id;
                 project = task.project;
                 origin = Overview_render.swarm_child_origin;
                 title = name;
                 status;
                 branch = record.job.Job.branch;
                 workspaces = [];
                 url = None }
         | _ -> None)

let inventory ~home ~scope ?run ?project ?group ?(sync = false) ?(swarm = false) () =
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
    Project_overview.load_tasks_with_warnings ~home ?project ?group ~scan ~all:true ()
  in
  let warnings = List.sort_uniq String.compare (sync_warnings @ inventory_warnings) in
  let tasks =
    tasks |> List.filter (task_in_scope scope)
    |> List.filter (task_matches_run task_keys)
  in
  let tasks = if swarm then tasks @ swarm_child_rows scan tasks else tasks in
  Ok (tasks, warnings)

let run ~home ~scope ?run ?project ?group ?(banner = "") ?(sync = true) ?swarm () =
  let ( let* ) = Result.bind in
  let* tasks, warnings = inventory ~home ~scope ?run ?project ?group ~sync ?swarm () in
  print_warnings warnings;
  Fmt.pr "%s%s" banner (Project_overview.render_tasks tasks);
  Ok ()
