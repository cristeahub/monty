open Overview_types

let project_memory_file = Project_storage.project_memory_file

let load_projects = Project_storage.load_projects
let resolve_project = Project_storage.resolve_project
let add_project = Project_storage.add_project
let list_projects = Project_storage.list_projects
let list_groups = Project_storage.list_groups
let add_group = Project_storage.add_group
let delete_group = Project_storage.delete_group
let set_project_group = Project_storage.set_project_group

let load_local_tasks = Task_storage.load_local_tasks
let add_local_task = Task_storage.add_local_task
let done_local_task = Task_storage.done_local_task
let add_task_workspace = Task_storage.add_task_workspace
let merge_local_tasks = Task_storage.merge_local_tasks

let open_task_worker ~home task_id =
  let ( let* ) = Result.bind in
  let task_key = "local:" ^ task_id in
  let* tasks = load_local_tasks ~home in
  let* task =
    match List.find_opt (fun (task : local_task) -> String.equal task.id task_id) tasks with
    | None -> Error (Printf.sprintf "no local Monty task matching %S" task_id)
    | Some task when not (String.equal (String.lowercase_ascii task.status) "open") ->
        Error
          (Printf.sprintf "local Monty task %s is %s, not open" task.id
             task.status)
    | Some task -> Ok task
  in
  let* records = Job_store.load_all ~home in
  let* record =
    match
      records
      |> List.filter (fun record ->
             not (Job_store.is_archived record)
             && record.Job_store.job.Job.task_key = Some task_key)
    with
    | [ record ] -> Ok record
    | [] ->
        Error
          (Printf.sprintf
             "local Monty task %s has no linked active worker; launch or headless preparation materializes planned workspaces"
             task.id)
    | _ ->
        Error
          (Printf.sprintf "multiple active workers link local Monty task %s"
             task.id)
  in
  Ok (task, record)

let require_open_worker_task ~home record =
  let ( let* ) = Result.bind in
  let* linked = Reconciliation.validate_worker_task_link ~home record in
  match linked with
  | None -> Ok ()
  | Some task_id ->
      let* tasks = load_local_tasks ~home in
      (match List.find_opt (fun (task : local_task) -> task.id = task_id) tasks with
      | Some task when String.lowercase_ascii task.status = "open" -> Ok ()
      | Some task -> Error (Printf.sprintf "local Monty task %s is %s, not open" task.id task.status)
      | None -> Error (Printf.sprintf "local Monty task %s disappeared" task_id))

let ensure_worker_workspaces_unlocked ~home ~(record : Job_store.record)
    ?repo ~wt_command () =
  let ( let* ) = Result.bind in
  let* () =
    match record.transition with
    | None -> Ok ()
    | Some transition ->
        Error
          (Printf.sprintf "worker %s is in a %s transition"
             record.id (Job_store.operation_name transition.operation))
  in
  let* selected_repo =
    match repo with
    | None -> Ok None
    | Some repo when Filename.is_relative repo ->
        Error (Printf.sprintf "workspace repo must be an absolute path: %s" repo)
    | Some repo -> Ok (Some (Shell.normalize repo))
  in
  let selected (workspace : Job_store.workspace_state) =
    match selected_repo with
    | None -> true
    | Some repo -> String.equal workspace.repo repo
  in
  let targets = List.filter selected record.workspaces in
  let* () =
    match (selected_repo, targets) with
    | Some repo, [] ->
        Error
          (Printf.sprintf "worker %s has no workspace for repo %s" record.id repo)
    | _ -> Ok ()
  in
  let workspace_identity workspaces =
    workspaces
    |> List.map (fun (workspace : Job_store.workspace_state) ->
           (Shell.normalize (Shell.abs_path workspace.repo), workspace.branch))
  in
  let persist workspaces =
    State_store.with_lock ~home (fun () ->
        let* current = Job_store.parse_job_file ~home record.path in
        let* () = require_open_worker_task ~home current in
        let* () =
          if current.job.Job.task_key = record.job.Job.task_key then Ok ()
          else Error (Printf.sprintf "worker %s task link changed during workspace materialization" current.id)
        in
        let* () =
          match current.transition with
          | None -> Ok ()
          | Some transition ->
              Error
                (Printf.sprintf "worker %s entered a %s transition"
                   current.id (Job_store.operation_name transition.operation))
        in
        let* () =
          if workspace_identity current.workspaces = workspace_identity record.workspaces
          then Ok ()
          else Error (Printf.sprintf "worker %s workspace identity changed" current.id)
        in
        Job_store.update_file_unlocked record.path
          (Job_store.materialized_workspace_updates workspaces))
  in
  let replace_workspace states replacement =
    states
    |> List.map (fun (workspace : Job_store.workspace_state) ->
           if
             String.equal workspace.repo replacement.Job_store.repo
             && workspace.branch = replacement.branch
           then replacement
           else workspace)
  in
  let rec ensure states = function
    | [] -> Ok states
    | (workspace : Job_store.workspace_state) :: rest ->
        let* branch =
          match workspace.branch with
          | Some branch when String.trim branch <> "" -> Ok branch
          | _ -> Error (Printf.sprintf "workspace in repo %s has no branch" workspace.repo)
        in
        let* worktree =
          match String.lowercase_ascii record.worktree_mode with
          | "never" ->
              if Sys.file_exists workspace.repo && Sys.is_directory workspace.repo then
                Ok workspace.repo
              else Error (Printf.sprintf "workspace repository is unavailable: %s" workspace.repo)
          | "always" -> Wt.create_or_reuse ~wt_command ~repo:workspace.repo ~branch
          | mode -> Error (Printf.sprintf "unsupported persisted worktree mode %S" mode)
        in
        let replacement = { workspace with worktree = Some worktree } in
        let states = replace_workspace states replacement in
        let* () = persist states in
        ensure states rest
  in
  let* workspaces = ensure record.workspaces targets in
  Ok (List.filter selected workspaces)

(* While a worker's live service terminal holds its lock, a client reattaching to that
   runtime reads the worktrees it already runs in; anything still to materialize waits
   for the lock. [live] proves the runtime from the service. *)
let live_workspaces ~live ~(record : Job_store.record) select =
  let selected = List.filter select record.workspaces in
  let materialized (workspace : Job_store.workspace_state) =
    match workspace.worktree with
    | Some path -> Sys.file_exists path && Sys.is_directory path
    | None -> false
  in
  if record.transition = None && selected <> [] && List.for_all materialized selected && live record
  then Some selected
  else None

let ensure_task_workspaces ~live ~home ~id ?repo ~wt_command () =
  let ( let* ) = Result.bind in
  let task_id = Task_storage.normalize_local_id id in
  let* _, initial_record = open_task_worker ~home task_id in
  let reattach () =
    live_workspaces ~live ~record:initial_record (fun workspace ->
        match repo with
        | None -> true
        | Some repo -> String.equal workspace.Job_store.repo (Shell.normalize repo))
    |> Option.map Result.ok
  in
  Worker_lock.with_lock ~runtime:reattach ~home ~record:initial_record (fun () ->
      let* record =
        State_store.with_lock ~home (fun () ->
            let* _, record = open_task_worker ~home task_id in
            if not (String.equal record.Job_store.path initial_record.Job_store.path) then
              Error
                (Printf.sprintf
                   "local Monty task %s worker identity changed before workspace materialization"
                   task_id)
            else
              let* () = require_open_worker_task ~home record in
              Ok record)
      in
      ensure_worker_workspaces_unlocked ~home ~record ?repo ~wt_command ())

let ensure_worker_workspace ~live ~(state : State_path.t) ~repo ~branch ~wt_command () =
  let ( let* ) = Result.bind in
  let home = state.home in
  let* initial = Job_store.parse_job_file ~home state.job_file in
  let reattach () =
    match
      live_workspaces ~live ~record:initial (fun workspace ->
          workspace.Job_store.repo = repo && workspace.branch = Some branch)
    with
    | Some [ { worktree = Some path; _ } ] -> Some (Ok path)
    | _ -> None
  in
  Worker_lock.with_lock ~runtime:reattach ~home ~record:initial (fun () ->
      let* record =
        State_store.with_lock ~home (fun () ->
            let* current = Job_store.parse_job_file ~home state.job_file in
            let* () =
              if Job_store.is_archived current || current.transition <> None
                 || not (List.mem current.status
                           [ "active"; "prepared"; "launch-failed"; "launch-requested" ])
              then Error (Printf.sprintf "worker %s is not open for workspace materialization" current.id)
              else require_open_worker_task ~home current
            in
            if List.exists
                 (fun (workspace : Job_store.workspace_state) ->
                   workspace.repo = repo && workspace.branch = Some branch)
                 current.workspaces
            then Ok current
            else Error (Printf.sprintf "requested repo and branch do not belong to worker %s" current.id))
      in
      let* workspaces = ensure_worker_workspaces_unlocked ~home ~record ~repo ~wt_command () in
      match workspaces with
      | [ { worktree = Some path; _ } ] -> Ok path
      | _ -> Error (Printf.sprintf "worker %s did not materialize its requested workspace" record.id))

let show_task ~home id =
  let ( let* ) = Result.bind in
  let id =
    if String.starts_with ~prefix:"local:" id then
      String.sub id 6 (String.length id - 6)
    else id
  in
  let* projects = load_projects ~home in
  let* tasks = load_local_tasks ~home in
  let* task =
    match List.find_opt (fun (task : local_task) -> String.equal task.id id) tasks with
    | Some task -> Ok task
    | None -> Error (Printf.sprintf "no local Monty task matching %S" id)
  in
  let* scan = Job_store.scan ~home in
  let task_key = "local:" ^ task.id in
  let record =
    scan.records
    |> List.find_opt (fun record -> record.Job_store.job.Job.task_key = Some task_key)
  in
  let workspaces =
    match (task.workspaces, record) with
    | _ :: _ as workspaces, _ -> workspaces
    | [], Some record -> Reconciliation.task_workspaces_of_job record.job
    | [], None -> []
  in
  let state_for (workspace : task_workspace) =
    match record with
    | None -> (None, "not-materialized")
    | Some record -> (
        match
          List.find_opt
            (fun (candidate : Job_store.workspace_state) ->
              String.equal candidate.repo workspace.repo
              && candidate.branch = Some workspace.branch)
            record.workspaces
        with
        | None -> (None, "not-materialized")
        | Some { worktree = None; _ } -> (None, "not-materialized")
        | Some { worktree = Some path; _ }
          when not (Sys.file_exists path && Sys.is_directory path) ->
            (Some path, "missing")
        | Some { worktree = Some path; _ } -> (
            let branch =
              if record.worktree_mode = "never" then None else Some workspace.branch
            in
            match Wt.validate_worktree ~repo:workspace.repo ?branch path with
            | Ok path -> (Some path, "present")
            | Error _ -> (Some path, "invalid-workspace")))
  in
  let project_label repo =
    projects
    |> List.find_opt (fun (project : project) -> String.equal project.repo repo)
    |> Option.map (fun (project : project) -> project.id)
    |> Option.value ~default:"<unknown>"
  in
  let rows =
    workspaces
    |> List.map (fun workspace ->
           let worktree, state = state_for workspace in
           ( project_label workspace.repo,
             workspace.repo,
             workspace.branch,
             Option.value ~default:"<not materialized>" worktree,
             state ))
  in
  let row (project, repo, branch, worktree, state) =
    String.concat "\t" [ project; repo; branch; worktree; state ]
  in
  let launch_cwd =
    match rows with
    | (_, repo, _, "<not materialized>", _) :: _ -> repo
    | (_, _, _, worktree, _) :: _ -> worktree
    | [] -> "<none>"
  in
  Ok
    (String.concat "\n"
       ([ "Task: local:" ^ task.id ^ " — " ^ task.title;
          "Status: " ^ task.status;
          "Worker: "
          ^ Option.value ~default:"<not launched>" task.worker_id;
          "Launch cwd: " ^ launch_cwd;
          "";
          "PROJECT\tREPO\tBRANCH\tWORKTREE\tSTATE" ]
       @ List.map row rows)
    ^ "\n")

let sync_jobs_to_local_tasks = Reconciliation.sync_jobs_to_local_tasks
let diagnostic_task_key = Reconciliation.diagnostic_task_key
let load_tasks_with_warnings = Reconciliation.load_tasks_with_warnings
let load_tasks = Reconciliation.load_tasks
let validate_job_project = Reconciliation.validate_job_project
let validate_worker_task_link = Reconciliation.validate_worker_task_link
let set_worker_task_status = Reconciliation.set_worker_task_status
let preflight_launch_task_links = Reconciliation.preflight_launch_task_links
let reserve_launch_task_links_unlocked = Reconciliation.reserve_launch_task_links_unlocked
let repair_legacy_task_link = Reconciliation.repair_legacy_task_link

let render_projects = Overview_render.render_projects
let show_project = Overview_render.show_project
let render_tasks = Overview_render.render_tasks
let overview = Overview_render.overview
