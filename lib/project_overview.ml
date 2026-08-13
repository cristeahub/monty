type github_source = Overview_types.github_source = {
  repo : string;
  query : string option;
}

type source = Overview_types.source = Github_issues of github_source

type raw_project = Overview_types.raw_project = {
  persisted_id : string option;
  repo : string;
  sources : source list;
}

type project = Overview_types.project = {
  id : string;
  repo : string;
  sources : source list;
}

type task_workspace = Overview_types.task_workspace = {
  repo : string;
  branch : string;
}

type local_task = Overview_types.local_task = {
  id : string;
  project : string;
  title : string;
  status : string;
  branch : string option;
  workspaces : task_workspace list;
  notes : string option;
  worker_id : string option;
  worker_key : string option;
  external_key : string option;
  external_url : string option;
  external_source : string option;
  created_at : string option;
  updated_at : string option;
}

type task = Overview_types.task = {
  key : string;
  display_id : string;
  project : string;
  origin : string;
  title : string;
  status : string;
  branch : string option;
  workspaces : task_workspace list;
  url : string option;
}

type sync_result = Overview_types.sync_result = {
  created : int;
  updated : int;
  linked_jobs : int;
  warnings : string list;
}

let monty_dir = Project_storage.monty_dir
let projects_file = Project_storage.projects_file
let projects_dir = Project_storage.projects_dir
let local_tasks_file = Task_storage.local_tasks_file
let project_memory_file = Project_storage.project_memory_file

let load_projects = Project_storage.load_projects
let resolve_project = Project_storage.resolve_project
let add_project = Project_storage.add_project

let load_local_tasks = Task_storage.load_local_tasks
let save_local_tasks = Task_storage.save_local_tasks
let save_local_tasks_unlocked = Task_storage.save_local_tasks_unlocked
let add_local_task = Task_storage.add_local_task
let set_local_task_status = Task_storage.set_local_task_status
let done_local_task = Task_storage.done_local_task
let reopen_local_task = Task_storage.reopen_local_task
let add_task_workspace = Task_storage.add_task_workspace
let merge_local_tasks = Task_storage.merge_local_tasks

let ensure_task_workspaces ~home ~id ?repo ~wt_command () =
  let ( let* ) = Result.bind in
  let task_id =
    if String.starts_with ~prefix:"local:" id then
      String.sub id 6 (String.length id - 6)
    else id
  in
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
          (Printf.sprintf "local Monty task %s has no workspace for repo %s"
             task.id repo)
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
        let* current_tasks = load_local_tasks ~home in
        let* () =
          match
            List.find_opt
              (fun (candidate : local_task) -> String.equal candidate.id task.id)
              current_tasks
          with
          | Some candidate
            when String.equal (String.lowercase_ascii candidate.status) "open" ->
              Ok ()
          | Some candidate ->
              Error
                (Printf.sprintf "local Monty task %s changed status to %s"
                   candidate.id candidate.status)
          | None -> Error (Printf.sprintf "local Monty task %s disappeared" task.id)
        in
        let* () =
          if current.job.Job.task_key = Some task_key then Ok ()
          else Error (Printf.sprintf "worker %s no longer links %s" current.id task_key)
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
        let first_worktree =
          match workspaces with
          | first :: _ -> first.Job_store.worktree
          | [] -> None
        in
        let updates =
          [ Job_store.workspaces_update workspaces;
            Job_store.string "updated_at" (Worker_memory.now_utc ()) ]
          @
          match first_worktree with
          | None -> []
          | Some path -> [ Job_store.string "last_known_worktree" path ]
        in
        Job_store.update_file_unlocked record.path updates)
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
          Wt.create_or_reuse ~wt_command ~repo:workspace.repo ~branch
        in
        let replacement = { workspace with worktree = Some worktree } in
        let states = replace_workspace states replacement in
        let* () = persist states in
        ensure states rest
  in
  let* workspaces = ensure record.workspaces targets in
  Ok (List.filter selected workspaces)

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
            match Wt.validate_worktree ~repo:workspace.repo path with
            | Ok path -> (Some path, "present")
            | Error _ -> (Some path, "invalid-repo")))
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
let ensure_worker_task_link = Reconciliation.ensure_worker_task_link
let repair_legacy_task_link = Reconciliation.repair_legacy_task_link

let render_projects = Overview_render.render_projects
let show_project = Overview_render.show_project
let render_tasks = Overview_render.render_tasks
let render_active_jobs = Overview_render.render_active_jobs
let overview = Overview_render.overview
