open Yojson.Safe
open Overview_types

let now_utc = Worker_memory.now_utc
let load_projects = Project_storage.load_projects
let resolve_project = Project_storage.resolve_project
let load_local_tasks = Task_storage.load_local_tasks
let save_local_tasks_unlocked = Task_storage.save_local_tasks_unlocked
let next_local_id = Task_storage.next_local_id
let task_of_local = Task_storage.task_of_local

let fold_results values ~init ~f =
  List.fold_left
    (fun acc value ->
      match acc with Error _ as err -> err | Ok acc -> f acc value)
    (Ok init) values

let local_task_key id = "local:" ^ id

let project_for_repo projects repo =
  let repo = Shell.normalize (Shell.abs_path repo) in
  match List.find_opt (fun (project : project) -> String.equal project.repo repo) projects with
  | Some project -> Ok project
  | None -> Error (Printf.sprintf "no Monty project for worker repo: %s" repo)

let find_local_task_by_id tasks id =
  List.find_opt (fun (task : local_task) -> String.equal task.id id) tasks

let replace_local_task tasks updated =
  tasks
  |> List.map (fun (task : local_task) -> if String.equal task.id updated.id then updated else task)

let status_of_job record = if Job_store.is_archived record then "done" else "open"

let preferred_job_record (left : Job_store.record) (right : Job_store.record) =
  match (Job_store.is_archived left, Job_store.is_archived right) with
  | false, true -> left
  | true, false -> right
  | _ -> (
      match String.compare left.Job_store.id right.Job_store.id with
      | value when value < 0 -> left
      | value when value > 0 -> right
      | _ -> if String.compare left.path right.path <= 0 then left else right)

let linked_job_display_ids records =
  records
  |> List.fold_left
       (fun acc (record : Job_store.record) ->
         match record.Job_store.job.Job.task_key with
         | None -> acc
         | Some task_key ->
             let preferred =
               match List.assoc_opt task_key acc with
               | None -> record
               | Some current -> preferred_job_record current record
             in
             (task_key, preferred) :: List.remove_assoc task_key acc)
       []
  |> List.map (fun (task_key, record) -> (task_key, record.Job_store.id))

let parse_github_issue ~(project : project) ~repo json =
  match (Util.member "number" json, Util.member "title" json) with
  | `Int number, `String title ->
      let key = Printf.sprintf "github:%s#%d" repo number in
      let status =
        match Util.member "state" json with `String value -> value | _ -> "open"
      in
      let url = match Util.member "url" json with `String value -> Some value | _ -> None in
      Ok
        {
          key;
          display_id = key;
          project = project.id;
          origin = "github";
          title;
          status;
          branch = None;
          url;
        }
  | _ -> Error "GitHub issue JSON missing number or title"

let fetch_github_tasks ~project { repo; query } =
  Result.bind (External_metadata.fetch_github_issues ~repo ~query) (fun issues ->
      fold_results issues ~init:[] ~f:(fun acc json ->
          parse_github_issue ~project ~repo json
          |> Result.map (fun task -> task :: acc))
      |> Result.map List.rev)

let fetch_project_tasks (project : project) =
  let fetch_source = function
    | Github_issues source -> fetch_github_tasks ~project source
  in
  fold_results project.sources ~init:[] ~f:(fun acc source ->
      fetch_source source |> Result.map (fun tasks -> acc @ tasks))

type job_patch = Job_store.record * string

let compare_job_records (left : Job_store.record) (right : Job_store.record) =
  match String.compare left.id right.id with
  | 0 -> String.compare left.path right.path
  | value -> value

let sort_uniq_strings values = List.sort_uniq String.compare values

let registration_warning ~home repo =
  Printf.sprintf
    "worker repo is not registered: %s; run exactly: monty projects add --repo %s --home %s"
    repo (Shell.quote repo) (Shell.quote home)

let project_for_repo_opt projects repo =
  let repo = Shell.normalize (Shell.abs_path repo) in
  List.find_opt (fun (project : project) -> String.equal project.repo repo) projects

let worker_key ~repo ~branch ~id =
  Yojson.Safe.to_string
    (`List
      [ `String (Shell.normalize (Shell.abs_path repo));
        `String (Option.value ~default:"" branch);
        `String id ])

let find_tasks_by_worker tasks key =
  List.filter
    (fun (task : local_task) -> task.worker_key = Some key)
    tasks

let validate_worker_task_link_in projects tasks (record : Job_store.record) =
  let ( let* ) = Result.bind in
  match record.job.Job.task_key with
  | None -> Ok None
  | Some key ->
      let* task_id = Job_store.local_task_id_of_key (Some key) in
      let* task_id =
        match task_id with
        | Some id -> Ok id
        | None ->
            Error
              (Printf.sprintf "worker %s has unsupported non-local task key %S"
                 record.id key)
      in
      let* project =
        match project_for_repo_opt projects record.job.Job.repo with
        | Some project -> Ok project
        | None ->
            Error
              (Printf.sprintf "worker repo is not registered: %s"
                 record.job.Job.repo)
      in
      let* task =
        match find_local_task_by_id tasks task_id with
        | Some task -> Ok task
        | None ->
            Error (Printf.sprintf "linked local Monty task is missing: %s" task_id)
      in
      if not (String.equal task.project project.id) then
        Error
          (Printf.sprintf
             "worker %s in project %s links task %s from project %s"
             record.id project.id key task.project)
      else
        let stable_key =
          worker_key ~repo:record.job.Job.repo ~branch:record.job.Job.branch
            ~id:record.id
        in
        match (task.worker_key, task.worker_id) with
        | Some owner, _ when not (String.equal owner stable_key) ->
            Error
              (Printf.sprintf
                 "linked local Monty task %s is owned by another repo+branch+worker identity"
                 task.id)
        | _, Some owner when not (String.equal owner record.id) ->
            Error
              (Printf.sprintf "linked local Monty task %s is owned by worker %s"
                 task.id owner)
        | _ -> Ok (Some task.id)

let validate_worker_task_link ~home record =
  let ( let* ) = Result.bind in
  let* projects = load_projects ~home in
  let* tasks = load_local_tasks ~home in
  validate_worker_task_link_in projects tasks record

let set_worker_task_status ~home record status =
  State_store.with_lock ~home (fun () ->
      let ( let* ) = Result.bind in
      let* projects = load_projects ~home in
      let* tasks = load_local_tasks ~home in
      let* task_id = validate_worker_task_link_in projects tasks record in
      match task_id with
      | None -> Ok ()
      | Some id -> (
          match find_local_task_by_id tasks id with
          | None -> Error (Printf.sprintf "linked local Monty task is missing: %s" id)
          | Some task when String.equal task.status status -> Ok ()
          | Some task ->
              let updated =
                { task with status; updated_at = Some (now_utc ()) }
              in
              Task_storage.save_local_tasks_unlocked ~home
                (replace_local_task tasks updated)))

let valid_local_task_link projects tasks (record : Job_store.record) =
  match
    ( project_for_repo_opt projects record.job.Job.repo,
      record.job.Job.task_key )
  with
  | Some project, Some key -> (
      match Job_store.local_task_id_of_key (Some key) with
      | Ok (Some id) -> (
          match find_local_task_by_id tasks id with
          | Some task -> String.equal task.project project.id
          | None -> false)
      | _ -> false)
  | _ -> false

let update_external_tasks tasks imports =
  let imports =
    List.sort
      (fun left right ->
        match String.compare left.project right.project with
        | 0 -> String.compare left.key right.key
        | value -> value)
      imports
  in
  List.fold_left
    (fun (tasks, created, updated) import ->
      let external_key = import.key in
      match
        List.find_opt
          (fun (task : local_task) -> task.external_key = Some external_key)
          tasks
      with
      | None ->
          let now = now_utc () in
          let task =
            {
              id = next_local_id tasks;
              project = import.project;
              title = import.title;
              status = "open";
              branch = None;
              notes = None;
              worker_id = None;
              worker_key = None;
              external_key = Some external_key;
              external_url = import.url;
              external_source = Some import.origin;
              created_at = Some now;
              updated_at = Some now;
            }
          in
          (tasks @ [ task ], created + 1, updated)
      | Some current ->
          let candidate =
            {
              current with
              project = import.project;
              title = import.title;
              external_url = import.url;
              external_source = Some import.origin;
            }
          in
          if candidate = current then (tasks, created, updated)
          else
            let candidate = { candidate with updated_at = Some (now_utc ()) } in
            (replace_local_task tasks candidate, created, updated + 1))
    (tasks, 0, 0) imports

let duplicate_task_claim_warnings records =
  let explicit_claims =
    records
    |> List.filter_map (fun record ->
           Option.map (fun key -> (key, record)) record.Job_store.job.Job.task_key)
  in
  explicit_claims
  |> List.map fst |> List.sort_uniq String.compare
  |> List.filter_map (fun key ->
         let claimants =
           explicit_claims
           |> List.filter_map (fun (claimed, record) ->
                  if String.equal claimed key then Some record else None)
           |> List.sort compare_job_records
         in
         match claimants with
         | _ :: _ :: _ ->
             let winner = List.fold_left preferred_job_record (List.hd claimants)
                 (List.tl claimants)
             in
             Some
               (Printf.sprintf
                  "multiple workers claim task %s: %s; using %s for display"
                  key
                  (claimants
                  |> List.map (fun record -> record.Job_store.id)
                  |> String.concat ", ")
                  winner.Job_store.id)
         | _ -> None)

let plan_jobs ~home projects initial_tasks records initial_warnings =
  let records = List.sort compare_job_records records in
  let duplicate_warnings =
    records
    |> List.filter (valid_local_task_link projects initial_tasks)
    |> duplicate_task_claim_warnings
  in
  let record_worker_key record =
    worker_key ~repo:record.Job_store.job.Job.repo
      ~branch:record.job.Job.branch ~id:record.id
  in
  let worker_keys = List.map record_worker_key records in
  let ambiguous_worker_keys =
    worker_keys |> List.sort_uniq String.compare
    |> List.filter (fun key ->
           List.length (List.filter (String.equal key) worker_keys) > 1)
  in
  let worker_identity_warnings =
    ambiguous_worker_keys
    |> List.map (fun key ->
           let paths =
             records
             |> List.filter (fun record -> String.equal key (record_worker_key record))
             |> List.map (fun record -> record.Job_store.path)
             |> List.sort String.compare
           in
           Printf.sprintf
             "multiple worker records share one stable repo/branch/id identity and remain unlinked: %s"
             (String.concat ", " paths))
  in
  List.fold_left
    (fun (tasks, patches, created, warnings) (record : Job_store.record) ->
      match project_for_repo_opt projects record.job.Job.repo with
      | None ->
          ( tasks,
            patches,
            created,
            (Printf.sprintf "%s (%s)"
               (registration_warning ~home record.job.Job.repo) record.path)
            :: warnings )
      | Some project -> (
          match record.job.Job.task_key with
          | Some key -> (
              match Job_store.local_task_id_of_key (Some key) with
              | Error msg ->
                  (tasks, patches, created, (msg ^ " in " ^ record.path) :: warnings)
              | Ok None ->
                  ( tasks,
                    patches,
                    created,
                    (Printf.sprintf "worker %s has unsupported non-local task key %S"
                       record.id key)
                    :: warnings )
              | Ok (Some id) -> (
                  match find_local_task_by_id tasks id with
                  | None ->
                      ( tasks,
                        patches,
                        created,
                        (Printf.sprintf "worker %s links missing local task %s"
                           record.id key)
                        :: warnings )
                  | Some task when not (String.equal task.project project.id) ->
                      ( tasks,
                        patches,
                        created,
                        (Printf.sprintf
                           "worker %s in project %s links task %s from project %s"
                           record.id project.id key task.project)
                        :: warnings )
                  | Some _ -> (tasks, patches, created, warnings)))
          | None -> (
              let stable_key =
                worker_key ~repo:record.job.Job.repo ~branch:record.job.Job.branch
                  ~id:record.id
              in
              if List.exists (String.equal stable_key) ambiguous_worker_keys then
                (tasks, patches, created, warnings)
              else
                match find_tasks_by_worker tasks stable_key with
              | [ task ] ->
                  ( tasks,
                    (record, local_task_key task.id) :: patches,
                    created,
                    warnings )
              | [] ->
                  let now = now_utc () in
                  let task =
                    {
                      id = next_local_id tasks;
                      project = project.id;
                      title = record.job.Job.title;
                      status = status_of_job record;
                      branch = record.job.Job.branch;
                      notes = None;
                      worker_id = Some record.id;
                      worker_key = Some stable_key;
                      external_key = None;
                      external_url = None;
                      external_source = None;
                      created_at = Some now;
                      updated_at = Some now;
                    }
                  in
                  ( tasks @ [ task ],
                    (record, local_task_key task.id) :: patches,
                    created + 1,
                    warnings )
              | many ->
                  let ids =
                    many |> List.map (fun (task : local_task) -> task.id)
                    |> List.sort String.compare
                  in
                  ( tasks,
                    patches,
                    created,
                    (Printf.sprintf
                       "worker %s matches multiple stable worker_id tasks: %s; repair explicitly"
                       record.id (String.concat ", " ids))
                    :: warnings ))))
    ( initial_tasks,
      [],
      0,
      worker_identity_warnings @ duplicate_warnings @ initial_warnings )
    records

let fetch_external_imports projects =
  fold_results projects ~init:[] ~f:(fun acc project ->
      fetch_project_tasks project |> Result.map (fun tasks -> acc @ tasks))

let sync_jobs_to_local_tasks ~home =
  let ( let* ) = Result.bind in
  (* External commands run before the state lock. The locked phase reloads every
     durable input and only accepts imports for projects that still exist. *)
  let* projects_before_lock = load_projects ~home in
  let* imports = fetch_external_imports projects_before_lock in
  State_store.with_lock ~home (fun () ->
      let* projects = load_projects ~home in
      let* tasks = load_local_tasks ~home in
      let* scan = Job_store.scan ~home in
      let valid_project_ids = List.map (fun (project : project) -> project.id) projects in
      let imports =
        imports
        |> List.filter (fun task -> List.exists (String.equal task.project) valid_project_ids)
      in
      let tasks, external_created, external_updated =
        update_external_tasks tasks imports
      in
      let tasks, patches, job_created, warnings =
        plan_jobs ~home projects tasks scan.records scan.warnings
      in
      let changed_tasks = external_created + external_updated + job_created > 0 in
      let* () =
        if changed_tasks then save_local_tasks_unlocked ~home tasks else Ok ()
      in
      let* () =
        match Sys.getenv_opt "MONTY_FAULT_INJECT" with
        | Some "sync-after-tasks" -> Error "fault injected at sync-after-tasks"
        | _ -> Ok ()
      in
      let patches =
        patches
        |> List.sort (fun (left, _) (right, _) -> compare_job_records left right)
      in
      let* linked_jobs =
        fold_results patches ~init:0 ~f:(fun count (record, task_key) ->
            match record.Job_store.job.Job.task_key with
            | Some current when String.equal current task_key -> Ok count
            | _ ->
                let* () =
                  Job_store.update_file_unlocked record.path
                    [ Job_store.string "task_key" task_key ]
                in
                Ok (count + 1))
      in
      Ok
        {
          created = external_created + job_created;
          updated = external_updated;
          linked_jobs;
          warnings = sort_uniq_strings warnings;
        })

let diagnostic_task_key (record : Job_store.record) =
  "worker:" ^ record.id ^ ":" ^ record.path

let diagnostic_task projects (record : Job_store.record) =
  let project =
    match project_for_repo_opt projects record.job.Job.repo with
    | Some project -> project.id
    | None -> "<unknown>"
  in
  {
    key = diagnostic_task_key record;
    display_id = record.id;
    project;
    origin = "worker";
    title = record.job.Job.title;
    status = status_of_job record;
    branch = record.job.Job.branch;
    url = None;
  }

let load_tasks_with_warnings ~home ?project ?(all = false) () =
  let ( let* ) = Result.bind in
  let* projects = load_projects ~home in
  let* selected_projects =
    match project with
    | None -> Ok projects
    | Some needle -> resolve_project projects needle |> Result.map (fun value -> [ value ])
  in
  let* local_tasks = load_local_tasks ~home in
  let* scan = Job_store.scan ~home in
  let selected_ids = List.map (fun (project : project) -> project.id) selected_projects in
  let visible_task (task : local_task) =
    all || not (String.equal (String.lowercase_ascii task.status) "done")
  in
  let local_items =
    local_tasks
    |> List.filter (fun (task : local_task) ->
           List.exists (String.equal task.project) selected_ids
           && visible_task task)
    |> List.map task_of_local
  in
  let orphan_tasks =
    match project with
    | Some _ -> []
    | None ->
        local_tasks
        |> List.filter (fun (task : local_task) ->
               not
                 (List.exists
                    (fun (known : project) -> String.equal known.id task.project)
                    projects)
               && visible_task task)
        |> List.map task_of_local
  in
  let local_items = local_items @ orphan_tasks in
  let display_ids =
    scan.records
    |> List.filter (valid_local_task_link projects local_tasks)
    |> linked_job_display_ids
  in
  let local_items =
    local_items
    |> List.map (fun task ->
           match List.assoc_opt task.key display_ids with
           | None -> task
           | Some display_id -> { task with display_id })
  in
  let diagnostics, unknown_warnings =
    scan.records
    |> List.fold_left
         (fun (items, warnings) record ->
           let linked = valid_local_task_link projects local_tasks record in
           let selected =
             match project_for_repo_opt selected_projects record.job.Job.repo with
             | Some _ -> true
             | None -> project = None
           in
           let visible = all || not (Job_store.is_archived record) in
           let warnings =
             match project_for_repo_opt projects record.job.Job.repo with
             | None ->
                 (Printf.sprintf "%s (%s)"
                    (registration_warning ~home record.job.Job.repo) record.path)
                 :: warnings
             | Some worker_project -> (
                 match record.job.Job.task_key with
                 | None -> warnings
                 | Some key -> (
                     match Job_store.local_task_id_of_key (Some key) with
                     | Ok (Some id) -> (
                         match find_local_task_by_id local_tasks id with
                         | Some task
                           when not (String.equal task.project worker_project.id) ->
                             (Printf.sprintf
                                "worker %s in project %s links task %s from project %s"
                                record.id worker_project.id key task.project)
                             :: warnings
                         | _ -> warnings)
                     | _ -> warnings))
           in
           if linked || not selected || not visible then (items, warnings)
           else (diagnostic_task projects record :: items, warnings))
         ([], [])
  in
  let orphan_warnings =
    orphan_tasks
    |> List.map (fun task ->
           Printf.sprintf
             "local task %s references unknown project %s; register or repair the project before mutating it"
             task.key task.project)
  in
  Ok
    ( local_items @ List.rev diagnostics,
      sort_uniq_strings
        ( scan.warnings
        @ duplicate_task_claim_warnings
            (List.filter (valid_local_task_link projects local_tasks) scan.records)
        @ unknown_warnings @ orphan_warnings ) )

let load_tasks ~home ?project ?(all = false) () =
  load_tasks_with_warnings ~home ?project ~all () |> Result.map fst

let validate_job_project ~home repo =
  let ( let* ) = Result.bind in
  let* projects = load_projects ~home in
  match project_for_repo_opt projects repo with
  | Some project -> Ok project
  | None ->
      Error
        (registration_warning ~home (Shell.normalize (Shell.abs_path repo)))

let plan_launch_task_links ~home (jobs : (string * Job.t) list) =
  let ( let* ) = Result.bind in
  let* projects = load_projects ~home in
  let* initial_tasks = load_local_tasks ~home in
  let plan_one (tasks, planned, changed) (worker_id, (job : Job.t)) =
    let* project =
      match project_for_repo_opt projects job.repo with
      | Some project -> Ok project
      | None ->
          Error
            (registration_warning ~home
               (Shell.normalize (Shell.abs_path job.repo)))
    in
    let stable_key = worker_key ~repo:job.repo ~branch:job.branch ~id:worker_id in
    let link (task : local_task) =
      if not (String.equal task.status "open") then
        Error
          (Printf.sprintf "linked local Monty task %s is %s, not open"
             task.id task.status)
      else if not (String.equal task.project project.id) then
        Error
          (Printf.sprintf
             "linked task local:%s belongs to project %s, not worker repo %s"
             task.id task.project job.repo)
      else
        match (task.worker_key, task.worker_id) with
        | Some owner, _ when not (String.equal owner stable_key) ->
            Error
              (Printf.sprintf
                 "linked local Monty task %s is already reserved for another repo+branch+worker identity"
                 task.id)
        | _, Some owner when not (String.equal owner worker_id) ->
            Error
              (Printf.sprintf
                 "linked local Monty task %s is already reserved for worker %s"
                 task.id owner)
        | _ ->
            let linked =
              { task with
                worker_id = Some worker_id;
                worker_key = Some stable_key;
                branch = job.branch }
            in
            let changed = changed || linked <> task in
            let linked =
              if linked = task then linked
              else { linked with updated_at = Some (now_utc ()) }
            in
            Ok
              ( replace_local_task tasks linked,
                (worker_id, { job with Job.task_key = Some (local_task_key task.id) })
                :: planned,
                changed )
    in
    match job.task_key with
    | Some key -> (
        match Job_store.local_task_id_of_key (Some key) with
        | Error msg -> Error msg
        | Ok None ->
            Error
              (Printf.sprintf "worker task key must be local:<id>, got %S" key)
        | Ok (Some id) -> (
            match find_local_task_by_id tasks id with
            | None ->
                Error
                  (Printf.sprintf "linked local Monty task is missing: %s" id)
            | Some task -> link task))
    | None -> (
        match find_tasks_by_worker tasks stable_key with
        | [ task ] -> link task
        | [] ->
            let now = now_utc () in
            let task =
              { id = next_local_id tasks;
                project = project.id;
                title = job.title;
                status = "open";
                branch = job.branch;
                notes = None;
                worker_id = Some worker_id;
                worker_key = Some stable_key;
                external_key = None;
                external_url = None;
                external_source = None;
                created_at = Some now;
                updated_at = Some now }
            in
            Ok
              ( tasks @ [ task ],
                (worker_id, { job with Job.task_key = Some (local_task_key task.id) })
                :: planned,
                true )
        | _ ->
            Error
              (Printf.sprintf
                 "multiple local tasks store worker identity for %S; repair the ambiguity before launch"
                 worker_id))
  in
  let* tasks, planned, changed =
    fold_results jobs ~init:(initial_tasks, [], false) ~f:plan_one
  in
  Ok (tasks, List.rev planned, changed)

let preflight_launch_task_links ~home jobs =
  plan_launch_task_links ~home jobs
  |> Result.map (fun (_, planned, _) -> planned)

let reserve_launch_task_links_unlocked ~home jobs =
  let ( let* ) = Result.bind in
  let* tasks, planned, changed = plan_launch_task_links ~home jobs in
  let* () = if changed then save_local_tasks_unlocked ~home tasks else Ok () in
  Ok planned

let ensure_worker_task_link ~home ~worker_id (job : Job.t) =
  State_store.with_lock ~home (fun () ->
      let ( let* ) = Result.bind in
      let* projects = load_projects ~home in
      let* project =
        match project_for_repo_opt projects job.repo with
        | Some project -> Ok project
        | None ->
            Error
              (registration_warning ~home
                 (Shell.normalize (Shell.abs_path job.repo)))
      in
      let* tasks = load_local_tasks ~home in
      let stable_key = worker_key ~repo:job.repo ~branch:job.branch ~id:worker_id in
      match job.task_key with
      | Some key -> (
          match Job_store.local_task_id_of_key (Some key) with
          | Error msg -> Error msg
          | Ok None -> Error (Printf.sprintf "worker task key must be local:<id>, got %S" key)
          | Ok (Some id) -> (
              match find_local_task_by_id tasks id with
              | None -> Error (Printf.sprintf "linked local Monty task is missing: %s" id)
              | Some task when not (String.equal task.project project.id) ->
                  Error
                    (Printf.sprintf "linked task %s belongs to project %s, not worker repo %s"
                       key task.project job.repo)
              | Some task ->
                  let linked =
                    { task with
                      worker_id = Some worker_id;
                      worker_key = Some stable_key }
                  in
                  let* () =
                    if linked = task then Ok ()
                    else
                      let linked = { linked with updated_at = Some (now_utc ()) } in
                      save_local_tasks_unlocked ~home
                        (replace_local_task tasks linked)
                  in
                  Ok job))
      | None -> (
          match find_tasks_by_worker tasks stable_key with
          | [ task ] -> Ok { job with Job.task_key = Some (local_task_key task.id) }
          | [] ->
              let now = now_utc () in
              let task =
                {
                  id = next_local_id tasks;
                  project = project.id;
                  title = job.title;
                  status = "open";
                  branch = job.branch;
                  notes = None;
                  worker_id = Some worker_id;
                  worker_key = Some stable_key;
                  external_key = None;
                  external_url = None;
                  external_source = None;
                  created_at = Some now;
                  updated_at = Some now;
                }
              in
              let* () = save_local_tasks_unlocked ~home (tasks @ [ task ]) in
              Ok { job with Job.task_key = Some (local_task_key task.id) }
          | _ ->
              Error
                (Printf.sprintf
                   "multiple local tasks store worker identity for %S; repair the ambiguity before launch"
                   worker_id)))

let repair_legacy_task_link ~home worker =
  State_store.with_lock ~home (fun () ->
      let ( let* ) = Result.bind in
      let* record = Job_store.find ~home ~scope:Job_store.All worker in
      match record.job.Job.task_key with
      | Some key -> Error (Printf.sprintf "worker %s already links %s" record.id key)
      | None ->
          let* projects = load_projects ~home in
          let* project = project_for_repo projects record.job.Job.repo in
          let* tasks = load_local_tasks ~home in
          let matches =
            tasks
            |> List.filter (fun (task : local_task) ->
                   String.equal task.project project.id
                   && (String.equal task.title record.job.Job.title
                      || Option.equal String.equal task.branch record.job.Job.branch))
          in
          (match matches with
          | [ task ] ->
              let key = local_task_key task.id in
              let task =
                { task with
                  worker_id = Some record.id;
                  worker_key =
                    Some
                      (worker_key ~repo:record.job.Job.repo
                         ~branch:record.job.Job.branch ~id:record.id);
                  updated_at = Some (now_utc ()) }
              in
              let* () = save_local_tasks_unlocked ~home (replace_local_task tasks task) in
              let* () =
                Job_store.update_file_unlocked record.path [ Job_store.string "task_key" key ]
              in
              Ok key
          | [] -> Error (Printf.sprintf "no legacy local task matches worker %s" record.id)
          | many ->
              let ids = many |> List.map (fun task -> task.id) |> List.sort String.compare in
              Error
                (Printf.sprintf "legacy repair is ambiguous for worker %s: %s"
                   record.id (String.concat ", " ids))))
