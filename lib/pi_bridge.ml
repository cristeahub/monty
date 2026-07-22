open Overview_types

let tasks_schema = "monty:pi-tasks:v1"
let entry_schema = "monty:pi-entry:v1"
let ( let* ) = Result.bind

let key (t : local_task) = "local:" ^ t.id
let done_ (t : local_task) = String.equal (String.lowercase_ascii t.status) "done"
let opt_json = Option.fold ~none:`Null ~some:(fun v -> `String v)

let worker_json (r : Job_store.record) =
  `Assoc
    [ ("id", `String r.id); ("status", `String r.status);
      ("branch", opt_json r.job.Job.branch) ]

let linked records t =
  List.filter (fun r -> r.Job_store.job.Job.task_key = Some (key t)) records

let action records (t : local_task) =
  if done_ t then "done"
  else
    match linked records t with
    | [ r ] when not (Job_store.is_archived r) && r.transition = None -> "open"
    | [] when t.worker_id = None && t.worker_key = None -> "plan"
    | _ -> "blocked"

let branch records (t : local_task) =
  match linked records t with
  | r :: _ -> (match r.job.Job.branch with Some _ as v -> v | None -> t.branch)
  | [] -> t.branch

let task_json records (t : local_task) =
  let workers = linked records t in
  `Assoc
    [ ("key", `String (key t));
      ("id", `String (Option.value ~default:(key t) t.external_key));
      ("project", `String t.project); ("status", `String t.status);
      ("title", `String t.title); ("branch", opt_json (branch records t));
      ("action", `String (action records t));
      ("worker", match workers with [ r ] -> worker_json r | _ -> `Null) ]

let task_warnings records (t : local_task) =
  match linked records t with
  | [] when t.worker_id <> None || t.worker_key <> None ->
      [ Printf.sprintf "task %s references worker %s, but no canonical worker record exists"
          (key t) (Option.value ~default:"<unknown>" t.worker_id) ]
  | _ :: _ :: _ -> [ Printf.sprintf "task %s has multiple canonical worker records" (key t) ]
  | [ r ] when Job_store.is_archived r && not (done_ t) ->
      [ Printf.sprintf "open task %s links to archived worker %s" (key t) r.id ]
  | [ r ] when r.transition <> None ->
      [ Printf.sprintf "task %s worker %s is in a lifecycle transition" (key t) r.id ]
  | _ -> []

let compare_task (a : local_task) (b : local_task) =
  match String.compare a.project b.project with
  | 0 -> String.compare a.id b.id
  | n -> n

let tasks_json ~home ?project ~all () =
  let* scan = Job_store.scan ~home in
  let* tasks = Task_storage.load_local_tasks ~home in
  let* project =
    match project with
    | None -> Ok None
    | Some value ->
        let* ps = Project_storage.load_projects ~home in
        let* p = Project_storage.resolve_project ps value in
        Ok (Some p.id)
  in
  let tasks : local_task list =
    tasks
    |> List.filter (fun (t : local_task) -> all || not (done_ t))
    |> List.filter (fun (t : local_task) ->
           Option.fold ~none:true ~some:(String.equal t.project) project)
    |> List.sort compare_task
  in
  let warnings =
    scan.warnings @ List.concat_map (task_warnings scan.records) tasks
    |> List.sort_uniq String.compare
  in
  Ok
    (`Assoc
      [ ("schema", `String tasks_schema); ("home", `String home);
        ("tasks", `List (List.map (task_json scan.records) tasks));
        ("warnings", `List (List.map (fun s -> `String s) warnings)) ])

let exact_match value (t : local_task) =
  [ key t; t.id; Option.value ~default:"" t.external_key;
    Option.value ~default:"" t.worker_id ]
  |> List.exists (String.equal value)

let slug_match value (t : local_task) =
  String.equal (Slug.of_title value) (Slug.of_title t.title)

let resolve ~home value =
  let* tasks = Task_storage.load_local_tasks ~home in
  let value = String.trim value in
  let exact = List.filter (exact_match value) tasks in
  let matches = if exact = [] then List.filter (slug_match value) tasks else exact in
  match matches with
  | [ t ] -> Ok t
  | [] -> Error (Printf.sprintf "no Monty task matching %S" value)
  | many ->
      Error
        (Printf.sprintf "multiple Monty tasks match %S:\n%s" value
           (many |> List.map (fun t -> "- " ^ key t ^ " " ^ t.title)
           |> String.concat "\n"))

let record ~home t =
  let* scan = Job_store.scan ~home in
  match linked scan.records t with
  | [ r ] when not (Job_store.is_archived r) -> Ok r
  | [] when t.worker_id <> None || t.worker_key <> None ->
      Error
        (Printf.sprintf
           "task %s references a missing canonical worker; repair that worker before entering or preparing the task"
           (key t))
  | [] -> Error (Printf.sprintf "task %s has no prepared worker" (key t))
  | [ r ] -> Error (Printf.sprintf "task %s worker %s is archived" (key t) r.id)
  | _ -> Error (Printf.sprintf "task %s has multiple canonical workers" (key t))

let save_cwd ~home (r : Job_store.record) cwd =
  State_store.with_lock ~home (fun () ->
      let* cur = Job_store.parse_job_file ~home r.path in
      if cur.id <> r.id || cur.job.Job.task_key <> r.job.Job.task_key then
        Error (Printf.sprintf "worker %s identity changed while entering its task" r.id)
      else
        match cur.transition with
        | Some tr ->
            Error
              (Printf.sprintf "worker %s is in a %s transition" r.id
                 (Job_store.operation_name tr.operation))
        | None ->
            let* () =
              Project_overview.validate_worker_task_open_unlocked ~home cur
            in
            Job_store.update_file_unlocked cur.path
              [ Job_store.string "last_known_worktree" cwd;
                Job_store.string "updated_at" (Worker_memory.now_utc ()) ])

let entry_json t (r : Job_store.record) cwd =
  let instructions = Worker_memory.instructions_file r.worker_dir in
  `Assoc
    [ ("schema", `String entry_schema);
      ("task", task_json [ r ] t); ("worker", worker_json r);
      ("cwd", `String cwd); ("instructions", `String instructions);
      ("context", `String r.job.Job.context); ("memory", `String (Worker_memory.memory_file r.worker_dir));
      ("prompt", `String (Job.prompt r.job)) ]

let enter ~home ~wt_command value =
  let* t = resolve ~home value in
  if done_ t then Error (Printf.sprintf "task %s is done" (key t))
  else
    let* r = record ~home t in
    let* () =
      match r.transition with
      | None -> Ok ()
      | Some transition ->
          Error
            (Printf.sprintf "worker %s is in a %s transition" r.id
               (Job_store.operation_name transition.operation))
    in
    let* _ = Project_overview.validate_worker_task_link ~home r in
    let* cwd =
      match String.lowercase_ascii r.worktree_mode with
      | "always" ->
          let* branch =
            match r.job.Job.branch with
            | Some v -> Ok v
            | None -> Error (Printf.sprintf "worker %s has no branch" r.id)
          in
          Wt.create_or_reuse ~wt_command ~repo:r.job.Job.repo ~branch
      | "never" -> Launcher.canonical_existing "repo" r.job.Job.repo
      | mode -> Error (Printf.sprintf "worker %s has unknown worktree mode %S" r.id mode)
    in
    let* () = save_cwd ~home r cwd in
    let* r = Job_store.parse_job_file ~home r.path in
    let* _ = Launcher.canonical_existing "context" r.job.Job.context in
    let* _ = Launcher.canonical_existing "context" (Worker_memory.instructions_file r.worker_dir) in
    Ok (entry_json t r cwd)

let context (t : local_task) repo branch plan =
  String.concat "\n"
    [ "# " ^ t.title; ""; "## Task"; "";
      "Task: " ^ key t; "Project: " ^ t.project; "Repo: " ^ repo;
      "Branch: " ^ branch; ""; "## Plan"; ""; String.trim plan; "";
      "## Constraints"; "";
      "Keep durable discoveries and handoff notes in the Monty worker memory.";
      "Do not push, open a pull request, or post remotely without explicit approval.";
      ""; "## Acceptance criteria"; "";
      "Implement the approved plan completely.";
      "Run relevant non-destructive validation.";
      "Leave the task open until the user explicitly completes it."; "";
      "## Headless review chain"; "";
      "Monty's fixed chain supplies one implementer, two independent parallel reviewers, and one fixer.";
      "The implementer must not invoke /review or launch subagents.";
      "The reviewers verify the implementation independently, and the fixer resolves valid findings.";
      "" ]

let write_context ~home ~path text =
  State_store.with_lock ~home (fun () ->
      let* path = State_path.path_under_resolved_home ~home path in
      if Sys.file_exists path then
        if String.equal (Shell.read_file path) text then Ok false
        else Error (Printf.sprintf "task context already exists with different contents: %s" path)
      else
        let* () = State_store.write_file_atomic ~path ~perm:0o600 text in
        Ok true)

let remove_unclaimed_context ~home path =
  State_store.with_lock ~home (fun () ->
      let* path = State_path.path_under_resolved_home ~home path in
      let* records = Job_store.load_all ~home in
      let claimed =
        List.exists
          (fun (r : Job_store.record) ->
            match State_path.canonicalize r.job.Job.context with
            | Ok context -> String.equal context path
            | Error _ -> false)
          records
      in
      if claimed || not (State_path.path_exists path) then Ok ()
      else
        try
          if (Unix.lstat path).Unix.st_kind = Unix.S_REG then Unix.unlink path;
          Ok ()
        with Unix.Unix_error (err, fn, arg) ->
          Error
            (Printf.sprintf "cannot remove unclaimed task context via %s(%s): %s"
               fn arg (Unix.error_message err)))

let prepare_locked options plan_file (t : local_task) =
  let home = options.Launcher.home in
  if done_ t then Error (Printf.sprintf "task %s is done" (key t))
  else
    match record ~home t with
    | Ok _ -> enter ~home ~wt_command:options.wt_command (key t)
    | Error _ when t.worker_id <> None || t.worker_key <> None ->
        record ~home t |> Result.map (fun _ -> assert false)
    | Error _ ->
        let* plan = Launcher.canonical_existing "plan" plan_file in
        let plan = Shell.read_file plan in
        let* projects = Project_storage.load_projects ~home in
        let* project = Project_storage.resolve_project projects t.project in
        let* _ = Launcher.canonical_existing "repo" project.repo in
        let dependency_options =
          { options with Launcher.worktree_mode = Launcher.Always }
        in
        let* () = Launcher.check_worktree_dependency dependency_options in
        let id = t.id in
        let branch =
          Option.value
            ~default:(Slug.branch ~prefix:options.branch_prefix (t.id ^ " " ^ t.title))
            t.branch
        in
        let* state = State_path.active ~home ~run_id:"pi" ~id in
        let context_file = Filename.concat state.run_dir (id ^ ".md") in
        let text = context t project.repo branch plan in
        let* created = write_context ~home ~path:context_file text in
        let job =
          Job.make ~id ~branch ~worker_dir:state.worker_dir ~task_key:(key t)
            ~title:t.title ~repo:project.repo ~context:context_file ()
        in
        (match Headless.prepare_many ~dry_run:false options [ (1, job) ] with
        | Ok _ -> enter ~home ~wt_command:options.wt_command (key t)
        | Error message ->
            if not created then Error message
            else
              (match remove_unclaimed_context ~home context_file with
              | Ok () -> Error message
              | Error cleanup -> Error (message ^ "; " ^ cleanup)))

let prepare options value plan_file =
  let home = options.Launcher.home in
  let* t = resolve ~home value in
  State_store.with_named_lock ~home ~name:("prepare-" ^ t.id ^ ".lock")
    (fun () ->
      let* current = resolve ~home (key t) in
      prepare_locked options plan_file current)
