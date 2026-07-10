open Overview_types

let project_memory_file = Project_storage.project_memory_file
let source_label = Project_storage.source_label
let sources_label = Project_storage.sources_label
let compare_project = Project_storage.compare_project
let load_projects = Project_storage.load_projects
let load_tasks = Reconciliation.load_tasks
let sync_jobs_to_local_tasks = Reconciliation.sync_jobs_to_local_tasks

let render_projects (projects : project list) =
  let projects = List.sort compare_project projects in
  let header = Printf.sprintf "%-20s %-48s %s" "ID" "REPO" "SOURCES" in
  let lines =
    projects
    |> List.map (fun (project : project) ->
           Printf.sprintf "%-20s %-48s %s" project.id project.repo
             (sources_label project.sources))
  in
  String.concat "\n" (header :: lines) ^ "\n"

let show_project ~home (project : project) =
  let memory = project_memory_file ~home project.id in
  let memory_text =
    if Sys.file_exists memory then Shell.read_file memory
    else "No project memory yet. Create " ^ memory ^ " to add stable project context.\n"
  in
  String.concat "\n"
    [ "Project: " ^ project.id;
      "Repo: " ^ project.repo;
      "Sources: " ^ sources_label project.sources;
      "Memory: " ^ memory;
      "";
      memory_text ]

let compare_tasks left right =
  match String.compare left.project right.project with
  | 0 -> String.compare left.key right.key
  | value -> value

let width minimum values =
  values |> List.fold_left (fun current value -> max current (String.length value)) minimum

let pad_right width value =
  value ^ String.make (max 0 (width - String.length value)) ' '

let render_tasks tasks =
  let tasks = List.sort compare_tasks tasks in
  let ids = List.map (fun task -> task.display_id) tasks in
  let projects = List.map (fun task -> task.project) tasks in
  let statuses = List.map (fun task -> task.status) tasks in
  let titles = List.map (fun task -> task.title) tasks in
  let branches = List.map (fun task -> Option.value ~default:"" task.branch) tasks in
  let id_width = width 2 ("ID" :: ids) in
  let project_width = width 7 ("PROJECT" :: projects) in
  let status_width = width 6 ("STATUS" :: statuses) in
  let title_width = width 5 ("TITLE" :: titles) in
  let render_row id project status title branch =
    String.concat " "
      [ pad_right id_width id;
        pad_right project_width project;
        pad_right status_width status;
        pad_right title_width title;
        branch ]
  in
  let header = render_row "ID" "PROJECT" "STATUS" "TITLE" "BRANCH" in
  let lines =
    List.map2
      (fun task branch -> render_row task.display_id task.project task.status task.title branch)
      tasks branches
  in
  String.concat "\n" (header :: lines) ^ "\n"

let render_active_jobs jobs =
  let header = Printf.sprintf "%-16s %-32s %-24s %s" "ID" "TITLE" "BRANCH" "DIR" in
  let lines =
    jobs
    |> List.map (fun record ->
           Printf.sprintf "%-16s %-32s %-24s %s" record.Job_store.id
             record.Job_store.job.Job.title
             (Option.value ~default:"<no-branch>" record.Job_store.job.Job.branch)
             record.Job_store.worker_dir)
  in
  String.concat "\n" (header :: lines) ^ "\n"

let overview ~home =
  let ( let* ) = Result.bind in
  let* _sync_result = sync_jobs_to_local_tasks ~home in
  let* projects = load_projects ~home in
  let* tasks = load_tasks ~home () in
  let* jobs = Job_store.load ~home ~scope:Job_store.Active in
  Ok
    (String.concat "\n"
       [ "# Monty overview";
         "";
         "## Projects";
         "";
         render_projects projects;
         "## Tasks";
         "";
         render_tasks tasks;
         "## Active jobs";
         "";
         render_active_jobs jobs ])
