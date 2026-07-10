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

type local_task = Overview_types.local_task = {
  id : string;
  project : string;
  title : string;
  status : string;
  branch : string option;
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
