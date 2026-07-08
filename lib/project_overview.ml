open Yojson.Safe

type github_source = {
  repo : string;
  query : string option;
}

type source = Github_issues of github_source

type raw_project = {
  repo : string;
  sources : source list;
}

type project = {
  id : string;
  repo : string;
  sources : source list;
}

type local_task = {
  id : string;
  project : string;
  title : string;
  status : string;
  priority : string option;
  branch : string option;
  notes : string option;
  created_at : string option;
  updated_at : string option;
}

type task = {
  key : string;
  display_id : string;
  project : string;
  origin : string;
  title : string;
  status : string;
  priority : string option;
  branch : string option;
  url : string option;
}

let monty_dir home = Filename.concat home ".monty"
let projects_file home = Filename.concat (monty_dir home) "projects.json"
let projects_dir home = Filename.concat (monty_dir home) "projects"
let local_tasks_file home = Filename.concat (monty_dir home) "tasks.local.json"
let priorities_file home = Filename.concat (monty_dir home) "priorities.json"
let project_memory_file ~home id = Filename.concat (projects_dir home) (id ^ ".md")

let now_utc = Worker_memory.now_utc

let prefix text value =
  let text_len = String.length text in
  let value_len = String.length value in
  value_len >= text_len && String.sub value 0 text_len = text

let strip_prefix text value =
  if prefix text value then
    Some (String.sub value (String.length text) (String.length value - String.length text))
  else None

let member_string json name =
  match Util.member name json with
  | `String value when String.trim value <> "" -> Ok value
  | _ -> Error (Printf.sprintf "missing string field %S" name)

let optional_string json name =
  match Util.member name json with
  | `Null -> Ok None
  | `String value when String.trim value = "" -> Ok None
  | `String value -> Ok (Some value)
  | _ -> Error (Printf.sprintf "field %S must be a string when present" name)

let list_field json name =
  match Util.member name json with
  | `Null -> Ok []
  | `List values -> Ok values
  | _ -> Error (Printf.sprintf "field %S must be an array" name)

let read_json_file path =
  try Ok (Yojson.Safe.from_file path) with
  | Sys_error msg -> Error msg
  | Yojson.Json_error msg -> Error ("invalid JSON in " ^ path ^ ": " ^ msg)

let fold_results values ~init ~f =
  List.fold_left
    (fun acc value ->
      match acc with Error _ as err -> err | Ok acc -> f acc value)
    (Ok init) values

let parse_source json =
  let ( let* ) = Result.bind in
  let* kind = member_string json "kind" in
  match kind with
  | "github_issues" ->
      let* repo = member_string json "repo" in
      let* query = optional_string json "query" in
      Ok (Github_issues { repo; query })
  | _ -> Error (Printf.sprintf "unknown project source kind %S" kind)

let json_of_source = function
  | Github_issues { repo; query } ->
      let fields =
        [ ("kind", `String "github_issues"); ("repo", `String repo) ]
        @ (match query with None -> [] | Some value -> [ ("query", `String value) ])
      in
      `Assoc fields

let parse_raw_project json =
  let ( let* ) = Result.bind in
  let* repo = member_string json "repo" in
  let repo = Shell.normalize (Shell.abs_path repo) in
  let* sources_json = list_field json "sources" in
  let* sources = fold_results sources_json ~init:[] ~f:(fun acc json -> parse_source json |> Result.map (fun source -> source :: acc)) in
  Ok { repo; sources = List.rev sources }

let json_of_raw_project (project : raw_project) =
  `Assoc
    [ ("repo", `String project.repo);
      ("sources", `List (List.map json_of_source project.sources)) ]

let repo_basename repo =
  match Filename.basename repo with "" | "." | "/" -> "repo" | name -> name

let base_id (project : raw_project) = Slug.of_title (repo_basename project.repo)

let first_github_repo sources =
  sources
  |> List.find_map (function Github_issues { repo; _ } -> Some repo)

let disambiguated_id (project : raw_project) =
  match first_github_repo project.sources with
  | Some repo -> Slug.of_title repo
  | None ->
      let parent = project.repo |> Filename.dirname |> Filename.basename |> Slug.of_title in
      let base = base_id project in
      if parent = "" || parent = base then base else parent ^ "-" ^ base

let unique_ids (projects : raw_project list) : (string * raw_project) list =
  let count_base base =
    projects |> List.filter (fun project -> String.equal (base_id project) base) |> List.length
  in
  let candidate project =
    let base = base_id project in
    if count_base base = 1 then base else disambiguated_id project
  in
  let rec unique seen id index =
    let value = if index = 0 then id else id ^ "-" ^ string_of_int (index + 1) in
    if List.exists (String.equal value) seen then unique seen id (index + 1)
    else value
  in
  let rec loop seen acc = function
    | [] -> List.rev acc
    | project :: rest ->
        let id = unique seen (candidate project) 0 in
        loop (id :: seen) ((id, project) :: acc) rest
  in
  loop [] [] projects

let with_ids (projects : raw_project list) : project list =
  unique_ids projects
  |> List.map (fun (id, (project : raw_project)) ->
         { id; repo = project.repo; sources = project.sources })

let load_raw_projects ~home =
  let path = projects_file home in
  if not (Sys.file_exists path) then Ok []
  else
    let ( let* ) = Result.bind in
    let* json = read_json_file path in
    let* projects_json = list_field json "projects" in
    fold_results projects_json ~init:[] ~f:(fun acc json ->
        parse_raw_project json |> Result.map (fun project -> project :: acc))
    |> Result.map List.rev

let save_raw_projects ~home (projects : raw_project list) =
  let path = projects_file home in
  Shell.ensure_dir (Filename.dirname path);
  Yojson.Safe.to_file path (`Assoc [ ("projects", `List (List.map json_of_raw_project projects)) ]);
  Ok ()

let load_projects ~home = load_raw_projects ~home |> Result.map with_ids

let source_label = function
  | Github_issues { repo; query } -> (
      match query with
      | None -> "github:" ^ repo
      | Some query -> "github:" ^ repo ^ " search:" ^ query)

let sources_label sources =
  match sources with [] -> "local" | sources -> sources |> List.map source_label |> String.concat ", "

let compare_project (left : project) (right : project) = String.compare left.id right.id

let resolve_project (projects : project list) needle =
  let needle_slug = Slug.of_title needle in
  let matches =
    projects
    |> List.filter (fun (project : project) ->
           String.equal needle project.id
           || String.equal needle_slug project.id
           || String.equal needle project.repo
           || String.equal needle_slug (base_id { repo = project.repo; sources = project.sources }))
  in
  match matches with
  | [ project ] -> Ok project
  | [] -> Error (Printf.sprintf "no Monty project matching %S" needle)
  | many ->
      let labels =
        many
        |> List.map (fun (project : project) -> "- " ^ project.id ^ " " ^ project.repo)
        |> String.concat "\n"
      in
      Error (Printf.sprintf "multiple Monty projects match %S:\n%s" needle labels)

let project_memory_template (project : project) =
  String.concat "\n"
    [ "# " ^ project.id;
      "";
      "Repo: " ^ project.repo;
      "Sources: " ^ sources_label project.sources;
      "";
      "## What this project is";
      "";
      "TODO";
      "";
      "## How to work on it";
      "";
      "TODO";
      "";
      "## Current direction";
      "";
      "TODO";
      "" ]

let ensure_project_memory ~home (project : project) =
  let path = project_memory_file ~home project.id in
  if Sys.file_exists path then Ok ()
  else (
    Shell.ensure_dir (Filename.dirname path);
    Shell.write_file path (project_memory_template project);
    Ok ())

let project_id_changes old_projects new_projects =
  new_projects
  |> List.filter_map (fun project ->
         match List.find_opt (fun old -> String.equal old.repo project.repo) old_projects with
         | Some old when not (String.equal old.id project.id) -> Some (old.id, project.id)
         | _ -> None)

let migrate_project_memory ~home old_projects new_projects =
  let changes = project_id_changes old_projects new_projects in
  let migrate (old_id, new_id) =
    let old_path = project_memory_file ~home old_id in
    let new_path = project_memory_file ~home new_id in
    if Sys.file_exists old_path && not (Sys.file_exists new_path) then
      try
        Shell.ensure_dir (Filename.dirname new_path);
        Unix.rename old_path new_path;
        Ok ()
      with Unix.Unix_error (err, fn, arg) ->
        Error
          (Printf.sprintf "failed to migrate project memory via %s(%s): %s" fn arg
             (Unix.error_message err))
    else Ok ()
  in
  fold_results changes ~init:() ~f:(fun () change -> migrate change)

let add_project ~home ~repo ?github ?query () =
  let repo = Shell.normalize (Shell.abs_path repo) in
  if not (Sys.file_exists repo && Sys.is_directory repo) then
    Error (Printf.sprintf "repo is not an existing directory: %s" repo)
  else
    let ( let* ) = Result.bind in
    let* projects = load_raw_projects ~home in
    if List.exists (fun (project : raw_project) -> String.equal project.repo repo) projects then
      Error (Printf.sprintf "project already exists for repo: %s" repo)
    else
      let old_projects = with_ids projects in
      let sources =
        match github with
        | None -> []
        | Some repo -> [ Github_issues { repo; query } ]
      in
      let* () = save_raw_projects ~home (projects @ [ { repo; sources } ]) in
      let* projects = load_projects ~home in
      let* () = migrate_project_memory ~home old_projects projects in
      let* project = resolve_project projects repo in
      let* () = ensure_project_memory ~home project in
      Ok project

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

let parse_local_task json =
  let ( let* ) = Result.bind in
  let* id = member_string json "id" in
  let* project = member_string json "project" in
  let* title = member_string json "title" in
  let* status = optional_string json "status" in
  let* priority = optional_string json "priority" in
  let* branch = optional_string json "branch" in
  let* notes = optional_string json "notes" in
  let* created_at = optional_string json "created_at" in
  let* updated_at = optional_string json "updated_at" in
  Ok
    {
      id;
      project;
      title;
      status = Option.value ~default:"open" status;
      priority;
      branch;
      notes;
      created_at;
      updated_at;
    }

let json_of_local_task task =
  let fields =
    [ ("id", `String task.id);
      ("project", `String task.project);
      ("title", `String task.title);
      ("status", `String task.status) ]
    @ (match task.priority with None -> [] | Some value -> [ ("priority", `String value) ])
    @ (match task.branch with None -> [] | Some value -> [ ("branch", `String value) ])
    @ (match task.notes with None -> [] | Some value -> [ ("notes", `String value) ])
    @ (match task.created_at with None -> [] | Some value -> [ ("created_at", `String value) ])
    @ (match task.updated_at with None -> [] | Some value -> [ ("updated_at", `String value) ])
  in
  `Assoc fields

let load_local_tasks ~home =
  let path = local_tasks_file home in
  if not (Sys.file_exists path) then Ok []
  else
    let ( let* ) = Result.bind in
    let* json = read_json_file path in
    let* tasks_json = list_field json "tasks" in
    fold_results tasks_json ~init:[] ~f:(fun acc json ->
        parse_local_task json |> Result.map (fun task -> task :: acc))
    |> Result.map List.rev

let save_local_tasks ~home tasks =
  let path = local_tasks_file home in
  Shell.ensure_dir (Filename.dirname path);
  Yojson.Safe.to_file path (`Assoc [ ("tasks", `List (List.map json_of_local_task tasks)) ]);
  Ok ()

let parse_local_number id =
  match strip_prefix "local-" id with
  | None -> None
  | Some suffix -> (try Some (int_of_string suffix) with Failure _ -> None)

let next_local_id tasks =
  let max_id =
    tasks |> List.filter_map (fun task -> parse_local_number task.id)
    |> List.fold_left max 0
  in
  Printf.sprintf "local-%03d" (max_id + 1)

let load_priorities ~home =
  let path = priorities_file home in
  if not (Sys.file_exists path) then Ok []
  else
    match read_json_file path with
    | Error msg -> Error msg
    | Ok (`Assoc fields) ->
        fields
        |> List.fold_left
             (fun acc (key, value) ->
               match (acc, value) with
               | Error _ as err, _ -> err
               | Ok priorities, `String priority -> Ok ((key, priority) :: priorities)
               | Ok _, _ -> Error (Printf.sprintf "priority for %S must be a string" key))
             (Ok [])
        |> Result.map List.rev
    | Ok _ -> Error (path ^ " must contain a JSON object")

let priority_for priorities key = List.assoc_opt key priorities

let normalize_task_key key =
  if prefix "github:" key || prefix "local:" key then key
  else if prefix "local-" key then "local:" ^ key
  else key

let save_priorities ~home priorities =
  let path = priorities_file home in
  Shell.ensure_dir (Filename.dirname path);
  Yojson.Safe.to_file path
    (`Assoc (List.map (fun (key, priority) -> (key, `String priority)) priorities));
  Ok ()

let set_priority ~home ~task ~priority =
  let ( let* ) = Result.bind in
  let* priorities = load_priorities ~home in
  let key = normalize_task_key task in
  let priorities = (key, priority) :: List.remove_assoc key priorities in
  save_priorities ~home priorities

let first_some left right = match left with Some _ -> left | None -> right

let task_of_local priorities (task : local_task) =
  let key = "local:" ^ task.id in
  {
    key;
    display_id = key;
    project = task.project;
    origin = "local";
    title = task.title;
    status = task.status;
    priority = first_some (priority_for priorities key) task.priority;
    branch = task.branch;
    url = None;
  }

let add_local_task ~home ~project ~title ?priority () =
  let ( let* ) = Result.bind in
  let* projects = load_projects ~home in
  let* project = resolve_project projects project in
  let* tasks = load_local_tasks ~home in
  let now = now_utc () in
  let task =
    {
      id = next_local_id tasks;
      project = project.id;
      title;
      status = "open";
      priority;
      branch = None;
      notes = None;
      created_at = Some now;
      updated_at = Some now;
    }
  in
  let* () = save_local_tasks ~home (tasks @ [ task ]) in
  Ok task

let done_local_task ~home id =
  let ( let* ) = Result.bind in
  let* tasks = load_local_tasks ~home in
  let id = match strip_prefix "local:" id with Some value -> value | None -> id in
  if not (List.exists (fun task -> String.equal task.id id) tasks) then
    Error (Printf.sprintf "no local Monty task matching %S" id)
  else
    let now = now_utc () in
    let tasks =
      tasks
      |> List.map (fun task ->
             if String.equal task.id id then { task with status = "done"; updated_at = Some now }
             else task)
    in
    let* () = save_local_tasks ~home tasks in
    Ok ()

type sync_result = {
  created : int;
  updated : int;
  linked_jobs : int;
}

let empty_sync_result = { created = 0; updated = 0; linked_jobs = 0 }

let sync_merge left right =
  {
    created = left.created + right.created;
    updated = left.updated + right.updated;
    linked_jobs = left.linked_jobs + right.linked_jobs;
  }

let local_task_key id = "local:" ^ id

let local_task_id_from_key key =
  match strip_prefix "local:" key with
  | Some id when prefix "local-" id -> Some id
  | _ when prefix "local-" key -> Some key
  | _ -> None

let project_for_repo projects repo =
  let repo = Shell.normalize (Shell.abs_path repo) in
  match List.find_opt (fun (project : project) -> String.equal project.repo repo) projects with
  | Some project -> Ok project
  | None -> Error (Printf.sprintf "no Monty project for worker repo: %s" repo)

let find_local_task_by_id tasks id =
  List.find_opt (fun (task : local_task) -> String.equal task.id id) tasks

let find_local_task_for_job tasks project_id branch title =
  match
    tasks
    |> List.find_opt (fun (task : local_task) ->
           String.equal task.project project_id
           && Option.equal String.equal task.branch (Some branch))
  with
  | Some task -> Some task
  | None ->
      tasks
      |> List.find_opt (fun (task : local_task) ->
             String.equal task.project project_id && String.equal task.title title)

let replace_local_task tasks updated =
  tasks
  |> List.map (fun (task : local_task) -> if String.equal task.id updated.id then updated else task)

let status_of_job record = if Job_store.is_archived record then "done" else "open"

let preferred_job_record (left : Job_store.record) (right : Job_store.record) =
  match (Job_store.is_archived left, Job_store.is_archived right) with
  | false, true -> left
  | true, false -> right
  | _ ->
      if String.compare left.Job_store.id right.Job_store.id <= 0 then left
      else right

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

let apply_linked_job_display_ids ~home tasks =
  let ( let* ) = Result.bind in
  let* jobs = Job_store.load ~home ~scope:Job_store.All in
  let display_ids = linked_job_display_ids jobs in
  Ok
    (tasks
    |> List.map (fun task ->
           match List.assoc_opt task.key display_ids with
           | None -> task
           | Some display_id -> { task with display_id }))

let task_id_for_job tasks project_id record =
  match record.Job_store.job.Job.task_key with
  | Some key -> local_task_id_from_key key
  | None -> (
      match local_task_id_from_key record.Job_store.id with
      | Some id -> Some id
      | None ->
          let branch = Option.value ~default:"" record.Job_store.job.Job.branch in
          find_local_task_for_job tasks project_id branch record.Job_store.job.Job.title
          |> Option.map (fun task -> task.id))

let sync_job_to_local_task projects (tasks, result) record =
  let ( let* ) = Result.bind in
  let* project = project_for_repo projects record.Job_store.job.Job.repo in
  let branch = Option.value ~default:"" record.Job_store.job.Job.branch in
  let status = status_of_job record in
  let now = now_utc () in
  let task_id = task_id_for_job tasks project.id record in
  let tasks, result, task =
    match Option.bind task_id (find_local_task_by_id tasks) with
    | Some task ->
        let candidate = { task with project = project.id; status; branch = Some branch } in
        if candidate = task then (tasks, result, task)
        else
          let updated = { candidate with updated_at = Some now } in
          (replace_local_task tasks updated, { result with updated = result.updated + 1 }, updated)
    | None ->
        let task =
          {
            id = next_local_id tasks;
            project = project.id;
            title = record.Job_store.job.Job.title;
            status;
            priority = None;
            branch = Some branch;
            notes = None;
            created_at = Some now;
            updated_at = Some now;
          }
        in
        (tasks @ [ task ], { result with created = result.created + 1 }, task)
  in
  let desired_task_key = local_task_key task.id in
  match record.Job_store.job.Job.task_key with
  | Some task_key when String.equal task_key desired_task_key -> Ok (tasks, result)
  | _ ->
      let* () = Job_store.update_file record.Job_store.path [ Job_store.string "task_key" desired_task_key ] in
      Ok (tasks, { result with linked_jobs = result.linked_jobs + 1 })

let sync_jobs_to_local_tasks ~home =
  let ( let* ) = Result.bind in
  let* projects = load_projects ~home in
  let* tasks = load_local_tasks ~home in
  let* jobs = Job_store.load ~home ~scope:Job_store.All in
  let* tasks, result =
    fold_results jobs ~init:(tasks, empty_sync_result) ~f:(sync_job_to_local_task projects)
  in
  let* () = save_local_tasks ~home tasks in
  Ok result

let parse_github_issue ~(project : project) ~repo ~priorities json =
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
          priority = priority_for priorities key;
          branch = None;
          url;
        }
  | _ -> Error "GitHub issue JSON missing number or title"

let fetch_github_tasks ~project ~priorities { repo; query } =
  let query_arg =
    match query with
    | None -> ""
    | Some query -> " --search " ^ Shell.quote query
  in
  let command =
    Printf.sprintf
      "gh issue list --repo %s --limit 100 --json number,title,state,url,updatedAt%s"
      (Shell.quote repo) query_arg
  in
  match Process.run_success command with
  | Error msg -> Error ("failed to fetch GitHub issues for " ^ repo ^ ": " ^ msg)
  | Ok output -> (
      try
        match Yojson.Safe.from_string output with
        | `List issues ->
            fold_results issues ~init:[] ~f:(fun acc json ->
                parse_github_issue ~project ~repo ~priorities json
                |> Result.map (fun task -> task :: acc))
            |> Result.map List.rev
        | _ -> Error ("GitHub issue output for " ^ repo ^ " was not a JSON array")
      with Yojson.Json_error msg -> Error ("invalid GitHub issue JSON for " ^ repo ^ ": " ^ msg))

let fetch_project_tasks ~priorities (project : project) =
  let fetch_source = function
    | Github_issues source -> fetch_github_tasks ~project ~priorities source
  in
  fold_results project.sources ~init:[] ~f:(fun acc source ->
      fetch_source source |> Result.map (fun tasks -> acc @ tasks))

let load_tasks ~home ?project ?(all = false) () =
  let ( let* ) = Result.bind in
  let* projects = load_projects ~home in
  let* selected_projects =
    match project with
    | None -> Ok projects
    | Some project -> resolve_project projects project |> Result.map (fun project -> [ project ])
  in
  let* priorities = load_priorities ~home in
  let* external_tasks =
    fold_results selected_projects ~init:[] ~f:(fun acc project ->
        fetch_project_tasks ~priorities project |> Result.map (fun tasks -> acc @ tasks))
  in
  let* local_tasks = load_local_tasks ~home in
  let selected_ids = selected_projects |> List.map (fun (project : project) -> project.id) in
  let local_task_items =
    (local_tasks : local_task list)
    |> List.filter (fun (task : local_task) ->
           (project = None || List.exists (String.equal task.project) selected_ids)
           && (all || not (String.equal (String.lowercase_ascii task.status) "done")))
    |> List.map (task_of_local priorities)
  in
  apply_linked_job_display_ids ~home (external_tasks @ local_task_items)

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
  let priorities = List.map (fun task -> Option.value ~default:"" task.priority) tasks in
  let statuses = List.map (fun task -> task.status) tasks in
  let titles = List.map (fun task -> task.title) tasks in
  let branches = List.map (fun task -> Option.value ~default:"" task.branch) tasks in
  let id_width = width 2 ("ID" :: ids) in
  let project_width = width 7 ("PROJECT" :: projects) in
  let priority_width = width 8 ("PRIORITY" :: priorities) in
  let status_width = width 6 ("STATUS" :: statuses) in
  let title_width = width 5 ("TITLE" :: titles) in
  let render_row id project priority status title branch =
    String.concat " "
      [ pad_right id_width id;
        pad_right project_width project;
        pad_right priority_width priority;
        pad_right status_width status;
        pad_right title_width title;
        branch ]
  in
  let header = render_row "ID" "PROJECT" "PRIORITY" "STATUS" "TITLE" "BRANCH" in
  let lines =
    List.map2
      (fun task branch ->
        render_row task.display_id task.project
          (Option.value ~default:"" task.priority) task.status task.title branch)
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
