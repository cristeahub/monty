open Yojson.Safe
open Overview_types

let local_tasks_file home =
  Filename.concat (Project_storage.monty_dir home) "tasks.local.json"

let now_utc = Worker_memory.now_utc
let load_projects = Project_storage.load_projects
let resolve_project = Project_storage.resolve_project

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

let prefix text value =
  let length = String.length text in
  String.length value >= length && String.sub value 0 length = text

let strip_prefix text value =
  if prefix text value then
    Some (String.sub value (String.length text) (String.length value - String.length text))
  else None

let duplicate_value values =
  values
  |> List.sort String.compare
  |> List.find_opt (fun value ->
         List.length (List.filter (String.equal value) values) > 1)

let fold_results values ~init ~f =
  List.fold_left
    (fun acc value ->
      match acc with Error _ as err -> err | Ok acc -> f acc value)
    (Ok init) values

let parse_local_task json =
  let ( let* ) = Result.bind in
  let* id = member_string json "id" in
  let* project = member_string json "project" in
  let* title = member_string json "title" in
  let* status = optional_string json "status" in
  let* branch = optional_string json "branch" in
  let* notes = optional_string json "notes" in
  let* worker_id = optional_string json "worker_id" in
  let* worker_key = optional_string json "worker_key" in
  let* external_key = optional_string json "external_key" in
  let* external_url = optional_string json "external_url" in
  let* external_source = optional_string json "external_source" in
  let* created_at = optional_string json "created_at" in
  let* updated_at = optional_string json "updated_at" in
  Ok
    {
      id;
      project;
      title;
      status = Option.value ~default:"open" status;
      branch;
      notes;
      worker_id;
      worker_key;
      external_key;
      external_url;
      external_source;
      created_at;
      updated_at;
    }

let json_of_local_task task =
  let fields =
    [ ("id", `String task.id);
      ("project", `String task.project);
      ("title", `String task.title);
      ("status", `String task.status) ]
    @ (match task.branch with None -> [] | Some value -> [ ("branch", `String value) ])
    @ (match task.notes with None -> [] | Some value -> [ ("notes", `String value) ])
    @ (match task.worker_id with None -> [] | Some value -> [ ("worker_id", `String value) ])
    @ (match task.worker_key with None -> [] | Some value -> [ ("worker_key", `String value) ])
    @ (match task.external_key with None -> [] | Some value -> [ ("external_key", `String value) ])
    @ (match task.external_url with None -> [] | Some value -> [ ("external_url", `String value) ])
    @ (match task.external_source with None -> [] | Some value -> [ ("external_source", `String value) ])
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
    let* tasks =
      fold_results tasks_json ~init:[] ~f:(fun acc json ->
          parse_local_task json |> Result.map (fun task -> task :: acc))
      |> Result.map List.rev
    in
    let unique_optional label values =
      match duplicate_value (List.filter_map Fun.id values) with
      | Some value ->
          Error (Printf.sprintf "duplicate %s %S in %s" label value path)
      | None -> Ok ()
    in
    let* () =
      match duplicate_value (List.map (fun (task : local_task) -> task.id) tasks) with
      | Some id -> Error (Printf.sprintf "duplicate local task id %S in %s" id path)
      | None -> Ok ()
    in
    let* () = unique_optional "external task key" (List.map (fun (task : local_task) -> task.external_key) tasks) in
    let* () = unique_optional "worker key" (List.map (fun (task : local_task) -> task.worker_key) tasks) in
    Ok tasks

let save_local_tasks_unlocked ~home tasks =
  let path = local_tasks_file home in
  State_store.write_json_atomic ~path
    (`Assoc [ ("tasks", `List (List.map json_of_local_task tasks)) ])

let save_local_tasks ~home tasks =
  State_store.with_lock ~home (fun () -> save_local_tasks_unlocked ~home tasks)

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

let task_of_local (task : local_task) =
  let key = "local:" ^ task.id in
  {
    key;
    display_id = Option.value ~default:key task.external_key;
    project = task.project;
    origin = Option.value ~default:"local" task.external_source;
    title = task.title;
    status = task.status;
    branch = task.branch;
    url = task.external_url;
  }

let add_local_task ~home ~project ~title () =
  State_store.with_lock ~home (fun () ->
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
          branch = None;
          notes = None;
          worker_id = None;
          worker_key = None;
          external_key = None;
          external_url = None;
          external_source = None;
          created_at = Some now;
          updated_at = Some now;
        }
      in
      let* () = save_local_tasks_unlocked ~home (tasks @ [ task ]) in
      Ok task)

let set_local_task_status ~home id status =
  State_store.with_lock ~home (fun () ->
      let ( let* ) = Result.bind in
      let* tasks = load_local_tasks ~home in
      let id = match strip_prefix "local:" id with Some value -> value | None -> id in
      match List.find_opt (fun task -> String.equal task.id id) tasks with
      | None -> Error (Printf.sprintf "no local Monty task matching %S" id)
      | Some task when String.equal (String.lowercase_ascii task.status) status -> Ok ()
      | Some _ ->
          let now = now_utc () in
          let tasks =
            tasks
            |> List.map (fun task ->
                   if String.equal task.id id then
                     { task with status; updated_at = Some now }
                   else task)
          in
          let* () = save_local_tasks_unlocked ~home tasks in
          Ok ())

let done_local_task ~home id = set_local_task_status ~home id "done"
let reopen_local_task ~home id = set_local_task_status ~home id "open"
