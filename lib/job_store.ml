open Yojson.Safe

type scope = Active | Archived | All

type record = {
  path : string;
  id : string;
  job : Job.t;
  status : string;
  worker_dir : string;
  run_dir : string;
  worktree_mode : string;
  last_known_worktree : string option;
  updated_at : string option;
  completed_at : string option;
  archived_at : string option;
}

let member_string json name =
  match Util.member name json with
  | `String value when String.trim value <> "" -> Ok value
  | _ -> Error (Printf.sprintf "job.json missing string field %S" name)

let optional_string json name =
  match Util.member name json with
  | `Null -> Ok None
  | `String value when String.trim value = "" -> Ok None
  | `String value -> Ok (Some value)
  | _ -> Error (Printf.sprintf "job.json field %S must be a string when present" name)

let path_has_component component path =
  path |> String.split_on_char '/'
  |> List.exists (fun part -> String.equal part component)

let default_status path = if path_has_component "archive" path then "done" else "active"

let default_run_dir worker_dir =
  worker_dir |> Filename.dirname |> Filename.dirname |> Shell.normalize

let collect_job_files root =
  let rec loop acc path =
    if Sys.file_exists path && Sys.is_directory path then
      Sys.readdir path |> Array.fold_left
        (fun acc name ->
          let child = Filename.concat path name in
          if Sys.is_directory child then loop acc child
          else if name = "job.json" then child :: acc
          else acc)
        acc
    else acc
  in
  loop [] root

let parse_job_file path =
  try
    let json = Yojson.Safe.from_file path in
    let ( let* ) = Result.bind in
    let* id = member_string json "id" in
    let* title = member_string json "title" in
    let* repo = member_string json "repo" in
    let* branch = member_string json "branch" in
    let* context = member_string json "context" in
    let* worker_dir = member_string json "worker_dir" in
    let* prompt = optional_string json "prompt" in
    let* status = optional_string json "status" in
    let* run_dir = optional_string json "run_dir" in
    let* worktree_mode = optional_string json "worktree_mode" in
    let* last_known_worktree = optional_string json "last_known_worktree" in
    let* updated_at = optional_string json "updated_at" in
    let* completed_at = optional_string json "completed_at" in
    let* archived_at = optional_string json "archived_at" in
    let worker_dir = Shell.normalize worker_dir in
    let run_dir = run_dir |> Option.value ~default:(default_run_dir worker_dir) |> Shell.normalize in
    let status = status |> Option.value ~default:(default_status path) in
    let worktree_mode = worktree_mode |> Option.value ~default:"always" in
    let job = Job.make ~id ~branch ~worker_dir ?prompt ~title ~repo ~context () in
    Ok
      {
        path = Shell.normalize path;
        id;
        job;
        status;
        worker_dir;
        run_dir;
        worktree_mode;
        last_known_worktree;
        updated_at;
        completed_at;
        archived_at;
      }
  with
  | Sys_error msg -> Error msg
  | Yojson.Json_error msg -> Error ("invalid JSON in " ^ path ^ ": " ^ msg)

let is_archived record =
  let status = String.lowercase_ascii record.status in
  String.equal status "done"
  || String.equal status "archived"
  || String.equal status "completed"
  || path_has_component "archive" record.path

let scope_label = function Active -> "active" | Archived -> "archived" | All -> "all"

let in_scope scope record =
  match scope with
  | Active -> not (is_archived record)
  | Archived -> is_archived record
  | All -> true

let branch_leaf branch = Job.branch_leaf branch

let matches needle record =
  let job = record.job in
  let needle_slug = Slug.of_title needle in
  let id = Option.value ~default:record.id job.Job.id in
  let branch = Option.value ~default:"" job.Job.branch in
  let title_slug = Slug.of_title job.Job.title in
  String.equal needle id
  || String.equal needle branch
  || String.equal needle (branch_leaf branch)
  || String.equal needle_slug id
  || String.equal needle_slug (branch_leaf branch)
  || String.equal needle_slug title_slug

let load_all ~home =
  let root = Filename.concat home ".monty/runs" in
  collect_job_files root
  |> List.fold_left
       (fun acc path ->
         match (acc, parse_job_file path) with
         | Error _ as err, _ -> err
         | Ok records, Ok record -> Ok (record :: records)
         | Ok _, Error msg -> Error msg)
       (Ok [])
  |> Result.map List.rev

let load ~home ~scope =
  load_all ~home |> Result.map (List.filter (in_scope scope))

let find ~home ?(scope = Active) needle =
  match load ~home ~scope with
  | Error msg -> Error msg
  | Ok records -> (
      let matches = List.filter (matches needle) records in
      match matches with
      | [] ->
          Error
            (Printf.sprintf "no %s Monty worker matching %S under %s"
               (scope_label scope) needle
               (Filename.concat home ".monty/runs"))
      | [ record ] -> Ok record
      | many ->
          let labels =
            many
            |> List.map (fun record ->
                   Printf.sprintf "- %s (%s, %s, %s)" record.job.Job.title
                     (Option.value ~default:"<no-branch>" record.job.Job.branch)
                     record.status record.path)
            |> String.concat "\n"
          in
          Error (Printf.sprintf "multiple Monty workers match %S:\n%s" needle labels))

let archive_dir record =
  Filename.concat record.run_dir (Filename.concat "archive" record.id)
  |> Shell.normalize

let active_dir record =
  Filename.concat record.run_dir (Filename.concat "workers" record.id)
  |> Shell.normalize

let active_job record =
  { record.job with Job.worker_dir = Some (active_dir record) }

let assoc_without names fields =
  List.filter (fun (name, _) -> not (List.exists (String.equal name) names)) fields

let upsert_assoc ~remove updates = function
  | `Assoc fields ->
      let names = remove @ List.map fst updates in
      `Assoc (updates @ assoc_without names fields)
  | _ -> `Assoc updates

let update_file ?(remove = []) path updates =
  try
    let json = Yojson.Safe.from_file path in
    Yojson.Safe.to_file path (upsert_assoc ~remove updates json);
    Ok ()
  with
  | Sys_error msg -> Error msg
  | Yojson.Json_error msg -> Error ("invalid JSON in " ^ path ^ ": " ^ msg)

let string name value = (name, `String value)

let maybe_string name = function None -> [] | Some value -> [ string name value ]

let reactivate record =
  if not (is_archived record) then Ok record.job
  else
    let source_dir = record.worker_dir in
    let target_dir = active_dir record in
    if not (Sys.file_exists source_dir && Sys.is_directory source_dir) then
      Error (Printf.sprintf "archived worker directory is missing: %s" source_dir)
    else if Sys.file_exists target_dir then
      Error (Printf.sprintf "active worker directory already exists: %s" target_dir)
    else
      let now = Worker_memory.now_utc () in
      let target_job = Filename.concat target_dir "job.json" in
      try
        Shell.ensure_dir (Filename.dirname target_dir);
        Unix.rename source_dir target_dir;
        let updates =
          [ string "id" record.id;
            string "title" record.job.Job.title;
            string "repo" record.job.Job.repo;
            string "branch" (Option.value ~default:"" record.job.Job.branch);
            string "context" record.job.Job.context;
            string "worker_dir" target_dir;
            string "run_dir" record.run_dir;
            string "status" "active";
            string "worktree_mode" record.worktree_mode;
            string "updated_at" now;
            string "reopened_at" now ]
          @ maybe_string "last_known_worktree" record.last_known_worktree
        in
        let remove =
          [ "completed_at"; "archived_at"; "deleted_worktree"; "deleted_branch" ]
        in
        match update_file ~remove target_job updates with
        | Error msg -> Error msg
        | Ok () -> (
            match parse_job_file target_job with
            | Error msg -> Error msg
            | Ok record -> Ok record.job)
      with Unix.Unix_error (err, fn, arg) ->
        Error
          (Printf.sprintf "failed to reactivate %s via %s(%s): %s" record.id fn arg
             (Unix.error_message err))
