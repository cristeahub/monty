open Yojson.Safe

let string_field obj name =
  match Util.member name obj with
  | `String value when String.trim value <> "" -> Ok value
  | `Null -> Error (Printf.sprintf "missing required string field %S" name)
  | _ -> Error (Printf.sprintf "field %S must be a non-empty string" name)

let optional_string_field obj name =
  match Util.member name obj with
  | `Null -> Ok None
  | `String value when String.trim value = "" -> Ok None
  | `String value -> Ok (Some value)
  | _ -> Error (Printf.sprintf "field %S must be a string when present" name)

let ( let* ) = Result.bind

let optional_worker_dir_field obj =
  match Util.member "worker_dir" obj with
  | `String _ as value -> optional_string_field (`Assoc [ ("worker_dir", value) ]) "worker_dir"
  | `Null -> optional_string_field obj "memory_dir"
  | _ -> optional_string_field obj "worker_dir"

let optional_task_key_field obj =
  match Util.member "task_key" obj with
  | `String _ as value -> optional_string_field (`Assoc [ ("task_key", value) ]) "task_key"
  | `Null -> optional_string_field obj "task"
  | _ -> optional_string_field obj "task_key"

let parse_workspace json =
  let* repo = string_field json "repo" in
  if Filename.is_relative repo then
    Error (Printf.sprintf "workspace repo must be an absolute path: %s" repo)
  else
    let* branch = optional_string_field json "branch" in
    Ok Job.{ repo = Shell.normalize repo; branch }

let parse_workspaces json =
  match Util.member "workspaces" json with
  | `Null -> Ok None
  | `List [] -> Error "field \"workspaces\" must not be empty"
  | `List values ->
      values
      |> List.fold_left
           (fun result json ->
             let* workspaces = result in
             let* workspace = parse_workspace json in
             Ok (workspace :: workspaces))
           (Ok [])
      |> Result.map (fun values -> Some (List.rev values))
  | _ -> Error "field \"workspaces\" must be an array when present"

let parse_job index json =
  let* title = string_field json "title" in
  let* context = string_field json "context" in
  let* id =
    match Util.member "id" json with
    | `Null -> Ok None
    | `String value ->
        State_path.safe_component ~label:"manifest worker id" value
        |> Result.map Option.some
    | _ -> Error "field \"id\" must be a string when present"
  in
  let* branch = optional_string_field json "branch" in
  let* workspaces = parse_workspaces json in
  let* worker_dir = optional_worker_dir_field json in
  let* prompt = optional_string_field json "prompt" in
  let* task_key = optional_task_key_field json in
  let* job =
    match workspaces with
    | None ->
        let* repo = string_field json "repo" in
        Ok
          (Job.make ?id ?branch ?worker_dir ?prompt ?task_key ~title ~repo
             ~context ())
    | Some workspaces ->
        let repo_present = Util.member "repo" json <> `Null in
        let branch_present = Util.member "branch" json <> `Null in
        if repo_present || branch_present then
          Error
            "manifest job must use either top-level repo/branch or workspaces, not both"
        else
          Ok
            (Job.make_with_workspaces ?id ?worker_dir ?prompt ?task_key ~title
               ~workspaces ~context ())
  in
  Ok (index, job)

let jobs_json json =
  match json with
  | `Assoc _ -> (
      match Util.member "jobs" json with
      | `List jobs -> Ok jobs
      | `Null -> Error "manifest must contain a \"jobs\" array"
      | _ -> Error "manifest field \"jobs\" must be an array")
  | `List jobs -> Ok jobs
  | _ -> Error "manifest must be an object with a \"jobs\" array or a jobs array"

let resolve_context ~cwd ~manifest_dir path =
  if Filename.is_relative path |> not then Shell.normalize path
  else
    let from_cwd = Filename.concat cwd path in
    if Sys.file_exists from_cwd then Shell.normalize from_cwd
    else Shell.normalize (Filename.concat manifest_dir path)

let resolve_repo ~cwd path =
  if Filename.is_relative path |> not then Shell.normalize path
  else Shell.normalize (Filename.concat cwd path)

let resolve_worker_dir ?home ~manifest_dir path =
  if Filename.is_relative path |> not then Shell.normalize path
  else if String.equal path ".monty" || String.starts_with ~prefix:".monty/" path then
    match home with
    | Some home -> Filename.concat home path |> Shell.normalize
    | None -> Filename.concat manifest_dir path |> Shell.normalize
  else Filename.concat manifest_dir path |> Shell.normalize

let default_worker_dir ~manifest_dir job =
  let branch = match job.Job.branch with Some branch -> branch | None -> job.Job.title in
  let id = Job.id_or_default ~branch job in
  Filename.concat (Filename.concat manifest_dir "workers") id |> Shell.normalize

let resolve_job_paths ?home ~cwd ~manifest_dir (index, job) =
  let workspaces =
    job.Job.workspaces
    |> List.map (fun (workspace : Job.workspace) ->
           { workspace with repo = resolve_repo ~cwd workspace.repo })
  in
  let context = resolve_context ~cwd ~manifest_dir job.Job.context in
  let worker_dir =
    match job.Job.worker_dir with
    | Some worker_dir -> Some (resolve_worker_dir ?home ~manifest_dir worker_dir)
    | None -> Some (default_worker_dir ~manifest_dir job)
  in
  ( index,
    Job.with_workspaces
      {
        job with
        Job.context;
        worker_dir;
      }
      workspaces )

let load ?home path =
  let cwd = Sys.getcwd () in
  let manifest_path = Shell.abs_path ~base:cwd path |> Shell.normalize in
  let manifest_dir = Filename.dirname manifest_path in
  try
    let json = Yojson.Safe.from_file manifest_path in
    let* jobs = jobs_json json in
    jobs |> List.mapi (fun i json -> parse_job (i + 1) json)
    |> List.fold_left
         (fun acc parsed ->
           match (acc, parsed) with
           | Error _ as err, _ -> err
           | Ok jobs, Ok job -> Ok (job :: jobs)
           | Ok _, Error msg -> Error msg)
         (Ok [])
    |> Result.map (fun jobs ->
           jobs |> List.rev
           |> List.map (resolve_job_paths ?home ~cwd ~manifest_dir))
  with
  | Sys_error msg -> Error msg
  | Yojson.Json_error msg -> Error ("invalid JSON manifest: " ^ msg)
