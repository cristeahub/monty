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

let parse_job index json =
  let* title = string_field json "title" in
  let* repo = string_field json "repo" in
  let* context = string_field json "context" in
  let* id = optional_string_field json "id" in
  let* branch = optional_string_field json "branch" in
  let* worker_dir = optional_worker_dir_field json in
  let* prompt = optional_string_field json "prompt" in
  Ok (index, Job.make ?id ?branch ?worker_dir ?prompt ~title ~repo ~context ())

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

let resolve_worker_dir ~cwd ~manifest_dir path =
  if Filename.is_relative path |> not then Shell.normalize path
  else
    let from_cwd = Filename.concat cwd path in
    if Sys.file_exists from_cwd then Shell.normalize from_cwd
    else Shell.normalize (Filename.concat manifest_dir path)

let default_worker_dir ~manifest_dir job =
  let branch = match job.Job.branch with Some branch -> branch | None -> job.Job.title in
  let id = Job.id_or_default ~branch job in
  Filename.concat (Filename.concat manifest_dir "workers") id |> Shell.normalize

let resolve_job_paths ~cwd ~manifest_dir (index, job) =
  let repo = resolve_repo ~cwd job.Job.repo in
  let context = resolve_context ~cwd ~manifest_dir job.Job.context in
  let worker_dir =
    match job.Job.worker_dir with
    | Some worker_dir -> Some (resolve_worker_dir ~cwd ~manifest_dir worker_dir)
    | None -> Some (default_worker_dir ~manifest_dir job)
  in
  ( index,
    {
      Job.id = job.Job.id;
      title = job.Job.title;
      repo;
      branch = job.Job.branch;
      context;
      worker_dir;
      prompt = job.Job.prompt;
    } )

let load path =
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
           |> List.map (resolve_job_paths ~cwd ~manifest_dir))
  with
  | Sys_error msg -> Error msg
  | Yojson.Json_error msg -> Error ("invalid JSON manifest: " ^ msg)
