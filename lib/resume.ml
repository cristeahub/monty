open Yojson.Safe

let member_string json name =
  match Util.member name json with
  | `String value when String.trim value <> "" -> Ok value
  | _ -> Error (Printf.sprintf "job.json missing string field %S" name)

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

let branch_leaf branch = Job.branch_leaf branch

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
    Ok (path, Job.make ~id ~branch ~worker_dir ~title ~repo ~context ())
  with
  | Sys_error msg -> Error msg
  | Yojson.Json_error msg -> Error ("invalid JSON in " ^ path ^ ": " ^ msg)

let matches needle job =
  let needle_slug = Slug.of_title needle in
  let id = Option.value ~default:"" job.Job.id in
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
         | Ok jobs, Ok job -> Ok (job :: jobs)
         | Ok _, Error msg -> Error msg)
       (Ok [])
  |> Result.map List.rev

let find ~home needle =
  match load_all ~home with
  | Error msg -> Error msg
  | Ok jobs -> (
      let matches = List.filter (fun (_, job) -> matches needle job) jobs in
      match matches with
      | [] ->
          Error
            (Printf.sprintf "no Monty worker matching %S under %s" needle
               (Filename.concat home ".monty/runs"))
      | [ (_, job) ] -> Ok job
      | many ->
          let labels =
            many
            |> List.map (fun (path, job) ->
                   Printf.sprintf "- %s (%s, %s)" job.Job.title
                     (Option.value ~default:"<no-branch>" job.Job.branch)
                     path)
            |> String.concat "\n"
          in
          Error (Printf.sprintf "multiple Monty workers match %S:\n%s" needle labels))
