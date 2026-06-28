let run_matches run record =
  match run with
  | None -> true
  | Some value ->
      let value = String.trim value in
      String.equal value ""
      || String.equal value record.Job_store.run_dir
      || String.equal value (Filename.basename record.Job_store.run_dir)

let status_label record = if Job_store.is_archived record then "DONE" else "ACTIVE"

let line record =
  Printf.sprintf "%-16s %-7s %-32s %-24s %s" record.Job_store.id
    (status_label record)
    record.Job_store.job.Job.title
    (Option.value ~default:"<no-branch>" record.Job_store.job.Job.branch)
    record.Job_store.worker_dir

let compare_records left right =
  match String.compare left.Job_store.run_dir right.Job_store.run_dir with
  | 0 -> String.compare left.Job_store.id right.Job_store.id
  | value -> value

let render records =
  let records = List.sort compare_records records in
  let header = Printf.sprintf "%-16s %-7s %-32s %-24s %s" "ID" "STATUS" "TITLE" "BRANCH" "DIR" in
  String.concat "\n" (header :: List.map line records) ^ "\n"

let run ~home ~scope ?run () =
  match Job_store.load ~home ~scope with
  | Error msg -> Error msg
  | Ok records ->
      let records = records |> List.filter (run_matches run) in
      Fmt.pr "%s" (render records);
      Ok ()
