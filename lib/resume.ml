let find_record ~home ?(scope = Job_store.Active) needle =
  Job_store.find ~home ~scope needle

let find ~home ?(scope = Job_store.Active) needle =
  find_record ~home ~scope needle |> Result.map (fun record -> record.Job_store.job)

let find_resumable ~home needle =
  let ( let* ) = Result.bind in
  let* record = find_record ~home ~scope:Job_store.Active needle in
  match record.Job_store.transition with
  | Some transition when transition.operation = Job_store.Complete ->
      Error
        (Printf.sprintf
           "worker %s is completing; recover it with monty done %s --home %s"
           record.id (Shell.quote record.id) (Shell.quote home))
  | Some transition when transition.operation = Job_store.Reopen ->
      Error
        (Printf.sprintf
           "worker %s is reopening; recover it with monty resume --archived %s --home %s"
           record.id (Shell.quote record.id) (Shell.quote home))
  | Some _ -> Error (Printf.sprintf "worker %s has an unknown transition" record.id)
  | None
    when List.mem record.status
           [ "active"; "prepared"; "launch-failed"; "launch-requested" ] ->
      Ok record
  | None ->
      Error
        (Printf.sprintf "worker %s has status %S and cannot be resumed"
           record.id record.status)

let local_task_id = Job_store.local_task_id_of_key

let fault checkpoint =
  match Sys.getenv_opt "MONTY_FAULT_INJECT" with
  | Some value when String.equal value checkpoint ->
      Error (Printf.sprintf "fault injected at %s" checkpoint)
  | _ -> Ok ()

let find_reactivatable ~home needle =
  let ( let* ) = Result.bind in
  let* record = Job_store.find ~home ~scope:Job_store.All needle in
  match record.Job_store.transition with
  | Some transition when transition.operation = Job_store.Reopen -> Ok record
  | Some transition ->
      Error
        (Printf.sprintf "worker %s is in a %s transition, not a reopen transition"
           record.Job_store.id (Job_store.operation_name transition.operation))
  | None when Job_store.is_archived record && String.equal record.status "done" -> Ok record
  | None ->
      Error
        (Printf.sprintf "worker %s is not an archived done worker" record.Job_store.id)

let plan_reactivate ~home record =
  let ( let* ) = Result.bind in
  let* _task_id =
    Project_overview.validate_worker_task_link ~home record
  in
  match record.Job_store.transition with
  | Some transition when transition.operation = Job_store.Reopen ->
      Ok (Job_store.active_job record)
  | Some _ -> Error "worker is in a different lifecycle transition"
  | None ->
      let target = Job_store.active_dir record in
      if Sys.file_exists target then
        Error (Printf.sprintf "active worker directory already exists: %s" target)
      else Ok (Job_store.active_job record)

let reactivate ~home record =
  let ( let* ) = Result.bind in
  let* _linked_task_id =
    Project_overview.validate_worker_task_link ~home record
  in
  let* record = Job_store.prepare_reopen record in
  let* () = fault "reopen-after-intent" in
  let* transition =
    match record.Job_store.transition with
    | Some transition when transition.operation = Job_store.Reopen -> Ok transition
    | _ -> Error "reopen intent was not persisted"
  in
  let* record = Job_store.relocate_transition record Job_store.Reopen in
  let* () = fault "reopen-after-move" in
  let* record = Job_store.normalize_transition record Job_store.Reopen in
  let* () = fault "reopen-after-normalize" in
  let* _task_id = local_task_id transition.task_key in
  let* () =
    Project_overview.set_worker_task_status ~home record "open"
  in
  let* () = fault "reopen-after-task" in
  let* () = fault "reopen-before-finalize" in
  let* record = Job_store.finalize_transition record Job_store.Reopen in
  Ok record.Job_store.job
