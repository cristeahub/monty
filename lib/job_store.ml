open Yojson.Safe

type scope = Active | Archived | All

type transition_operation = Complete | Reopen

type transition = {
  operation : transition_operation;
  source : string;
  target : string;
  task_key : string option;
  force : bool;
  started_at : string;
}

type record = {
  path : string;
  id : string;
  job : Job.t;
  status : string;
  worker_dir : string;
  run_dir : string;
  worktree_mode : string;
  last_known_worktree : string option;
  launch_script : string option;
  updated_at : string option;
  completed_at : string option;
  archived_at : string option;
  transition : transition option;
  state_path : State_path.t option;
  home : string option;
}

let ( let* ) = Result.bind

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

let transition_operation_of_string = function
  | "complete" -> Ok Complete
  | "reopen" -> Ok Reopen
  | value -> Error (Printf.sprintf "unknown job transition operation %S" value)

let parse_transition json =
  match Util.member "transition" json with
  | `Null -> Ok None
  | `Assoc _ as value ->
      let* operation = member_string value "operation" in
      let* operation = transition_operation_of_string operation in
      let* source = member_string value "source" in
      let* target = member_string value "target" in
      let* task_key = optional_string value "task_key" in
      let* started_at = member_string value "started_at" in
      let* force =
        match Util.member "force" value with
        | `Bool value -> Ok value
        | `Null when operation = Reopen -> Ok false
        | _ -> Error "job transition field \"force\" must be a boolean"
      in
      Ok (Some { operation; source; target; task_key; force; started_at })
  | _ -> Error "job.json field \"transition\" must be an object when present"

let operation_name = function Complete -> "complete" | Reopen -> "reopen"

let prefix text value =
  let text_length = String.length text in
  String.length value >= text_length
  && String.sub value 0 text_length = text

let all_digits value =
  String.length value > 0
  && String.for_all (function '0' .. '9' -> true | _ -> false) value

let local_task_id_of_key = function
  | None -> Ok None
  | Some key ->
      let id =
        if prefix "local:" key then
          Some (String.sub key 6 (String.length key - 6))
        else if prefix "local-" key then Some key
        else None
      in
      (match id with
      | None -> Ok None
      | Some id ->
          let suffix =
            if prefix "local-" id then
              String.sub id 6 (String.length id - 6)
            else ""
          in
          if all_digits suffix then Ok (Some id)
          else
            Error
              (Printf.sprintf
                 "invalid local task key %S; expected local:local-NNN"
                 key))

let path_has_component component path =
  path |> String.split_on_char '/'
  |> List.exists (fun part -> String.equal part component)

let default_status path = if path_has_component "archive" path then "done" else "active"

let default_run_dir worker_dir =
  worker_dir |> Filename.dirname |> Filename.dirname |> Shell.normalize

let inspect_kind path =
  try Ok (Some (Unix.lstat path).Unix.st_kind) with
  | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> Ok None
  | Unix.Unix_error (err, fn, arg) ->
      Error
        (Printf.sprintf "failed to inspect Monty state via %s(%s): %s" fn arg
           (Unix.error_message err))

let directory_entries path =
  try Ok (Sys.readdir path |> Array.to_list |> List.sort String.compare) with
  | Sys_error msg -> Error (Printf.sprintf "failed to read Monty state directory %s: %s" path msg)

let unsafe_symlink path =
  Error
    (Printf.sprintf
       "unsafe symlink in Monty job state: %s; job discovery will not traverse it, so replace or remove the symlink before retrying"
       path)

let collect_job_files root =
  let collect_workers acc container =
    let* kind = inspect_kind container in
    match kind with
    | None -> Ok acc
    | Some Unix.S_LNK -> unsafe_symlink container
    | Some Unix.S_DIR ->
        let* names = directory_entries container in
        List.fold_left
          (fun result name ->
            let* acc = result in
            let worker_dir = Filename.concat container name in
            let* kind = inspect_kind worker_dir in
            match kind with
            | Some Unix.S_LNK -> unsafe_symlink worker_dir
            | Some Unix.S_DIR ->
                let job_file = Filename.concat worker_dir "job.json" in
                let* job_kind = inspect_kind job_file in
                (match job_kind with
                | Some Unix.S_LNK -> unsafe_symlink job_file
                | Some Unix.S_REG -> Ok (job_file :: acc)
                | _ -> Ok acc)
            | _ -> Ok acc)
          (Ok acc) names
    | Some _ ->
        Error (Printf.sprintf "Monty worker-state container is not a directory: %s" container)
  in
  let collect_run acc run_dir =
    let* acc = collect_workers acc (Filename.concat run_dir "workers") in
    collect_workers acc (Filename.concat run_dir "archive")
  in
  let* kind = inspect_kind root in
  match kind with
  | None -> Ok []
  | Some Unix.S_LNK -> unsafe_symlink root
  | Some Unix.S_DIR ->
      let* runs = directory_entries root in
      List.fold_left
        (fun result name ->
          let* acc = result in
          let run_dir = Filename.concat root name in
          let* kind = inspect_kind run_dir in
          match kind with
          | Some Unix.S_LNK -> unsafe_symlink run_dir
          | Some Unix.S_DIR -> collect_run acc run_dir
          | _ -> Ok acc)
        (Ok []) runs
      |> Result.map List.rev
  | Some _ -> Error (Printf.sprintf "Monty runs path is not a directory: %s" root)

let parse_job_file ?home path =
  try
    let json = Yojson.Safe.from_file path in
    let* id = member_string json "id" in
    let* id = State_path.safe_component ~label:"persisted worker id" id in
    let* title = member_string json "title" in
    let* repo = member_string json "repo" in
    let* branch = optional_string json "branch" in
    let* context = member_string json "context" in
    let* persisted_worker_dir = optional_string json "worker_dir" in
    let* prompt = optional_string json "prompt" in
    let* task_key = optional_string json "task_key" in
    let* persisted_status = optional_string json "status" in
    let* persisted_run_dir = optional_string json "run_dir" in
    let* worktree_mode = optional_string json "worktree_mode" in
    let* last_known_worktree = optional_string json "last_known_worktree" in
    let* launch_script = optional_string json "launch_script" in
    let* updated_at = optional_string json "updated_at" in
    let* completed_at = optional_string json "completed_at" in
    let* archived_at = optional_string json "archived_at" in
    let* transition = parse_transition json in
    let* () =
      match transition with
      | Some transition when transition.task_key <> task_key ->
          Error
            (Printf.sprintf
               "job transition task_key does not match top-level task_key in %s"
               path)
      | _ -> Ok ()
    in
    let* state_path =
      match home with
      | None -> Ok None
      | Some home -> State_path.of_job_file ~home path |> Result.map Option.some
    in
    let* () =
      match state_path with
      | Some state when not (String.equal state.State_path.id id) ->
          Error
            (Printf.sprintf
               "unsafe legacy job metadata: persisted id %S does not match physical path id %S in %s; repair the record explicitly"
               id state.id path)
      | _ -> Ok ()
    in
    let worker_dir =
      match (state_path, persisted_worker_dir) with
      | Some state, _ -> state.State_path.worker_dir
      | None, Some value -> Shell.normalize value
      | None, None -> Filename.dirname path |> Shell.normalize
    in
    let run_dir =
      match (state_path, persisted_run_dir) with
      | Some state, _ -> state.State_path.run_dir
      | None, Some value -> Shell.normalize value
      | None, None -> default_run_dir worker_dir
    in
    let* () =
      match state_path with
      | None -> Ok ()
      | Some state ->
          let supplied_home = Option.value ~default:state.State_path.home home in
          let* active =
            State_path.active ~home:supplied_home ~run_id:state.run_id ~id:state.id
          in
          let* archived =
            State_path.archived ~home:supplied_home ~run_id:state.run_id ~id:state.id
          in
          let* () =
            match transition with
            | None ->
                State_path.validate_persisted_path ~home:supplied_home
                  ~label:"worker_dir" ~expected:state.worker_dir persisted_worker_dir
            | Some transition ->
                let expected_source, expected_target, expected_status =
                  match transition.operation with
                  | Complete -> (active.worker_dir, archived.worker_dir, "completing")
                  | Reopen -> (archived.worker_dir, active.worker_dir, "reopening")
                in
                let* source = State_path.path_under_resolved_home ~home:supplied_home transition.source in
                let* target = State_path.path_under_resolved_home ~home:supplied_home transition.target in
                if not (String.equal source expected_source && String.equal target expected_target)
                then
                  Error
                    (Printf.sprintf
                       "unsafe %s transition paths in %s; expected source %s and target %s"
                       (operation_name transition.operation) path expected_source expected_target)
                else if persisted_status <> Some expected_status then
                  Error
                    (Printf.sprintf
                       "job transition %s requires status %S in %s"
                       (operation_name transition.operation) expected_status path)
                else
                  (match persisted_worker_dir with
                  | None -> Ok ()
                  | Some value ->
                      let* value = State_path.path_under_resolved_home ~home:supplied_home value in
                      if String.equal value expected_source || String.equal value expected_target
                      then Ok ()
                      else
                        Error
                          (Printf.sprintf
                             "unsafe transition worker_dir %s does not match canonical source or target in %s"
                             value path))
          in
          State_path.validate_persisted_path ~home:supplied_home ~label:"run_dir"
            ~expected:state.run_dir persisted_run_dir
    in
    let status =
      match (persisted_status, state_path) with
      | Some value, _ -> value
      | None, Some { State_path.location = Archived; _ } -> "done"
      | None, Some { State_path.location = Active; _ } -> "active"
      | None, None -> default_status path
    in
    let worktree_mode = Option.value ~default:"always" worktree_mode in
    let job = Job.make ~id ?branch ~worker_dir ?prompt ?task_key ~title ~repo ~context () in
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
        launch_script;
        updated_at;
        completed_at;
        archived_at;
        transition;
        state_path;
        home = Option.map (fun value -> Shell.normalize (Shell.abs_path value)) home;
      }
  with
  | Sys_error msg -> Error msg
  | Yojson.Json_error msg -> Error ("invalid JSON in " ^ path ^ ": " ^ msg)

let is_archived record =
  match record.state_path with
  | Some { State_path.location = Archived; _ } -> true
  | Some { State_path.location = Active; _ } -> false
  | None ->
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
  let* canonical_home = State_path.canonicalize home in
  let root = Filename.concat canonical_home ".monty/runs" in
  let* paths = collect_job_files root in
  paths
  |> List.fold_left
       (fun acc path ->
         match (acc, parse_job_file ~home path) with
         | Error _ as err, _ -> err
         | Ok records, Ok record -> Ok (record :: records)
         | Ok _, Error msg -> Error msg)
       (Ok [])
  |> Result.map List.rev

type scan_result = {
  records : record list;
  warnings : string list;
}

let symlink_warning path =
  Printf.sprintf
    "unsafe symlink in Monty job state: %s; job discovery skipped it, so replace or remove the symlink before retrying"
    path

let tolerant_job_paths root =
  let inspect path =
    match inspect_kind path with Ok kind -> (kind, []) | Error warning -> (None, [ warning ])
  in
  let entries path =
    match directory_entries path with Ok names -> (names, []) | Error warning -> ([], [ warning ])
  in
  let collect_container container =
    let kind, warnings = inspect container in
    match kind with
    | None -> ([], warnings)
    | Some Unix.S_LNK -> ([], symlink_warning container :: warnings)
    | Some Unix.S_DIR ->
        let names, entry_warnings = entries container in
        List.fold_left
          (fun (paths, warnings) name ->
            let worker_dir = Filename.concat container name in
            let kind, kind_warnings = inspect worker_dir in
            match kind with
            | Some Unix.S_LNK ->
                (paths, symlink_warning worker_dir :: kind_warnings @ warnings)
            | Some Unix.S_DIR ->
                let job_file = Filename.concat worker_dir "job.json" in
                let job_kind, job_warnings = inspect job_file in
                (match job_kind with
                | Some Unix.S_LNK ->
                    (paths, symlink_warning job_file :: job_warnings @ kind_warnings @ warnings)
                | Some Unix.S_REG ->
                    (job_file :: paths, job_warnings @ kind_warnings @ warnings)
                | _ -> (paths, job_warnings @ kind_warnings @ warnings))
            | _ -> (paths, kind_warnings @ warnings))
          ([], entry_warnings @ warnings) names
    | Some _ ->
        ([], Printf.sprintf "Monty worker-state container is not a directory: %s" container :: warnings)
  in
  let root_kind, root_warnings = inspect root in
  match root_kind with
  | None -> ([], root_warnings)
  | Some Unix.S_LNK -> ([], symlink_warning root :: root_warnings)
  | Some Unix.S_DIR ->
      let runs, run_warnings = entries root in
      List.fold_left
        (fun (paths, warnings) name ->
          let run_dir = Filename.concat root name in
          let kind, kind_warnings = inspect run_dir in
          match kind with
          | Some Unix.S_LNK ->
              (paths, symlink_warning run_dir :: kind_warnings @ warnings)
          | Some Unix.S_DIR ->
              let active, active_warnings =
                collect_container (Filename.concat run_dir "workers")
              in
              let archived, archive_warnings =
                collect_container (Filename.concat run_dir "archive")
              in
              (active @ archived @ paths,
               active_warnings @ archive_warnings @ kind_warnings @ warnings)
          | _ -> (paths, kind_warnings @ warnings))
        ([], run_warnings @ root_warnings) runs
  | Some _ ->
      ([], Printf.sprintf "Monty runs path is not a directory: %s" root :: root_warnings)

let scan ~home =
  let* canonical_home = State_path.canonicalize home in
  let root = Filename.concat canonical_home ".monty/runs" in
  let paths, structural_warnings = tolerant_job_paths root in
  let records, parse_warnings =
    paths |> List.sort String.compare
    |> List.fold_left
         (fun (records, warnings) path ->
           match parse_job_file ~home path with
           | Ok record -> (record :: records, warnings)
           | Error warning -> (records, warning :: warnings))
         ([], [])
  in
  Ok
    {
      records = List.rev records;
      warnings = List.sort_uniq String.compare (structural_warnings @ parse_warnings);
    }

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

let update_file_unlocked ?(remove = []) path updates =
  let* json = State_store.read_json ~path in
  match json with
  | None -> Error (Printf.sprintf "job.json is missing: %s" path)
  | Some json ->
      State_store.write_json_atomic ~path (upsert_assoc ~remove updates json)

let update_file ?home ?(remove = []) path updates =
  match home with
  | Some home ->
      State_store.with_lock ~home (fun () ->
          update_file_unlocked ~remove path updates)
  | None -> update_file_unlocked ~remove path updates

let string name value = (name, `String value)

let maybe_string name = function None -> [] | Some value -> [ string name value ]

let transition_json transition =
  `Assoc
    ([ string "operation" (operation_name transition.operation);
       string "source" transition.source;
       string "target" transition.target;
       ("force", `Bool transition.force);
       string "started_at" transition.started_at ]
    @ maybe_string "task_key" transition.task_key)

let pair_for_record ~home record =
  match record.state_path with
  | None -> Error "cannot transition a job record loaded without canonical state metadata"
  | Some state ->
      let* active = State_path.active ~home ~run_id:state.run_id ~id:state.id in
      let* archived = State_path.archived ~home ~run_id:state.run_id ~id:state.id in
      Ok (active, archived)

let same_operation expected transition = transition.operation = expected

let load_identity_unlocked ~home ~run_id ~id ~operation =
  let* active = State_path.active ~home ~run_id ~id in
  let* archived = State_path.archived ~home ~run_id ~id in
  let active_exists = State_path.path_exists active.job_file in
  let archived_exists = State_path.path_exists archived.job_file in
  match (active_exists, archived_exists) with
  | true, true ->
      Error
        (Printf.sprintf
           "worker %s exists at both canonical active and archive paths; repair the collision before retrying"
           id)
  | false, false -> Error (Printf.sprintf "worker state is missing for %s" id)
  | true, false ->
      let* record = parse_job_file ~home active.job_file in
      (match record.transition with
      | Some transition when same_operation operation transition -> Ok record
      | _ -> Error (Printf.sprintf "worker %s is not in the matching transition" id))
  | false, true ->
      let* record = parse_job_file ~home archived.job_file in
      (match record.transition with
      | Some transition when same_operation operation transition -> Ok record
      | _ -> Error (Printf.sprintf "worker %s is not in the matching transition" id))

let prepare_transition record ~operation ~task_key ~force =
  match record.home with
  | None -> Error "cannot transition a job record loaded without its Monty home"
  | Some home ->
      let* active, archived = pair_for_record ~home record in
      let source, target, allowed_initial_statuses, transition_status =
        match operation with
        | Complete ->
            ( active,
              archived,
              [ "active"; "prepared"; "launch-failed"; "launch-requested" ],
              "completing" )
        | Reopen -> (archived, active, [ "done" ], "reopening")
      in
      State_store.with_lock ~home (fun () ->
          if State_path.path_exists target.worker_dir then
            Error
              (Printf.sprintf "%s directory already exists: %s"
                 (if operation = Complete then "archive" else "active worker")
                 target.worker_dir)
          else
            let* current = parse_job_file ~home source.job_file in
            if not (List.mem current.status allowed_initial_statuses) then
              Error
                (Printf.sprintf "worker %s has status %S, expected one of %s"
                   current.id current.status
                   (String.concat ", " allowed_initial_statuses))
            else if current.job.Job.task_key <> record.job.Job.task_key then
              Error
                (Printf.sprintf "worker %s task link changed during transition preflight" current.id)
            else
              let started_at = Worker_memory.now_utc () in
              let transition =
                {
                  operation;
                  source = source.worker_dir;
                  target = target.worker_dir;
                  task_key;
                  force;
                  started_at;
                }
              in
              let updates =
                [ string "status" transition_status;
                  string "worker_dir" source.worker_dir;
                  string "run_dir" source.run_dir;
                  string "updated_at" started_at;
                  ("transition", transition_json transition) ]
                @ maybe_string "task_key" task_key
              in
              let* () = update_file_unlocked source.job_file updates in
              parse_job_file ~home source.job_file)

let prepare_completion record ~task_key ~force =
  match record.transition with
  | Some transition when transition.operation = Complete -> Ok record
  | Some transition ->
      Error
        (Printf.sprintf "worker %s is already in a %s transition" record.id
           (operation_name transition.operation))
  | None -> prepare_transition record ~operation:Complete ~task_key ~force

let prepare_reopen record =
  match record.transition with
  | Some transition when transition.operation = Reopen -> Ok record
  | Some transition ->
      Error
        (Printf.sprintf "worker %s is already in a %s transition" record.id
           (operation_name transition.operation))
  | None ->
      prepare_transition record ~operation:Reopen ~task_key:record.job.Job.task_key
        ~force:false

let reload_transition record operation =
  match (record.home, record.state_path) with
  | Some home, Some state ->
      State_store.with_lock ~home (fun () ->
          load_identity_unlocked ~home ~run_id:state.run_id ~id:state.id ~operation)
  | _ -> Error "cannot reload a transition without canonical home metadata"

let relocate_transition record operation =
  match (record.home, record.state_path) with
  | Some home, Some state ->
      State_store.with_lock ~home (fun () ->
          let* current =
            load_identity_unlocked ~home ~run_id:state.run_id ~id:state.id ~operation
          in
          let* transition =
            match current.transition with
            | Some transition -> Ok transition
            | None -> Error "matching transition metadata disappeared"
          in
          if String.equal current.worker_dir transition.target then Ok current
          else if not (String.equal current.worker_dir transition.source) then
            Error "transition record is outside its canonical source and target"
          else if State_path.path_exists transition.target then
            Error (Printf.sprintf "transition destination already exists: %s" transition.target)
          else
            try
              let target_parent = Filename.dirname transition.target in
              let target_parent_existed = State_path.path_exists target_parent in
              Shell.ensure_dir target_parent;
              if not target_parent_existed then
                State_store.fsync_directory (Filename.dirname target_parent);
              Unix.rename transition.source transition.target;
              State_store.fsync_directory (Filename.dirname transition.source);
              State_store.fsync_directory target_parent;
              parse_job_file ~home (Filename.concat transition.target "job.json")
            with Unix.Unix_error (err, fn, arg) ->
              Error
                (Printf.sprintf "failed to relocate %s via %s(%s): %s" current.id fn
                   arg (Unix.error_message err)))
  | _ -> Error "cannot relocate a transition without canonical home metadata"

let normalize_transition record operation =
  match (record.home, record.state_path) with
  | Some home, Some state ->
      State_store.with_lock ~home (fun () ->
          let* current =
            load_identity_unlocked ~home ~run_id:state.run_id ~id:state.id ~operation
          in
          let* transition =
            match current.transition with
            | Some transition -> Ok transition
            | None -> Error "matching transition metadata disappeared"
          in
          if not (String.equal current.worker_dir transition.target) then
            Error (Printf.sprintf "worker %s has not reached its transition target" current.id)
          else
            let updates =
              [ string "worker_dir" transition.target;
                string "run_dir" current.run_dir;
                string "updated_at" (Worker_memory.now_utc ()) ]
            in
            let* () = update_file_unlocked current.path updates in
            parse_job_file ~home current.path)
  | _ -> Error "cannot normalize a transition without canonical home metadata"

let finalize_transition record operation =
  match (record.home, record.state_path) with
  | Some home, Some state ->
      State_store.with_lock ~home (fun () ->
          let* current =
            load_identity_unlocked ~home ~run_id:state.run_id ~id:state.id ~operation
          in
          let now = Worker_memory.now_utc () in
          let status, extra, remove =
            match operation with
            | Complete ->
                ( "done",
                  [ string "completed_at" now; string "archived_at" now ],
                  [ "transition" ] )
            | Reopen ->
                ( "active",
                  [ string "reopened_at" now ],
                  [ "transition"; "completed_at"; "archived_at";
                    "deleted_worktree"; "deleted_branch" ] )
          in
          let updates =
            [ string "status" status;
              string "worker_dir" current.worker_dir;
              string "updated_at" now ]
            @ extra
          in
          let* () = update_file_unlocked ~remove current.path updates in
          parse_job_file ~home current.path)
  | _ -> Error "cannot finalize a transition without canonical home metadata"
