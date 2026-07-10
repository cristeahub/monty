type worktree_mode = Always | Never

type options = {
  backend : Terminal.backend;
  target : Terminal.target;
  pi_command : string;
  wt_command : string;
  worktree_mode : worktree_mode;
  branch_prefix : string;
  fork : string option;
  home : string;
  script_dir : string;
  monty_command : string;
}

type existing = New | Retryable of string | Requested

type prepared = {
  index : int;
  job : Job.t;
  id : string;
  branch : string;
  repo : string;
  repo_identity : string;
  context : string;
  state_path : State_path.t;
  worker_dir : string;
  instructions : string;
  script_path : string;
  requested_task_key : string option;
  existing : existing;
}

type outcome =
  | Launch_requested of string option
  | Launch_failed of string
  | Unattempted_prepared

type job_result = {
  id : string;
  title : string;
  outcome : outcome;
  recovery : string;
}

type batch_result = { jobs : job_result list }

let ( let* ) = Result.bind

let worktree_mode_of_string = function
  | "always" | "yes" | "true" -> Ok Always
  | "never" | "no" | "false" -> Ok Never
  | value -> Error (`Msg (Printf.sprintf "unknown worktree mode %S" value))

let worktree_mode_to_string = function Always -> "always" | Never -> "never"
let worktree_mode_string options = worktree_mode_to_string options.worktree_mode

let options_with_persisted_worktree_mode options persisted =
  match worktree_mode_of_string (String.lowercase_ascii persisted) with
  | Error (`Msg message) -> Error message
  | Ok worktree_mode -> Ok { options with worktree_mode }

let ensure_directory label path =
  if Sys.file_exists path && Sys.is_directory path then Ok ()
  else Error (Printf.sprintf "%s is not an existing directory: %s" label path)

let ensure_regular_file label path =
  try
    if (Unix.stat path).Unix.st_kind = Unix.S_REG then Ok ()
    else Error (Printf.sprintf "%s is not an existing regular file: %s" label path)
  with Unix.Unix_error _ ->
    Error (Printf.sprintf "%s is not an existing regular file: %s" label path)

let canonical_existing label path =
  let* () =
    if String.equal label "repo" then ensure_directory label path
    else ensure_regular_file label path
  in
  try Ok (Unix.realpath path)
  with Unix.Unix_error (err, fn, arg) ->
    Error
      (Printf.sprintf "cannot canonicalize %s %s via %s(%s): %s" label path fn
         arg (Unix.error_message err))

let check_dependency label command =
  match Process.command_exists_with_arguments command with
  | Ok _ -> Ok ()
  | Error msg -> Error (Printf.sprintf "%s dependency unavailable: %s" label msg)

let check_dependencies options =
  let* () = check_dependency "pi" options.pi_command in
  let* () =
    match options.worktree_mode with
    | Never -> Ok ()
    | Always -> check_dependency "wt" options.wt_command
  in
  match options.backend with
  | Terminal.Dry_run -> Ok ()
  | Terminal.Ghostty ->
      let* () = check_dependency "Ghostty application" "ghostty" in
      check_dependency "Ghostty terminal request" "osascript"

let script_path options id =
  let* directory = State_path.canonicalize options.script_dir in
  let path = Filename.concat directory ("monty-" ^ id ^ "-launch.sh") in
  let* path = State_path.canonicalize path in
  if State_path.is_contained ~root:directory path then Ok path
  else Error (Printf.sprintf "unsafe launch script path escapes %s: %s" directory path)

let prepare_identity ?index options manifest_index job =
  let repo_path = Shell.normalize (Shell.abs_path job.Job.repo) in
  let context_path = Shell.normalize (Shell.abs_path job.Job.context) in
  let* repo_identity = canonical_existing "repo" repo_path in
  let repo = repo_path in
  let* context = canonical_existing "context" context_path in
  let branch = Job.branch_or_default ~prefix:options.branch_prefix ?index job in
  if String.trim branch = "" then Error "worker branch must not be empty"
  else
    let id = Job.id_or_default ~branch job in
    let* _ = State_path.safe_component ~label:"worker id" id in
    let job =
      { job with Job.id = Some id; repo; branch = Some branch; context }
    in
    let* state_path = Worker_memory.worker_state ~home:options.home ~id job in
    let* () = State_path.ensure_contained_for_mutation state_path in
    let worker_dir = state_path.State_path.worker_dir in
    let instructions = Worker_memory.instructions_file worker_dir in
    let* script_path = script_path options id in
    Ok
      { index = manifest_index;
        job;
        id;
        branch;
        repo;
        repo_identity;
        context;
        state_path;
        worker_dir;
        instructions;
        script_path;
        requested_task_key = job.Job.task_key;
        existing = New }

let duplicate_error label identity left right =
  Error
    (Printf.sprintf
       "duplicate launch %s %S between manifest job %d (%S) and job %d (%S)"
       label identity left.index left.job.Job.title right.index right.job.Job.title)

let reject_duplicate label identity jobs =
  let rec loop seen = function
    | [] -> Ok ()
    | job :: rest -> (
        let key = identity job in
        match List.assoc_opt key seen with
        | Some other -> duplicate_error label key other job
        | None -> loop ((key, job) :: seen) rest)
  in
  loop [] jobs

let reject_duplicate_optional label identity jobs =
  let rec loop seen = function
    | [] -> Ok ()
    | job :: rest -> (
        match identity job with
        | None -> loop seen rest
        | Some key -> (
            match List.assoc_opt key seen with
            | Some other -> duplicate_error label key other job
            | None -> loop ((key, job) :: seen) rest))
  in
  loop [] jobs

let canonical_path_equal left right =
  match (State_path.canonicalize left, State_path.canonicalize right) with
  | Ok left, Ok right -> String.equal left right
  | _ -> false

let same_record_identity (prepared : prepared) (record : Job_store.record) =
  String.equal record.id prepared.id
  && canonical_path_equal record.worker_dir prepared.worker_dir
  && (match canonical_existing "repo" record.job.Job.repo with
      | Ok identity -> String.equal identity prepared.repo_identity
      | Error _ -> false)
  && record.job.Job.branch = Some prepared.branch
  && (match canonical_existing "context" record.job.Job.context with
      | Ok context -> String.equal context prepared.context
      | Error _ -> false)
  && String.equal record.job.Job.title prepared.job.Job.title
  && record.job.Job.task_key = prepared.job.Job.task_key

let same_record prepared (record : Job_store.record) =
  same_record_identity prepared record
  &&
  match record.launch_script with
  | Some path -> canonical_path_equal path prepared.script_path
  | None -> false

let script_has_owner_marker (options : options) (prepared : prepared) path =
  try
    let pi_options =
      Pi_command.
        { pi_command = options.pi_command;
          fork = options.fork;
          script_dir = options.script_dir;
          branch_prefix = options.branch_prefix;
          monty_command = options.monty_command }
    in
    let expected =
      Pi_command.launch_script_contents ~options:pi_options
        ~job:prepared.job ~id:prepared.id ~branch:prepared.branch
        ~source_repo:prepared.repo ~initial_workdir:prepared.repo
        ~context:prepared.context ~instructions:prepared.instructions
        ~worker_dir:prepared.worker_dir
        ~worktree_mode:(worktree_mode_string options)
        ~wt_command:options.wt_command
    in
    let legacy_expected =
      expected |> String.split_on_char '\n'
      |> List.filter (fun line ->
             not (String.equal line "# monty-launch-script-v1"))
      |> String.concat "\n"
    in
    let contents = Shell.read_file path in
    String.equal contents expected || String.equal contents legacy_expected
  with Sys_error _ -> false

let trusted_script_destination options path =
  let roots =
    [ options.script_dir; Home.runtime_script_dir ~home:options.home () ]
  in
  roots
  |> List.exists (fun root ->
         match State_path.canonicalize root with
         | Ok root -> State_path.is_contained ~root path
         | Error _ -> false)

let recorded_script options (prepared : prepared) (record : Job_store.record) =
  match record.launch_script with
  | None ->
      Error
        (Printf.sprintf
           "existing worker %s has no recorded launch script ownership; repair it explicitly before retrying"
           record.id)
  | Some recorded_path ->
      let* () =
        try
          match (Unix.lstat recorded_path).Unix.st_kind with
          | Unix.S_LNK ->
              Error
                (Printf.sprintf "recorded launch script must not be a symlink: %s"
                   recorded_path)
          | _ -> Ok ()
        with
        | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> Ok ()
        | Unix.Unix_error (err, fn, arg) ->
            Error
              (Printf.sprintf "cannot inspect launch script via %s(%s): %s" fn arg
                 (Unix.error_message err))
      in
      let* path = State_path.canonicalize recorded_path in
      let expected = "monty-" ^ prepared.id ^ "-launch.sh" in
      if not (String.equal (Filename.basename path) expected) then
        Error
          (Printf.sprintf
             "existing worker %s records unexpected launch script %s; expected basename %s"
             record.id path expected)
      else
        let* () =
          try
            match (Unix.lstat path).Unix.st_kind with
            | Unix.S_REG ->
                if script_has_owner_marker options prepared path then Ok ()
                else
                  Error
                    (Printf.sprintf
                       "recorded launch script does not match the complete Monty-owned script for worker %s: %s"
                       prepared.id path)
            | Unix.S_LNK -> assert false
            | _ ->
                Error
                  (Printf.sprintf "recorded launch script is not a regular file: %s" path)
          with
          | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
              if trusted_script_destination options path then Ok ()
              else
                Error
                  (Printf.sprintf
                     "recorded launch script for worker %s is absent outside the configured or Monty runtime script directory: %s"
                     prepared.id path)
          | Unix.Unix_error (err, fn, arg) ->
              Error
                (Printf.sprintf "cannot inspect launch script via %s(%s): %s" fn arg
                   (Unix.error_message err))
        in
        Ok { prepared with script_path = path }

let classify_existing options records (prepared : prepared) =
  let same_path record =
    canonical_path_equal record.Job_store.worker_dir prepared.worker_dir
  in
  let other_records = List.filter (fun record -> not (same_path record)) records in
  let conflict label identity predicate =
    match List.find_opt predicate other_records with
    | None -> Ok ()
    | Some record ->
        Error
          (Printf.sprintf
             "launch %s identity %S for manifest job %d (%S) is already owned by worker %s at %s"
             label identity prepared.index prepared.job.Job.title record.id
             record.path)
  in
  let* () =
    conflict "worker id" prepared.id (fun record ->
        String.equal record.Job_store.id prepared.id)
  in
  let* () =
    match prepared.job.Job.task_key with
    | None -> Ok ()
    | Some task_key ->
        conflict "stable task link" task_key (fun record ->
            record.Job_store.job.Job.task_key = Some task_key)
  in
  let* () =
    conflict "repo+branch"
      (prepared.repo_identity ^ " + " ^ prepared.branch)
      (fun record ->
        (match canonical_existing "repo" record.Job_store.job.Job.repo with
        | Ok identity -> String.equal identity prepared.repo_identity
        | Error _ -> false)
        && record.Job_store.job.Job.branch = Some prepared.branch)
  in
  match List.filter same_path records with
  | [] ->
      if State_path.path_exists prepared.worker_dir then
        Error
          (Printf.sprintf
             "canonical worker directory already exists without matching job state for manifest job %d (%S): %s"
             prepared.index prepared.job.Job.title prepared.worker_dir)
      else if State_path.path_exists prepared.script_path then
        Error
          (Printf.sprintf
             "launch script already exists without matching worker reservation for manifest job %d (%S): %s"
             prepared.index prepared.job.Job.title prepared.script_path)
      else Ok { prepared with existing = New }
  | [ record ] when not (same_record_identity prepared record) ->
      Error
        (Printf.sprintf
           "existing worker state conflicts with manifest job %d (%S): %s"
           prepared.index prepared.job.Job.title record.path)
  | [ record ] ->
      let* () =
        if String.equal record.worktree_mode (worktree_mode_string options) then
          Ok ()
        else
          Error
            (Printf.sprintf
               "existing worker %s uses persisted worktree mode %S, not requested mode %S; retry with the persisted mode"
               record.id record.worktree_mode (worktree_mode_string options))
      in
      let* prepared = recorded_script options prepared record in
      (match record.status with
      | "prepared" | "launch-failed" ->
          Ok { prepared with existing = Retryable record.status }
      | "launch-requested" -> Ok { prepared with existing = Requested }
      | status ->
          Error
            (Printf.sprintf
               "existing worker %s has status %S and cannot be batch relaunched; recover it with monty resume %s"
               prepared.id status (Shell.quote prepared.id)))
  | _ ->
      Error
        (Printf.sprintf
           "multiple durable records occupy canonical worker directory %s for manifest job %d (%S)"
           prepared.worker_dir prepared.index prepared.job.Job.title)

let map_task_jobs (prepared : prepared list) planned =
  prepared
  |> List.map (fun (item : prepared) ->
         match List.assoc_opt item.id planned with
         | Some job -> { item with job }
         | None -> item)

let prepare_batch options indexed_jobs =
  let use_indices = List.length indexed_jobs <> 1 in
  let* () = check_dependencies options in
  let rec prepare acc = function
    | [] -> Ok (List.rev acc)
    | (index, job) :: rest ->
        let* item =
          if use_indices then prepare_identity ~index options index job
          else prepare_identity options index job
        in
        prepare (item :: acc) rest
  in
  let* prepared = prepare [] indexed_jobs in
  let* () =
    reject_duplicate "worker id" (fun (item : prepared) -> item.id) prepared
  in
  let* () =
    reject_duplicate "canonical worker directory"
      (fun (item : prepared) -> item.worker_dir) prepared
  in
  let* () =
    reject_duplicate "canonical repo+branch"
      (fun (item : prepared) -> item.repo_identity ^ "\000" ^ item.branch)
      prepared
  in
  let* () =
    reject_duplicate_optional "stable task link"
      (fun (item : prepared) -> item.requested_task_key)
      prepared
  in
  let* planned =
    Project_overview.preflight_launch_task_links ~home:options.home
      (List.map (fun (item : prepared) -> (item.id, item.job)) prepared)
  in
  let prepared = map_task_jobs prepared planned in
  let* () =
    reject_duplicate "stable task link"
      (fun (item : prepared) ->
        Option.value ~default:"<missing>" item.job.Job.task_key)
      prepared
  in
  let* records = Job_store.load_all ~home:options.home in
  let rec classify acc = function
    | [] -> Ok (List.rev acc)
    | item :: rest ->
        let* item = classify_existing options records item in
        classify (item :: acc) rest
  in
  classify [] prepared

let matching_current (prepared : prepared) =
  if not (State_path.path_exists prepared.state_path.State_path.job_file) then
    Ok None
  else
    let* record =
      Job_store.parse_job_file ~home:prepared.state_path.home
        prepared.state_path.job_file
    in
    if same_record prepared record then Ok (Some record)
    else
      Error
        (Printf.sprintf "worker reservation changed during launch: %s"
           prepared.state_path.job_file)

let rec remove_staging_tree path =
  try
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR ->
        Sys.readdir path
        |> Array.iter (fun name -> remove_staging_tree (Filename.concat path name));
        Unix.rmdir path
    | _ -> Unix.unlink path
  with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> ()

let validate_staged_reservation options (item : prepared) staging_dir =
  let expected_entries = [ "MONTY.md"; "artifacts"; "job.json"; "memory.md" ] in
  let* entries =
    try Ok (Sys.readdir staging_dir |> Array.to_list |> List.sort String.compare)
    with Sys_error msg -> Error msg
  in
  if entries <> expected_entries then
    Error
      (Printf.sprintf
         "staged reservation %s has unexpected contents; expected exactly %s"
         staging_dir (String.concat ", " expected_entries))
  else
    let regular name =
      let path = Filename.concat staging_dir name in
      try
        let stat = Unix.lstat path in
        if stat.Unix.st_kind <> Unix.S_REG then
          Error (Printf.sprintf "staged reservation file is not regular: %s" path)
        else if stat.Unix.st_nlink <> 1 then
          Error
            (Printf.sprintf "staged reservation file must not be hard-linked: %s"
               path)
        else Ok ()
      with Unix.Unix_error (err, fn, arg) ->
        Error
          (Printf.sprintf "cannot inspect staged file via %s(%s): %s" fn arg
             (Unix.error_message err))
    in
    let* () = regular "MONTY.md" in
    let* () = regular "memory.md" in
    let* () = regular "job.json" in
    let artifacts = Filename.concat staging_dir "artifacts" in
    let* () =
      try
        let stat = Unix.lstat artifacts in
        if stat.Unix.st_kind <> Unix.S_DIR then
          Error
            (Printf.sprintf "staged artifacts path is not a directory: %s"
               artifacts)
        else if Array.length (Sys.readdir artifacts) <> 0 then
          Error
            (Printf.sprintf "staged artifacts directory is not empty: %s"
               artifacts)
        else Ok ()
      with
      | Sys_error msg -> Error msg
      | Unix.Unix_error (err, fn, arg) ->
          Error
            (Printf.sprintf "cannot inspect staged artifacts via %s(%s): %s"
               fn arg (Unix.error_message err))
    in
    let* record =
      Job_store.parse_job_file (Filename.concat staging_dir "job.json")
    in
    if not (same_record item record) then
      Error "staged job identity does not match the prepared launch"
    else if not (String.equal record.status "prepared") then
      Error
        (Printf.sprintf "staged job has status %S, expected prepared"
           record.status)
    else if not (canonical_path_equal record.run_dir item.state_path.run_dir) then
      Error
        (Printf.sprintf "staged job run_dir %s is not canonical %s"
           record.run_dir item.state_path.run_dir)
    else if
      not
        (String.equal record.worktree_mode (worktree_mode_string options))
    then
      Error
        (Printf.sprintf "staged job worktree mode %S does not match %S"
           record.worktree_mode (worktree_mode_string options))
    else if record.last_known_worktree <> None || record.transition <> None
            || record.completed_at <> None || record.archived_at <> None
    then Error "staged job contains lifecycle fields that are invalid for a prepared reservation"
    else Ok ()

let fsync_staged_reservation staging_dir =
  try
    List.iter
      (fun name ->
        State_store.fsync_regular_file (Filename.concat staging_dir name))
      [ "MONTY.md"; "memory.md"; "job.json" ];
    State_store.fsync_directory (Filename.concat staging_dir "artifacts");
    State_store.fsync_directory staging_dir;
    State_store.fsync_directory (Filename.dirname staging_dir);
    Ok ()
  with Unix.Unix_error (err, fn, arg) ->
    Error
      (Printf.sprintf "failed to sync staged reservation via %s(%s): %s" fn arg
         (Unix.error_message err))

let stage_reservation options (item : prepared) =
  let staging_root =
    Filename.concat item.state_path.State_path.run_dir ".reservations"
  in
  let staging_dir = Filename.concat staging_root item.id in
  let create () =
    try
      Shell.ensure_dir (Filename.dirname item.worker_dir);
      Shell.ensure_dir staging_root;
      State_store.fsync_directory item.state_path.State_path.run_dir;
      Unix.mkdir staging_dir 0o700;
      State_store.fsync_directory staging_root;
      let result =
        try
          Worker_memory.write_instructions ~destination_dir:staging_dir
            ~worker_dir:item.worker_dir ~id:item.id ~job:item.job
            ~branch:item.branch ~repo:item.repo ~context:item.context
            ~worktree_mode:(worktree_mode_string options) ();
          State_store.write_json_atomic
            ~path:(Filename.concat staging_dir "job.json")
            (Worker_memory.job_json ~status:"prepared"
               ~launch_script:item.script_path ~worker_dir:item.worker_dir
               ~id:item.id ~job:item.job ~branch:item.branch ~repo:item.repo
               ~context:item.context
               ~worktree_mode:(worktree_mode_string options)
               ~last_known_worktree:None ())
        with
        | Sys_error msg -> Error msg
        | Unix.Unix_error (err, fn, arg) ->
            Error
              (Printf.sprintf "failed to stage worker %s via %s(%s): %s"
                 item.id fn arg (Unix.error_message err))
      in
      (match result with
      | Ok () -> (
          match validate_staged_reservation options item staging_dir with
          | Ok () -> (
              match fsync_staged_reservation staging_dir with
              | Ok () -> Ok staging_dir
              | Error _ as error ->
                  remove_staging_tree staging_dir;
                  error)
          | Error _ as error ->
              remove_staging_tree staging_dir;
              error)
      | Error _ as error ->
          remove_staging_tree staging_dir;
          error)
    with
    | Sys_error msg -> Error msg
    | Unix.Unix_error (err, fn, arg) ->
        Error
          (Printf.sprintf "failed to stage worker %s via %s(%s): %s" item.id
             fn arg (Unix.error_message err))
  in
  let* staging_root =
    State_path.path_under_resolved_home ~home:options.home staging_root
  in
  let* () =
    try
      match (Unix.lstat staging_root).Unix.st_kind with
      | Unix.S_DIR -> Ok ()
      | Unix.S_LNK ->
          Error
            (Printf.sprintf "reservation staging root must not be a symlink: %s"
               staging_root)
      | _ ->
          Error
            (Printf.sprintf "reservation staging root is not a directory: %s"
               staging_root)
    with
    | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> Ok ()
    | Unix.Unix_error (err, fn, arg) ->
        Error
          (Printf.sprintf "cannot inspect reservation staging root via %s(%s): %s"
             fn arg (Unix.error_message err))
  in
  let staging_dir = Filename.concat staging_root item.id in
  match
    try Ok (Some (Unix.lstat staging_dir).Unix.st_kind) with
    | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> Ok None
    | Unix.Unix_error (err, fn, arg) ->
        Error
          (Printf.sprintf "cannot inspect staged reservation via %s(%s): %s" fn
             arg (Unix.error_message err))
  with
  | Error _ as error -> error
  | Ok None -> create ()
  | Ok (Some Unix.S_DIR) ->
      let job_file = Filename.concat staging_dir "job.json" in
      if not (Sys.file_exists job_file) then (
        remove_staging_tree staging_dir;
        create ())
      else
        (match validate_staged_reservation options item staging_dir with
        | Ok () -> Ok staging_dir
        | Error message ->
            Error
              (Printf.sprintf
                 "staged reservation conflicts with manifest job %d (%S): %s; %s; repair or remove it explicitly"
                 item.index item.job.Job.title staging_dir message))
  | Ok (Some _) ->
      Error
        (Printf.sprintf "staged reservation path is not a directory: %s"
           staging_dir)

let reservation_fault checkpoint =
  match Sys.getenv_opt "MONTY_FAULT_INJECT" with
  | Some value when String.equal value checkpoint ->
      Error (Printf.sprintf "fault injected at %s" checkpoint)
  | _ -> Ok ()

let reserve_batch options prepared =
  State_store.with_lock ~home:options.home (fun () ->
      let input_jobs =
        List.map
          (fun (item : prepared) ->
            ( item.id,
              { item.job with Job.task_key = item.requested_task_key } ))
          prepared
      in
      let* planned =
        Project_overview.preflight_launch_task_links ~home:options.home input_jobs
      in
      let prepared = map_task_jobs prepared planned in
      let* () =
        reject_duplicate "stable task link"
          (fun (item : prepared) ->
            Option.value ~default:"<missing>" item.job.Job.task_key)
          prepared
      in
      let* records = Job_store.load_all ~home:options.home in
      let rec reclassify acc = function
        | [] -> Ok (List.rev acc)
        | item :: rest ->
            let* item = classify_existing options records item in
            reclassify (item :: acc) rest
      in
      let* prepared = reclassify [] prepared in
      let rec stage acc = function
        | [] -> Ok (List.rev acc)
        | item :: rest -> (
            match item.existing with
            | New -> (
                match stage_reservation options item with
                | Ok path -> stage ((item, Some path) :: acc) rest
                | Error message ->
                    List.iter
                      (fun (_, path) -> Option.iter remove_staging_tree path)
                      acc;
                    Error message)
            | Retryable _ | Requested -> stage ((item, None) :: acc) rest)
      in
      let* staged = stage [] prepared in
      let* () = reservation_fault "reserve-abrupt-after-stage" in
      let* () =
        match reservation_fault "reserve-after-stage" with
        | Ok () -> Ok ()
        | Error message ->
            List.iter
              (fun (_, path) -> Option.iter remove_staging_tree path)
              staged;
            Error message
      in
      let reserved_tasks =
        Project_overview.reserve_launch_task_links_unlocked ~home:options.home
          input_jobs
      in
      (match reserved_tasks with
      | Error message ->
          List.iter
            (fun (_, path) -> Option.iter remove_staging_tree path)
            staged;
          Error message
      | Ok reserved_jobs ->
          let prepared = map_task_jobs prepared reserved_jobs in
          let* () = reservation_fault "reserve-abrupt-after-tasks" in
          (match reservation_fault "reserve-after-tasks" with
          | Error message ->
              List.iter
                (fun (_, path) -> Option.iter remove_staging_tree path)
                staged;
              Error message
          | Ok () ->
          let rec install installed = function
            | [] -> Ok (List.rev installed)
            | ((staged_item : prepared), None) :: rest ->
                let item =
                  match
                    List.find_opt
                      (fun (item : prepared) ->
                        String.equal item.id staged_item.id)
                      prepared
                  with
                  | Some item -> item
                  | None -> staged_item
                in
                install (item :: installed) rest
            | ((staged_item : prepared), Some staging_dir) :: rest ->
                let item =
                  match
                    List.find_opt
                      (fun (item : prepared) ->
                        String.equal item.id staged_item.id)
                      prepared
                  with
                  | Some item -> item
                  | None -> staged_item
                in
                if item.job.Job.task_key <> staged_item.job.Job.task_key then (
                  remove_staging_tree staging_dir;
                  List.iter
                    (fun (_, path) -> Option.iter remove_staging_tree path)
                    rest;
                  Error
                    (Printf.sprintf
                       "worker %s task reservation changed while installing its durable state"
                       item.id))
                else
                  (match
                     validate_staged_reservation options item staging_dir
                   with
                  | Error message ->
                      remove_staging_tree staging_dir;
                      List.iter
                        (fun (_, path) -> Option.iter remove_staging_tree path)
                        rest;
                      Error message
                  | Ok () -> (
                      match fsync_staged_reservation staging_dir with
                      | Error message -> Error message
                      | Ok () ->
                      try
                        Unix.rename staging_dir item.worker_dir;
                        State_store.fsync_directory
                          (Filename.dirname item.worker_dir);
                        State_store.fsync_directory
                          (Filename.dirname staging_dir);
                        install (item :: installed) rest
                   with Unix.Unix_error (err, fn, arg) ->
                     remove_staging_tree staging_dir;
                     List.iter
                       (fun (_, path) -> Option.iter remove_staging_tree path)
                       rest;
                     Error
                       (Printf.sprintf
                          "failed to install worker reservation %s via %s(%s): %s"
                          item.id fn arg (Unix.error_message err))))
          in
          let* installed = install [] staged in
          let* () = reservation_fault "reserve-after-install" in
          Ok installed)))

let dry_run options (prepared : prepared) =
  let wt_line, workdir =
    match options.worktree_mode with
    | Never -> (None, prepared.repo)
    | Always ->
        ( Some
            (Printf.sprintf
               "%s ensure-worktree --repo %s --branch %s --wt-command %s"
               (Shell.quote options.monty_command) (Shell.quote prepared.repo)
               (Shell.quote prepared.branch) (Shell.quote options.wt_command)),
          "<worktree selected for repo by monty>" )
  in
  Fmt.pr "[dry-run] job: %s\n" prepared.job.Job.title;
  Fmt.pr "[dry-run] id: %s\n" prepared.id;
  Option.iter (Fmt.pr "[dry-run] rehydrate worktree: %s\n") wt_line;
  Fmt.pr "[dry-run] workdir: %s\n" workdir;
  Fmt.pr "[dry-run] worker memory: %s\n" prepared.worker_dir;
  Fmt.pr "[dry-run] monty instructions: %s\n" prepared.instructions;
  Fmt.pr "[dry-run] context: %s\n" prepared.context;
  Fmt.pr "[dry-run] terminal: %s %s\n"
    (Terminal.backend_to_string options.backend)
    (Terminal.target_to_string options.target);
  let pi_options =
    Pi_command.
      { pi_command = options.pi_command;
        fork = options.fork;
        script_dir = options.script_dir;
        branch_prefix = options.branch_prefix;
        monty_command = options.monty_command }
  in
  Fmt.pr "[dry-run] pi: %s\n"
    (Pi_command.build_command ~options:pi_options
       ~instructions:(Some prepared.instructions) ~job:prepared.job
       ~context:prepared.context)

let fault checkpoint =
  match Sys.getenv_opt "MONTY_FAULT_INJECT" with
  | Some value when String.equal value checkpoint ->
      Error (Printf.sprintf "fault injected at %s" checkpoint)
  | _ -> Ok ()

let update_launch_state options (prepared : prepared) ~expected_statuses ~status
    ?error ?worktree () =
  let updates =
    [ Job_store.string "status" status;
      Job_store.string "updated_at" (Worker_memory.now_utc ());
      Job_store.string "launch_script" prepared.script_path ]
    @
    match worktree with
    | None -> []
    | Some path -> [ Job_store.string "last_known_worktree" path ]
  in
  let updates =
    match error with
    | None -> updates
    | Some message -> Job_store.string "launch_error" message :: updates
  in
  State_store.with_lock ~home:options.home (fun () ->
      let* current =
        Job_store.parse_job_file ~home:options.home prepared.state_path.job_file
      in
      let* () =
        match current.Job_store.transition with
        | Some transition ->
            Error
              (Printf.sprintf
                 "worker %s entered a %s transition while its launch state was changing; recover that lifecycle transition instead"
                 current.id (Job_store.operation_name transition.operation))
        | None -> Ok ()
      in
      let* () =
        if List.mem current.status expected_statuses then Ok ()
        else
          Error
            (Printf.sprintf
               "worker %s launch state changed from expected %s to %S; reload before retrying"
               current.id (String.concat ", " expected_statuses) current.status)
      in
      let* () =
        if same_record prepared current then Ok ()
        else
          Error
            (Printf.sprintf "worker %s identity changed during launch" current.id)
      in
      Job_store.update_file_unlocked
        ~remove:(if error = None then [ "launch_error" ] else [])
        prepared.state_path.job_file updates)

let mark_failed options (prepared : prepared) ~expected_statuses message =
  match
    update_launch_state options prepared ~expected_statuses
      ~status:"launch-failed" ~error:message ()
  with
  | Ok () -> message
  | Error persist_error ->
      Printf.sprintf "%s; additionally failed to persist launch-failed: %s" message
        persist_error

let pi_options (options : options) =
  Pi_command.
    { pi_command = options.pi_command;
      fork = options.fork;
      script_dir = options.script_dir;
      branch_prefix = options.branch_prefix;
      monty_command = options.monty_command }

let request_one options (prepared : prepared) ~expected_statuses =
  let workdir_result =
    match options.worktree_mode with
    | Never -> Ok prepared.repo
    | Always ->
        Wt.create_or_reuse ~wt_command:options.wt_command ~repo:prepared.repo
          ~branch:prepared.branch
  in
  match workdir_result with
  | Error message ->
      `Failed (mark_failed options prepared ~expected_statuses message)
  | Ok workdir ->
      let initial_workdir =
        match options.worktree_mode with Always -> prepared.repo | Never -> workdir
      in
      let script_result =
        try
          ignore
            (Pi_command.write_launch_script ~path:prepared.script_path
               ~options:(pi_options options) ~job:prepared.job ~id:prepared.id
               ~branch:prepared.branch ~source_repo:prepared.repo
               ~initial_workdir ~context:prepared.context
               ~instructions:prepared.instructions
               ~worker_dir:prepared.worker_dir
               ~worktree_mode:(worktree_mode_string options)
               ~wt_command:options.wt_command ());
          Ok ()
        with
        | Sys_error msg -> Error msg
        | Unix.Unix_error (err, fn, arg) ->
            Error
              (Printf.sprintf "failed to write launch script via %s(%s): %s" fn arg
                 (Unix.error_message err))
      in
      (match script_result with
      | Error message ->
          `Failed (mark_failed options prepared ~expected_statuses message)
      | Ok () -> (
          match fault "launch-before-request-state" with
          | Error message ->
              `Failed (mark_failed options prepared ~expected_statuses message)
          | Ok () -> (
              match
                update_launch_state options prepared ~expected_statuses
                  ~status:"launch-requested" ~worktree:workdir ()
              with
              | Error message -> `Failed message
              | Ok () -> (
                  match fault "launch-after-request-state" with
                  | Error message -> `Requested_error message
                  | Ok () -> (
                      match
                        Ghostty.launch ~target:options.target
                          ~workdir:initial_workdir
                          ~script_path:prepared.script_path
                      with
                      | Error message -> `Requested_error message
                      | Ok () -> (
                          match fault "launch-after-terminal-request" with
                          | Error message -> `Requested_error message
                          | Ok () -> `Requested))))))

let common_retry_options options =
  String.concat " "
    ([ "--home"; Shell.quote options.home;
       "--terminal"; Terminal.backend_to_string options.backend;
       "--target"; Terminal.target_to_string options.target;
       "--worktree"; worktree_mode_string options;
       "--branch-prefix"; Shell.quote options.branch_prefix;
       "--pi-command"; Shell.quote options.pi_command;
       "--wt-command"; Shell.quote options.wt_command;
       "--script-dir"; Shell.quote options.script_dir ]
    @
    match options.fork with
    | None -> []
    | Some session -> [ "--fork"; Shell.quote session ])

let result_for options retry (prepared : prepared) outcome =
  let recovery =
    match outcome with
    | Launch_requested _ ->
        String.concat " "
          [ Shell.quote options.monty_command; "resume";
            Shell.quote prepared.id; common_retry_options options ]
    | Launch_failed _ | Unattempted_prepared -> retry
  in
  { id = prepared.id; title = prepared.job.Job.title; outcome; recovery }

let execute_batch options ~retry prepared =
  let rec loop results = function
    | [] -> { jobs = List.rev results }
    | item :: rest -> (
        match item.existing with
        | Requested ->
            loop
              (result_for options retry item (Launch_requested None) :: results)
              rest
        | (New | Retryable _) as existing -> (
            let expected_statuses =
              match existing with
              | New -> [ "prepared" ]
              | Retryable status -> [ status ]
              | Requested -> assert false
            in
            match request_one options item ~expected_statuses with
            | `Requested ->
                loop
                  (result_for options retry item (Launch_requested None) :: results)
                  rest
            | `Requested_error message ->
                let requested =
                  result_for options retry item (Launch_requested (Some message))
                in
                let later =
                  List.map
                    (fun job ->
                      result_for options retry job Unattempted_prepared)
                    rest
                in
                { jobs = List.rev results @ (requested :: later) }
            | `Failed message ->
                let failed =
                  result_for options retry item (Launch_failed message)
                in
                let later =
                  List.map
                    (fun job ->
                      result_for options retry job Unattempted_prepared)
                    rest
                in
                { jobs = List.rev results @ (failed :: later) }))
  in
  loop [] prepared

let outcome_name = function
  | Launch_requested _ -> "launch-requested"
  | Launch_failed _ -> "launch-failed"
  | Unattempted_prepared -> "unattempted/prepared"

let render_human result =
  result.jobs
  |> List.map (fun job ->
         let detail =
           match job.outcome with
           | Launch_requested (Some message) | Launch_failed message ->
               "\n  Error: " ^ message
           | _ -> ""
         in
         Printf.sprintf "%s (%s): %s%s\n  Recovery: %s" job.id job.title
           (outcome_name job.outcome) detail job.recovery)
  |> String.concat "\n"

let has_failure result =
  List.exists
    (fun job ->
      match job.outcome with
      | Launch_failed _ | Unattempted_prepared | Launch_requested (Some _) -> true
      | Launch_requested None -> false)
    result.jobs

let retry_launch_many_command options manifest =
  String.concat " "
    [ Shell.quote options.monty_command; "launch-many"; "--manifest";
      Shell.quote manifest; common_retry_options options ]

let retry_launch_command options (job : Job.t) =
  String.concat " "
    ([ Shell.quote options.monty_command; "launch"; "--repo";
       Shell.quote job.repo; "--title"; Shell.quote job.title; "--context";
       Shell.quote job.context ]
    @
    (match job.branch with
    | None -> []
    | Some branch -> [ "--branch"; Shell.quote branch ])
    @ [ common_retry_options options ])

let launch_many ?(retry_command = "monty launch-many") options indexed_jobs =
  let* prepared = prepare_batch options indexed_jobs in
  match options.backend with
  | Terminal.Dry_run ->
      List.iter (dry_run options) prepared;
      Ok ()
  | Terminal.Ghostty -> (
      match reserve_batch options prepared with
      | Error message ->
          let result =
            { jobs =
                List.map
                  (fun item ->
                    result_for options retry_command item Unattempted_prepared)
                  prepared }
          in
          Fmt.pr "%s\n" (render_human result);
          Error ("batch reservation failed: " ^ message)
      | Ok reserved ->
          let result = execute_batch options ~retry:retry_command reserved in
          Fmt.pr "%s\n" (render_human result);
          if has_failure result then Error "batch launch did not request every worker"
          else Ok ())

let launch_one ?index ?retry_command options job =
  let index = Option.value ~default:1 index in
  let retry_command =
    Option.value ~default:(retry_launch_command options job) retry_command
  in
  launch_many ~retry_command options [ (index, job) ]

let script_for_resume options prepared (record : Job_store.record) =
  match record.launch_script with
  | Some _ -> recorded_script options prepared record
  | None ->
      State_store.with_lock ~home:options.home (fun () ->
          let* current = Job_store.parse_job_file ~home:options.home record.path in
          let* () =
            match current.Job_store.transition with
            | None -> Ok ()
            | Some transition ->
                Error
                  (Printf.sprintf
                     "worker %s entered a %s transition before launch script ownership was recorded"
                     current.id (Job_store.operation_name transition.operation))
          in
          let* () =
            if String.equal current.status record.status then Ok ()
            else
              Error
                (Printf.sprintf
                   "worker %s changed status from %S to %S before launch script ownership was recorded"
                   current.id record.status current.status)
          in
          let* () =
            if same_record_identity prepared current then Ok ()
            else Error (Printf.sprintf "worker %s identity changed during resume" current.id)
          in
          let* () =
            match current.launch_script with
            | None -> Ok ()
            | Some _ ->
                Error
                  (Printf.sprintf
                     "worker %s launch script ownership changed during resume"
                     current.id)
          in
          if State_path.path_exists prepared.script_path then
            Error
              (Printf.sprintf
                 "launch script already exists without recorded ownership for worker %s: %s"
                 prepared.id prepared.script_path)
          else
            let* () =
              Job_store.update_file_unlocked record.path
                [ Job_store.string "launch_script" prepared.script_path;
                  Job_store.string "updated_at" (Worker_memory.now_utc ()) ]
            in
            Ok prepared)

let resume_job ?(validate_open_task = true) ~persisted_worktree_mode options
    job =
  let* options =
    options_with_persisted_worktree_mode options persisted_worktree_mode
  in
  let* () = check_dependencies options in
  let* prepared = prepare_identity options 1 job in
  let* prepared =
    match (validate_open_task, prepared.job.Job.task_key) with
    | true, Some _ ->
        let* planned =
          Project_overview.preflight_launch_task_links ~home:options.home
            [ (prepared.id, prepared.job) ]
        in
        (match planned with
        | [ (_, job) ] -> Ok { prepared with job }
        | _ -> Ok prepared)
    | _ ->
        let* _ =
          Project_overview.validate_job_project ~home:options.home prepared.repo
        in
        Ok prepared
  in
  match options.backend with
  | Terminal.Dry_run ->
      dry_run options prepared;
      Ok ()
  | Terminal.Ghostty ->
      let* record =
        if not (State_path.path_exists prepared.state_path.State_path.job_file) then
          Error
            (Printf.sprintf "worker %s has no durable active reservation"
               prepared.id)
        else
          let* record =
            Job_store.parse_job_file ~home:options.home
              prepared.state_path.State_path.job_file
          in
          if String.equal record.Job_store.id prepared.id then Ok record
          else
            Error
              (Printf.sprintf
                 "worker reservation identity changed during resume: %s"
                 prepared.state_path.State_path.job_file)
      in
      let* () =
        match record.Job_store.transition with
        | None -> Ok ()
        | Some transition ->
            Error
              (Printf.sprintf "worker %s is in a %s transition" prepared.id
                 (Job_store.operation_name transition.operation))
      in
      let* _task_id =
        Project_overview.validate_worker_task_link ~home:options.home record
      in
      let* prepared = script_for_resume options prepared record in
      let retry =
        String.concat " "
          [ Shell.quote options.monty_command; "resume";
            Shell.quote prepared.id; common_retry_options options ]
      in
      let result =
        match request_one options prepared
                ~expected_statuses:[ record.Job_store.status ]
        with
        | `Requested ->
            result_for options retry prepared (Launch_requested None)
        | `Requested_error message ->
            result_for options retry prepared (Launch_requested (Some message))
        | `Failed message ->
            result_for options retry prepared (Launch_failed message)
      in
      Fmt.pr "%s\n" (render_human { jobs = [ result ] });
      match result.outcome with
      | Launch_requested None -> Ok ()
      | Launch_requested (Some _) | Launch_failed _ | Unattempted_prepared ->
          Error "worker resume did not complete the terminal request"
