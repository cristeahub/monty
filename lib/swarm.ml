(* `monty swarm`: the driver's commands over its Monty-owned children. Every
   command runs inside the driver's own session, resolved from the worker
   environment, and refuses other sessions. Children are ordinary worker
   records in the driver's run, spawned as headless chains of the worker
   profile on a branch forked from the driver's branch; the driver lands
   checked child heads into its branch by merge and retires landed children.

   A driver may span repositories. Each child owns an ordered subset of the
   driver's workspaces and has one branch per repository, forked from the
   driver's branch there; landing checks and merges every one of them. *)

let ( let* ) = Result.bind

type driver = {
  home : string;
  record : Job_store.record;
  swarm : Agent_profile.swarm;
}

let spawn_schema = "monty:swarm-spawn:v1"
let status_schema = "monty:swarm-status:v1"

let load_driver ~home (record : Job_store.record) =
  match Job.swarm_child record.job with
  | Some (parent, name) ->
      Error
        (Printf.sprintf
           "monty swarm runs in the driver session; this is swarm child %s of driver %s, which commits on its own branch and reports through its handoff"
           name parent)
  | None ->
      let* () =
        if Job_store.is_archived record then
          Error (Printf.sprintf "swarm driver %s is archived" record.id)
        else
          match record.transition with
          | None -> Ok ()
          | Some transition ->
              Error
                (Printf.sprintf "swarm driver %s is in a %s transition" record.id
                   (Job_store.operation_name transition.operation))
      in
      let* profile =
        Agent_profile.load_pinned ~home ~worker_dir:record.worker_dir record.job.agent_profile
      in
      (match profile.swarm with
      | Some swarm -> Ok { home; record; swarm }
      | None ->
          Error
            (Printf.sprintf
               "worker %s is pinned to agent profile %s, not swarm; monty swarm needs a swarm driver session"
               record.id profile.id))

let resolve_driver ~home =
  let home = Shell.normalize (Shell.abs_path home) in
  let* record =
    if Done.env_worker_dir () = None && Done.env_job_id () = None then
      Error "monty swarm runs inside a swarm driver session; MONTY_WORKER_DIR is not set"
    else Done.resolve_current ~home
  in
  load_driver ~home record

(* The driver id when the current session is a swarm driver, for commands
   that default to the driver's inbox. Never an error. *)
let current_driver_id ~home =
  match resolve_driver ~home with Ok driver -> Some driver.record.id | Error _ -> None

let worker_dir driver = driver.record.worker_dir

(* One of the driver's repositories with the driver's branch in it. *)
type workspace = {
  repo : string;
  branch : string;
  worktree : string option;
}

let driver_workspaces driver =
  Json_fields.fold_results driver.record.workspaces ~init:[]
    ~f:(fun acc (workspace : Job_store.workspace_state) ->
      match workspace.branch with
      | Some branch when String.trim branch <> "" ->
          Ok ({ repo = workspace.repo; branch; worktree = workspace.worktree } :: acc)
      | _ ->
          Error
            (Printf.sprintf "swarm driver %s has no branch in %s" driver.record.id workspace.repo))
  |> Result.map List.rev

(* A workspace is named by its repository path or, when that is unique among
   the driver's workspaces, by the repository's directory name. *)
let resolve_workspace workspaces key =
  let matches =
    if Filename.is_relative key then
      List.filter (fun workspace -> String.equal (Filename.basename workspace.repo) key) workspaces
    else
      let path = Shell.normalize key in
      List.filter (fun workspace -> String.equal workspace.repo path) workspaces
  in
  match matches with
  | [ workspace ] -> Ok workspace
  | found ->
      Error
        (Printf.sprintf "%s driver workspace matches %s; the driver's workspaces are: %s"
           (if found = [] then "no" else "more than one")
           key
           (String.concat ", " (List.map (fun workspace -> workspace.repo) workspaces)))

let driver_workspace workspaces repo =
  match List.find_opt (fun workspace -> String.equal workspace.repo repo) workspaces with
  | Some workspace -> Ok workspace
  | None -> Error (Printf.sprintf "%s is not a workspace of the swarm driver" repo)

let materialized_worktree ~owner workspace =
  match workspace.worktree with
  | Some path when Sys.file_exists path && Sys.is_directory path -> Ok path
  | _ ->
      Error
        (Printf.sprintf "%s has no materialized worktree for %s; resume it first" owner workspace.repo)

let child_id driver name = driver.record.id ^ "--" ^ name
let child_branch branch name = branch ^ "--" ^ name

(* The repositories and branches of a child. Ledger entries written before
   children could span repositories own the driver's first workspace. *)
let child_workspaces workspaces (child : Swarm_ledger.child) =
  match (child.workspaces, workspaces) with
  | [], first :: _ ->
      [ { Swarm_ledger.repo = first.repo; branch = child.branch; landed_commit = child.landed_commit } ]
  | recorded, _ -> recorded

(* The landing check for one repository: its own entry, else the shared one. *)
let check_for workspaces (ledger : Swarm_ledger.t) repo =
  let* specific =
    Json_fields.fold_results ledger.checks ~init:None ~f:(fun found (key, command) ->
        let* workspace =
          resolve_workspace workspaces key
          |> Result.map_error (fun message -> "swarm checks: " ^ message)
        in
        Ok (if String.equal workspace.repo repo then Some command else found))
  in
  match (specific, ledger.check) with
  | Some check, _ | None, Some check -> Ok check
  | None, None ->
      Error
        (Printf.sprintf
           "no landing check is set for %s; run monty swarm init --check CMD (add --workspace REPO for a check of that repository only)"
           repo)

let default_ledger driver : Swarm_ledger.t =
  let config = driver.record.job.Job.swarm in
  let setting pick default =
    match Option.bind config pick with Some value -> value | None -> default
  in
  { driver = driver.record.id;
    check = Option.bind config (fun (config : Job.swarm_config) -> config.check);
    checks = Option.fold ~none:[] ~some:(fun (config : Job.swarm_config) -> config.checks) config;
    max_workers = setting (fun config -> config.max_workers) driver.swarm.max_workers;
    max_spawns = driver.swarm.max_spawns;
    worker_profile = setting (fun config -> config.worker_profile) driver.swarm.worker_profile;
    children = [] }

let git ~cwd arguments =
  Process.run_success ~cwd ("git " ^ String.concat " " (List.map Shell.quote arguments))
  |> Result.map String.trim

(* Whether the child's head is in the driver's branch in every repository. *)
let head_in_driver driver (child : Swarm_ledger.child) =
  match driver_workspaces driver with
  | Error _ -> false
  | Ok workspaces ->
      List.for_all
        (fun (owned : Swarm_ledger.workspace) ->
          match driver_workspace workspaces owned.repo with
          | Error _ -> false
          | Ok workspace ->
              Result.is_ok
                (git ~cwd:owned.repo [ "merge-base"; "--is-ancestor"; owned.branch; workspace.branch ]))
        (child_workspaces workspaces child)

(* Child records are found by exact id, never by Job_store.find's slug matching. *)
let find_record records (child : Swarm_ledger.child) =
  match List.filter (fun (record : Job_store.record) -> String.equal record.id child.id) records with
  | [ record ] -> Ok (Some record)
  | [] -> Ok None
  | _ -> Error (Printf.sprintf "swarm child %s has more than one worker record" child.name)

let active_record ~home (child : Swarm_ledger.child) =
  let* records = Job_store.load_all ~home in
  match find_record records child with
  | Ok (Some record) when not (Job_store.is_archived record) -> Ok record
  | Ok _ -> Error (Printf.sprintf "swarm child %s has no active worker record" child.name)
  | Error _ as error -> error

let update_child driver name transform =
  Swarm_ledger.update ~home:driver.home ~worker_dir:(worker_dir driver) (function
    | None -> Error (Printf.sprintf "swarm ledger is missing for driver %s" driver.record.id)
    | Some ledger -> (
        match Swarm_ledger.find_child ledger name with
        | None -> Error (Printf.sprintf "swarm child %s is not in the ledger" name)
        | Some child -> Ok (Swarm_ledger.replace_child ledger (transform child))))
  |> Result.map ignore

let mark_failed driver name message =
  update_child driver name (fun child -> { child with state = Swarm_ledger.Failed; error = Some message })

(* Running children whose supervisor released the worker lock have finished
   or failed; their handoff since the run started says which. Probing locks
   happens before the state lock, and only still-running entries change. *)
let reconcile driver =
  let* ledger = Swarm_ledger.load_opt ~worker_dir:(worker_dir driver) in
  match ledger with
  | None -> Ok None
  | Some ledger ->
      let* records = Job_store.load_all ~home:driver.home in
      let* outcomes =
        Json_fields.fold_results (Swarm_ledger.running ledger) ~init:[]
          ~f:(fun acc (child : Swarm_ledger.child) ->
            let* record = find_record records child in
            match record with
            | None ->
                Ok ((child.name, Swarm_ledger.Failed, Some "worker record is missing") :: acc)
            | Some record when Job_store.is_archived record ->
                Ok ((child.name, Swarm_ledger.Retired, None) :: acc)
            | Some record -> (
                let* busy = Worker_lock.is_busy ~home:driver.home ~record in
                if busy then Ok acc
                else
                  match Run_handoff.latest ~home:driver.home record with
                  | Ok handoff
                    when String.compare handoff.finished_at (Swarm_ledger.run_started_at child) >= 0 -> (
                      match handoff.outcome with
                      | Run_handoff.Failed ->
                          Ok ((child.name, Swarm_ledger.Failed, handoff.error) :: acc)
                      | Run_handoff.Ready_for_review | Run_handoff.Needs_attention ->
                          Ok ((child.name, Swarm_ledger.Finished, None) :: acc))
                  | _ -> Ok acc))
      in
      if outcomes = [] then Ok (Some ledger)
      else
        Swarm_ledger.update ~home:driver.home ~worker_dir:(worker_dir driver) (function
          | None -> Ok ledger
          | Some current ->
              Ok
                (List.fold_left
                   (fun ledger (name, state, error) ->
                     match Swarm_ledger.find_child ledger name with
                     | Some child when child.state = Swarm_ledger.Running ->
                         Swarm_ledger.replace_child ledger { child with state; error }
                     | _ -> ledger)
                   current outcomes))
        |> Result.map Option.some

let require_child driver name =
  let* ledger = reconcile driver in
  match ledger with
  | None -> Error (Printf.sprintf "swarm driver %s has no ledger yet; nothing was spawned" driver.record.id)
  | Some ledger -> (
      match Swarm_ledger.find_child ledger name with
      | Some child -> Ok (ledger, child)
      | None ->
          Error
            (Printf.sprintf "no swarm child named %s; known children: %s" name
               (match ledger.children with
               | [] -> "<none>"
               | children ->
                   String.concat ", "
                     (List.map (fun (child : Swarm_ledger.child) -> child.name) children))))

let init driver ~check ~workspace =
  let check = String.trim check in
  if check = "" then Error "the landing check command must not be empty"
  else
    let* workspaces = driver_workspaces driver in
    let* target =
      match workspace with
      | None -> Ok None
      | Some key -> resolve_workspace workspaces key |> Result.map Option.some
    in
    let set (ledger : Swarm_ledger.t) =
      match target with
      | None -> Ok { ledger with check = Some check }
      | Some target ->
          (* Replace whatever entry already named this repository. *)
          let* others =
            Json_fields.fold_results ledger.checks ~init:[] ~f:(fun acc (key, command) ->
                let* existing = resolve_workspace workspaces key in
                Ok (if String.equal existing.repo target.repo then acc else (key, command) :: acc))
          in
          Ok { ledger with checks = List.rev others @ [ (target.repo, check) ] }
    in
    let* ledger =
      Swarm_ledger.update ~home:driver.home ~worker_dir:(worker_dir driver) (fun ledger ->
          set (Option.value ~default:(default_ledger driver) ledger))
    in
    Fmt.pr "Swarm check%s: %s\nLedger: %s\nLimits: %d running, %d total, worker profile %s\n"
      (Option.fold ~none:"" ~some:(fun target -> " for " ^ target.repo) target)
      check (Swarm_ledger.path (worker_dir driver)) ledger.max_workers ledger.max_spawns
      ledger.worker_profile;
    Ok ()

let read_brief path =
  let path = Shell.normalize (Shell.abs_path path) in
  match State_store.lstat path with
  | Ok (Some { Unix.st_kind = Unix.S_REG; _ }) ->
      let contents = Shell.read_file path in
      if String.trim contents = "" then Error (Printf.sprintf "brief is empty: %s" path)
      else Ok contents
  | Ok (Some { Unix.st_kind = Unix.S_LNK; _ }) -> Error (Printf.sprintf "brief must not be a symlink: %s" path)
  | Ok _ -> Error (Printf.sprintf "brief is not a regular file: %s" path)
  | Error message -> Error message

let write_child_state (options : Launcher.options) driver ~profile (prepared : Launcher.prepared) =
  try
    Worker_memory.write_instructions ~swarm_brief:driver.swarm.worker_brief
      ~worker_dir:prepared.worker_dir ~id:prepared.id ~job:prepared.job ~profile
      ~branch:prepared.branch ~repo:prepared.repo ~context:prepared.context
      ~worktree_mode:(Launcher.worktree_mode_string options) ();
    let* () = Agent_profile.write_snapshot ~worker_dir:prepared.worker_dir profile in
    Worker_memory.write_job_json_unlocked ~status:"prepared"
      ~launch_script:prepared.script_path ~worker_dir:prepared.worker_dir ~id:prepared.id
      ~job:prepared.job ~branch:prepared.branch ~repo:prepared.repo ~context:prepared.context
      ~worktree_mode:(Launcher.worktree_mode_string options) ~last_known_worktree:None ()
  with
  | Sys_error message -> Error message
  | Unix.Unix_error (err, fn, arg) ->
      Error (Printf.sprintf "failed to prepare swarm child via %s(%s): %s" fn arg (Unix.error_message err))

let start_chain (options : Launcher.options) driver ~name ~explicit_resume id =
  match Headless.run_worker ~wait:false ~explicit_resume options id with
  | Ok receipt -> Ok receipt
  | Error message ->
      let* () = mark_failed driver name message in
      Error
        (Printf.sprintf
           "%s\nswarm child %s is recorded as failed; retry with monty swarm resume %s --brief FILE or remove it with monty swarm retire %s --force"
           message name (Shell.quote name) (Shell.quote name))

let workspaces_json (workspaces : Swarm_ledger.workspace list) =
  `List
    (List.map
       (fun (workspace : Swarm_ledger.workspace) ->
         `Assoc
           [ ("repo", `String workspace.repo);
             ("branch", `String workspace.branch);
             ( "landed_commit",
               Option.fold ~none:`Null ~some:(fun commit -> `String commit) workspace.landed_commit ) ])
       workspaces)

let spawn (options : Launcher.options) driver ~name ~brief ~profile ~workspaces:selected =
  let* name = State_path.safe_component ~label:"swarm child name" name in
  let* contents = read_brief brief in
  let options = { options with Launcher.worktree_mode = Launcher.Always } in
  let* () = Launcher.check_dependencies options in
  let* workspaces = driver_workspaces driver in
  (* A child owns the workspaces it names, else the driver's first one. *)
  let* sources =
    match (selected, workspaces) with
    | [], first :: _ -> Ok [ first ]
    | [], [] -> Error (Printf.sprintf "swarm driver %s has no workspace" driver.record.id)
    | keys, _ ->
        Json_fields.fold_results keys ~init:[] ~f:(fun acc key ->
            let* workspace = resolve_workspace workspaces key in
            if List.exists (fun chosen -> String.equal chosen.repo workspace.repo) acc then
              Error (Printf.sprintf "workspace %s is named more than once" workspace.repo)
            else Ok (workspace :: acc))
        |> Result.map List.rev
  in
  let owned =
    List.map
      (fun source ->
        { Swarm_ledger.repo = source.repo; branch = child_branch source.branch name;
          landed_commit = None })
      sources
  in
  let first = List.hd owned in
  (* Reconcile first so finished children no longer count against max_workers. *)
  let* _ = reconcile driver in
  let* worker_profile =
    Agent_profile.find ~home:driver.home
      (Option.value profile ~default:(default_ledger driver).worker_profile)
  in
  let* () =
    if worker_profile.swarm = None then Ok ()
    else Error (Printf.sprintf "swarm children cannot run the %s profile" worker_profile.id)
  in
  let id = child_id driver name in
  let brief_path = Swarm_ledger.brief_path (worker_dir driver) name in
  let* state =
    match driver.record.state_path with
    | Some state -> State_path.active ~home:driver.home ~run_id:state.run_id ~id
    | None -> Error "swarm driver must be loaded from canonical Monty state"
  in
  let now = Worker_memory.now_utc () in
  let child : Swarm_ledger.child =
    { name; id; branch = first.branch; workspaces = owned; brief = brief_path; spawned_at = now;
      resumed_at = None;
      state = Swarm_ledger.Running; landed_commit = None; last_check = None; error = None }
  in
  (* Limits, the brief and the child record are committed together under the
     state lock; git and the headless supervisor run only after it. *)
  let* prepared =
    State_store.with_lock ~home:driver.home (fun () ->
        let* ledger = Swarm_ledger.load_opt ~worker_dir:(worker_dir driver) in
        let ledger = Option.value ~default:(default_ledger driver) ledger in
        let* () =
          Json_fields.fold_results owned ~init:() ~f:(fun () (workspace : Swarm_ledger.workspace) ->
              check_for workspaces ledger workspace.repo |> Result.map ignore)
        in
        let* () =
          if Swarm_ledger.find_child ledger name <> None then
            Error (Printf.sprintf "swarm child %s already exists; names are unique for the driver" name)
          else Ok ()
        in
        let running = List.length (Swarm_ledger.running ledger) in
        let* () =
          if running >= ledger.max_workers then
            Error
              (Printf.sprintf
                 "swarm limit reached: %d of %d children are running; land and retire or wait for one to finish"
                 running ledger.max_workers)
          else Ok ()
        in
        let* () =
          if List.length ledger.children >= ledger.max_spawns then
            Error
              (Printf.sprintf "swarm spawn limit reached: %d children were spawned (max_spawns %d)"
                 (List.length ledger.children) ledger.max_spawns)
          else Ok ()
        in
        let* () =
          if State_path.path_exists state.worker_dir then
            Error (Printf.sprintf "swarm child directory already exists: %s" state.worker_dir)
          else Ok ()
        in
        let* () =
          State_store.ensure_real_directory ~label:"swarm briefs directory" ~mode:0o700
            (Swarm_ledger.briefs_dir (worker_dir driver))
        in
        let* () = State_store.write_file_atomic ~path:brief_path ~perm:0o600 contents in
        let job =
          Job.make ~id ~branch:first.branch ~worker_dir:state.worker_dir
            ~workspaces:
              (List.map
                 (fun (workspace : Swarm_ledger.workspace) ->
                   Job.workspace ~branch:workspace.branch workspace.repo)
                 owned)
            ~title:(driver.record.job.Job.title ^ ": " ^ name) ~repo:first.repo
            ~context:brief_path ~agent_profile:worker_profile.id ~swarm_parent:driver.record.id
            ~swarm_name:name ()
        in
        let* prepared = Launcher.prepare_identity ~profile:worker_profile options 1 job in
        let* () = write_child_state options driver ~profile:worker_profile prepared in
        let* () =
          Swarm_ledger.save_unlocked ~worker_dir:(worker_dir driver)
            { ledger with children = ledger.children @ [ child ] }
        in
        Ok prepared)
  in
  let* () =
    Json_fields.fold_results (List.combine sources owned) ~init:()
      ~f:(fun () (source, (workspace : Swarm_ledger.workspace)) ->
        match git ~cwd:source.repo [ "branch"; workspace.branch; source.branch ] with
        | Ok _ -> Ok ()
        | Error message ->
            let* () = mark_failed driver name message in
            Error
              (Printf.sprintf
                 "%s\nswarm child %s is recorded as failed; remove it with monty swarm retire %s --force"
                 message name (Shell.quote name)))
  in
  let* receipt = start_chain options driver ~name ~explicit_resume:false prepared.id in
  Ok
    (`Assoc
      [ ("schema", `String spawn_schema);
        ("name", `String name);
        ("worker_id", `String prepared.id);
        ("branch", `String first.branch);
        ("workspaces", workspaces_json owned);
        ("brief", `String brief_path);
        ("worker_dir", `String prepared.worker_dir);
        ("agent_profile", `String worker_profile.id);
        ("receipt", receipt) ])

let resume (options : Launcher.options) driver ~name ~brief =
  let* contents = read_brief brief in
  let options = { options with Launcher.worktree_mode = Launcher.Always } in
  let* () = Launcher.check_dependencies options in
  let* _, child = require_child driver name in
  let* () =
    match child.state with
    | Swarm_ledger.Running -> Error (Printf.sprintf "swarm child %s is still running" name)
    | Swarm_ledger.Retired -> Error (Printf.sprintf "swarm child %s is retired" name)
    | Swarm_ledger.Finished | Swarm_ledger.Landed | Swarm_ledger.Failed -> Ok ()
  in
  let* record = active_record ~home:driver.home child in
  (* A finished chain leaves the record launch-requested; a spawn that failed
     before its chain started leaves it prepared. *)
  let explicit_resume = String.equal record.status "launch-requested" in
  let now = Worker_memory.now_utc () in
  let* () =
    State_store.with_lock ~home:driver.home (fun () ->
        let current = if Sys.file_exists child.brief then Shell.read_file child.brief else "" in
        State_store.write_file_atomic ~path:child.brief ~perm:0o600
          (current ^ "\n\n## Follow-up from the driver (" ^ now ^ ")\n\n" ^ contents))
  in
  let* () =
    update_child driver name (fun child ->
        { child with state = Swarm_ledger.Running; resumed_at = Some now; error = None })
  in
  let* receipt = start_chain options driver ~name ~explicit_resume record.id in
  Ok
    (`Assoc
      [ ("schema", `String spawn_schema);
        ("name", `String name);
        ("worker_id", `String record.id);
        ("branch", `String child.branch);
        ("brief", `String child.brief);
        ("receipt", receipt) ])

let status driver ~json =
  let* ledger = reconcile driver in
  let ledger = Option.value ~default:(default_ledger driver) ledger in
  let* records = Job_store.load_all ~home:driver.home in
  let* pending = Run_handoff.pending ~inbox:driver.record.id ~home:driver.home () in
  let* workspaces = driver_workspaces driver in
  let* rows =
    Json_fields.fold_results ledger.children ~init:[] ~f:(fun acc child ->
        let* record = find_record records child in
        let job_status = match record with Some record -> record.status | None -> "missing" in
        let pending =
          List.length
            (List.filter
               (fun (_, (handoff : Run_handoff.t)) -> String.equal handoff.worker_id child.id)
               pending)
        in
        Ok ((child, job_status, head_in_driver driver child, pending) :: acc))
    |> Result.map List.rev
  in
  let running = List.length (Swarm_ledger.running ledger) in
  let optional = Option.fold ~none:`Null ~some:(fun value -> `String value) in
  if json then
    Headless.print_json
      (`Assoc
        [ ("schema", `String status_schema);
          ("driver", `String driver.record.id);
          ("check", optional ledger.check);
          ("checks", Job.swarm_checks_json ledger.checks);
          ("max_workers", `Int ledger.max_workers);
          ("max_spawns", `Int ledger.max_spawns);
          ("worker_profile", `String ledger.worker_profile);
          ("running", `Int running);
          ("ledger", `String (Swarm_ledger.path (worker_dir driver)));
          ( "children",
            `List
              (List.map
                 (fun ((child : Swarm_ledger.child), job_status, in_driver, pending) ->
                   `Assoc
                     [ ("name", `String child.name);
                       ("worker_id", `String child.id);
                       ("state", `String (Swarm_ledger.state_to_string child.state));
                       ("job_status", `String job_status);
                       ("branch", `String child.branch);
                       ("workspaces", workspaces_json (child_workspaces workspaces child));
                       ("head_in_driver", `Bool in_driver);
                       ("pending_handoffs", `Int pending);
                       ("landed_commit", optional child.landed_commit);
                       ("last_check", optional child.last_check);
                       ("error", optional child.error) ])
                 rows) ) ])
  else (
    Fmt.pr "Driver: %s\nCheck: %s\nRunning: %d of %d (spawned %d of %d, worker profile %s)\n"
      driver.record.id (Option.value ~default:"<not set: run monty swarm init --check CMD>" ledger.check)
      running ledger.max_workers (List.length ledger.children) ledger.max_spawns ledger.worker_profile;
    List.iter (fun (workspace, check) -> Fmt.pr "Check for %s: %s\n" workspace check) ledger.checks;
    Fmt.pr "%-16s %-9s %-17s %-7s %-8s %s\n" "NAME" "STATE" "JOB" "LANDED" "PENDING" "BRANCH";
    List.iter
      (fun ((child : Swarm_ledger.child), job_status, in_driver, pending) ->
        Fmt.pr "%-16s %-9s %-17s %-7s %-8d %s\n" child.name
          (Swarm_ledger.state_to_string child.state) job_status
          (if in_driver then "yes" else "no") pending child.branch;
        (* The row shows the first workspace; a child spanning repositories lists them all. *)
        (match child_workspaces workspaces child with
        | [] | [ _ ] -> ()
        | owned ->
            List.iter
              (fun (workspace : Swarm_ledger.workspace) ->
                Fmt.pr "  workspace: %s | %s\n" workspace.repo workspace.branch)
              owned);
        Option.iter (fun error -> Fmt.pr "  error: %s\n" (Run_handoff.compact_summary error)) child.error;
        Option.iter (fun path -> Fmt.pr "  last check: %s\n" path) child.last_check)
      rows);
  Ok ()

let clean_worktree ~label path =
  Wt.ensure_clean ~worktree:path
  |> Result.map_error (fun message ->
         Printf.sprintf "%s worktree must be clean before landing: %s" label message)

(* One repository of a landing: the driver's side, the child's side and the
   check that gates the merge. *)
type landing = {
  owned : Swarm_ledger.workspace;
  driver_branch : string;
  driver_worktree : string;
  child_worktree : string;
  check : string;
}

(* Landing is all or nothing across the child's repositories: the driver's
   branch is merged into the child everywhere, then every check runs, and only
   when all pass is the child merged into the driver. Those final merges cannot
   conflict because each child already contains its driver branch. *)
let land_child driver ~name ~message =
  let* ledger, child = require_child driver name in
  let* () =
    match child.state with
    | Swarm_ledger.Finished -> Ok ()
    | state ->
        Error
          (Printf.sprintf "swarm child %s is %s, not finished; only a finished child can be landed"
             name (Swarm_ledger.state_to_string state))
  in
  let* workspaces = driver_workspaces driver in
  let* record = active_record ~home:driver.home child in
  let* landings =
    Json_fields.fold_results (child_workspaces workspaces child) ~init:[]
      ~f:(fun acc (owned : Swarm_ledger.workspace) ->
        let* source = driver_workspace workspaces owned.repo in
        let* check = check_for workspaces ledger owned.repo in
        let* driver_worktree =
          materialized_worktree ~owner:("swarm driver " ^ driver.record.id) source
        in
        let* _ = Wt.validate_worktree ~repo:owned.repo ~branch:source.branch driver_worktree in
        let* () = clean_worktree ~label:"driver" driver_worktree in
        let* child_worktree =
          match
            List.find_opt
              (fun (workspace : Job_store.workspace_state) -> String.equal workspace.repo owned.repo)
              record.workspaces
          with
          | Some { worktree = Some path; _ } when Sys.file_exists path -> Ok path
          | _ ->
              Error
                (Printf.sprintf "swarm child %s has no materialized worktree for %s" name owned.repo)
        in
        Ok ({ owned; driver_branch = source.branch; driver_worktree; child_worktree; check } :: acc))
    |> Result.map List.rev
  in
  let each step = Json_fields.fold_results landings ~init:() ~f:(fun () landing -> step landing) in
  let message = Option.value ~default:(Printf.sprintf "Land swarm child %s" name) message in
  let artifacts = Swarm_ledger.check_artifacts_dir (worker_dir driver) name in
  (* Timestamps have one-second resolution, so a landing retried within the
     same second gets a numbered name instead of replacing the earlier log. *)
  let log =
    let stamp = "check-" ^ Worker_memory.now_utc () in
    let rec unused attempt =
      let name = if attempt = 1 then stamp else Printf.sprintf "%s-%d" stamp attempt in
      let path = Filename.concat artifacts (name ^ ".log") in
      if Sys.file_exists path then unused (attempt + 1) else path
    in
    unused 1
  in
  Worker_lock.with_lock ~home:driver.home ~record (fun () ->
      let* () =
        each (fun landing ->
            let* _ =
              Wt.validate_worktree ~repo:landing.owned.repo ~branch:landing.owned.branch
                landing.child_worktree
            in
            clean_worktree ~label:"child" landing.child_worktree)
      in
      let* () =
        each (fun landing ->
            match git ~cwd:landing.child_worktree [ "merge"; "--no-edit"; landing.driver_branch ] with
            | Ok _ -> Ok ()
            | Error merge_error ->
                ignore (git ~cwd:landing.child_worktree [ "merge"; "--abort" ]);
                let* () =
                  mark_failed driver name
                    (Printf.sprintf "merging the driver branch conflicted in %s: %s" landing.owned.repo
                       merge_error)
                in
                Error
                  (Printf.sprintf
                     "merging driver branch %s into child %s conflicted in %s; resume the child with a brief to resolve it:\n%s"
                     landing.driver_branch name landing.owned.repo merge_error))
      in
      let* () = State_store.ensure_real_directory ~label:"swarm check artifacts" ~mode:0o700 artifacts in
      (* Every check runs even after one fails, so one resume can fix them all. *)
      let* results =
        Json_fields.fold_results landings ~init:[] ~f:(fun acc landing ->
            let* result = Process.run_capture ~cwd:landing.child_worktree landing.check in
            Ok ((landing, result) :: acc))
        |> Result.map List.rev
      in
      let* () =
        State_store.write_file_atomic ~path:log ~perm:0o600
          (String.concat "\n"
             (List.map
                (fun (landing, (result : Process.captured)) ->
                  Printf.sprintf "# %s\n$ %s\n%s\n[%s]\n" landing.owned.repo landing.check
                    result.stdout (Process.status_to_string result.status))
                results))
      in
      let* () = update_child driver name (fun child -> { child with last_check = Some log }) in
      match List.filter (fun (_, (result : Process.captured)) -> result.status <> `Exited 0) results with
      | (failed, result) :: _ ->
          Error
            (Printf.sprintf "landing check %s for swarm child %s in %s; output: %s"
               (Process.status_to_string result.status) name failed.owned.repo log)
      | [] ->
          let* landed =
            Json_fields.fold_results landings ~init:[] ~f:(fun acc landing ->
                match
                  git ~cwd:landing.driver_worktree
                    [ "merge"; "--no-ff"; "-m"; message; landing.owned.branch ]
                with
                | Ok _ ->
                    let* commit = git ~cwd:landing.owned.repo [ "rev-parse"; landing.owned.branch ] in
                    Ok ({ landing.owned with landed_commit = Some commit } :: acc)
                | Error merge_error ->
                    ignore (git ~cwd:landing.driver_worktree [ "merge"; "--abort" ]);
                    Error
                      (Printf.sprintf
                         "merging child %s into driver branch %s failed in %s; fix the cause and land again, which skips repositories that are already merged:\n%s"
                         name landing.driver_branch landing.owned.repo merge_error))
            |> Result.map List.rev
          in
          let* () =
            update_child driver name (fun child ->
                { child with
                  state = Swarm_ledger.Landed;
                  workspaces = landed;
                  landed_commit = Option.bind (List.nth_opt landed 0) (fun first -> first.landed_commit);
                  error = None })
          in
          Fmt.pr "Landed swarm child %s\n" name;
          List.iter2
            (fun landing (workspace : Swarm_ledger.workspace) ->
              Fmt.pr "  %s: %s into %s\n" workspace.repo
                (Option.value ~default:"" workspace.landed_commit) landing.driver_branch)
            landings landed;
          Fmt.pr "Check output: %s\n" log;
          Ok ())

let retire ~wt_command driver ~name ~force =
  let* _, child = require_child driver name in
  let* () =
    match child.state with
    | Swarm_ledger.Retired -> Error (Printf.sprintf "swarm child %s is already retired" name)
    | Swarm_ledger.Running when not force ->
        Error (Printf.sprintf "swarm child %s is still running; wait for its handoff or use --force" name)
    | _ -> Ok ()
  in
  let* () =
    if force || head_in_driver driver child then Ok ()
    else
      Error
        (Printf.sprintf
           "swarm child %s has commits that are not in the driver branch; land it first or retire it with --force to discard them"
           name)
  in
  let* records = Job_store.load_all ~home:driver.home in
  let* record = find_record records child in
  let* () =
    match record with
    | None -> update_child driver name (fun child -> { child with state = Swarm_ledger.Retired })
    | Some record ->
        Done.retire_child_record ~home:driver.home ~wt_command ~force
          ~driver_dir:(worker_dir driver) record child
  in
  Fmt.pr "Retired swarm child %s\n" name;
  Ok ()
