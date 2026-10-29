type level = Pass | Warn | Fail

type check = {
  name : string;
  level : level;
  message : string;
  recovery : string list;
}

let level_to_string = function Pass -> "PASS" | Warn -> "WARN" | Fail -> "FAIL"

let check_command find_command ~required ~name ~command ~recovery =
  match find_command command with
  | Ok path -> { name; level = Pass; message = path; recovery = [] }
  | Error message ->
      {
        name;
        level = (if required then Fail else Warn);
        message;
        recovery;
      }

let transition_check ~home ~wt_command ~backend (record : Job_store.record) =
  let backend =
    match backend with
    | Terminal.Dry_run -> Terminal.Rex
    | backend -> backend
  in
  match record.transition with
  | Some transition when transition.operation = Job_store.Complete ->
      Some
        {
          name = "worker " ^ record.id;
          level = Warn;
          message = "incomplete completing transition at " ^ record.path;
          recovery =
            [ Printf.sprintf "monty done %s --home %s --wt-command %s"
                (Shell.quote record.id) (Shell.quote home) (Shell.quote wt_command) ];
        }
  | Some transition when transition.operation = Job_store.Reopen ->
      Some
        {
          name = "worker " ^ record.id;
          level = Warn;
          message = "incomplete reopening transition at " ^ record.path;
          recovery =
            [ Printf.sprintf
                "monty resume --archived %s --home %s --terminal %s --worktree %s --wt-command %s"
                (Shell.quote record.id) (Shell.quote home)
                (Terminal.backend_to_string backend)
                (Shell.quote record.worktree_mode) (Shell.quote wt_command) ];
        }
  | Some _ | None -> None

let launch_state_check ~home (record : Job_store.record) =
  match record.transition with
  | Some _ -> None
  | None ->
      let state = String.lowercase_ascii record.status in
      if List.mem state [ "prepared"; "launch-failed"; "launch-requested" ] then
        let recovery =
          Printf.sprintf "monty resume %s --home %s" (Shell.quote record.id)
            (Shell.quote home)
        in
        Some
          {
            name = "worker " ^ record.id;
            level = Warn;
            message = state ^ " at " ^ record.path;
            recovery = [ recovery ];
          }
      else None

let duplicate_identity_checks records =
  let check label identities =
    identities
    |> List.filter_map (fun (identity, record) ->
           Option.map (fun identity -> (identity, record)) identity)
    |> List.map fst |> List.sort_uniq String.compare
    |> List.filter_map (fun identity ->
           let claimants =
             identities
             |> List.filter_map (fun (candidate, record) ->
                    match candidate with
                    | Some candidate when String.equal candidate identity ->
                        Some record
                    | _ -> None)
             |> List.sort (fun left right -> String.compare left.Job_store.path right.path)
           in
           match claimants with
           | _ :: _ :: _ ->
               Some
                 {
                   name = "worker identity";
                   level = Fail;
                   message =
                     Printf.sprintf "duplicate %s %S at %s" label identity
                       (claimants
                       |> List.map (fun record -> record.Job_store.path)
                       |> String.concat ", ");
                   recovery =
                     [ "Repair or remove the duplicate durable worker records, then rerun monty doctor." ];
                 }
           | _ -> None)
  in
  let ids =
    List.map (fun record -> (Some record.Job_store.id, record)) records
  in
  let repo_branches =
    records
    |> List.concat_map (fun record ->
           record.Job_store.job.Job.workspaces
           |> List.map (fun (workspace : Job.workspace) ->
                  let repo =
                    try Unix.realpath workspace.repo
                    with Unix.Unix_error _ ->
                      Shell.normalize (Shell.abs_path workspace.repo)
                  in
                  let branch = Option.value ~default:"" workspace.branch in
                  (Some (repo ^ " + " ^ branch), record)))
  in
  let task_links =
    List.map
      (fun record -> (record.Job_store.job.Job.task_key, record))
      records
  in
  check "worker id" ids @ check "repo+branch" repo_branches
  @ check "task link" task_links

(* Swarm children need a live driver, and a ledger entry that says running
   needs a supervisor holding the child's lock. *)
let swarm_checks ~home records =
  let active = List.filter (fun record -> not (Job_store.is_archived record)) records in
  let find_active id =
    List.find_opt (fun record -> String.equal record.Job_store.id id) active
  in
  let orphans =
    active
    |> List.filter_map (fun (record : Job_store.record) ->
           match record.job.Job.swarm_parent with
           | Some parent when find_active parent = None ->
               Some
                 {
                   name = "swarm child " ^ record.id;
                   level = Warn;
                   message = Printf.sprintf "driver %s is missing or archived at %s" parent record.path;
                   recovery =
                     [ Printf.sprintf "monty done %s --force --home %s" (Shell.quote record.id)
                         (Shell.quote home) ];
                 }
           | _ -> None)
  in
  let stale =
    active
    |> List.concat_map (fun (driver : Job_store.record) ->
           match Swarm_ledger.load_opt ~worker_dir:driver.worker_dir with
           | Error message ->
               [ { name = "swarm ledger " ^ driver.id; level = Fail; message;
                   recovery = [ "Repair or remove the reported swarm ledger, then rerun monty doctor." ] } ]
           | Ok None -> []
           | Ok (Some ledger) ->
               Swarm_ledger.running ledger
               |> List.filter_map (fun (child : Swarm_ledger.child) ->
                      let idle =
                        match find_active child.id with
                        | None -> true
                        | Some record -> (
                            match Worker_lock.is_busy ~home ~record with
                            | Ok busy -> not busy
                            | Error _ -> false)
                      in
                      if not idle then None
                      else
                        Some
                          {
                            name = "swarm child " ^ child.id;
                            level = Warn;
                            message =
                              Printf.sprintf
                                "ledger of driver %s records %s as running but no supervisor holds its worker lock"
                                driver.id child.name;
                            recovery =
                              [ "From the driver session: monty swarm status, then monty swarm resume "
                                ^ Shell.quote child.name ^ " --brief FILE or monty swarm retire "
                                ^ Shell.quote child.name ^ " --force" ];
                          }))
  in
  orphans @ stale

let state_checks ?(backend = Terminal.Rex) ~home ~wt_command () =
  match Job_store.scan ~home with
  | Error message ->
      [
        {
          name = "worker state";
          level = Fail;
          message;
          recovery = [ "Repair the reported Monty state path, then rerun monty doctor." ];
        };
      ]
  | Ok scan ->
      let warning_checks =
        List.map
          (fun message ->
            {
              name = "worker state";
              level = Fail;
              message;
              recovery = [ "Repair or remove the reported unsafe worker record." ];
            })
          scan.Job_store.warnings
      in
      let record_checks =
        scan.records
        |> List.filter_map (fun record ->
               match transition_check ~home ~wt_command ~backend record with
               | Some _ as check -> check
               | None -> launch_state_check ~home record)
      in
      let identity_checks = duplicate_identity_checks scan.records in
      let swarm_checks = swarm_checks ~home scan.records in
      if warning_checks = [] && record_checks = [] && identity_checks = [] && swarm_checks = [] then
        [ { name = "worker state"; level = Pass; message = "records are readable and no recovery is pending"; recovery = [] } ]
      else warning_checks @ identity_checks @ record_checks @ swarm_checks

let checks ?(find_command = Process.command_exists_with_arguments)
    ~home ~harness ~harness_command ~wt_command
    ~backend ~worktree_mode () =
  let home = Shell.normalize (Shell.abs_path home) in
  let required =
    [ check_command find_command ~required:true ~name:(Harness.to_string harness)
        ~command:harness_command
        ~recovery:
          [ Printf.sprintf
              "Install the configured %s executable or pass --%s-command COMMAND."
              (Harness.to_string harness) (Harness.to_string harness) ] ]
  in
  let required = check_command find_command ~required:true ~name:"server-runtime-python" ~command:"python3" ~recovery:["Install Python 3.9+ for Monty server runtime ownership guards."] :: required in
  let required =
    match worktree_mode with
    | Launcher.Never -> required
    | Launcher.Always ->
        required
        @ [
            check_command find_command ~required:true ~name:"wt" ~command:wt_command
              ~recovery:[ "Install the configured wt executable or pass --wt-command COMMAND." ];
          ]
  in
  let required =
    match backend with
    | Terminal.Dry_run -> required
    | Terminal.Rex -> (
        let cli =
          check_command find_command ~required:true ~name:"rex" ~command:"rex"
            ~recovery:[ "Install Rex (its CLI ships inside the Rex app) or use --terminal dry-run." ]
        in
        (* The server runs with the Rex app; without it no tab can be created. *)
        let server =
          match cli.level with
          | Pass -> (
              let server = (Rex.context_of_env ()).Rex.server in
              match Rex.endpoint ?server () with
              | Ok endpoint ->
                  (* A persisted rex-session must resolve on this server, or
                     every tab requested from outside Rex fails in preflight. *)
                  let profile = Mantle.profile Sys.getenv_opt in
                  let scope = match profile with Some p -> " for profile " ^ p | None -> "" in
                  let session =
                    match Settings.load ~home with
                    | Ok settings -> (
                        match Settings.rex_session_for settings profile with
                        | Some setting -> (
                            match Rex.resolve_session ?server ~endpoint setting with
                            | Ok session ->
                                [ { name = "rex-session"; level = Pass;
                                    message = Printf.sprintf "%s (%s)%s" setting session scope; recovery = [] } ]
                            | Error message ->
                                [ { name = "rex-session"; level = Fail; message;
                                    recovery =
                                      [ "Run `rex ls`, then `monty settings set rex-session <session>` or clear it with `monty settings set rex-session ''`." ] } ])
                        | None -> [])
                    | Error _ -> []
                  in
                  { name = "rex-server"; level = Pass; message = endpoint; recovery = [] } :: session
              | Error message ->
                  [ { name = "rex-server"; level = Fail; message;
                      recovery = [ "Open the Rex app, or set REX_SERVER to a running server, or use --terminal dry-run." ] } ])
          | Warn | Fail -> []
        in
        required @ (cli :: server))
    | Terminal.Ghostty ->
        required
        @ [
            check_command find_command ~required:true ~name:"ghostty" ~command:"ghostty"
              ~recovery:[ "Install Ghostty or use --terminal dry-run." ];
            check_command find_command ~required:true ~name:"osascript" ~command:"osascript"
              ~recovery:[ "Install osascript or use --terminal dry-run." ];
          ]
  in
  required
  @ [
      check_command find_command ~required:false ~name:"gh" ~command:"gh"
        ~recovery:[ "Install gh to use GitHub issue metadata." ];
      check_command find_command ~required:false ~name:"sdef" ~command:"sdef"
        ~recovery:[ "Install sdef to inspect the Ghostty AppleScript dictionary." ];
    ]
  @ state_checks ~backend ~home ~wt_command ()

let width minimum values =
  List.fold_left (fun width value -> max width (String.length value)) minimum values

let pad_right width value = value ^ String.make (max 0 (width - String.length value)) ' '

let render checks =
  let level_width = width 5 (List.map (fun check -> level_to_string check.level) checks) in
  let name_width = width 4 (List.map (fun check -> check.name) checks) in
  let lines =
    (pad_right level_width "LEVEL" ^ "  " ^ pad_right name_width "CHECK" ^ "  MESSAGE")
    :: (String.make level_width '-' ^ "  " ^ String.make name_width '-' ^ "  -------")
       :: List.concat_map
            (fun check ->
              (pad_right level_width (level_to_string check.level) ^ "  "
             ^ pad_right name_width check.name ^ "  " ^ check.message)
              :: List.map (fun command -> "Recovery: " ^ command) check.recovery)
            checks
  in
  String.concat "\n" lines ^ "\n"

let exit_code checks =
  if List.exists (fun check -> check.level = Fail) checks then 1 else 0

let run ~home ~harness ~harness_command ~wt_command ~backend ~worktree_mode =
  let checks =
    checks ~home ~harness ~harness_command ~wt_command ~backend ~worktree_mode
      ()
  in
  Fmt.pr "%s" (render checks);
  if exit_code checks = 0 then Ok () else Error "doctor found failing checks"
