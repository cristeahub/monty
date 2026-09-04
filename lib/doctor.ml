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

let transition_check ~home ~wt_command (record : Job_store.record) =
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
                "monty resume --archived %s --home %s --terminal ghostty --worktree %s --wt-command %s"
                (Shell.quote record.id) (Shell.quote home)
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
          match (record.container_worker, state) with
          | Some _, "prepared" ->
              Printf.sprintf "monty headless run %s --home %s"
                (Shell.quote record.id) (Shell.quote home)
          | Some _, "launch-requested" ->
              Printf.sprintf "monty headless resume %s --home %s"
                (Shell.quote record.id) (Shell.quote home)
          | Some _, "launch-failed" ->
              Printf.sprintf "monty headless prepare-many --manifest %s --home %s"
                (Shell.quote (Filename.concat record.run_dir "jobs.json"))
                (Shell.quote home)
          | None, _ ->
              Printf.sprintf "monty resume %s --home %s" (Shell.quote record.id)
                (Shell.quote home)
          | Some _, _ -> assert false
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

let state_checks ~home ~wt_command =
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
               match transition_check ~home ~wt_command record with
               | Some _ as check -> check
               | None -> launch_state_check ~home record)
      in
      let identity_checks = duplicate_identity_checks scan.records in
      if warning_checks = [] && record_checks = [] && identity_checks = [] then
        [ { name = "worker state"; level = Pass; message = "records are readable and no recovery is pending"; recovery = [] } ]
      else warning_checks @ identity_checks @ record_checks

let active_container_workers ~home =
  match Job_store.scan ~home with
  | Error _ -> false
  | Ok scan ->
      List.exists
        (fun record ->
          not (Job_store.is_archived record)
          && Option.is_some record.Job_store.container_worker)
        scan.records

let container_check find_command ~home =
  let command = Container_worker.command () in
  match find_command command with
  | Error message ->
      { name = "apple-container"; level = Fail; message;
        recovery = [ "Install Apple container and start its service." ] }
  | Ok _ -> (
      match Container_worker.preflight_image ~home with
      | Ok digest ->
          { name = "apple-container"; level = Pass; message = digest; recovery = [] }
      | Error message ->
          { name = "apple-container"; level = Fail; message;
            recovery =
              [ Printf.sprintf
                  "Build %s explicitly, then run monty container-image register --home %s."
                  Container_worker.image (Shell.quote home) ] })

let checks ?(find_command = Process.command_exists_with_arguments)
    ?(container_workers = false) ~home ~harness ~harness_command ~wt_command
    ~backend ~worktree_mode () =
  let home = Shell.normalize (Shell.abs_path home) in
  let needs_container = container_workers || active_container_workers ~home in
  let required =
    if container_workers then [ container_check find_command ~home ]
    else
      [ check_command find_command ~required:true ~name:(Harness.to_string harness)
          ~command:harness_command
          ~recovery:
            [ Printf.sprintf
                "Install the configured %s executable or pass --%s-command COMMAND."
                (Harness.to_string harness) (Harness.to_string harness) ] ]
  in
  let required =
    match (container_workers, worktree_mode) with
    | true, _ | false, Launcher.Never -> required
    | false, Launcher.Always ->
        required
        @ [
            check_command find_command ~required:true ~name:"wt" ~command:wt_command
              ~recovery:[ "Install the configured wt executable or pass --wt-command COMMAND." ];
          ]
  in
  let required =
    match (container_workers, backend) with
    | true, _ | false, Terminal.Dry_run -> required
    | false, Terminal.Ghostty ->
        required
        @ [
            check_command find_command ~required:true ~name:"ghostty" ~command:"ghostty"
              ~recovery:[ "Install Ghostty or use --terminal dry-run." ];
            check_command find_command ~required:true ~name:"osascript" ~command:"osascript"
              ~recovery:[ "Install osascript or use --terminal dry-run." ];
          ]
  in
  (if needs_container && not container_workers then
     required @ [ container_check find_command ~home ]
   else required)
  @ [
      check_command find_command ~required:false ~name:"gh" ~command:"gh"
        ~recovery:[ "Install gh to use GitHub issue metadata." ];
      check_command find_command ~required:false ~name:"sdef" ~command:"sdef"
        ~recovery:[ "Install sdef to inspect the Ghostty AppleScript dictionary." ];
    ]
  @ state_checks ~home ~wt_command

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

let run ~home ~harness ~harness_command ~wt_command ~backend ~worktree_mode
    ~container_workers =
  let checks =
    checks ~home ~harness ~harness_command ~wt_command ~backend ~worktree_mode
      ~container_workers ()
  in
  Fmt.pr "%s" (render checks);
  if exit_code checks = 0 then Ok () else Error "doctor found failing checks"
