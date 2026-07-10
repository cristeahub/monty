type level = Pass | Warn | Fail

type check = {
  name : string;
  level : level;
  message : string;
  recovery : string list;
}

type operations = { find_command : string -> (string, string) result }

let default_operations =
  {
    find_command =
      (fun command ->
        match Process.command_exists_with_arguments command with
        | Ok path -> Ok path
        | Error message -> Error message);
  }

let level_to_string = function Pass -> "PASS" | Warn -> "WARN" | Fail -> "FAIL"

let check_command operations ~required ~name ~command ~recovery =
  match operations.find_command command with
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
        Some
          {
            name = "worker " ^ record.id;
            level = Warn;
            message = state ^ " at " ^ record.path;
            recovery =
              [ Printf.sprintf "monty resume %s --home %s" (Shell.quote record.id)
                  (Shell.quote home) ];
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
    List.map
      (fun record ->
        let repo =
          try Unix.realpath record.Job_store.job.Job.repo
          with Unix.Unix_error _ ->
            Shell.normalize (Shell.abs_path record.job.Job.repo)
        in
        let branch = Option.value ~default:"" record.job.Job.branch in
        (Some (repo ^ " + " ^ branch), record))
      records
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

let checks ?(operations = default_operations) ~home ~pi_command ~wt_command ~backend
    ~worktree_mode () =
  let home = Shell.normalize (Shell.abs_path home) in
  let required =
    [
      check_command operations ~required:true ~name:"pi" ~command:pi_command
        ~recovery:[ "Install the configured pi executable or pass --pi-command COMMAND." ];
    ]
  in
  let required =
    match worktree_mode with
    | Launcher.Never -> required
    | Launcher.Always ->
        required
        @ [
            check_command operations ~required:true ~name:"wt" ~command:wt_command
              ~recovery:[ "Install the configured wt executable or pass --wt-command COMMAND." ];
          ]
  in
  let required =
    match backend with
    | Terminal.Dry_run -> required
    | Terminal.Ghostty ->
        required
        @ [
            check_command operations ~required:true ~name:"ghostty" ~command:"ghostty"
              ~recovery:[ "Install Ghostty or use --terminal dry-run." ];
            check_command operations ~required:true ~name:"osascript" ~command:"osascript"
              ~recovery:[ "Install osascript or use --terminal dry-run." ];
          ]
  in
  required
  @ [
      check_command operations ~required:false ~name:"gh" ~command:"gh"
        ~recovery:[ "Install gh to use GitHub issue metadata." ];
      check_command operations ~required:false ~name:"sdef" ~command:"sdef"
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

let run ~home ~pi_command ~wt_command ~backend ~worktree_mode =
  let checks = checks ~home ~pi_command ~wt_command ~backend ~worktree_mode () in
  Fmt.pr "%s" (render checks);
  if exit_code checks = 0 then Ok () else Error "doctor found failing checks"
