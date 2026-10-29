let ( let* ) = Result.bind

let prepare_schema = "monty:headless-prepare:v2"
let codex_run_schema = "monty:headless-codex-run:v1"
let codex_run_many_schema = "monty:headless-codex-run-many:v1"
let attempt_schema = "monty:headless-attempt:v2"
let started_schema = "monty:headless-start:v1"

type prepared_job = {
  id : string;
  title : string;
  branch : string;
  worktree : string option;
  workspaces : Job_store.workspace_state list;
  worker_dir : string;
  status : string;
  profile : Agent_profile.t;
}

type dispatch = {
  id : string;
  title : string;
  repo : string;
  branch : string;
  worktree : string;
  workspaces : Job_store.workspace_state list;
  worker_dir : string;
  instructions : string;
  context : string;
  home : string;
  profile : Agent_profile.t;
  swarm_child : (string * string) option;
}

type review_path = {
  review : Agent_profile.review;
  output : string;
}

type attempt_paths = {
  id : string;
  root : string;
  implementation : string;
  reviews : review_path list;
  final : string;
  prompts : string;
  events : string;
  logs : string;
  descriptor : string;
}

type begin_plan = {
  options : Launcher.options;
  prepared : Launcher.prepared;
  record : Job_store.record;
}

type phase_inputs = {
  instructions_contents : string;
  context_contents : string;
}

let prepared_job_json (job : prepared_job) =
  `Assoc
    ([ ("id", `String job.id);
       ("title", `String job.title);
       ("branch", `String job.branch);
       ("worktree", Option.fold ~none:`Null ~some:(fun path -> `String path) job.worktree);
       ("workspaces", `List (List.map Job_store.json_of_workspace job.workspaces));
       ("worker_dir", `String job.worker_dir);
       ("status", `String job.status) ]
    @ [ ("agent_profile", `String job.profile.id);
        ("stages", `String (Agent_profile.stage_summary job.profile)) ])

let prepare_json ~harness ~codex_yolo jobs =
  `Assoc
    [ ("schema", `String prepare_schema);
      ("harness", `String (Harness.to_string harness));
      ("codex_yolo", `Bool (harness = Harness.Codex && codex_yolo));
      ("jobs", `List (List.map prepared_job_json jobs)) ]

let fresh_attempt_id () =
  let micros = Int64.of_float (Unix.gettimeofday () *. 1_000_000.) in
  Printf.sprintf "attempt-%Ld-%d" micros (Unix.getpid ())

let attempt_paths (dispatch : dispatch) attempt_id =
  let root =
    Filename.concat dispatch.worker_dir
      (Filename.concat "artifacts" (Filename.concat "headless" attempt_id))
  in
  let reviews_dir = Filename.concat root "reviews" in
  {
    id = attempt_id;
    root;
    implementation = Filename.concat root "implementation.md";
    reviews =
      List.map
        (fun (review : Agent_profile.review) ->
          { review;
            output = Filename.concat reviews_dir (review.id ^ ".md") })
        dispatch.profile.reviews;
    final = Filename.concat root "final.md";
    prompts = Filename.concat root "prompts";
    events = Filename.concat root "events";
    logs = Filename.concat root "logs";
    descriptor = Filename.concat root "attempt.json";
  }

let ensure_attempt_directories (dispatch : dispatch) (paths : attempt_paths) =
  let directories =
    [ Filename.concat dispatch.worker_dir "artifacts";
      Filename.concat (Filename.concat dispatch.worker_dir "artifacts")
        "headless";
      paths.root;
      paths.prompts;
      paths.events;
      paths.logs ]
    @
    (if paths.reviews = [] then []
     else [ Filename.concat paths.root "reviews" ])
  in
  List.fold_left
    (fun result directory ->
      let* () = result in
      State_store.ensure_real_directory ~label:"headless artifact directory"
        ~mode:0o700 directory)
    (Ok ()) directories

let attempt_descriptor_json (dispatch : dispatch) (paths : attempt_paths) source =
  `Assoc
    ([ ("schema", `String attempt_schema);
       ("attempt_id", `String paths.id);
       ("worker_id", `String dispatch.id);
       ("source", `String source);
       ("created_at", `String (Worker_memory.now_utc ())) ]
    @ [ ("agent_profile", `String dispatch.profile.id) ])

let prepare_attempt (plan : begin_plan) (dispatch : dispatch)
    (paths : attempt_paths) source =
  State_store.with_lock ~home:dispatch.home (fun () ->
      let* current =
        Job_store.parse_job_file ~home:plan.options.home
          plan.prepared.state_path.job_file
      in
      let* () =
        match current.transition with
        | None -> Ok ()
        | Some transition ->
            Error
              (Printf.sprintf
                 "worker %s entered a %s transition before its headless attempt was prepared"
                 current.id (Job_store.operation_name transition.operation))
      in
      let* () =
        if String.equal current.status plan.record.status then Ok ()
        else
          Error
            (Printf.sprintf
               "worker %s changed from %S to %S before its headless attempt was prepared"
               current.id plan.record.status current.status)
      in
      let* () = ensure_attempt_directories dispatch paths in
      State_store.write_json_atomic ~path:paths.descriptor
        (attempt_descriptor_json dispatch paths source))

(* Swarm children commit on their own branch; every other chain leaves the
   tree unstaged. Reviewers always get the read-only list. *)
let mutation_prohibitions ?(writable = true) (dispatch : dispatch) =
  let commit_rule =
    match dispatch.swarm_child with
    | Some _ when writable ->
        [ "Commit all your work on the current branch as small imperative commits; leave the tree clean (git status prints nothing) when you finish.";
          "Do not run monty swarm." ]
    | _ -> [ "Do not stage or commit changes." ]
  in
  String.concat "\n"
    ([ "Do not run monty done.";
       "Do not invoke /review or launch subagents.";
       "Do not create, switch, or remove worktrees.";
       "Do not modify job.json or Monty task, settings, or project state." ]
    @ commit_rule
    @ [ "Do not push, open a pull request, submit a review, post comments, or perform any other remote write." ])

let implementation_task (dispatch : dispatch) =
  let worktree_setup =
    match dispatch.workspaces with
    | [ _ ] ->
        "You are in a new worktree. Install the project in this worktree before starting the task. Do not create or use symlinks into the main repository."
    | _ ->
        "You are in the first of multiple Monty worktrees. The Monty instructions list every absolute workspace. Install and modify the relevant projects in those exact worktrees. Do not create or use symlinks into the main repositories."
  in
  String.concat "\n\n"
    [ worktree_setup;
      Printf.sprintf "Implement Monty worker %s: %s." dispatch.id dispatch.title;
      "Read only the supplied Monty instructions and task context before inspecting the repository.";
      "Use the absolute MONTY_JOB_FILE path in the instructions to read the persisted workspace map; its workspaces array is authoritative.";
      "You are the only writer in this phase.";
      dispatch.profile.implementation;
      "Include important implementation discoveries and handoff notes in the final response so Monty can persist them.";
      mutation_prohibitions dispatch;
      "Finish with a compact handoff containing changed files, validation commands and results, unresolved risks, and a concise diff summary." ]

let reviewer_task (dispatch : dispatch) (review : Agent_profile.review) implementation_handoff =
  String.concat "\n\n"
    [ Printf.sprintf "Independently review Monty worker %s: %s." dispatch.id
        dispatch.title;
      String.concat "\n" Reviewer_prompt.review_preamble;
      review.instructions;
      Reviewer_prompt.read_only_worktree_rule;
      "Use shell commands only for read-only inspection and non-mutating validation.";
      mutation_prohibitions ~writable:false dispatch;
      "Implementation handoff for orientation only; verify every claim against the worktree:";
      implementation_handoff;
      String.concat "\n" Reviewer_prompt.review_output_requirements ]

let fixer_task (dispatch : dispatch) (paths : review_path list) reports implementation_handoff =
  let reports =
    List.map2 (fun path report -> [ path.review.title ^ ":"; report ]) paths reports
    |> List.concat
  in
  String.concat "\n\n"
    ([ Printf.sprintf "Finalize Monty worker %s: %s." dispatch.id dispatch.title;
      "Read the supplied Monty instructions and task context, inspect every supplied worktree, and verify the implementation handoff against the actual code.";
      "Use the absolute MONTY_JOB_FILE path in the Monty instructions to resolve every persisted workspace path.";
      "You are the only writer in this phase.";
      Option.value ~default:"" dispatch.profile.fix;
      "Include changes, validation, and the final handoff in the final response so Monty can persist them.";
      mutation_prohibitions dispatch;
      "Implementation handoff:";
      implementation_handoff ]
    @ reports
    @ [ "Finish with changed files, "
        ^ (if paths = [] then "" else "accepted and rejected findings, ")
        ^ "validation commands and results, residual risks, and git status confirming "
        ^ (if dispatch.swarm_child = None then "nothing is staged." else "a clean, fully committed tree.") ])

let print_json json = Fmt.pr "%s\n" (Yojson.Safe.pretty_to_string json)

let status_before_prepare prepared =
  match prepared.Launcher.existing with
  | Launcher.New -> "prepared"
  | Launcher.Retryable status -> status
  | Launcher.Requested -> "launch-requested"

let planned_job (prepared : Launcher.prepared) =
  {
    id = prepared.id;
    title = prepared.job.Job.title;
    branch = prepared.branch;
    worktree = None;
    workspaces =
      List.map
        (fun (workspace : Launcher.prepared_workspace) ->
          Job_store.
            {
              repo = workspace.repo;
              branch = Some workspace.branch;
              worktree = None;
            })
        prepared.workspaces;
    worker_dir = prepared.worker_dir;
    status = "planned";
    profile = prepared.profile;
  }

let ensure_worktree options (prepared : Launcher.prepared) =
  let expected_status = status_before_prepare prepared in
  let materialized =
    Launcher.materialize_workspaces ~expected_statuses:[ expected_status ]
      options prepared
  in
  match materialized with
  | Error message -> Error message
  | Ok workspaces ->
      let worktree =
        match workspaces with
        | first :: _ -> Option.value ~default:first.repo first.worktree
        | [] -> prepared.repo
      in
      let* () =
        Launcher.update_launch_state options prepared
          ~expected_statuses:[ expected_status ] ~status:"prepared"
          ~worktree ~workspaces ()
      in
      Ok
        {
          id = prepared.id;
          title = prepared.job.Job.title;
          branch = prepared.branch;
          worktree = Some worktree;
          workspaces;
          worker_dir = prepared.worker_dir;
          status = "prepared";
          profile = prepared.profile;
        }

let prepare_many ~dry_run options indexed_jobs =
  let options =
    { options with
      Launcher.backend = Terminal.Dry_run;
      worktree_mode = Launcher.Always }
  in
  let* prepared = Launcher.preflight_batch options indexed_jobs in
  let* () = if List.exists (fun worker -> worker.Launcher.profile.reviews<>[]) prepared then
    Harness.check_review_command options.harness options.harness_command else Ok () in
  let* () = Launcher.check_dependencies options in
  let* () =
    match
      List.find_opt
        (fun worker -> worker.Launcher.existing = Launcher.Requested)
        prepared
    with
    | None -> Ok ()
    | Some worker ->
        Error
          (Printf.sprintf
             "worker %s is already launch-requested; use headless resume when another headless run is intentional"
             worker.Launcher.id)
  in
  if dry_run then
    Ok
      (prepare_json ~harness:options.harness ~codex_yolo:options.codex_yolo
         (List.map planned_job prepared))
  else
    let* reserved =
      Launcher.reserve_batch ~reject_requested:true options prepared
    in
    let rec materialize acc = function
      | [] -> Ok (List.rev acc)
      | worker :: rest ->
          let* job =
            Launcher.with_prepared_worker_lock options worker
              ~expected_statuses:[ status_before_prepare worker ]
              (fun () -> ensure_worktree options worker)
          in
          materialize (job :: acc) rest
    in
    let* jobs = materialize [] reserved in
    Ok
      (prepare_json ~harness:options.harness ~codex_yolo:options.codex_yolo jobs)

let validate_begin_status ~explicit_resume (record : Job_store.record) =
  let expected = if explicit_resume then "launch-requested" else "prepared" in
  if String.equal record.status expected then Ok ()
  else
    let recovery =
      if explicit_resume then
        "headless resume requires an existing launch-requested chain"
      else
        "an initial headless run requires prepared, while an intentional successor run must use headless resume"
    in
    Error
      (Printf.sprintf "worker %s has status %S; %s" record.id record.status
         recovery)

let validate_worktree_mode (record : Job_store.record) =
  match String.lowercase_ascii record.worktree_mode with
  | "always" -> Ok ()
  | mode ->
      Error
        (Printf.sprintf
           "worker %s uses worktree mode %S; headless execution requires a Monty-owned worktree"
           record.id mode)

let prepare_existing options (record : Job_store.record) =
  let* options =
    Launcher.options_with_persisted_worktree_mode options record.worktree_mode
  in
  let* () = Launcher.check_dependencies options in
  let* profile =
    Agent_profile.load_pinned ~home:options.home ~worker_dir:record.worker_dir
      record.job.Job.agent_profile
  in
  let* () =
    if profile.swarm = None then Ok ()
    else
      Error
        (Printf.sprintf
           "worker %s is pinned to the swarm agent profile; a swarm driver runs interactively (monty launch or monty resume) and spawns its headless children with monty swarm spawn"
           record.id)
  in
  let* prepared = Launcher.prepare_identity ~profile options 1 record.job in
  let* _ = Project_overview.validate_worker_task_link ~home:options.home record in
  let* () =
    match record.transition with
    | None -> Ok ()
    | Some transition ->
        Error
          (Printf.sprintf "worker %s is in a %s transition" record.id
             (Job_store.operation_name transition.operation))
  in
  Ok (options, prepared)

let prepare_begin ~explicit_resume options worker =
  let* record =
    if explicit_resume then Resume.find_resumable ~home:options.Launcher.home worker
    else Job_store.find ~home:options.home ~scope:Job_store.Active worker
  in
  let* () = validate_begin_status ~explicit_resume record in
  let* () = validate_worktree_mode record in
  let* options, prepared = prepare_existing options record in
  Ok { options; prepared; record }

let swarm_child (plan : begin_plan) = Job.swarm_child plan.record.job

let handoff_recipient (plan : begin_plan) =
  match swarm_child plan with
  | Some (parent, _) -> parent
  | None -> Run_handoff.head_butler_inbox

let claim_begin (plan : begin_plan) =
  let requested =
    Launcher.begin_request ~persist_failure:false ~write_script:false
      plan.options plan.prepared
      ~expected_statuses:[ plan.record.Job_store.status ]
  in
  match requested with
  | `Failed message -> Error message
  | `Ready request ->
      Ok
        {
          id = plan.prepared.id;
          title = plan.prepared.job.Job.title;
          repo = plan.prepared.repo;
          branch = plan.prepared.branch;
          worktree = request.workdir;
          workspaces =
            (match
               Job_store.parse_job_file ~home:plan.options.home
                 plan.prepared.state_path.job_file
             with
            | Ok record -> record.workspaces
            | Error _ -> plan.record.workspaces);
          worker_dir = plan.prepared.worker_dir;
          instructions = plan.prepared.instructions;
          context = plan.prepared.context;
          home = plan.options.home;
          profile = plan.prepared.profile;
          swarm_child = swarm_child plan;
        }

let provisional_dispatch (plan : begin_plan) =
  {
    id = plan.prepared.id;
    title = plan.prepared.job.Job.title;
    repo = plan.prepared.repo;
    branch = plan.prepared.branch;
    worktree =
      Option.value ~default:plan.prepared.repo plan.record.last_known_worktree;
    workspaces = plan.record.workspaces;
    worker_dir = plan.prepared.worker_dir;
    instructions = plan.prepared.instructions;
    context = plan.prepared.context;
    home = plan.options.home;
    profile = plan.prepared.profile;
    swarm_child = swarm_child plan;
  }

let read_required_file ~label path =
  try Ok (Shell.read_file path)
  with Sys_error message ->
    Error (Printf.sprintf "failed to read %s %s: %s" label path message)

let load_phase_inputs (plan : begin_plan) =
  let* instructions_contents =
    read_required_file ~label:"Monty instructions" plan.prepared.instructions
  in
  let* context_contents =
    read_required_file ~label:"task context" plan.prepared.context
  in
  Ok { instructions_contents; context_contents }

let append_phase_memory (dispatch : dispatch) (paths : attempt_paths) ~harness ~phase
    contents =
  let path = Worker_memory.memory_file dispatch.worker_dir in
  State_store.with_lock ~home:dispatch.home (fun () ->
      let* existing = State_store.lstat path in
      let* () =
        match existing with
        | Some { Unix.st_kind = Unix.S_REG; _ } -> Ok ()
        | Some { Unix.st_kind = Unix.S_LNK; _ } ->
            Error (Printf.sprintf "unsafe worker memory file is a symlink: %s" path)
        | Some _ ->
            Error (Printf.sprintf "worker memory path is not a regular file: %s" path)
        | None -> Error (Printf.sprintf "worker memory file is missing: %s" path)
      in
      let* current = read_required_file ~label:"worker memory" path in
      let addition =
        String.concat "\n"
          [ "";
            Printf.sprintf "## Headless %s %s (%s)" (Harness.to_string harness) phase paths.id;
            "";
            contents;
            "" ]
      in
      State_store.write_file_atomic ~path ~perm:0o600 (current ^ addition))

let source_section label path contents =
  String.concat "\n"
    [ label ^ ": " ^ path; "-----"; contents; "-----" ]

let direct_memory_notice =
  "This is one direct headless phase, not an interactive worker session. You may modify only the supplied code workspaces. Do not write worker memory, publish a run handoff, or change other Monty state; Monty will append the captured handoff to durable memory after the phase succeeds and publish the run result."

let implementation_prompt dispatch inputs =
  let task = implementation_task dispatch in
  String.concat "\n\n"
    [ direct_memory_notice;
      task;
      source_section "Monty instructions" dispatch.instructions
        inputs.instructions_contents;
      source_section "Task context" dispatch.context inputs.context_contents ]

let reviewer_prompt dispatch inputs review implementation_handoff =
  let task = reviewer_task dispatch review implementation_handoff in
  String.concat "\n\n"
    [ task;
      source_section "Monty instructions" dispatch.instructions
        inputs.instructions_contents;
      source_section "Task context" dispatch.context inputs.context_contents ]

let fixer_prompt dispatch inputs paths reports implementation_handoff =
  let task = fixer_task dispatch paths reports implementation_handoff in
  String.concat "\n\n"
    [ direct_memory_notice;
      task;
      source_section "Monty instructions" dispatch.instructions
        inputs.instructions_contents;
      source_section "Task context" dispatch.context inputs.context_contents ]

let phase_path directory name extension =
  Filename.concat directory (name ^ extension)

let write_phase_prompt paths name contents =
  let path = phase_path paths.prompts name ".md" in
  let* () = State_store.write_file_atomic ~path ~perm:0o600 contents in
  Ok path

let codex_review_arguments () =
  let config=match Sys.getenv_opt "MONTY_CODEX_REVIEW_CONFIG" with None|Some ""->`Null|Some value->Yojson.Safe.from_string value in
  Native_command.codex_model_arguments config

let run_codex_phase (options : Launcher.options) (dispatch : dispatch)
    (paths : attempt_paths) ~name ~writable ~prompt ~output =
  let events = phase_path paths.events name ".jsonl" in
  let progress = phase_path paths.logs name ".log" in
  let additional_dirs =
    match dispatch.workspaces with
    | [] | [ _ ] -> ""
    | _ :: rest ->
        rest
        |> List.filter_map (fun (workspace : Job_store.workspace_state) ->
               workspace.worktree)
        |> List.map (fun path -> " --add-dir " ^ Shell.quote path)
        |> String.concat ""
  in
  let command =
    "umask 077 && " ^ options.harness_command ^ " exec"
    ^ Harness_command.codex_effort_arg
    ^ Codex_trust.arguments
        (match
           dispatch.workspaces
           |> List.filter_map (fun (workspace : Job_store.workspace_state) ->
                  workspace.worktree)
         with
        | [] -> [ dispatch.worktree ]
        | paths -> paths)
    ^ " --ephemeral --json --color never"
    ^ (if writable then "" else codex_review_arguments ())
    ^ Harness.codex_headless_permissions ~codex_yolo:options.codex_yolo ~writable
    ^ " -C " ^ Shell.quote dispatch.worktree
    ^ additional_dirs
    ^ " --output-last-message " ^ Shell.quote output ^ " - < "
    ^ Shell.quote prompt ^ " > " ^ Shell.quote events ^ " 2> "
    ^ Shell.quote progress
  in
  let* () = Skill.ensure ~home:dispatch.home ~harness:Harness.Codex in
  match Process.run_capture command with
  | Error message ->
      Error
        (Printf.sprintf "Codex %s phase could not start: %s (progress: %s)"
           name message progress)
  | Ok { status = `Exited 0; _ } ->
      if Sys.file_exists output then Ok ()
      else
        Error
          (Printf.sprintf
             "Codex %s phase exited successfully without writing %s (events: %s; progress: %s)"
             name output events progress)
  | Ok { status; _ } ->
      Error
        (Printf.sprintf "Codex %s phase failed with %s (events: %s; progress: %s)"
             name (Process.status_to_string status) events progress)

let pi_final_message events =
  let* () = Run_handoff.require_regular_file events in
  try
    In_channel.with_open_text events (fun channel ->
        let assistant = ref None in
        let ended = ref false in
        let rec read () =
          match In_channel.input_line channel with
          | None -> ()
          | Some line when String.trim line = "" -> read ()
          | Some line ->
              let json = Yojson.Safe.from_string line in
              let member = Yojson.Safe.Util.member in
              (match member "type" json with
              | `String "agent_start" -> ended := false; assistant := None
              | `String "agent_end" -> ended := true
              | `String "message_end" ->
                  ended := false;
                  let message = member "message" json in
                  if member "role" message = `String "assistant" then
                    assistant := Some message
              | _ -> ());
              read ()
        in
        read ();
        match !ended, !assistant with
        | true, Some message ->
            let member = Yojson.Safe.Util.member in
            (match member "stopReason" message with
            | `String "stop" ->
                let text =
                  member "content" message |> Yojson.Safe.Util.to_list
                  |> List.filter_map (fun content ->
                         if member "type" content = `String "text" then
                           Some (member "text" content |> Yojson.Safe.Util.to_string)
                         else None)
                  |> String.concat "\n" |> String.trim
                in
                if text = "" then Error "Pi completed without a final handoff"
                else Ok (text ^ "\n")
            | reason ->
                Error
                  (Printf.sprintf "Pi did not complete successfully (stopReason=%s, error=%s)"
                     (Yojson.Safe.to_string reason)
                     (Yojson.Safe.to_string (member "errorMessage" message))))
        | _ -> Error "Pi event stream ended without a completed agent and final assistant message")
  with
  | Sys_error message | Yojson.Json_error message -> Error message
  | Yojson.Safe.Util.Type_error (message, _) -> Error ("invalid Pi event: " ^ message)

let run_pi_phase (options : Launcher.options) (dispatch : dispatch)
    (paths : attempt_paths) ~name ~writable ~prompt ~output =
  let events = phase_path paths.events name ".jsonl" in
  let progress = phase_path paths.logs name ".log" in
  (* A shell cannot be made read-only with a prompt. Pi reviewers inspect with
     file tools; writers retain bash for installation and validation. *)
  let result =
    let* () = Skill.ensure ~home:dispatch.home ~harness:Harness.Pi in
    let command =
      "umask 077 && " ^ options.harness_command
      ^ Harness.pi_headless_args ~writable
      ^ " < " ^ Shell.quote prompt ^ " > " ^ Shell.quote events
      ^ " 2> " ^ Shell.quote progress
    in
    let* result = Process.run_capture ~cwd:dispatch.worktree command in
    match result.status with
    | `Exited 0 -> Ok ()
    | status -> Error (Process.status_to_string status)
  in
  let result =
    let* () = result in
    let* final = pi_final_message events in
    State_store.write_file_atomic ~path:output ~perm:0o600 final
  in
  Result.map_error (fun message ->
      Printf.sprintf "Pi %s phase failed: %s (events: %s; progress: %s)"
        name message events progress) result

let claude_final_message events =
  let* () = Run_handoff.require_regular_file events in
  try
    let records =
      Shell.read_file events |> String.split_on_char '\n'
      |> List.filter (fun line -> String.trim line <> "")
      |> List.map Yojson.Safe.from_string
    in
    let terminal_result value = Json_fields.field "type" value = `String "result" in
    if List.length (List.filter terminal_result records) <> 1 then
      Error "Claude stream must contain exactly one terminal result"
    else
    match List.rev records with
    | result :: _ when terminal_result result
      && Json_fields.field "subtype" result = `String "success"
      && Json_fields.field "is_error" result = `Bool false
      && (match Json_fields.field "permission_denials" result with
          | `Null | `List [] -> true
          | _ -> false) ->
        (match Json_fields.field "result" result, Json_fields.field "session_id" result with
        | `String text, `String session_id when String.trim text <> "" && session_id <> "" ->
            let* _ = Native_session.validate ~harness:Harness.Claude session_id in
            Ok (String.trim text ^ "\n")
        | _ -> Error "Claude terminal result is missing handoff or native session identity")
    | _ -> Error "Claude stream has no valid final result, or contains permission denials; inspect retained events"
  with Sys_error message | Yojson.Json_error message -> Error message

let claude_writer_tools = "Bash,Edit,MultiEdit,Write,NotebookEdit,Read,Glob,Grep"

let run_claude_phase (options:Launcher.options) (dispatch:dispatch) (paths:attempt_paths) ~name ~writable ~prompt ~output =
  let events=phase_path paths.events name ".jsonl" and progress=phase_path paths.logs name ".log" in
  let* ()=Skill.ensure ~home:dispatch.home ~harness:Harness.Claude in
  let dirs=dispatch.workspaces |> List.filter_map (fun (w:Job_store.workspace_state)->w.worktree) in
  (* Print mode never prompts, so a writer must have its editing tools
     pre-allowed, like Pi's writer tool list and Codex's workspace-write
     sandbox; otherwise every edit is a permission denial. *)
  let policy=if writable then " --allowedTools " ^ claude_writer_tools else
    " --restricted --safe-mode --tools Read,Grep,Glob --allowedTools Read,Grep,Glob --strict-mcp-config --mcp-config " ^ Shell.quote "{\"mcpServers\":{}}"
    ^ " --setting-sources " ^ Shell.quote "" ^ " --settings " ^ Shell.quote "{\"disableAllHooks\":true}" in
  let command="umask 077 && " ^ options.harness_command ^ " --print --verbose --output-format stream-json --no-session-persistence"
    ^ " --permission-mode default" ^ policy
    ^ (List.tl dirs |> List.map (fun path->" --add-dir " ^ Shell.quote path) |> String.concat "")
    ^ " < " ^ Shell.quote prompt ^ " > " ^ Shell.quote events ^ " 2> " ^ Shell.quote progress in
  let* result=Process.run_capture ~cwd:dispatch.worktree command in
  let* ()=match result.status with `Exited 0->Ok ()|status->Error("Claude phase failed: " ^ Process.status_to_string status) in
  let* final=claude_final_message events in
  State_store.write_file_atomic ~path:output ~perm:0o600 final

let run_phase options =
  match options.Launcher.harness with
  | Harness.Pi -> run_pi_phase options
  | Harness.Codex -> run_codex_phase options
  | Harness.Claude -> run_claude_phase options

type 'a spawned_result = {
  pid : int;
  receive : unit -> ('a, string) result;
}

let spawn_result (type payload) (operation : unit -> (payload, string) result)
    : payload spawned_result =
  let read_fd, write_fd = Unix.pipe () in
  match Unix.fork () with
  | exception exn ->
      Unix.close read_fd;
      Unix.close write_fd;
      raise exn
  | 0 ->
      Unix.close read_fd;
      let channel = Unix.out_channel_of_descr write_fd in
      let result =
        try operation ()
        with exn ->
          Error
            (Printf.sprintf "headless child failed unexpectedly: %s"
               (Printexc.to_string exn))
      in
      Marshal.to_channel channel result [ Marshal.No_sharing ];
      flush channel;
      close_out_noerr channel;
      Unix._exit 0
  | pid ->
      Unix.close write_fd;
      let channel = Unix.in_channel_of_descr read_fd in
      let receive () =
        Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
            (Marshal.from_channel channel : (payload, string) result))
      in
      { pid; receive }

let try_spawn_result operation =
  try Ok (spawn_result operation)
  with exn ->
    Error
      (Printf.sprintf "could not start headless child: %s"
         (Printexc.to_string exn))

let await_result child =
  let payload =
    try child.receive ()
    with exn ->
      Error
        (Printf.sprintf "headless child returned no result: %s"
           (Printexc.to_string exn))
  in
  let _, status = Process.waitpid [] child.pid in
  match (status, payload) with
  | Unix.WEXITED 0, result -> result
  | Unix.WEXITED code, _ ->
      Error (Printf.sprintf "headless child exited with code %d" code)
  | Unix.WSIGNALED signal, _ ->
      Error (Printf.sprintf "headless child was terminated by signal %d" signal)
  | Unix.WSTOPPED signal, _ ->
      Error (Printf.sprintf "headless child was stopped by signal %d" signal)

let run_parallel operations =
  let children = List.map try_spawn_result operations in
  let errors =
    children
    |> List.filter_map (function
         | Ok child -> (match await_result child with Ok () -> None | Error error -> Some error)
         | Error error -> Some error)
  in
  match errors with
  | [] -> Ok ()
  | errors ->
      Error
        ("Headless review phases failed:\n- " ^ String.concat "\n- " errors)

let run_result_json (options : Launcher.options) (dispatch : dispatch)
    (paths : attempt_paths) (published : Run_handoff.published) =
  let implementation =
    if dispatch.profile.fix = None then paths.final else paths.implementation
  in
  let review_outputs =
    paths.reviews
    |> List.map (fun path ->
           (path.review.id ^ "_review", `String path.output))
  in
  `Assoc
    [ ("schema", `String (match options.harness with
                          | Harness.Pi -> "monty:headless-pi-run:v1"
                          | Harness.Codex -> codex_run_schema
                          | Harness.Claude -> "monty:headless-claude-run:v1"));
      ("harness", `String (Harness.to_string options.harness));
      ("codex_yolo", `Bool (options.harness = Harness.Codex && options.codex_yolo));
      ("worker_id", `String dispatch.id);
      ("status", `String "launch-requested");
      ("run_status", `String "finished");
      ("outcome", `String "ready-for-review");
      ("attempt_id", `String paths.id);
      ("artifact_dir", `String paths.root);
      ("handoff", `String published.handoff.evidence.handoff);
      ("notice_id", `String published.notice.id);
      ("agent_profile", `String dispatch.profile.id);
      ( "outputs",
        `Assoc
          ([ ("implementation", `String implementation) ] @ review_outputs
          @ [ ("final", `String paths.final) ]) ) ]

let review_summary profile =
  match profile.Agent_profile.reviews with
  | [] -> None
  | reviews ->
      Some
        (Printf.sprintf "%d independent reviewer%s completed%s."
           (List.length reviews)
           (if List.length reviews = 1 then "" else "s")
           (if profile.fix = None then ""
            else match profile.fix_role with
              | Agent_profile.Fixer -> " and the fixer verified their reports"
              | Agent_profile.Simplifier -> " and the simplifier processed their reports"))

let validation_summary profile =
  if profile.Agent_profile.fix = None then
    "Exact validation commands and results are recorded in the final implementation handoff."
  else
    Printf.sprintf
      "Exact validation commands and results are recorded in the final %s handoff."
      (Agent_profile.fix_role_name profile.fix_role)

let preflight_worker ~explicit_resume options worker =
  let* home = State_path.canonicalize options.Launcher.home in
  let* script_dir = State_path.canonicalize options.script_dir in
  let options = { options with home; script_dir } in
  let* plan = prepare_begin ~explicit_resume options worker in
  let* () = if plan.prepared.profile.reviews<>[] then
    Harness.check_review_command plan.options.harness plan.options.harness_command else Ok () in
  let* harness_command = Process.absolute_command plan.options.harness_command in
  let* wt_command = Process.absolute_command plan.options.wt_command in
  let plan = { plan with options = { plan.options with harness_command; wt_command } } in
  let* inputs = load_phase_inputs plan in
  Ok (plan, inputs)

let started_json (plan : begin_plan) (dispatch : dispatch) paths =
  let* reference = Run_handoff.reference_of_record plan.record paths.id in
  Ok (`Assoc
    [ ("schema", `String started_schema);
      ("harness", `String (Harness.to_string plan.options.harness));
      ("worker_id", `String dispatch.id);
      ("status", `String "launch-requested");
      ("run_status", `String "started");
      ("attempt_id", `String paths.id);
      ("agent_profile", `String dispatch.profile.id);
      ("artifact_dir", `String paths.root);
      ("handoff", `String (Run_handoff.handoff_path dispatch.home reference)) ])

let set_worker_environment (plan : begin_plan) (dispatch : dispatch) =
  (* A nested launch must not retain paths from its caller's workspace map. *)
  Unix.environment () |> Array.iter (fun entry ->
      let name = String.sub entry 0 (String.index entry '=') in
      if String.starts_with ~prefix:"MONTY_WORKSPACE_" name then
        Unix.putenv name "");
  List.iter (fun (name, value) -> Unix.putenv name value)
    [ "MONTY_HOME", dispatch.home;
      "MONTY_HARNESS", Harness.to_string plan.options.harness;
      "MONTY_RUN_DIR", plan.record.run_dir;
      "MONTY_WORKER_DIR", dispatch.worker_dir;
      "MONTY_JOB_ID", dispatch.id;
      "MONTY_JOB_TITLE", dispatch.title;
      "MONTY_JOB_REPO", dispatch.repo;
      "MONTY_JOB_BRANCH", dispatch.branch;
      "MONTY_JOB_CONTEXT", dispatch.context;
      "MONTY_JOB_FILE", plan.record.path;
      "MONTY_TASK_KEY", Option.value ~default:"" plan.record.job.task_key;
      "MONTY_JOB_WORKTREE", dispatch.worktree;
      "MONTY_WORKTREE_MODE", plan.record.worktree_mode;
      "MONTY_SWARM_PARENT", Option.fold ~none:"" ~some:fst dispatch.swarm_child;
      "MONTY_SWARM_NAME", Option.fold ~none:"" ~some:snd dispatch.swarm_child ];
  List.iteri (fun index (workspace : Job_store.workspace_state) ->
      Unix.putenv (Printf.sprintf "MONTY_WORKSPACE_%d" (index + 1))
        (Option.value ~default:"" workspace.worktree)) dispatch.workspaces

let run_worker_unlocked ~on_started ~explicit_resume options worker =
  let* plan, inputs = preflight_worker ~explicit_resume options worker in
  let preview = provisional_dispatch plan in
  let paths = attempt_paths preview (fresh_attempt_id ()) in
  let source = match options.Launcher.harness with
    | Harness.Pi -> Run_handoff.Headless_pi | Harness.Codex -> Run_handoff.Headless_codex | Harness.Claude -> Run_handoff.Headless_claude in
  let* () = prepare_attempt plan preview paths
      (Run_handoff.source_to_string source) in
  let* dispatch = claim_begin plan in
  let phase = ref "implementation" in
  let execution =
    try
    set_worker_environment plan dispatch;
    let implementation_output =
      if dispatch.profile.fix = None then paths.final else paths.implementation
    in
    let* implementation_prompt_path =
      write_phase_prompt paths "implementation"
        (implementation_prompt dispatch inputs)
    in
    let* started = started_json plan dispatch paths in
    on_started paths started;
    let* () =
      run_phase plan.options dispatch paths ~name:"implementation"
        ~writable:true ~prompt:implementation_prompt_path
        ~output:implementation_output
    in
    let* implementation_handoff =
      read_required_file ~label:"implementation handoff"
        implementation_output
    in
    let* () =
      append_phase_memory dispatch paths ~harness:options.harness ~phase:"implementation"
        implementation_handoff
    in
    if paths.reviews <> [] then phase := "review";
    let* review_prompts =
      List.fold_left
        (fun result path ->
          let* prompts = result in
          let name = path.review.id ^ "-review" in
          let* prompt =
            write_phase_prompt paths name
              (reviewer_prompt dispatch inputs path.review
                 implementation_handoff)
          in
          Ok ((path, name, prompt) :: prompts))
        (Ok []) paths.reviews
      |> Result.map List.rev
    in
    let* () =
      review_prompts
      |> List.map (fun (path, name, prompt) () ->
             run_phase plan.options dispatch paths ~name ~writable:false
               ~prompt ~output:path.output)
      |> run_parallel
    in
    let* reports =
      List.fold_left
        (fun result path ->
          let* reports = result in
          let* report =
            read_required_file
              ~label:path.review.title path.output
          in
          Ok (report :: reports))
        (Ok []) paths.reviews
      |> Result.map List.rev
    in
    match dispatch.profile.fix with
    | None -> Ok implementation_handoff
    | Some _ ->
        let name = Agent_profile.fix_role_name dispatch.profile.fix_role in
        phase := name;
        let* fixer_prompt_path =
          write_phase_prompt paths name
            (fixer_prompt dispatch inputs paths.reviews reports implementation_handoff)
        in
        let* () =
          run_phase plan.options dispatch paths ~name ~writable:true
            ~prompt:fixer_prompt_path ~output:paths.final
        in
        let* final_handoff =
          read_required_file ~label:"final handoff" paths.final
        in
        phase := "durable-memory";
        let* () =
          append_phase_memory dispatch paths ~harness:options.harness ~phase:"final" final_handoff
        in
        Ok final_handoff
    with exn -> Error (Printexc.to_string exn)
  in
  (match execution with
  | Error message ->
      let handoff =
        Run_handoff.publish ~home:plan.options.home ~record:plan.record
          ~handoff_id:paths.id ~source ~recipient:(handoff_recipient plan)
          ~outcome:Run_handoff.Failed
          ~summary:(Printf.sprintf "Headless %s run failed during %s."
                      (Harness.to_string options.harness) !phase)
          ~risks:[ message ] ~last_phase:!phase ~error:message
          ~artifacts:[ paths.root ] ()
      in
      (match handoff with
      | Ok published ->
          Error
            (Printf.sprintf "%s\nRun handoff: %s" message
               published.handoff.evidence.handoff)
      | Error handoff_error ->
          Error
            (Printf.sprintf
               "%s\nAdditionally failed to persist the run handoff: %s" message
               handoff_error))
  | Ok final_handoff ->
      phase := "handoff";
      let* published =
        let review_summary = review_summary dispatch.profile in
        Run_handoff.publish ~home:plan.options.home ~record:plan.record
          ~handoff_id:paths.id ~source ~recipient:(handoff_recipient plan)
          ~outcome:Run_handoff.Ready_for_review
          ~summary:(Run_handoff.compact_summary final_handoff)
          ~validation:[ validation_summary dispatch.profile ]
          ?review_summary
          ~artifacts:[ paths.root ] ()
      in
      Ok (run_result_json plan.options dispatch paths published))

let background operation =
  let read_fd, write_fd = Unix.pipe ~cloexec:true () in
  flush_all ();
  match Unix.fork () with
  | exception exn ->
      Unix.close read_fd; Unix.close write_fd;
      Error (Printexc.to_string exn)
  | 0 ->
      Unix.close read_fd;
      let channel = Unix.out_channel_of_descr write_fd in
      let sent = ref false in
      let send result =
        sent := true;
        (* Losing the caller must not kill the worker or prevent its handoff. *)
        (try Marshal.to_channel channel result []; flush channel with Sys_error _ -> ());
        close_out_noerr channel
      in
      let result =
        try
          ignore (Sys.signal Sys.sigpipe Sys.Signal_ignore);
          ignore (Unix.setsid ());
          let null = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
          List.iter (Unix.dup2 null) [ Unix.stdin; Unix.stdout; Unix.stderr ];
          Unix.close null;
          Unix.chdir "/";
          operation (fun paths json ->
              let log = Filename.concat paths.logs "supervisor.log" in
              let fd = Unix.openfile log
                  [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL; Unix.O_CLOEXEC ] 0o600 in
              Unix.dup2 fd Unix.stdout; Unix.dup2 fd Unix.stderr;
              Unix.close fd;
              send (Ok json))
        with exn -> Error (Printexc.to_string exn)
      in
      if not !sent then send result;
      (match result with Ok json -> print_json json | Error message -> prerr_endline message);
      flush_all ();
      Unix._exit (if Result.is_ok result then 0 else 1)
  | pid ->
      Unix.close write_fd;
      let channel = Unix.in_channel_of_descr read_fd in
      let result =
        try Marshal.from_channel channel with exn ->
          Error ("headless supervisor failed before confirming startup: " ^ Printexc.to_string exn)
      in
      close_in_noerr channel;
      (match result with Error _ -> ignore (Process.waitpid [] pid) | Ok _ -> ());
      result

let run_worker ?(locked=false) ?(on_started=(fun _ _ -> ())) ~wait ~explicit_resume options worker =
  let* plan, _inputs = preflight_worker ~explicit_resume options worker in
  let options = plan.options in
  let record = plan.record in
  let operation on_started =
    let run () = run_worker_unlocked ~on_started ~explicit_resume options worker in
    if locked then run ()
    else Worker_lock.with_lock ~home:options.Launcher.home ~record run
  in
  if wait then operation on_started else background operation

let preflight_batch options indexed_jobs =
  let* prepared = Launcher.preflight_batch options indexed_jobs in
  let* () =
    match
      List.find_opt
        (fun worker ->
          match worker.Launcher.existing with
          | Launcher.Retryable "prepared" -> false
          | _ -> true)
        prepared
    with
    | None -> Ok ()
    | Some worker ->
        Error
          (Printf.sprintf
             "worker %s is not prepared for an initial headless run; use monty headless prepare-many or monty headless resume as appropriate"
             worker.id)
  in
  let workers =
    List.map (fun (worker : Launcher.prepared) -> worker.id) prepared
  in
  let* () =
    List.fold_left
      (fun result worker ->
        let* () = result in
        let* _ = preflight_worker ~explicit_resume:false options worker in
        Ok ())
      (Ok ()) workers
  in
  Ok workers

let run_many ~wait options indexed_jobs =
  let* workers = preflight_batch options indexed_jobs in
  let children =
    List.map
      (fun worker ->
        ( worker,
          if wait then
            `Waiting (try_spawn_result (fun () ->
                run_worker ~wait:true ~explicit_resume:false options worker))
          else `Started (run_worker ~wait:false ~explicit_resume:false options worker) ))
      workers
  in
  let results =
    List.map
      (fun (worker, child) ->
        let result =
          match child with
          | `Waiting (Ok child) -> await_result child
          | `Waiting (Error message) -> Error message
          | `Started result -> result
        in
        match result with
        | Ok result ->
            (true, `Assoc [ ("worker_id", `String worker); ("result", result) ])
        | Error message ->
            ( false,
              `Assoc
                [ ("worker_id", `String worker);
                  ("error", `String message) ] ))
      children
  in
  let success = List.for_all fst results in
  let json =
    `Assoc
      [ ("schema", `String (if not wait then "monty:headless-start-many:v1"
                            else match options.Launcher.harness with
                              | Harness.Pi -> "monty:headless-pi-run-many:v1"
                              | Harness.Codex -> codex_run_many_schema
                              | Harness.Claude -> "monty:headless-claude-run-many:v1"));
        ("harness", `String (Harness.to_string options.Launcher.harness));
        ("codex_yolo", `Bool (options.harness = Harness.Codex && options.codex_yolo));
        ("success", `Bool success);
        ("results", `List (List.map snd results)) ]
  in
  Ok (json, success)
