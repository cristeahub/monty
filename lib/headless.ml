let ( let* ) = Result.bind

let prepare_schema = "monty:headless-prepare:v2"
let dispatch_schema = "monty:headless-dispatch:v3"
let codex_run_schema = "monty:headless-codex-run:v1"
let codex_run_many_schema = "monty:headless-codex-run-many:v1"

type prepared_job = {
  id : string;
  title : string;
  branch : string;
  worktree : string option;
  workspaces : Job_store.workspace_state list;
  worker_dir : string;
  status : string;
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
}

type attempt_paths = {
  id : string;
  root : string;
  implementation : string;
  correctness : string;
  quality : string;
  final : string;
  prompts : string;
  events : string;
  logs : string;
}

type begin_plan = {
  options : Launcher.options;
  prepared : Launcher.prepared;
  record : Job_store.record;
}

type codex_inputs = {
  instructions_contents : string;
  context_contents : string;
}

let prepared_job_json (job : prepared_job) =
  `Assoc
    [ ("id", `String job.id);
      ("title", `String job.title);
      ("branch", `String job.branch);
      ("worktree", Option.fold ~none:`Null ~some:(fun path -> `String path) job.worktree);
      ("workspaces", `List (List.map Job_store.json_of_workspace job.workspaces));
      ("worker_dir", `String job.worker_dir);
      ("status", `String job.status) ]

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
  let reviews = Filename.concat root "reviews" in
  {
    id = attempt_id;
    root;
    implementation = Filename.concat root "implementation.md";
    correctness = Filename.concat reviews "correctness.md";
    quality = Filename.concat reviews "quality.md";
    final = Filename.concat root "final.md";
    prompts = Filename.concat root "prompts";
    events = Filename.concat root "events";
    logs = Filename.concat root "logs";
  }

let mutation_prohibitions =
  String.concat "\n"
    [ "Do not run monty done.";
      "Do not create, switch, or remove worktrees.";
      "Do not modify job.json or Monty task, settings, or project state.";
      "Do not stage or commit changes.";
      "Do not push, open a pull request, submit a review, post comments, or perform any other remote write." ]

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
      "Implement the requested scope completely and follow repository-local instructions.";
      "Do not invoke /review or spawn subagents; two independent reviewers run after this phase.";
      "Run relevant non-destructive validation and self-review the resulting diff.";
      Printf.sprintf "Append important implementation discoveries and handoff notes to %s."
        (Filename.concat dispatch.worker_dir "memory.md");
      mutation_prohibitions;
      "Finish with a compact handoff containing changed files, validation commands and results, unresolved risks, and a concise diff summary." ]

let reviewer_task (dispatch : dispatch) focus =
  String.concat "\n\n"
    [ Printf.sprintf "Independently review Monty worker %s: %s." dispatch.id
        dispatch.title;
      "Start from the supplied task context and inspect every supplied worktree directly.";
      "Use the absolute MONTY_JOB_FILE path in the Monty instructions to resolve every persisted workspace path.";
      "Do not rely on another agent's summary and do not look for another reviewer's output.";
      focus;
      "This is strictly read-only. Do not modify project, source, test, configuration, task, or worker-memory files.";
      "Use shell commands only for read-only inspection and non-mutating validation.";
      mutation_prohibitions;
      "Implementation handoff for orientation only; verify every claim against the worktree:";
      "{previous}";
      "Return only evidence-backed findings ordered by severity, each with file and line references, impact, and the smallest safe correction.";
      "State 'No findings' plainly if no correction is warranted." ]

let fixer_task (dispatch : dispatch) =
  String.concat "\n\n"
    [ Printf.sprintf "Finalize Monty worker %s: %s." dispatch.id dispatch.title;
      "Read the supplied task context, inspect every supplied worktree, and review both independent reports below.";
      "Use the absolute MONTY_JOB_FILE path in the Monty instructions to resolve every persisted workspace path.";
      "You are the only writer in this phase.";
      "Verify every finding against the code and requirements. Fix valid findings without widening scope and explicitly reject invalid findings.";
      "Rerun affected validation plus any broader checks required by the repository.";
      Printf.sprintf "Append the verified findings, fixes, validation, and final handoff to %s."
        (Filename.concat dispatch.worker_dir "memory.md");
      mutation_prohibitions;
      "Correctness review:";
      "{outputs.correctnessReview}";
      "Quality and tests review:";
      "{outputs.qualityReview}";
      "Finish with changed files, accepted and rejected findings, validation commands and results, residual risks, and git status confirming nothing is staged." ]

let reads_json paths = `List (List.map (fun path -> `String path) paths)

let acceptance_reason =
  "Monty's explicit 1-2-1 reviewers and fixer provide the review gate."

let child_json ~phase ~agent ~label ~as_name ~task ~cwd ~reads ~output =
  let fields =
    [ ("agent", `String agent);
      ("label", `String label);
      ("as", `String as_name);
      ("task", `String task);
      ("cwd", `String cwd);
      ("reads", reads_json reads);
      ("output", `String output);
      ("progress", `Bool false);
      ( "acceptance",
        `Assoc
          [ ("level", `String "none");
            ("reason", `String acceptance_reason) ] ) ]
  in
  match phase with
  | None -> `Assoc fields
  | Some phase -> `Assoc (("phase", `String phase) :: fields)

let harness_arguments_json (dispatch : dispatch) attempt_id =
  let paths = attempt_paths dispatch attempt_id in
  let implementation =
    child_json ~phase:(Some "Implementation") ~agent:"worker"
      ~label:(dispatch.title ^ " implementation") ~as_name:"implementation"
      ~task:(implementation_task dispatch) ~cwd:dispatch.worktree
      ~reads:[ dispatch.instructions; dispatch.context ]
      ~output:paths.implementation
  in
  let correctness_review =
    child_json ~phase:None ~agent:"monty-headless-reviewer"
      ~label:"Correctness review"
      ~as_name:"correctnessReview"
      ~task:
        (reviewer_task dispatch
           "Focus on correctness, regressions, edge cases, data integrity, security, and exact requirement compliance.")
      ~cwd:dispatch.worktree ~reads:[ dispatch.instructions; dispatch.context ]
      ~output:paths.correctness
  in
  let quality_review =
    child_json ~phase:None ~agent:"monty-headless-reviewer"
      ~label:"Quality and tests review" ~as_name:"qualityReview"
      ~task:
        (reviewer_task dispatch
           "Focus on tests, failure handling, maintainability, simplicity, architectural fit, and missing validation.")
      ~cwd:dispatch.worktree ~reads:[ dispatch.instructions; dispatch.context ]
      ~output:paths.quality
  in
  let reviews =
    `Assoc
      [ ("phase", `String "Review");
        ("label", `String (dispatch.title ^ " independent reviews"));
        ("parallel", `List [ correctness_review; quality_review ]);
        ("concurrency", `Int 2);
        ("failFast", `Bool false) ]
  in
  let fixer =
    child_json ~phase:(Some "Fix") ~agent:"worker"
      ~label:(dispatch.title ^ " verified fixes") ~as_name:"final"
      ~task:(fixer_task dispatch) ~cwd:dispatch.worktree
      ~reads:[ dispatch.instructions; dispatch.context ] ~output:paths.final
  in
  `Assoc
    [ ("chain", `List [ implementation; reviews; fixer ]);
      ("context", `String "fresh");
      ("async", `Bool true);
      ("clarify", `Bool false);
      ("agentScope", `String "project");
      ("cwd", `String dispatch.home);
      ("chainDir", `String (Filename.concat paths.root "chain"));
      ("sessionDir", `String (Filename.concat paths.root "sessions"));
      ("artifacts", `Bool false) ]

let dispatch_json ?attempt_id (dispatch : dispatch) =
  let attempt_id =
    match attempt_id with Some attempt_id -> attempt_id | None -> fresh_attempt_id ()
  in
  let harness_arguments = harness_arguments_json dispatch attempt_id in
  `Assoc
    [ ("schema", `String dispatch_schema);
      ( "worker",
        `Assoc
          [ ("id", `String dispatch.id);
            ("title", `String dispatch.title);
            ("repo", `String dispatch.repo);
            ("branch", `String dispatch.branch);
            ("worktree", `String dispatch.worktree);
            ("workspaces",
             `List (List.map Job_store.json_of_workspace dispatch.workspaces));
            ("worker_dir", `String dispatch.worker_dir);
            ("instructions", `String dispatch.instructions);
            ("context", `String dispatch.context) ] );
      ( "harness_call",
        `Assoc
          [ ("tool", `String "subagent");
            ("arguments", harness_arguments) ] ) ]

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
  }

let ensure_worktree options (prepared : Launcher.prepared) =
  let expected_status = status_before_prepare prepared in
  match
    Launcher.materialize_workspaces ~expected_statuses:[ expected_status ]
      options prepared
  with
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
        }

let prepare_many ~dry_run options indexed_jobs =
  let options =
    { options with
      Launcher.backend = Terminal.Dry_run;
      worktree_mode = Launcher.Always }
  in
  let* () =
    match options.Launcher.harness with
    | Harness.Pi -> Ok ()
    | Harness.Codex -> Launcher.check_dependencies options
  in
  let* () =
    match options.harness with
    | Harness.Pi -> Launcher.check_worktree_dependency options
    | Harness.Codex -> Ok ()
  in
  let* prepared = Launcher.preflight_batch options indexed_jobs in
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
          let* job = ensure_worktree options worker in
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
        "first headless begin requires prepared, while an intentional successor run must use headless resume"
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
  let* () = Launcher.check_worktree_dependency options in
  let* prepared = Launcher.prepare_identity options 1 record.job in
  let* _ = Project_overview.validate_worker_task_link ~home:options.home record in
  let* () =
    match record.transition with
    | None -> Ok ()
    | Some transition ->
        Error
          (Printf.sprintf "worker %s is in a %s transition" record.id
             (Job_store.operation_name transition.operation))
  in
  let* prepared = Launcher.script_for_resume options prepared record in
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

let claim_begin (plan : begin_plan) =
  match
    Launcher.begin_request ~persist_failure:false ~write_script:false plan.options
      plan.prepared ~expected_statuses:[ plan.record.Job_store.status ]
  with
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
        }

let begin_worker ~explicit_resume options worker =
  if options.Launcher.harness <> Harness.Pi then
    Error
      "headless begin emits a Pi subagent call, but the effective harness is codex; use monty headless run"
  else
    let* plan = prepare_begin ~explicit_resume options worker in
    let* dispatch = claim_begin plan in
    Ok (dispatch_json dispatch)

let read_required_file ~label path =
  try Ok (Shell.read_file path)
  with Sys_error message ->
    Error (Printf.sprintf "failed to read %s %s: %s" label path message)

let load_codex_inputs (plan : begin_plan) =
  let* instructions_contents =
    read_required_file ~label:"Monty instructions" plan.prepared.instructions
  in
  let* context_contents =
    read_required_file ~label:"task context" plan.prepared.context
  in
  Ok { instructions_contents; context_contents }

let append_codex_memory (dispatch : dispatch) (paths : attempt_paths) ~phase
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
            Printf.sprintf "## Headless Codex %s (%s)" phase paths.id;
            "";
            contents;
            "" ]
      in
      State_store.write_file_atomic ~path ~perm:0o600 (current ^ addition))

let require_codex_harness (options : Launcher.options) =
  match options.harness with
  | Harness.Codex -> Launcher.check_dependencies options
  | Harness.Pi ->
      Error
        "the effective harness is pi; use monty headless begin and invoke its generated subagent call"

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
  }

let ensure_attempt_directories (dispatch : dispatch) (paths : attempt_paths) =
  let directories =
    [ Filename.concat dispatch.worker_dir "artifacts";
      Filename.concat (Filename.concat dispatch.worker_dir "artifacts")
        "headless";
      paths.root;
      Filename.dirname paths.correctness;
      paths.prompts;
      paths.events;
      paths.logs ]
  in
  List.fold_left
    (fun result directory ->
      let* () = result in
      State_store.ensure_real_directory ~label:"headless artifact directory"
        ~mode:0o700 directory)
    (Ok ()) directories

let source_section label path contents =
  String.concat "\n"
    [ label ^ ": " ^ path; "-----"; contents; "-----" ]

let replace_literal ~pattern ~replacement text =
  if String.equal pattern "" then text
  else
    let pattern_length = String.length pattern in
    let text_length = String.length text in
    let buffer = Buffer.create (text_length + String.length replacement) in
    let matches index =
      index + pattern_length <= text_length
      && String.sub text index pattern_length = pattern
    in
    let rec loop index =
      if index >= text_length then ()
      else if matches index then (
        Buffer.add_string buffer replacement;
        loop (index + pattern_length))
      else (
        Buffer.add_char buffer text.[index];
        loop (index + 1))
    in
    loop 0;
    Buffer.contents buffer

let direct_memory_notice =
  "This direct headless Codex phase may modify only the supplied code worktree. Do not write worker memory or other Monty state directly; Monty will append the captured handoff to durable memory after the phase succeeds."

let implementation_prompt dispatch inputs =
  let memory_instruction =
    Printf.sprintf
      "Append important implementation discoveries and handoff notes to %s."
      (Worker_memory.memory_file dispatch.worker_dir)
  in
  let task =
    implementation_task dispatch
    |> replace_literal ~pattern:memory_instruction
         ~replacement:
           "Include important implementation discoveries and handoff notes in the final response so Monty can persist them."
  in
  String.concat "\n\n"
    [ direct_memory_notice;
      task;
      source_section "Monty instructions" dispatch.instructions
        inputs.instructions_contents;
      source_section "Task context" dispatch.context inputs.context_contents ]

let reviewer_prompt dispatch inputs focus implementation_handoff =
  let task =
    reviewer_task dispatch focus
    |> replace_literal ~pattern:"{previous}"
         ~replacement:implementation_handoff
  in
  String.concat "\n\n"
    [ task;
      source_section "Task context" dispatch.context inputs.context_contents ]

let fixer_prompt dispatch inputs correctness quality =
  let memory_instruction =
    Printf.sprintf
      "Append the verified findings, fixes, validation, and final handoff to %s."
      (Worker_memory.memory_file dispatch.worker_dir)
  in
  let task =
    fixer_task dispatch
    |> replace_literal ~pattern:memory_instruction
         ~replacement:
           "Include the verified findings, fixes, validation, and final handoff in the final response so Monty can persist them."
    |> replace_literal ~pattern:"{outputs.correctnessReview}"
         ~replacement:correctness
    |> replace_literal ~pattern:"{outputs.qualityReview}" ~replacement:quality
  in
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

let codex_permission_arg (options : Launcher.options) ~writable =
  if options.codex_yolo then
    " --dangerously-bypass-approvals-and-sandbox"
  else if writable then " --sandbox workspace-write"
  else " --sandbox read-only"

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
    ^ " --ephemeral --json --color never"
    ^ codex_permission_arg options ~writable
    ^ " -C " ^ Shell.quote dispatch.worktree
    ^ additional_dirs
    ^ " --output-last-message " ^ Shell.quote output ^ " - < "
    ^ Shell.quote prompt ^ " > " ^ Shell.quote events ^ " 2> "
    ^ Shell.quote progress
  in
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

type 'a spawned_result = {
  pid : int;
  channel : in_channel;
}

let spawn_result operation =
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
      { pid; channel = Unix.in_channel_of_descr read_fd }

let try_spawn_result operation =
  try Ok (spawn_result operation)
  with exn ->
    Error
      (Printf.sprintf "could not start headless child: %s"
         (Printexc.to_string exn))

let await_result child =
  let payload =
    try Ok (Marshal.from_channel child.channel)
    with exn ->
      Error
        (Printf.sprintf "headless child returned no result: %s"
           (Printexc.to_string exn))
  in
  close_in_noerr child.channel;
  let _, status = Unix.waitpid [] child.pid in
  match (status, payload) with
  | Unix.WEXITED 0, Ok result -> result
  | Unix.WEXITED 0, Error message -> Error message
  | Unix.WEXITED code, _ ->
      Error (Printf.sprintf "headless child exited with code %d" code)
  | Unix.WSIGNALED signal, _ ->
      Error (Printf.sprintf "headless child was terminated by signal %d" signal)
  | Unix.WSTOPPED signal, _ ->
      Error (Printf.sprintf "headless child was stopped by signal %d" signal)

let run_parallel left right =
  let left_child = try_spawn_result left in
  let right_child = try_spawn_result right in
  let await = function Ok child -> await_result child | Error message -> Error message in
  let left_result = await left_child in
  let right_result = await right_child in
  match (left_result, right_result) with
  | Ok (), Ok () -> Ok ()
  | Error left, Ok () -> Error left
  | Ok (), Error right -> Error right
  | Error left, Error right ->
      Error (Printf.sprintf "both Codex review phases failed:\n- %s\n- %s" left right)

let codex_run_result_json (options : Launcher.options) (dispatch : dispatch)
    (paths : attempt_paths) =
  `Assoc
    [ ("schema", `String codex_run_schema);
      ("harness", `String "codex");
      ("codex_yolo", `Bool options.codex_yolo);
      ("worker_id", `String dispatch.id);
      ("status", `String "launch-requested");
      ("attempt_id", `String paths.id);
      ("artifact_dir", `String paths.root);
      ( "outputs",
        `Assoc
          [ ("implementation", `String paths.implementation);
            ("correctness_review", `String paths.correctness);
            ("quality_review", `String paths.quality);
            ("final", `String paths.final) ] ) ]

let preflight_codex_worker ~explicit_resume options worker =
  let* () = require_codex_harness options in
  let* plan = prepare_begin ~explicit_resume options worker in
  let* inputs = load_codex_inputs plan in
  Ok (plan, inputs)

let run_codex_worker ~explicit_resume options worker =
  let* plan, inputs = preflight_codex_worker ~explicit_resume options worker in
  let preview = provisional_dispatch plan in
  let paths = attempt_paths preview (fresh_attempt_id ()) in
  let* () = ensure_attempt_directories preview paths in
  let* implementation_prompt_path =
    write_phase_prompt paths "implementation"
      (implementation_prompt preview inputs)
  in
  let* dispatch = claim_begin plan in
  let* () =
    run_codex_phase plan.options dispatch paths ~name:"implementation"
      ~writable:true ~prompt:implementation_prompt_path
      ~output:paths.implementation
  in
  let* implementation_handoff =
    read_required_file ~label:"Codex implementation handoff"
      paths.implementation
  in
  let* () =
    append_codex_memory dispatch paths ~phase:"implementation"
      implementation_handoff
  in
  let* correctness_prompt_path =
    write_phase_prompt paths "correctness-review"
      (reviewer_prompt dispatch inputs
         "Focus on correctness, regressions, edge cases, data integrity, security, and exact requirement compliance."
         implementation_handoff)
  in
  let* quality_prompt_path =
    write_phase_prompt paths "quality-review"
      (reviewer_prompt dispatch inputs
         "Focus on tests, failure handling, maintainability, simplicity, architectural fit, and missing validation."
         implementation_handoff)
  in
  let* () =
    run_parallel
      (fun () ->
        run_codex_phase plan.options dispatch paths ~name:"correctness-review"
          ~writable:false ~prompt:correctness_prompt_path
          ~output:paths.correctness)
      (fun () ->
        run_codex_phase plan.options dispatch paths ~name:"quality-review"
          ~writable:false ~prompt:quality_prompt_path ~output:paths.quality)
  in
  let* correctness =
    read_required_file ~label:"Codex correctness review" paths.correctness
  in
  let* quality =
    read_required_file ~label:"Codex quality review" paths.quality
  in
  let* fixer_prompt_path =
    write_phase_prompt paths "fixer"
      (fixer_prompt dispatch inputs correctness quality)
  in
  let* () =
    run_codex_phase plan.options dispatch paths ~name:"fixer" ~writable:true
      ~prompt:fixer_prompt_path ~output:paths.final
  in
  let* final_handoff =
    read_required_file ~label:"Codex final handoff" paths.final
  in
  let* () =
    append_codex_memory dispatch paths ~phase:"final" final_handoff
  in
  Ok (codex_run_result_json plan.options dispatch paths)

let preflight_codex_batch options indexed_jobs =
  let* () = require_codex_harness options in
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
             "worker %s is not prepared for an initial Codex run; use monty headless prepare-many or monty headless resume as appropriate"
             worker.id)
  in
  let workers =
    List.map (fun (worker : Launcher.prepared) -> worker.id) prepared
  in
  let* () =
    List.fold_left
      (fun result worker ->
        let* () = result in
        let* _ = preflight_codex_worker ~explicit_resume:false options worker in
        Ok ())
      (Ok ()) workers
  in
  Ok workers

let run_codex_many options indexed_jobs =
  let* workers = preflight_codex_batch options indexed_jobs in
  let children =
    List.map
      (fun worker ->
        ( worker,
          try_spawn_result (fun () ->
              run_codex_worker ~explicit_resume:false options worker) ))
      workers
  in
  let results =
    List.map
      (fun (worker, child) ->
        let result =
          match child with
          | Ok child -> await_result child
          | Error message -> Error message
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
      [ ("schema", `String codex_run_many_schema);
        ("harness", `String "codex");
        ("codex_yolo", `Bool options.Launcher.codex_yolo);
        ("success", `Bool success);
        ("results", `List (List.map snd results)) ]
  in
  Ok (json, success)
