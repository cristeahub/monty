let ( let* ) = Result.bind

let prepare_schema = "monty:headless-prepare:v1"
let dispatch_schema = "monty:headless-dispatch:v3"
let workflow_schema = "monty:tintin-chain:v1"

type prepared_job = {
  id : string;
  title : string;
  branch : string;
  worktree : string option;
  worker_dir : string;
  status : string;
}

type dispatch = {
  id : string;
  title : string;
  repo : string;
  branch : string;
  worktree : string;
  worker_dir : string;
  instructions : string;
  context : string;
  task_key : string option;
  home : string;
}

let prepared_job_json (job : prepared_job) =
  `Assoc
    [ ("id", `String job.id);
      ("title", `String job.title);
      ("branch", `String job.branch);
      ("worktree", Option.fold ~none:`Null ~some:(fun path -> `String path) job.worktree);
      ("worker_dir", `String job.worker_dir);
      ("status", `String job.status) ]

let prepare_json jobs =
  `Assoc
    [ ("schema", `String prepare_schema);
      ("jobs", `List (List.map prepared_job_json jobs)) ]

let fresh_attempt_id () =
  let micros = Int64.of_float (Unix.gettimeofday () *. 1_000_000.) in
  Printf.sprintf "attempt-%Ld-%d" micros (Unix.getpid ())

let mutation_prohibitions =
  String.concat "\n"
    [ "Do not run monty done.";
      "Do not create, switch, or remove worktrees.";
      "Do not stage or commit changes.";
      "Do not push, open a pull request, submit a review, post comments, or perform any other remote write." ]

let implementation_task (dispatch : dispatch) =
  String.concat "\n\n"
    [ Printf.sprintf "Implement Monty worker %s: %s." dispatch.id dispatch.title;
      "Read only the supplied Monty instructions and task context before inspecting the repository.";
      "Then find and read every applicable repository-local instruction file before editing.";
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
      "Start from the supplied task context and inspect the current worktree directly.";
      "Find and read every applicable repository-local instruction file before evaluating the implementation.";
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
      "Read the supplied task context, inspect the current worktree, and review both independent reports below.";
      "Find and read every applicable repository-local instruction file before changing the worktree.";
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

let optional_json = function None -> `Null | Some value -> `String value

let step_json ~role ~phase ~agent ~description ~prompt ~cwd ~reads ~output =
  `Assoc
    [ ("role", `String role);
      ("phase", `String phase);
      ("agent", `String agent);
      ("description", `String description);
      ("prompt", `String prompt);
      ("cwd", `String cwd);
      ("reads", reads_json reads);
      ("output", `String output) ]

let workflow_json (dispatch : dispatch) attempt_id =
  let attempt_root =
    Filename.concat dispatch.worker_dir
      (Filename.concat "artifacts" (Filename.concat "headless" attempt_id))
  in
  let implementation =
    step_json ~role:"implementation" ~phase:"Implementation"
      ~agent:"monty-headless-worker" ~description:("Implement " ^ dispatch.id)
      ~prompt:(implementation_task dispatch) ~cwd:dispatch.worktree
      ~reads:[ dispatch.instructions; dispatch.context ]
      ~output:(Filename.concat attempt_root "implementation.md")
  in
  let review_dir = Filename.concat attempt_root "reviews" in
  let correctness_review =
    step_json ~role:"correctnessReview" ~phase:"Review"
      ~agent:"monty-headless-reviewer" ~description:"Review correctness"
      ~prompt:
        (reviewer_task dispatch
           "Focus on correctness, regressions, edge cases, data integrity, security, and exact requirement compliance.")
      ~cwd:dispatch.worktree ~reads:[ dispatch.context ]
      ~output:(Filename.concat review_dir "correctness.md")
  in
  let quality_review =
    step_json ~role:"qualityReview" ~phase:"Review"
      ~agent:"monty-headless-reviewer" ~description:"Review quality and tests"
      ~prompt:
        (reviewer_task dispatch
           "Focus on tests, failure handling, maintainability, simplicity, architectural fit, and missing validation.")
      ~cwd:dispatch.worktree ~reads:[ dispatch.context ]
      ~output:(Filename.concat review_dir "quality.md")
  in
  let fixer =
    step_json ~role:"final" ~phase:"Fix" ~agent:"monty-headless-worker"
      ~description:"Apply verified fixes" ~prompt:(fixer_task dispatch)
      ~cwd:dispatch.worktree ~reads:[ dispatch.instructions; dispatch.context ]
      ~output:(Filename.concat attempt_root "final.md")
  in
  `Assoc
    [ ("schema", `String workflow_schema);
      ("backend", `String "@tintinweb/pi-subagents");
      ("home", `String dispatch.home);
      ("attempt_id", `String attempt_id);
      ("attempt_root", `String attempt_root);
      ("implementation", implementation);
      ("reviews", `List [ correctness_review; quality_review ]);
      ("fixer", fixer) ]

let worker_json (dispatch : dispatch) =
  `Assoc
    [ ("id", `String dispatch.id);
      ("title", `String dispatch.title);
      ("repo", `String dispatch.repo);
      ("branch", `String dispatch.branch);
      ("worktree", `String dispatch.worktree);
      ("worker_dir", `String dispatch.worker_dir);
      ("instructions", `String dispatch.instructions);
      ("context", `String dispatch.context);
      ("task_key", optional_json dispatch.task_key) ]

let dispatch_json ?attempt_id (dispatch : dispatch) =
  let attempt_id =
    match attempt_id with Some attempt_id -> attempt_id | None -> fresh_attempt_id ()
  in
  let worker = worker_json dispatch in
  let workflow = workflow_json dispatch attempt_id in
  `Assoc
    [ ("schema", `String dispatch_schema);
      ("worker", worker);
      ("workflow", workflow) ]

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
    worker_dir = prepared.worker_dir;
    status = "planned";
  }

let ensure_worktree options (prepared : Launcher.prepared) =
  let expected_status = status_before_prepare prepared in
  let worktree_result =
    match options.Launcher.worktree_mode with
    | Launcher.Never -> Ok prepared.repo
    | Launcher.Always ->
        Wt.create_or_reuse ~wt_command:options.wt_command ~repo:prepared.repo
          ~branch:prepared.branch
  in
  match worktree_result with
  | Error message -> Error message
  | Ok worktree ->
      let* () =
        Launcher.update_launch_state options prepared
          ~expected_statuses:[ expected_status ] ~status:"prepared"
          ~worktree ()
      in
      Ok
        {
          id = prepared.id;
          title = prepared.job.Job.title;
          branch = prepared.branch;
          worktree = Some worktree;
          worker_dir = prepared.worker_dir;
          status = "prepared";
        }

let prepare_many ~dry_run options indexed_jobs =
  let options =
    { options with
      Launcher.backend = Terminal.Dry_run;
      worktree_mode = Launcher.Always }
  in
  let* () = Launcher.check_worktree_dependency options in
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
             "worker %s is already launch-requested; use headless resume when another subagent run is intentional"
             worker.Launcher.id)
  in
  if dry_run then Ok (prepare_json (List.map planned_job prepared))
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
    Ok (prepare_json jobs)

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

let begin_worker ~explicit_resume options worker =
  let* record =
    if explicit_resume then Resume.find_resumable ~home:options.Launcher.home worker
    else Job_store.find ~home:options.home ~scope:Job_store.Active worker
  in
  let* () = validate_begin_status ~explicit_resume record in
  let* () = validate_worktree_mode record in
  let* options, prepared = prepare_existing options record in
  match
    Launcher.begin_request ~persist_failure:false ~write_script:false
      ~validate_current:(Project_overview.validate_worker_task_open_unlocked
        ~home:options.home)
      options prepared ~expected_statuses:[ record.Job_store.status ]
  with
  | `Failed message -> Error message
  | `Ready request ->
      Ok
        (dispatch_json
           {
             id = prepared.id;
             title = prepared.job.Job.title;
             repo = prepared.repo;
             branch = prepared.branch;
             worktree = request.workdir;
             worker_dir = prepared.worker_dir;
             instructions = prepared.instructions;
             context = prepared.context;
             task_key = prepared.job.Job.task_key;
             home = options.home;
           })
