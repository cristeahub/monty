open Yojson.Safe

let ( let* ) = Result.bind

let schema = "monty:run-handoff:v1"
let notice_schema = "monty:run-handoff-notice:v1"
let follow_up_schema = "monty:run-handoff-follow-up:v1"

type source = Interactive | Headless_codex | Headless_pi

type outcome = Ready_for_review | Needs_attention | Failed

type validation = {
  summary : string;
  result : string;
}

type review = {
  accepted : string list;
  fixed : string list;
  rejected : string list;
  unresolved : string list;
  summary : string option;
}

type workspace = {
  repo : string;
  branch : string option;
  worktree : string option;
}

type workspace_change = {
  repo : string;
  branch : string option;
  worktree : string option;
  files : string list;
  insertions : int;
  deletions : int;
  diff_error : string option;
}

type artifact = {
  path : string;
  relative_to_worker : string;
}

type evidence = {
  handoff : string;
  rendered : string;
  worker_memory : string;
  task_context : string;
  artifacts : artifact list;
}

type t = {
  id : string;
  finished_at : string;
  source : source;
  outcome : outcome;
  worker_run_id : string;
  worker_id : string;
  title : string;
  task_key : string option;
  job_status : string;
  summary : string;
  changes : workspace_change list;
  validation : validation list;
  review : review;
  risks : string list;
  last_phase : string option;
  error : string option;
  workspaces : workspace list;
  evidence : evidence;
  next_actions : string list;
}

type reference = {
  worker_run_id : string;
  worker_id : string;
  handoff_id : string;
}

type notice = {
  id : string;
  created_at : string;
  acknowledged_at : string option;
  reference : reference;
  handoff_path : string;
}

type published = {
  handoff : t;
  notice : notice;
}

let source_to_string = function
  | Interactive -> "interactive"
  | Headless_codex -> "headless-codex"
  | Headless_pi -> "headless-pi"

let source_of_string = function
  | "interactive" -> Ok Interactive
  | "headless-codex" -> Ok Headless_codex
  | "headless-pi" -> Ok Headless_pi
  | value -> Error (Printf.sprintf "unknown run-handoff source %S" value)

let outcome_to_string = function
  | Ready_for_review -> "ready-for-review"
  | Needs_attention -> "needs-attention"
  | Failed -> "failed"

let outcome_label = function
  | Ready_for_review -> "Ready for review"
  | Needs_attention -> "Needs attention"
  | Failed -> "Failed"

let outcome_of_string = function
  | "ready-for-review" | "ready" -> Ok Ready_for_review
  | "needs-attention" | "attention" -> Ok Needs_attention
  | "failed" | "failure" -> Ok Failed
  | value ->
      Error
        (Printf.sprintf
           "unknown run-handoff outcome %S; expected ready-for-review, needs-attention, or failed"
           value)

let optional_string json name =
  match Util.member name json with
  | `Null -> Ok None
  | `String value when String.trim value = "" -> Ok None
  | `String value -> Ok (Some value)
  | _ -> Error (Printf.sprintf "run handoff field %S must be a string when present" name)

let required_string json name =
  match Util.member name json with
  | `String value when String.trim value <> "" -> Ok value
  | _ -> Error (Printf.sprintf "run handoff missing string field %S" name)

let required_nonnegative_int json name =
  match Util.member name json with
  | `Int value when value >= 0 -> Ok value
  | _ ->
      Error
        (Printf.sprintf "run handoff field %S must be a non-negative integer" name)

let string_list json name =
  match Util.member name json with
  | `List values ->
      values
      |> List.fold_left
           (fun result -> function
             | `String value when String.trim value <> "" ->
                 let* values = result in
                 Ok (value :: values)
             | _ ->
                 Error
                   (Printf.sprintf
                      "run handoff field %S must contain only strings" name))
           (Ok [])
      |> Result.map List.rev
  | _ -> Error (Printf.sprintf "run handoff field %S must be an array" name)

let require_absolute ~label path =
  if Filename.is_relative path then
    Error (Printf.sprintf "%s must be absolute: %s" label path)
  else Ok (Shell.normalize path)

let safe_relative_path value =
  if value = "" || not (Filename.is_relative value) then
    Error (Printf.sprintf "artifact path must be a non-empty relative path: %S" value)
  else
    let parts = String.split_on_char '/' value in
    if
      List.exists
        (fun part -> part = "" || part = "." || part = "..")
        parts
    then Error (Printf.sprintf "artifact path contains an unsafe component: %S" value)
    else Ok (String.concat "/" parts)

let parse_workspace json =
  let* repo = required_string json "repo" in
  let* repo = require_absolute ~label:"workspace repo" repo in
  let* branch = optional_string json "branch" in
  let* worktree = optional_string json "worktree" in
  let* worktree =
    match worktree with
    | None -> Ok None
    | Some path -> require_absolute ~label:"workspace worktree" path |> Result.map Option.some
  in
  Ok { repo; branch; worktree }

let parse_workspace_change json =
  let* workspace = parse_workspace json in
  let* files = string_list json "files" in
  let* insertions = required_nonnegative_int json "insertions" in
  let* deletions = required_nonnegative_int json "deletions" in
  let* diff_error = optional_string json "diff_error" in
  Ok
    {
      repo = workspace.repo;
      branch = workspace.branch;
      worktree = workspace.worktree;
      files;
      insertions;
      deletions;
      diff_error;
    }

let parse_validation = function
  | `Assoc _ as json ->
      let* summary = required_string json "summary" in
      let* result = required_string json "result" in
      Ok { summary; result }
  | _ -> Error "run handoff validation entries must be objects"

let parse_artifact = function
  | `Assoc _ as json ->
      let* path = required_string json "path" in
      let* path = require_absolute ~label:"artifact evidence path" path in
      let* relative_to_worker = required_string json "relative_to_worker" in
      let* relative_to_worker = safe_relative_path relative_to_worker in
      Ok { path; relative_to_worker }
  | _ -> Error "run handoff artifact entries must be objects"

let parse_list parser label = function
  | `List values ->
      values
      |> List.fold_left
           (fun result value ->
             let* values = result in
             let* value = parser value in
             Ok (value :: values))
           (Ok [])
      |> Result.map List.rev
  | _ -> Error (Printf.sprintf "run handoff field %S must be an array" label)

let optional_assoc json name =
  match Util.member name json with
  | `Assoc _ as value -> Ok value
  | _ -> Error (Printf.sprintf "run handoff field %S must be an object" name)

let json_of_workspace (workspace : workspace) =
  `Assoc
    ([ ("repo", `String workspace.repo) ]
    @ (match workspace.branch with
      | None -> []
      | Some value -> [ ("branch", `String value) ])
    @
    match workspace.worktree with
    | None -> []
    | Some value -> [ ("worktree", `String value) ])

let json_of_workspace_change (change : workspace_change) =
  `Assoc
    ([ ("repo", `String change.repo);
       ("files", `List (List.map (fun value -> `String value) change.files));
       ("insertions", `Int change.insertions);
       ("deletions", `Int change.deletions) ]
    @ (match change.branch with
      | None -> []
      | Some value -> [ ("branch", `String value) ])
    @ (match change.worktree with
      | None -> []
      | Some value -> [ ("worktree", `String value) ])
    @
    match change.diff_error with
    | None -> []
    | Some value -> [ ("diff_error", `String value) ])

let json_of_validation (validation : validation) =
  `Assoc
    [ ("summary", `String validation.summary);
      ("result", `String validation.result) ]

let json_of_artifact (artifact : artifact) =
  `Assoc
    [ ("path", `String artifact.path);
      ("relative_to_worker", `String artifact.relative_to_worker) ]

let string_jsons values = `List (List.map (fun value -> `String value) values)

let to_json (handoff : t) =
  `Assoc
    [ ("schema", `String schema);
      ("id", `String handoff.id);
      ("finished_at", `String handoff.finished_at);
      ("source", `String (source_to_string handoff.source));
      ("outcome", `String (outcome_to_string handoff.outcome));
      ( "worker",
        `Assoc
          ([ ("run_id", `String handoff.worker_run_id);
             ("id", `String handoff.worker_id);
             ("title", `String handoff.title);
             ("job_status", `String handoff.job_status) ]
          @
          match handoff.task_key with
          | None -> []
          | Some value -> [ ("task_key", `String value) ]) );
      ("summary", `String handoff.summary);
      ("changes", `List (List.map json_of_workspace_change handoff.changes));
      ("validation", `List (List.map json_of_validation handoff.validation));
      ( "review",
        `Assoc
          ([ ("accepted", string_jsons handoff.review.accepted);
             ("fixed", string_jsons handoff.review.fixed);
             ("rejected", string_jsons handoff.review.rejected);
             ("unresolved", string_jsons handoff.review.unresolved) ]
          @
          match handoff.review.summary with
          | None -> []
          | Some value -> [ ("summary", `String value) ]) );
      ("risks", string_jsons handoff.risks);
      ( "last_phase",
        Option.fold ~none:`Null ~some:(fun value -> `String value) handoff.last_phase );
      ("error", Option.fold ~none:`Null ~some:(fun value -> `String value) handoff.error);
      ("workspaces", `List (List.map json_of_workspace handoff.workspaces));
      ( "evidence",
        `Assoc
          [ ("handoff", `String handoff.evidence.handoff);
            ("rendered", `String handoff.evidence.rendered);
            ("worker_memory", `String handoff.evidence.worker_memory);
            ("task_context", `String handoff.evidence.task_context);
            ("artifacts", `List (List.map json_of_artifact handoff.evidence.artifacts)) ] );
      ("next_actions", string_jsons handoff.next_actions) ]

let of_json json =
  let* parsed_schema = required_string json "schema" in
  if not (String.equal parsed_schema schema) then
    Error
      (Printf.sprintf "unsupported run handoff schema %S; expected %S" parsed_schema
         schema)
  else
    let* id = required_string json "id" in
    let* id = State_path.safe_component ~label:"run handoff id" id in
    let* finished_at = required_string json "finished_at" in
    let* source = required_string json "source" in
    let* source = source_of_string source in
    let* outcome = required_string json "outcome" in
    let* outcome = outcome_of_string outcome in
    let* worker = optional_assoc json "worker" in
    let* worker_run_id = required_string worker "run_id" in
    let* worker_run_id = State_path.safe_component ~label:"worker run id" worker_run_id in
    let* worker_id = required_string worker "id" in
    let* worker_id = State_path.safe_component ~label:"worker id" worker_id in
    let* title = required_string worker "title" in
    let* task_key = optional_string worker "task_key" in
    let* job_status = required_string worker "job_status" in
    let* summary = required_string json "summary" in
    let* changes = parse_list parse_workspace_change "changes" (Util.member "changes" json) in
    let* validation = parse_list parse_validation "validation" (Util.member "validation" json) in
    let* review_json = optional_assoc json "review" in
    let* accepted = string_list review_json "accepted" in
    let* fixed = string_list review_json "fixed" in
    let* rejected = string_list review_json "rejected" in
    let* unresolved = string_list review_json "unresolved" in
    let* review_summary = optional_string review_json "summary" in
    let review = { accepted; fixed; rejected; unresolved; summary = review_summary } in
    let* risks = string_list json "risks" in
    let* last_phase = optional_string json "last_phase" in
    let* error = optional_string json "error" in
    let* workspaces = parse_list parse_workspace "workspaces" (Util.member "workspaces" json) in
    let* evidence_json = optional_assoc json "evidence" in
    let* handoff_path = required_string evidence_json "handoff" in
    let* handoff_path = require_absolute ~label:"canonical handoff path" handoff_path in
    let* rendered = required_string evidence_json "rendered" in
    let* rendered = require_absolute ~label:"rendered handoff path" rendered in
    let* worker_memory = required_string evidence_json "worker_memory" in
    let* worker_memory = require_absolute ~label:"worker memory path" worker_memory in
    let* task_context = required_string evidence_json "task_context" in
    let* task_context = require_absolute ~label:"task context path" task_context in
    let* artifacts =
      parse_list parse_artifact "artifacts" (Util.member "artifacts" evidence_json)
    in
    let evidence =
      {
        handoff = handoff_path;
        rendered;
        worker_memory;
        task_context;
        artifacts;
      }
    in
    let* next_actions = string_list json "next_actions" in
    Ok
      {
        id;
        finished_at;
        source;
        outcome;
        worker_run_id;
        worker_id;
        title;
        task_key;
        job_status;
        summary;
        changes;
        validation;
        review;
        risks;
        last_phase;
        error;
        workspaces;
        evidence;
        next_actions;
      }

let compact_line value =
  value |> String.split_on_char '\n' |> List.map String.trim
  |> List.filter (fun line -> line <> "") |> String.concat " "

let truncate length value =
  if String.length value <= length then value
  else String.sub value 0 (max 0 (length - 1)) ^ "…"

let compact_summary value = value |> compact_line |> truncate 360

let summarize_changes (changes : workspace_change list) =
  let files =
    changes
    |> List.fold_left
         (fun acc change -> List.rev_append change.files acc)
         []
    |> List.sort_uniq String.compare |> List.length
  in
  let insertions =
    List.fold_left (fun total change -> total + change.insertions) 0 changes
  in
  let deletions =
    List.fold_left (fun total change -> total + change.deletions) 0 changes
  in
  let errors = List.filter (fun change -> change.diff_error <> None) changes |> List.length in
  let summary = Printf.sprintf "%d files · +%d/−%d" files insertions deletions in
  if errors = 0 then summary
  else Printf.sprintf "%s · %d workspace diff unavailable" summary errors

let summarize_validation (validations : validation list) =
  match validations with
  | [] -> "Not reported"
  | values ->
      values
      |> List.map (fun (item : validation) -> compact_summary item.summary)
      |> List.map (truncate 100) |> String.concat " · "

let summarize_review (review : review) =
  let parts =
    [ (List.length review.fixed, "fixed");
      (List.length review.accepted, "accepted");
      (List.length review.rejected, "rejected");
      (List.length review.unresolved, "unresolved") ]
    |> List.filter_map (fun (count, label) ->
           if count = 0 then None else Some (Printf.sprintf "%d %s" count label))
  in
  match (parts, review.summary) with
  | [], None -> "Not reported"
  | [], Some summary -> compact_summary summary |> truncate 180
  | parts, None -> String.concat " · " parts
  | parts, Some summary ->
      String.concat " · " parts ^ " · " ^ (compact_summary summary |> truncate 140)

let summarize_workspaces (workspaces : workspace list) =
  match workspaces with
  | [] -> "None"
  | [ workspace ] ->
      Option.value ~default:workspace.repo workspace.branch
  | values ->
      values
      |> List.map (fun (workspace : workspace) ->
             Option.value ~default:workspace.repo workspace.branch)
      |> String.concat " · "

let summarize_risks risks =
  match risks with
  | [] -> "None reported"
  | values -> values |> List.map compact_summary |> List.map (truncate 120) |> String.concat " · "

let render_markdown ?notice_id (handoff : t) =
  let notice_line =
    match notice_id with
    | None -> []
    | Some value -> [ "**Notice:** `" ^ value ^ "`" ]
  in
  String.concat "\n"
    ([ "### 🛎 Monty run finished";
       "";
       "**Task:** " ^ handoff.title;
       "**Outcome:** " ^ outcome_label handoff.outcome;
       "";
       "> " ^ compact_summary handoff.summary;
       "";
       "**Changed:** " ^ summarize_changes handoff.changes;
       "**Checks:** " ^ summarize_validation handoff.validation;
       "**Review:** " ^ summarize_review handoff.review;
       "**Risks:** " ^ summarize_risks handoff.risks;
       "**Workspace:** " ^ summarize_workspaces handoff.workspaces;
       "**Evidence:** `" ^ handoff.evidence.handoff ^ "`";
       "**Run:** `" ^ handoff.id ^ "`" ]
    @ notice_line
    @ [ "";
        "Next: **ask for details** · **continue interactively** · **run another pass** · **create a draft PR** · **mark done later**";
        "" ])

let render_plain ?notice_id (handoff : t) =
  let lines =
    [ "Monty run finished";
      "Task: " ^ handoff.title;
      "Outcome: " ^ outcome_label handoff.outcome;
      "Summary: " ^ compact_summary handoff.summary;
      "Changed: " ^ summarize_changes handoff.changes;
      "Checks: " ^ summarize_validation handoff.validation;
      "Review: " ^ summarize_review handoff.review;
      "Risks: " ^ summarize_risks handoff.risks;
      "Workspace: " ^ summarize_workspaces handoff.workspaces;
      "Evidence: " ^ handoff.evidence.handoff;
      "Run: " ^ handoff.id ]
  in
  let lines =
    match notice_id with None -> lines | Some value -> lines @ [ "Notice: " ^ value ]
  in
  String.concat "\n"
    (lines
    @ [ "Next: ask for details | continue interactively | run another pass | create a draft PR | mark done later";
        "" ])

let reference_of_record (record : Job_store.record) handoff_id =
  match record.state_path with
  | None -> Error "run handoffs require a worker loaded from canonical Monty state"
  | Some state ->
      Ok
        {
          worker_run_id = state.State_path.run_id;
          worker_id = state.id;
          handoff_id;
        }

let handoff_root home = Filename.concat home ".monty/handoffs" |> Shell.normalize

let handoff_dir home reference =
  Filename.concat (handoff_root home)
    (Filename.concat reference.worker_run_id reference.worker_id)
  |> Shell.normalize

let handoff_path home reference =
  Filename.concat (handoff_dir home reference) (reference.handoff_id ^ ".json")

let rendered_path home reference =
  Filename.concat (handoff_dir home reference) (reference.handoff_id ^ ".md")

let inbox_root home = Filename.concat home ".monty/inbox/run-handoffs" |> Shell.normalize

let notice_id reference =
  let encoded prefix value =
    Printf.sprintf "%s%d.%s" prefix (String.length value) value
  in
  String.concat "--"
    [ encoded "r" reference.worker_run_id;
      encoded "w" reference.worker_id;
      encoded "h" reference.handoff_id ]

let notice_path home id = Filename.concat (inbox_root home) (id ^ ".json")

let fresh_handoff_id () =
  let micros = Int64.of_float (Unix.gettimeofday () *. 1_000_000.) in
  Printf.sprintf "run-%Ld-%d" micros (Unix.getpid ())

let current_record_unlocked ~home reference =
  let* active =
    State_path.active ~home ~run_id:reference.worker_run_id ~id:reference.worker_id
  in
  let* archived =
    State_path.archived ~home ~run_id:reference.worker_run_id ~id:reference.worker_id
  in
  let active_exists = State_path.path_exists active.job_file in
  let archived_exists = State_path.path_exists archived.job_file in
  match (active_exists, archived_exists) with
  | true, true ->
      Error
        (Printf.sprintf
           "worker %s exists at both active and archive paths; repair the lifecycle collision before publishing a handoff"
           reference.worker_id)
  | false, false ->
      Error
        (Printf.sprintf "worker state disappeared while publishing run %s"
           reference.handoff_id)
  | true, false -> Job_store.parse_job_file ~home active.job_file
  | false, true -> Job_store.parse_job_file ~home archived.job_file

let parse_numstat_line line =
  match String.split_on_char '\t' line with
  | inserted :: deleted :: path_parts when path_parts <> [] ->
      let integer value = match int_of_string_opt value with Some value -> value | None -> 0 in
      Some (String.concat "\t" path_parts, integer inserted, integer deleted)
  | _ -> None

let lines value =
  value |> String.split_on_char '\n' |> List.map String.trim
  |> List.filter (fun line -> line <> "")

let inspect_workspace_change (workspace : Job_store.workspace_state) =
  let base diff_error files insertions deletions =
    {
      repo = workspace.repo;
      branch = workspace.branch;
      worktree = workspace.worktree;
      files;
      insertions;
      deletions;
      diff_error;
    }
  in
  match workspace.worktree with
  | None -> base (Some "worktree is not materialized") [] 0 0
  | Some worktree when not (Sys.file_exists worktree && Sys.is_directory worktree) ->
      base (Some ("worktree is unavailable: " ^ worktree)) [] 0 0
  | Some worktree -> (
      match
        ( Process.run_capture ~cwd:worktree "git diff --numstat HEAD --",
          Process.run_capture ~cwd:worktree
            "git ls-files --others --exclude-standard" )
      with
      | Ok { status = `Exited 0; stdout = diff },
        Ok { status = `Exited 0; stdout = untracked } ->
          let tracked = lines diff |> List.filter_map parse_numstat_line in
          let files =
            (List.map (fun (path, _, _) -> path) tracked @ lines untracked)
            |> List.sort_uniq String.compare
          in
          let insertions =
            List.fold_left (fun total (_, value, _) -> total + value) 0 tracked
          in
          let deletions =
            List.fold_left (fun total (_, _, value) -> total + value) 0 tracked
          in
          base None files insertions deletions
      | Error message, _ | _, Error message -> base (Some message) [] 0 0
      | Ok { status = `Exited 0; _ }, Ok { status; _ } ->
          base
            (Some
               (Printf.sprintf "git untracked-file scan failed with %s"
                  (Process.status_to_string status)))
            [] 0 0
      | Ok { status; _ }, _ ->
          base
            (Some
               (Printf.sprintf "git diff failed with %s"
                  (Process.status_to_string status)))
            [] 0 0
      )

let workspace_of_state (workspace : Job_store.workspace_state) =
  { repo = workspace.repo; branch = workspace.branch; worktree = workspace.worktree }

let ensure_handoff_directories ~home reference =
  let handoffs = handoff_root home in
  let handoff_run = Filename.concat handoffs reference.worker_run_id in
  let inbox = Filename.concat home ".monty/inbox" in
  let* () =
    State_store.ensure_real_directory ~label:"run-handoff root" ~mode:0o700
      handoffs
  in
  let* () =
    State_store.ensure_real_directory ~label:"run-handoff run directory"
      ~mode:0o700 handoff_run
  in
  let* () =
    State_store.ensure_real_directory ~label:"run-handoff worker directory"
      ~mode:0o700 (handoff_dir home reference)
  in
  let* () =
    State_store.ensure_real_directory ~label:"run-handoff inbox parent"
      ~mode:0o700 inbox
  in
  State_store.ensure_real_directory ~label:"run-handoff inbox" ~mode:0o700
    (inbox_root home)

let directory_chain_state ~label paths =
  paths
  |> List.fold_left
       (fun result path ->
         let* present = result in
         if not present then Ok false
         else
           let* state = State_store.lstat path in
           match state with
           | Some { Unix.st_kind = Unix.S_DIR; _ } -> Ok true
           | Some { Unix.st_kind = Unix.S_LNK; _ } ->
               Error (Printf.sprintf "unsafe %s is a symlink: %s" label path)
           | Some _ -> Error (Printf.sprintf "%s is not a directory: %s" label path)
           | None -> Ok false)
       (Ok true)

let handoff_directory_chain home reference =
  let root = handoff_root home in
  [ Filename.concat home ".monty";
    root;
    Filename.concat root reference.worker_run_id;
    handoff_dir home reference ]

let inbox_directory_chain home =
  [ Filename.concat home ".monty";
    Filename.concat home ".monty/inbox";
    inbox_root home ]

let require_regular_file path =
  let* state = State_store.lstat path in
  match state with
  | Some { Unix.st_kind = Unix.S_REG; _ } -> Ok ()
  | Some { Unix.st_kind = Unix.S_LNK; _ } ->
      Error (Printf.sprintf "unsafe run-handoff file is a symlink: %s" path)
  | Some _ -> Error (Printf.sprintf "run-handoff path is not a regular file: %s" path)
  | None -> Error (Printf.sprintf "run-handoff file is missing: %s" path)

let read_handoff_file ~path =
  let* () = require_regular_file path in
  let* json = State_store.read_json ~path in
  match json with
  | None -> Error (Printf.sprintf "run-handoff file is missing: %s" path)
  | Some json -> of_json json

let validate_loaded_reference ~home reference (handoff : t) =
  let expected = handoff_path home reference in
  let expected_rendered = rendered_path home reference in
  if not (String.equal handoff.id reference.handoff_id) then
    Error
      (Printf.sprintf "run-handoff id %S does not match its canonical filename %S"
         handoff.id reference.handoff_id)
  else if
    not
      (String.equal handoff.worker_run_id reference.worker_run_id
      && String.equal handoff.worker_id reference.worker_id)
  then Error "run-handoff worker identity does not match its canonical path"
  else if not (String.equal handoff.evidence.handoff expected) then
    Error
      (Printf.sprintf "forged canonical handoff path %s; expected %s"
         handoff.evidence.handoff expected)
  else if not (String.equal handoff.evidence.rendered expected_rendered) then
    Error
      (Printf.sprintf "forged rendered handoff path %s; expected %s"
         handoff.evidence.rendered expected_rendered)
  else
    let* active =
      State_path.active ~home ~run_id:reference.worker_run_id
        ~id:reference.worker_id
    in
    let* archived =
      State_path.archived ~home ~run_id:reference.worker_run_id
        ~id:reference.worker_id
    in
    let active_memory = Worker_memory.memory_file active.worker_dir in
    let archived_memory = Worker_memory.memory_file archived.worker_dir in
    if
      not
        (String.equal handoff.evidence.worker_memory active_memory
        || String.equal handoff.evidence.worker_memory archived_memory)
    then
      Error
        (Printf.sprintf
           "forged worker-memory evidence path %s; expected %s or %s"
           handoff.evidence.worker_memory active_memory archived_memory)
    else
      let worker_dir = Filename.dirname handoff.evidence.worker_memory in
      let forged_artifact =
        List.find_opt
          (fun artifact ->
            let expected = Filename.concat worker_dir artifact.relative_to_worker in
            not (String.equal artifact.path expected))
          handoff.evidence.artifacts
      in
      (match forged_artifact with
      | None -> Ok handoff
      | Some artifact ->
          Error
            (Printf.sprintf
               "forged artifact evidence path %s for worker-relative path %s"
               artifact.path artifact.relative_to_worker))

let load_reference ~home reference =
  let* home = State_path.canonicalize home in
  let* hierarchy = directory_chain_state ~label:"run-handoff hierarchy"
      (handoff_directory_chain home reference)
  in
  let* () =
    if hierarchy then Ok ()
    else Error (Printf.sprintf "run-handoff directory is missing: %s" (handoff_dir home reference))
  in
  let path = handoff_path home reference in
  let* handoff = read_handoff_file ~path in
  validate_loaded_reference ~home reference handoff

let notice_to_json notice =
  `Assoc
    [ ("schema", `String notice_schema);
      ("id", `String notice.id);
      ("created_at", `String notice.created_at);
      ( "acknowledged_at",
        Option.fold ~none:`Null ~some:(fun value -> `String value)
          notice.acknowledged_at );
      ( "handoff",
        `Assoc
          [ ("worker_run_id", `String notice.reference.worker_run_id);
            ("worker_id", `String notice.reference.worker_id);
            ("id", `String notice.reference.handoff_id);
            ("path", `String notice.handoff_path) ] ) ]

let notice_of_json ~home json =
  let* parsed_schema = required_string json "schema" in
  if not (String.equal parsed_schema notice_schema) then
    Error
      (Printf.sprintf "unsupported run-handoff notice schema %S" parsed_schema)
  else
    let* id = required_string json "id" in
    let* id = State_path.safe_component ~label:"run-handoff notice id" id in
    let* created_at = required_string json "created_at" in
    let* acknowledged_at = optional_string json "acknowledged_at" in
    let* handoff = optional_assoc json "handoff" in
    let* worker_run_id = required_string handoff "worker_run_id" in
    let* worker_run_id = State_path.safe_component ~label:"worker run id" worker_run_id in
    let* worker_id = required_string handoff "worker_id" in
    let* worker_id = State_path.safe_component ~label:"worker id" worker_id in
    let* handoff_id = required_string handoff "id" in
    let* handoff_id = State_path.safe_component ~label:"run handoff id" handoff_id in
    let reference = { worker_run_id; worker_id; handoff_id } in
    let expected_id = notice_id reference in
    let* handoff_path_value = required_string handoff "path" in
    let* handoff_path_value = require_absolute ~label:"notice handoff path" handoff_path_value in
    let expected_path = handoff_path home reference in
    if not (String.equal id expected_id) then
      Error
        (Printf.sprintf "run-handoff notice id %S does not match reference %S" id
           expected_id)
    else if not (String.equal handoff_path_value expected_path) then
      Error
        (Printf.sprintf "forged run-handoff notice path %s; expected %s"
           handoff_path_value expected_path)
    else
      Ok
        {
          id;
          created_at;
          acknowledged_at;
          reference;
          handoff_path = handoff_path_value;
        }

let load_notice_file ~home path =
  let* () = require_regular_file path in
  let* json = State_store.read_json ~path in
  match json with
  | None -> Error (Printf.sprintf "run-handoff notice is missing: %s" path)
  | Some json ->
      let* notice = notice_of_json ~home json in
      let expected = notice_path home notice.id in
      if String.equal (Shell.normalize path) expected then Ok notice
      else
        Error
          (Printf.sprintf
             "run-handoff notice filename does not match its canonical id: %s; expected %s"
             path expected)

let relative_artifact ~worker_dir value =
  if Filename.is_relative value then safe_relative_path value
  else
    let normalized = Shell.normalize value in
    match State_path.split_relative ~root:worker_dir normalized with
    | Some parts when parts <> [] -> safe_relative_path (String.concat "/" parts)
    | _ ->
        Error
          (Printf.sprintf
             "artifact evidence must be inside durable worker memory %s: %s"
             worker_dir value)

let default_next_actions =
  [ "ask-for-details";
    "continue-interactively";
    "run-another-pass";
    "create-draft-pr";
    "mark-done-later" ]

let normalize_report_values ~label values =
  values
  |> List.fold_left
       (fun result value ->
         let* values = result in
         let value = compact_summary value in
         if value = "" then
           Error (Printf.sprintf "run handoff %s must not contain an empty value" label)
         else Ok (value :: values))
       (Ok [])
  |> Result.map List.rev

let normalize_optional_report_value ~label = function
  | None -> Ok None
  | Some value ->
      let value = compact_summary value in
      if value = "" then Error (Printf.sprintf "run handoff %s must not be empty" label)
      else Ok (Some value)

let publication_checkpoint name =
  match Sys.getenv_opt "MONTY_FAULT_INJECT" with
  | Some value when String.equal value name ->
      Error (Printf.sprintf "fault injected at %s" name)
  | _ -> Ok ()

let publish ~home ~(record : Job_store.record) ?handoff_id ~source ~outcome
    ~summary ?(validation = []) ?(accepted = []) ?(fixed = []) ?(rejected = [])
    ?(unresolved = []) ?review_summary ?(risks = []) ?last_phase ?error
    ?(artifacts = []) () =
  let summary = compact_summary summary in
  if summary = "" then Error "run handoff summary must not be empty"
  else
    let handoff_id = Option.value ~default:(fresh_handoff_id ()) handoff_id in
    let* handoff_id = State_path.safe_component ~label:"run handoff id" handoff_id in
    let* reference = reference_of_record record handoff_id in
    let state_home = Option.value ~default:home record.home in
    let* home = State_path.canonicalize home in
    let* validation = normalize_report_values ~label:"validation" validation in
    let* accepted = normalize_report_values ~label:"accepted findings" accepted in
    let* fixed = normalize_report_values ~label:"fixed findings" fixed in
    let* rejected = normalize_report_values ~label:"rejected findings" rejected in
    let* unresolved =
      normalize_report_values ~label:"unresolved findings" unresolved
    in
    let* risks = normalize_report_values ~label:"risks" risks in
    let* review_summary =
      normalize_optional_report_value ~label:"review summary" review_summary
    in
    let* last_phase =
      normalize_optional_report_value ~label:"last phase" last_phase
    in
    let* error = normalize_optional_report_value ~label:"error" error in
    let artifact_base = record.worker_dir in
    let* artifact_relatives =
      artifacts
      |> List.fold_left
           (fun result value ->
             let* values = result in
             let* value = relative_artifact ~worker_dir:artifact_base value in
             Ok (value :: values))
           (Ok [])
      |> Result.map (fun values -> List.rev values |> List.sort_uniq String.compare)
    in
    (* Git is intentionally inspected before taking Monty's one-home state lock. *)
    let changes = List.map inspect_workspace_change record.workspaces in
    let finished_at = Worker_memory.now_utc () in
    State_store.with_lock ~home (fun () ->
        let* current = current_record_unlocked ~home:state_home reference in
        let* () = ensure_handoff_directories ~home reference in
        let json_path = handoff_path home reference in
        let markdown_path = rendered_path home reference in
        let notice_id = notice_id reference in
        let notice_path = notice_path home notice_id in
        let build () =
          let artifacts =
            artifact_relatives
            |> List.map (fun relative_to_worker ->
                   {
                     path = Filename.concat current.worker_dir relative_to_worker;
                     relative_to_worker;
                   })
          in
          let workspaces = List.map workspace_of_state current.workspaces in
          let review =
            { accepted; fixed; rejected; unresolved; summary = review_summary }
          in
          {
            id = handoff_id;
            finished_at;
            source;
            outcome;
            worker_run_id = reference.worker_run_id;
            worker_id = reference.worker_id;
            title = current.job.Job.title;
            task_key = current.job.task_key;
            job_status = current.status;
            summary;
            changes;
            validation =
              List.map (fun summary -> { summary; result = "reported" })
                validation;
            review;
            risks;
            last_phase;
            error;
            workspaces;
            evidence =
              {
                handoff = json_path;
                rendered = markdown_path;
                worker_memory = Worker_memory.memory_file current.worker_dir;
                task_context = current.job.context;
                artifacts;
              };
            next_actions = default_next_actions;
          }
        in
        let* handoff, created_handoff =
          match State_store.lstat json_path with
          | Error _ as error -> error
          | Ok (Some { Unix.st_kind = Unix.S_LNK; _ }) ->
              Error (Printf.sprintf "unsafe canonical handoff is a symlink: %s" json_path)
          | Ok (Some { Unix.st_kind = Unix.S_REG; _ }) ->
              let* existing = read_handoff_file ~path:json_path in
              let* existing = validate_loaded_reference ~home reference existing in
              Ok (existing, false)
          | Ok (Some _) ->
              Error (Printf.sprintf "canonical handoff path is not a file: %s" json_path)
          | Ok None ->
              let handoff = build () in
              let* () = State_store.write_json_atomic ~path:json_path (to_json handoff) in
              Ok (handoff, true)
        in
        let* () =
          if created_handoff then
            publication_checkpoint "run-handoff-after-canonical"
          else Ok ()
        in
        let* created_rendered =
          match State_store.lstat markdown_path with
          | Error _ as error -> error
          | Ok (Some { Unix.st_kind = Unix.S_LNK; _ }) ->
              Error (Printf.sprintf "unsafe rendered handoff is a symlink: %s" markdown_path)
          | Ok (Some { Unix.st_kind = Unix.S_REG; _ }) -> Ok false
          | Ok (Some _) ->
              Error (Printf.sprintf "rendered handoff path is not a file: %s" markdown_path)
          | Ok None ->
              let* () =
                State_store.write_file_atomic ~path:markdown_path ~perm:0o600
                  (render_markdown handoff)
              in
              Ok true
        in
        let* () =
          if created_rendered then
            publication_checkpoint "run-handoff-after-rendered"
          else Ok ()
        in
        let notice =
          {
            id = notice_id;
            created_at = handoff.finished_at;
            acknowledged_at =
              (match handoff.source with
              | Interactive -> Some handoff.finished_at
              | Headless_codex | Headless_pi -> None);
            reference;
            handoff_path = json_path;
          }
        in
        let* notice =
          match State_store.lstat notice_path with
          | Error _ as error -> error
          | Ok (Some { Unix.st_kind = Unix.S_LNK; _ }) ->
              Error (Printf.sprintf "unsafe run-handoff notice is a symlink: %s" notice_path)
          | Ok (Some { Unix.st_kind = Unix.S_REG; _ }) ->
              let* existing = load_notice_file ~home notice_path in
              (match (handoff.source, existing.acknowledged_at) with
              | Interactive, None ->
                  let existing =
                    {
                      existing with
                      acknowledged_at = Some handoff.finished_at;
                    }
                  in
                  let* () =
                    State_store.write_json_atomic ~path:notice_path
                      (notice_to_json existing)
                  in
                  Ok existing
              | _ -> Ok existing)
          | Ok (Some _) ->
              Error (Printf.sprintf "run-handoff notice path is not a file: %s" notice_path)
          | Ok None ->
              let* () = State_store.write_json_atomic ~path:notice_path (notice_to_json notice) in
              Ok notice
        in
        if notice.reference <> reference then
          Error "existing run-handoff notice references a different canonical handoff"
        else Ok { handoff; notice })

let directory_entries ~label root =
  let* state = State_store.lstat root in
  match state with
  | None -> Ok []
  | Some { Unix.st_kind = Unix.S_LNK; _ } ->
      Error (Printf.sprintf "unsafe %s is a symlink: %s" label root)
  | Some { Unix.st_kind = Unix.S_DIR; _ } -> (
      try Ok (Sys.readdir root |> Array.to_list |> List.sort String.compare)
      with Sys_error message -> Error message)
  | Some _ -> Error (Printf.sprintf "%s is not a directory: %s" label root)

let recover_orphaned_notices ~home =
  let* home = State_path.canonicalize home in
  State_store.with_lock ~home (fun () ->
      let root = handoff_root home in
      let* root_state = State_store.lstat root in
      match root_state with
      | None -> Ok 0
      | Some { Unix.st_kind = Unix.S_LNK; _ } ->
          Error (Printf.sprintf "unsafe run-handoff root is a symlink: %s" root)
      | Some { Unix.st_kind = Unix.S_DIR; _ } ->
          let* run_ids = directory_entries ~label:"run-handoff root" root in
          run_ids
          |> List.fold_left
               (fun result worker_run_id ->
                 let* repaired = result in
                 let* worker_run_id =
                   State_path.safe_component ~label:"worker run id" worker_run_id
                 in
                 let run_dir = Filename.concat root worker_run_id in
                 let* worker_ids =
                   directory_entries ~label:"run-handoff run directory" run_dir
                 in
                 worker_ids
                 |> List.fold_left
                      (fun result worker_id ->
                        let* repaired = result in
                        let* worker_id =
                          State_path.safe_component ~label:"worker id" worker_id
                        in
                        let worker_dir = Filename.concat run_dir worker_id in
                        let* names =
                          directory_entries ~label:"run-handoff worker directory"
                            worker_dir
                        in
                        names
                        |> List.filter (String.ends_with ~suffix:".json")
                        |> List.fold_left
                             (fun result name ->
                               let* repaired = result in
                               let handoff_id =
                                 String.sub name 0 (String.length name - 5)
                               in
                               let* handoff_id =
                                 State_path.safe_component
                                   ~label:"run handoff id" handoff_id
                               in
                               let reference =
                                 { worker_run_id; worker_id; handoff_id }
                               in
                               let* handoff = load_reference ~home reference in
                               let* () = ensure_handoff_directories ~home reference in
                               let markdown = rendered_path home reference in
                               let* repaired, rendered_created =
                                 match State_store.lstat markdown with
                                 | Error _ as error -> error
                                 | Ok (Some { Unix.st_kind = Unix.S_LNK; _ }) ->
                                     Error
                                       (Printf.sprintf
                                          "unsafe rendered handoff is a symlink: %s"
                                          markdown)
                                 | Ok (Some { Unix.st_kind = Unix.S_REG; _ }) ->
                                     Ok (repaired, false)
                                 | Ok (Some _) ->
                                     Error
                                       (Printf.sprintf
                                          "rendered handoff path is not a file: %s"
                                          markdown)
                                 | Ok None ->
                                     let* () =
                                       State_store.write_file_atomic ~path:markdown
                                         ~perm:0o600 (render_markdown handoff)
                                     in
                                     Ok (repaired + 1, true)
                               in
                               let id = notice_id reference in
                               let path = notice_path home id in
                               let expected =
                                 {
                                   id;
                                   created_at = handoff.finished_at;
                                   acknowledged_at =
                                     (match handoff.source with
                                     | Interactive -> Some handoff.finished_at
                                     | Headless_codex | Headless_pi -> None);
                                   reference;
                                   handoff_path = handoff_path home reference;
                                 }
                               in
                               (match State_store.lstat path with
                               | Error _ as error -> error
                               | Ok (Some { Unix.st_kind = Unix.S_LNK; _ }) ->
                                   Error
                                     (Printf.sprintf
                                        "unsafe run-handoff notice is a symlink: %s"
                                        path)
                               | Ok (Some { Unix.st_kind = Unix.S_REG; _ }) ->
                                   let* notice = load_notice_file ~home path in
                                   if notice.reference = reference then
                                     Ok repaired
                                   else
                                     Error
                                       "existing run-handoff notice references a different canonical handoff"
                               | Ok (Some _) ->
                                   Error
                                     (Printf.sprintf
                                        "run-handoff notice path is not a file: %s"
                                        path)
                               | Ok None ->
                                   let* () =
                                     State_store.write_json_atomic ~path
                                       (notice_to_json expected)
                                   in
                                   Ok (repaired + if rendered_created then 0 else 1)))
                             (Ok repaired))
                      (Ok repaired))
               (Ok 0)
      | Some _ -> Error (Printf.sprintf "run-handoff root is not a directory: %s" root))

let directory_json_files ~label root =
  let* state = State_store.lstat root in
  match state with
  | None -> Ok []
  | Some { Unix.st_kind = Unix.S_LNK; _ } ->
      Error (Printf.sprintf "unsafe %s is a symlink: %s" label root)
  | Some { Unix.st_kind = Unix.S_DIR; _ } -> (
      try
        Sys.readdir root |> Array.to_list |> List.sort String.compare
        |> List.filter (String.ends_with ~suffix:".json")
        |> List.map (Filename.concat root) |> fun files -> Ok files
      with Sys_error message -> Error message)
  | Some _ -> Error (Printf.sprintf "%s is not a directory: %s" label root)

let pending ~home =
  let* home = State_path.canonicalize home in
  let* hierarchy =
    directory_chain_state ~label:"run-handoff inbox hierarchy"
      (inbox_directory_chain home)
  in
  if not hierarchy then Ok []
  else
    let* paths = directory_json_files ~label:"run-handoff inbox" (inbox_root home) in
    paths
    |> List.fold_left
         (fun result path ->
           let* values = result in
           let* notice = load_notice_file ~home path in
           if notice.acknowledged_at <> None then Ok values
           else
             let* handoff = load_reference ~home notice.reference in
             Ok ((notice, handoff) :: values))
         (Ok [])
    |> Result.map List.rev

let acknowledge ~home id =
  let* id = State_path.safe_component ~label:"run-handoff notice id" id in
  let* home = State_path.canonicalize home in
  State_store.with_lock ~home (fun () ->
      let* hierarchy =
        directory_chain_state ~label:"run-handoff inbox hierarchy"
          (inbox_directory_chain home)
      in
      let* () =
        if hierarchy then Ok () else Error "run-handoff inbox is missing"
      in
      let path = notice_path home id in
      let* notice = load_notice_file ~home path in
      let* _handoff = load_reference ~home notice.reference in
      match notice.acknowledged_at with
      | Some _ -> Ok notice
      | None ->
          let notice =
            { notice with acknowledged_at = Some (Worker_memory.now_utc ()) }
          in
          let* () = State_store.write_json_atomic ~path (notice_to_json notice) in
          Ok notice)

let handoff_files_for_record ~home record =
  let* reference = reference_of_record record "placeholder" in
  let* home = State_path.canonicalize home in
  let* hierarchy =
    directory_chain_state ~label:"run-handoff hierarchy"
      (handoff_directory_chain home reference)
  in
  if hierarchy then
    directory_json_files ~label:"run-handoff worker directory"
      (handoff_dir home reference)
  else Ok []

let reference_from_path ~record path =
  let basename = Filename.basename path in
  let suffix = ".json" in
  if not (String.ends_with ~suffix basename) then Error "not a run-handoff JSON file"
  else
    let id = String.sub basename 0 (String.length basename - String.length suffix) in
    reference_of_record record id

let latest ~home record =
  let* paths = handoff_files_for_record ~home record in
  let* handoffs =
    paths
    |> List.fold_left
         (fun result path ->
           let* values = result in
           let* reference = reference_from_path ~record path in
           let* handoff = load_reference ~home reference in
           Ok (handoff :: values))
         (Ok [])
  in
  match
    List.sort
      (fun left right ->
        match String.compare right.finished_at left.finished_at with
        | 0 -> String.compare right.id left.id
        | value -> value)
      handoffs
  with
  | handoff :: _ -> Ok handoff
  | [] -> Error (Printf.sprintf "worker %s has no published run handoff" record.id)

let find_handoff ~home record = function
  | None -> latest ~home record
  | Some handoff_id ->
      let* handoff_id = State_path.safe_component ~label:"run handoff id" handoff_id in
      let* reference = reference_of_record record handoff_id in
      load_reference ~home reference

let availability path =
  match Unix.lstat path with
  | { Unix.st_kind = Unix.S_LNK; _ } ->
      Error (Printf.sprintf "unsafe follow-up evidence is a symlink: %s" path)
  | { Unix.st_kind = Unix.S_REG; _ } | { Unix.st_kind = Unix.S_DIR; _ } -> Ok true
  | _ -> Ok false
  | exception Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> Ok false
  | exception Unix.Unix_error (err, fn, arg) ->
      Error
        (Printf.sprintf "failed to inspect follow-up evidence via %s(%s): %s" fn arg
           (Unix.error_message err))

let artifact_availability ~worker_dir relative =
  let* relative = safe_relative_path relative in
  let components = String.split_on_char '/' relative in
  let rec inspect current = function
    | [] -> Ok true
    | component :: rest ->
        let path = Filename.concat current component in
        let* state = State_store.lstat path in
        (match (state, rest) with
        | Some { Unix.st_kind = Unix.S_LNK; _ }, _ ->
            Error
              (Printf.sprintf
                 "unsafe follow-up artifact hierarchy contains a symlink: %s"
                 path)
        | Some { Unix.st_kind = Unix.S_DIR; _ }, _ :: _ -> inspect path rest
        | Some { Unix.st_kind = Unix.S_REG; _ }, []
        | Some { Unix.st_kind = Unix.S_DIR; _ }, [] -> Ok true
        | None, _ -> Ok false
        | Some _, [] -> Ok false
        | Some _, _ :: _ ->
            Error
              (Printf.sprintf
                 "follow-up artifact hierarchy component is not a directory: %s"
                 path))
  in
  let* worker_state = State_store.lstat worker_dir in
  match worker_state with
  | Some { Unix.st_kind = Unix.S_DIR; _ } -> inspect worker_dir components
  | Some { Unix.st_kind = Unix.S_LNK; _ } ->
      Error
        (Printf.sprintf "unsafe durable worker memory is a symlink: %s" worker_dir)
  | Some _ -> Error (Printf.sprintf "durable worker memory is not a directory: %s" worker_dir)
  | None -> Ok false

let follow_up_json ~home ~(record : Job_store.record) ~(handoff : t) ~question =
  let question = compact_line question in
  if question = "" then Error "follow-up question must not be empty"
  else
    let* current_reference = reference_of_record record handoff.id in
    let state_home = Option.value ~default:home record.home in
    let* current = current_record_unlocked ~home:state_home current_reference in
    let dynamic_artifacts =
      handoff.evidence.artifacts
      |> List.map (fun artifact ->
             ( "artifact",
               Filename.concat current.worker_dir artifact.relative_to_worker,
               fun () ->
                 artifact_availability ~worker_dir:current.worker_dir
                   artifact.relative_to_worker ))
    in
    let reads =
      [ ("run_handoff", handoff.evidence.handoff, fun () -> availability handoff.evidence.handoff);
        ("rendered_handoff", handoff.evidence.rendered, fun () -> availability handoff.evidence.rendered);
        ( "worker_memory",
          Worker_memory.memory_file current.worker_dir,
          fun () -> availability (Worker_memory.memory_file current.worker_dir) );
        ("task_context", current.job.context, fun () -> availability current.job.context) ]
      @ dynamic_artifacts
    in
    let* reads =
      reads
      |> List.fold_left
           (fun result (kind, path, inspect) ->
             let* values = result in
             let* available = inspect () in
             Ok
               (`Assoc
                  [ ("kind", `String kind);
                    ("path", `String path);
                    ("available", `Bool available) ]
               :: values))
           (Ok [])
      |> Result.map List.rev
    in
    let workspaces = List.map workspace_of_state current.workspaces in
    let cwd =
      match current.workspaces with
      | first :: _ -> Option.value ~default:first.repo first.worktree
      | [] -> current.job.repo
    in
    Ok
      (`Assoc
        [ ("schema", `String follow_up_schema);
          ("read_only", `Bool true);
          ("question", `String question);
          ( "worker",
            `Assoc
              [ ("run_id", `String current_reference.worker_run_id);
                ("id", `String current.id);
                ("title", `String current.job.title);
                ("status", `String current.status) ] );
          ("handoff", `String handoff.evidence.handoff);
          ("cwd", `String cwd);
          ("workspaces", `List (List.map json_of_workspace workspaces));
          ("reads", `List reads);
          ( "instructions",
            `String
              "Answer the question by inspecting only the supplied durable handoff, task context, worker memory, artifacts, and current workspaces. This dispatch is read-only: do not modify code or Monty state. Use an explicit monty resume or headless resume only if the user asks to continue implementation." ) ])
