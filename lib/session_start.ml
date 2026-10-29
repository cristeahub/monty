(* What the SessionStart hook tells an agent, the way Rex's hook tells it which
   terminal it is in: the facts it would otherwise spend a turn discovering. It
   reads durable state only, writes nothing, never starts the service, and prints
   nothing outside a Monty session so other sessions see no noise. *)

let skill_reference = function
  | Harness.Claude -> "/monty"
  | Harness.Codex -> "$monty"
  | Harness.Pi -> "monty"

let worker_variables =
  [ "MONTY_JOB_ID"; "MONTY_JOB_TITLE"; "MONTY_TASK_KEY"; "MONTY_JOB_FILE"; "MONTY_JOB_CONTEXT";
    "MONTY_WORKER_DIR"; "MONTY_RUN_DIR"; "MONTY_JOB_WORKTREE"; "MONTY_JOB_REPO"; "MONTY_JOB_BRANCH";
    "MONTY_WORKTREE_MODE"; "MONTY_HOME"; "MONTY_SWARM_PARENT"; "MONTY_SWARM_NAME" ]

let bindings getenv names =
  List.filter_map (fun name -> Option.map (fun value -> name ^ "=" ^ value) (getenv name)) names

let workspace_bindings getenv =
  let rec collect index acc =
    let name = Printf.sprintf "MONTY_WORKSPACE_%d" index in
    match getenv name with
    | Some value -> collect (index + 1) ((name ^ "=" ^ value) :: acc)
    | None -> List.rev acc
  in
  collect 1 []

let block lines = "```text\n" ^ String.concat "\n" lines ^ "\n```"

(* A swarm driver's children report to its inbox, not the head butler's. *)
let swarm_driver ~getenv =
  match (getenv "MONTY_HOME", getenv "MONTY_WORKER_DIR") with
  | Some home, Some worker_dir when String.trim home <> "" && String.trim worker_dir <> "" -> (
      match Agent_profile.load_snapshot worker_dir with
      | Ok { Agent_profile.swarm = Some _; _ } -> (
          match Run_handoff.pending ~inbox:(Option.value ~default:"" (getenv "MONTY_JOB_ID")) ~home () with
          | Ok notices ->
              [ Printf.sprintf
                  "You are the swarm driver: %d child handoff%s %s waiting in your inbox. `monty swarm status` lists your children, `monty handoff pending` shows their handoffs, and `monty guide swarm` has the driver loop and command recipes."
                  (List.length notices) (if List.length notices = 1 then "" else "s")
                  (if List.length notices = 1 then "is" else "are") ]
          | Error message ->
              [ "You are the swarm driver; its child inbox could not be read (" ^ message
                ^ "). Run `monty swarm status` and `monty guide swarm`." ])
      | _ -> [])
  | _ -> []

let swarm_child ~getenv =
  match (getenv "MONTY_SWARM_PARENT", getenv "MONTY_SWARM_NAME") with
  | Some parent, Some name when String.trim parent <> "" && String.trim name <> "" ->
      [ Printf.sprintf
          "You are swarm child `%s` of driver `%s`: commit your work on the current branch in small commits and leave the tree clean; your handoff goes to the driver."
          name parent ]
  | _ -> []

let worker ~getenv ~harness id =
  let title = match getenv "MONTY_JOB_TITLE" with Some title when String.trim title <> "" -> ": " ^ title | _ -> "" in
  String.concat "\n\n"
    ([ Printf.sprintf "You're running as Monty worker %s%s." id title;
      block (bindings getenv worker_variables @ workspace_bindings getenv) ]
    @ swarm_driver ~getenv @ swarm_child ~getenv
    @ [
      "`MONTY_JOB_CONTEXT` is your task context file: read it first. It holds the task summary, the \
       ordered workspace map, links, constraints and acceptance criteria. `MONTY_JOB_FILE` is the \
       authoritative workspace map and `MONTY_WORKER_DIR` is your durable memory; worktrees are \
       ephemeral, so record discoveries, blockers and handoff notes there, never only in a worktree.";
      "When the run is ready for review, needs attention or failed, publish a handoff so the head \
       butler is told:";
      "```sh\nmonty handoff publish \"$MONTY_JOB_ID\" --summary 'What this run did.' --check 'dune runtest: passed'\n```";
      "Publishing leaves the task open. Do not commit, push, open pull requests, post reviews, send \
       remote messages, manage worktrees or run `monty done` unless the user explicitly authorizes it.";
      Printf.sprintf
        "The monty skill (%s) explains the worker workflow; `monty guide workers` and `monty guide \
         handoffs` print the same guidance, and `monty COMMAND --help=plain` gives exact syntax."
        (skill_reference harness) ])

let pending_handoffs ~home =
  match Run_handoff.pending ~home () with
  | Error message -> Printf.sprintf "Pending worker handoffs could not be read (%s); run `monty handoff pending` before presenting the task inventory." message
  | Ok [] -> "No worker handoffs are pending right now. Run `monty handoff pending` again before presenting the task inventory, because workers publish while you plan."
  | Ok notices ->
      let count = List.length notices in
      String.concat "\n"
        ((Printf.sprintf "%d worker handoff%s %s pending:" count (if count = 1 then "" else "s") (if count = 1 then "is" else "are"))
        :: List.map
             (fun ((notice : Run_handoff.notice), (handoff : Run_handoff.t)) ->
               Printf.sprintf "- notice %s: worker %s, %s (%s)" notice.id handoff.worker_id handoff.title
                 (Run_handoff.outcome_to_string handoff.outcome))
             notices
        @ [ "Run `monty handoff pending` before presenting the task inventory, show each notice at the next safe message boundary, then run `monty handoff acknowledge <notice-id>`. Reading a handoff never starts work or changes task status." ])

let head_butler ~getenv ~harness home =
  let profile =
    match Mantle.profile getenv with
    | Some name -> Printf.sprintf " The active Mantle profile is %s; task lists default to its project group." name
    | None -> ""
  in
  String.concat "\n\n"
    [ "You're running in the Monty control room.";
      block [ "MONTY_HOME=" ^ home ];
      Printf.sprintf
        "This is Monty's head-butler session: plan work, choose actionable tasks and launch workers \
         through the `monty` CLI, following AGENTS.md in this directory and the user's instructions. \
         The harness is %s.%s"
        (Harness.to_string harness) profile;
      pending_handoffs ~home;
      "When the user says \"swarm\", wants many agents on one goal, or brings a large goal that \
       splits into independent areas with disjoint files, propose the `swarm` agent profile (see \
       `monty guide swarm`) with the areas and the landing check before launching; small or \
       sequential tasks stay on the default profile.";
      Printf.sprintf
        "The monty skill (%s) explains the workflows. `monty guide` lists the topics, `monty guide \
         TOPIC` prints one, and `monty COMMAND --help=plain` gives exact syntax. Help does not \
         authorize actions."
        (skill_reference harness) ]

let canonical path = match State_path.canonicalize path with Ok path -> path | Error _ -> path

(* The context for a session: a worker's when the worker environment is set,
   the head butler's when the session starts in the Monty home, else none. *)
let render ~getenv ~cwd ~harness =
  match getenv "MONTY_JOB_ID" with
  | Some id when String.trim id <> "" -> Some (worker ~getenv ~harness id)
  | _ -> (
      match getenv "MONTY_HOME" with
      | Some home when String.trim home <> "" && canonical cwd = canonical home ->
          Some (head_butler ~getenv ~harness (canonical home))
      | _ -> None)

(* Reads the hook's stdin for the session's directory; anything unparseable
   falls back to the process directory. *)
let hook_cwd () =
  if Unix.isatty Unix.stdin then Sys.getcwd ()
  else
    let buffer = Buffer.create 4096 in
    (try
       let chunk = Bytes.create 4096 in
       let rec read () =
         let count = input stdin chunk 0 (Bytes.length chunk) in
         if count > 0 && Buffer.length buffer < 1_048_576 then (
           Buffer.add_subbytes buffer chunk 0 count;
           read ())
       in
       read ()
     with Sys_error _ | End_of_file -> ());
    match Yojson.Safe.from_string (Buffer.contents buffer) with
    | `Assoc fields -> ( match List.assoc_opt "cwd" fields with Some (`String cwd) when cwd <> "" -> cwd | _ -> Sys.getcwd ())
    | _ -> Sys.getcwd ()
    | exception _ -> Sys.getcwd ()

(* `monty __internal session-start --harness HARNESS`: the command the installed
   hook runs. It exits zero whatever happens, because a hook must never break a
   session. *)
let internal ~getenv = function
  | "session-start" :: rest ->
      let harness =
        match rest with
        | [ "--harness"; value ] -> Harness.of_string value
        | [ value ] when String.starts_with ~prefix:"--harness=" value ->
            Harness.of_string (String.sub value 10 (String.length value - 10))
        | _ -> Error (`Msg "usage: monty __internal session-start --harness HARNESS")
      in
      (match harness with
       | Error (`Msg message) -> prerr_endline ("monty: " ^ message)
       | Ok harness -> (
           match render ~getenv ~cwd:(hook_cwd ()) ~harness with
           | Some text -> print_string (text ^ "\n")
           | None -> ()
           | exception error -> prerr_endline ("monty: session-start: " ^ Printexc.to_string error)));
      0
  | _ ->
      prerr_endline "monty: __internal commands are run by the files that monty skill setup installs: session-start";
      2
