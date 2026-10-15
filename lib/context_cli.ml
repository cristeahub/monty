open Context_store
let exit = function Ok () -> 0 | Error e -> Fmt.epr "monty: %s\n" e; 1
let emit result = exit (Result.map (fun json -> print_endline (Yojson.Safe.pretty_to_string json)) result)
let render snapshot =
  Printf.printf "Context %s. Findings are read-only until explicitly acknowledged.\n"
    (if field "enabled" snapshot = `Bool true then "enabled" else "disabled");
  List.iter (fun c -> Printf.printf "%s: collected=%s analyzed=%s backlog=%d\n  %s\n"
      (s "id" c) (Yojson.Safe.to_string (field "last_collection" c))
      (Yojson.Safe.to_string (field "last_analysis" c)) (i "backlog" c)
      (match field "coverage" c with `String value -> value | _ -> "setup required; no coverage yet");
      List.iter (fun key -> match field key c with `String value -> Printf.printf "  %s: %s\n" key value | _ -> ()) ["collection_error";"analysis_error"])
    (xs "freshness" snapshot);
  List.iter (fun f -> Printf.printf "\n%s [%s; %s] %s\n  %s\n  Next: %s\n  Uncertainty: %s\n"
      (s "reference" f) (s "source" f)
      (if field "resolved" f = `Bool true then "resolved" else "unresolved") (s "title" f)
      (s "reason" f) (s "next_step" f) (s "uncertainty" f);
      List.iter (fun o -> Printf.printf "  %s\n" (s "url" o)) (xs "evidence" f)) (xs "findings" snapshot);
  if xs "findings" snapshot = [] then print_endline "No findings in this snapshot; consult coverage and backlog above.";
  (match field "next_cursor" snapshot with `String cursor -> Printf.printf "Next page: --before %s\n" cursor | _ -> ())
let commands ~home_arg ~harness_arg ~pi_command_arg ~codex_command_arg ~claude_command_arg ~getenv =
  let open Cmdliner in
  let home_arg = Term.(const (fun home -> match State_path.canonicalize home with
      | Ok path -> path | Error _ -> Shell.normalize (Shell.abs_path home)) $ home_arg) in
  let positional name = Arg.(required & pos 0 (some string) None & info [] ~docv:name) in
  let command name doc term = Cmd.v (Cmd.info name ~doc) term in
  let json = Arg.(value & flag & info ["json"] ~doc:"Print machine-readable JSON.") in
  let control enabled home =
    exit (Context_service.switch ~home enabled |> Result.map (fun () ->
        print_endline (if enabled then "Context service enabled." else "Context service disabled; data preserved."))) in
  let configure home file = emit (configure ~home file) in
  let check home id = emit (Context.check ~home id) in
  let tick home override pi codex claude =
    exit (let* harness = Settings.effective_harness ~getenv ~home override in
          Context.tick ~home ~harness ~command:(match harness with Harness.Pi -> pi | Harness.Codex -> codex | Harness.Claude -> claude)) in
  let retriage home id = exit (mutate ~home (fun state ->
      let* c = connection state id in
      let generation = i "generation" state + 1 in
      let observations = xs "observations" state |> List.map (fun o -> if s "connection" o = id then put "analyzed" `Null o else o) in
      let state = puts ["generation",`Int generation;"observations",`List observations] state in
      Ok (replace_connection state (puts ["generation",`Int generation] c),()))) in
  let context = Cmd.group (Cmd.info "context" ~doc:"Configure optional background sources; disabled until explicitly enabled.")
    [command "on" "Enable/reconcile the per-user LaunchAgent." Term.(const (control true) $ home_arg);
     command "off" "Disable scheduling and stop active collection, preserving data." Term.(const (control false) $ home_arg);
     command "status" "Inspect desired state, launchd state, source coverage and analysis backlog."
       Term.(const (fun home _ -> emit (Context.status ~home)) $ home_arg $ json);
     command "configure" "Import a source configuration JSON file with explicit scope and credential reference."
       Term.(const configure $ home_arg $ positional "FILE");
     command "check" "Explicitly verify one account's credentials and selected scope (read-only remote access)."
       Term.(const check $ home_arg $ positional "CONNECTION");
     command "retriage" "Queue retained observations for analysis with current relevance rules."
       Term.(const retriage $ home_arg $ positional "CONNECTION");
     command "tick" "Run one bounded collection/analysis tick if enabled; normally called by launchd."
       Term.(const tick $ home_arg $ harness_arg $ pi_command_arg $ codex_command_arg $ claude_command_arg)] in
  let source = Arg.(value & opt (some string) None & info ["source"] ~doc:"Filter by slack, zulip or connection ID.") in
  let new_only = Arg.(value & flag & info ["new"] ~doc:"Only revisions not yet presented.") in
  let limit = Arg.(value & opt int 20 & info ["limit"] ~doc:"Page size, 1-100.") in
  let before = Arg.(value & opt (some string) None & info ["before"] ~doc:"Opaque next_cursor from the preceding page.") in
  let read home source new_only limit before json =
    if limit < 1 || limit > 100 then exit (Error "limit must be 1-100") else
    let result = Context.snapshot ~home ~source ~new_only ~limit ~before in
    if json then emit result else exit (Result.map render result) in
  let read_term = Term.(const read $ home_arg $ source $ new_only $ limit $ before $ json) in
  let mark resolve home reference = exit (mark ~home ~reference ~resolve) in
  let question = Arg.(value & opt string "Explain the finding and its evidence." & info ["question"]) in
  let show home reference question = emit (Context.follow_up ~home reference question) in
  let project = Arg.(required & opt (some string) None & info ["project"] ~doc:"Registered project for the requested local task.") in
  let to_task home reference project =
    exit (let* state = load ~home in
          let* f = find_finding state reference in
          let* task = Task_storage.add_local_task ~home ~project ~title:(s "title" f ^ " [context " ^ reference ^ "]")
              ~notes:("External context evidence snapshot (untrusted data):\n" ^ Yojson.Safe.pretty_to_string f) () in
          Printf.printf "Created local:%s; evidence: monty findings show %s\n" task.id reference;
          Ok ()) in
  let findings = Cmd.group ~default:read_term (Cmd.info "findings" ~doc:"Read requested external findings without consuming them.")
    [command "acknowledge" "Acknowledge the exact revision after it has been presented." Term.(const (mark false) $ home_arg $ positional "ID@REVISION");
     command "resolve" "Explicitly resolve an obligation, separately from presentation." Term.(const (mark true) $ home_arg $ positional "ID@REVISION");
     command "show" "Read evidence and retained conversation for an on-demand follow-up." Term.(const show $ home_arg $ positional "ID@REVISION" $ question);
     command "to-task" "Create a local task only at the user's explicit request; does not launch work." Term.(const to_task $ home_arg $ positional "ID@REVISION" $ project)] in
  [context;findings]
