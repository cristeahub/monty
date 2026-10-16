open Conversation_store
let exit f = try f ();0 with
  | Failure(_,m) | Sys_error m | Invalid_argument m -> Fmt.epr "monty: %s\n" m;1
  | Unix.Unix_error(e,f,_) -> Fmt.epr "monty: %s: %s\n" f (Unix.error_message e);1
let command ~home_arg =
  let open Cmdliner in
  let config=Arg.(required & opt (some string) None & info ["config"] ~docv:"FILE" ~doc:"Trusted JSON profiles and workspace roots.") in
  let port=Arg.(value & opt int 0 & info ["port"] ~docv:"PORT" ~doc:"Loopback port; zero selects an available port.") in
  let serve=Cmd.v (Cmd.info "serve" ~doc:"Run the authenticated local client API in the foreground.")
    Term.(const (fun home config_path port -> exit (fun () -> Client_service.serve ~home ~config_path ~port)) $ home_arg $ config $ port) in
  let config=Arg.(value & opt (some string) None & info ["config"] ~docv:"FILE" ~doc:"Trusted profile configuration; defaults to the saved configuration.") in
  let port=Arg.(value & opt (some int) None & info ["port"] ~docv:"PORT" ~doc:"Loopback port; defaults to the saved port, or zero for an available port.") in
  let start=Cmd.v (Cmd.info "start" ~doc:"Start the client service in the background, or reuse the running service.")
    Term.(const (fun home config_path port -> exit (fun () ->
      ignore(Lwt_main.run (Client_terminal.ensure_service ?config_path ?port ~home:(Unix.realpath home) ()));
      Printf.printf "Monty service is running.\n")) $ home_arg $ config $ port) in
  let stop=Cmd.v (Cmd.info "stop" ~doc:"Stop the client service and its owned runtimes, preserving configuration and history.")
    Term.(const (fun home -> exit (fun () ->
      let stopped=Lwt_main.run (Client_terminal.stop_service ~home:(Unix.realpath home)) in
      Printf.printf "Monty service %s.\n" (if stopped then "stopped" else "is already stopped"))) $ home_arg) in
  Cmd.group (Cmd.info "service" ~doc:"Monty's local conversations and inventory API.") [start;stop;serve]

let terminal ~home_arg =
  let open Cmdliner in
  let opt name doc=Arg.(value & opt (some string) None & info [name] ~doc) in
  let profile=Arg.(value & opt (some string) (Sys.getenv_opt "MONTY_API_PROFILE") & info ["profile"] ~doc:"Configured service account profile.") in
  let harness=Arg.(value & opt string "codex" & info ["harness"] ~doc:"pi, codex or claude.") in
  let yolo=Arg.(value & flag & info ["codex-yolo"] ~doc:"Run Codex without approvals or sandboxing. This is dangerous.") in
  let worker=Arg.(value & flag & info ["worker"] ~doc:"Use the explicit worker environment, workspaces and native sidecar.") in
  let mode=Arg.(value & opt string "fresh" & info ["mode"] ~doc:"fresh, picker, last or exact with --session.") in
  let title=Arg.(value & opt string "Monty conversation" & info ["title"]) in
  Cmd.v (Cmd.info "terminal" ~doc:"Display a native terminal owned by Monty's server; client exit detaches.")
    Term.(const(fun home profile harness mode session_id prompt instructions context request_id yolo worker title codex pi claude fork -> exit(fun () ->
      let harness=get(Harness.of_string harness |> Result.map_error(fun (`Msg m)->m)) in
      let codex_yolo=get(Settings.effective_codex_yolo ~getenv:Sys.getenv_opt ~home yolo) in
      let title=if worker then Option.value ~default:title (Sys.getenv_opt "MONTY_JOB_TITLE") else title in
      let command_override=match harness with Harness.Pi->pi|Codex->codex|Claude->claude in
      let code=Client_terminal.launch ~home ~profile ~harness ~command_override ~fork ~codex_yolo ~mode ~session_id ~prompt ~instructions ~context ~request_id ~worker ~title in
      if code<>0 then Stdlib.exit code))
      $ home_arg $ profile $ harness $ mode $ opt "session" "Exact registered native session."
      $ opt "prompt" "Initial prompt." $ opt "instructions" "Worker instructions file." $ opt "context" "Task context file."
      $ opt "request-id" "Stable command receipt ID." $ yolo $ worker $ title
      $ opt "codex-command" "Use the matching trusted server command." $ opt "pi-command" "Use the matching trusted server command."
      $ opt "claude-command" "Use the matching trusted server command." $ opt "fork" "Pi fork selector.")
