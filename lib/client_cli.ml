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
  let stop=Cmd.v (Cmd.info "stop" ~doc:"Stop the client service, preserving its configuration and shared Codex daemon.")
    Term.(const (fun home -> exit (fun () ->
      let stopped=Lwt_main.run (Client_terminal.stop_service ~home:(Unix.realpath home)) in
      Printf.printf "Monty service %s.\n" (if stopped then "stopped" else "is already stopped"))) $ home_arg) in
  Cmd.group (Cmd.info "service" ~doc:"Monty's local conversations and inventory API.") [start;stop;serve]

let terminal ~home_arg =
  let open Cmdliner in
  let opt name doc = Arg.(value & opt (some string) None & info [name] ~doc) in
  let profile=Arg.(value & opt string (Option.value ~default:"default" (Sys.getenv_opt "MONTY_API_PROFILE")) & info ["profile"]) in
  let codex=Arg.(value & opt string "codex" & info ["codex-command"]) in
  let yolo=Arg.(value & flag & info ["codex-yolo"] ~doc:"Run Codex without approvals or sandboxing. This is dangerous.") in
  let worker=Arg.(value & flag & info ["worker"] ~doc:"Use the explicit Monty worker environment, task link, workspaces and session sidecar.") in
  let mode=Arg.(value & opt string "fresh" & info ["mode"] ~doc:"fresh, picker, last or exact (with --session).") in
  Cmd.v (Cmd.info "terminal" ~doc:"Open native Codex on an exact Monty service thread; never falls back to standalone.")
    Term.(const (fun home profile codex_command mode session_id prompt instructions context request_id yolo worker -> exit (fun () ->
      let codex_yolo=get(Settings.effective_codex_yolo ~getenv:Sys.getenv_opt ~home yolo) in
      Client_terminal.launch ~home ~profile ~codex_command ~codex_yolo ~mode ~session_id ~prompt ~instructions ~context ~request_id ~worker))
      $ home_arg $ profile $ codex $ mode $ opt "session" "Exact registered native thread."
      $ opt "prompt" "Initial user prompt." $ opt "instructions" "Worker instructions file."
      $ opt "context" "Task context file." $ opt "request-id" "Stable command ID for retry/receipt lookup." $ yolo $ worker)
