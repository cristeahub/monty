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
  Cmd.group (Cmd.info "service" ~doc:"Monty's local conversations and inventory API.") [serve]

let terminal ~home_arg =
  let open Cmdliner in
  let opt name doc = Arg.(value & opt (some string) None & info [name] ~doc) in
  let profile=Arg.(value & opt string (Option.value ~default:"default" (Sys.getenv_opt "MONTY_API_PROFILE")) & info ["profile"]) in
  let codex=Arg.(value & opt string "codex" & info ["codex-command"]) in
  let yolo=Arg.(value & flag & info ["codex-yolo"] ~doc:"Rejected for managed terminals; explicit unsafe policy is not implemented.") in
  let worker=Arg.(value & flag & info ["worker"] ~doc:"Use the explicit Monty worker environment, task link, workspaces and session sidecar.") in
  let mode=Arg.(value & opt string "fresh" & info ["mode"] ~doc:"fresh, picker, last or exact (with --session).") in
  Cmd.v (Cmd.info "terminal" ~doc:"Open native Codex on an exact Monty service thread; never falls back to standalone.")
    Term.(const (fun home profile codex_command mode session_id prompt instructions context request_id yolo worker -> exit (fun () ->
      if yolo then fail 400 "Managed terminal does not support approval/sandbox bypass";
      Client_terminal.launch ~home ~profile ~codex_command ~mode ~session_id ~prompt ~instructions ~context ~request_id ~worker))
      $ home_arg $ profile $ codex $ mode $ opt "session" "Exact registered native thread."
      $ opt "prompt" "Initial user prompt." $ opt "instructions" "Worker instructions file."
      $ opt "context" "Task context file." $ opt "request-id" "Stable command ID for retry/receipt lookup." $ yolo $ worker)
