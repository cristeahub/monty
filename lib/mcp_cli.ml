let command ~home_arg ~monty_command ~command_help =
  let open Cmdliner in
  let serve = Cmd.v (Cmd.info "serve" ~doc:"Serve portable read-only CLI documentation over MCP stdio.")
      Term.(const (fun () -> Mcp.serve ~command_help ()) $ const ()) in
  let guide =
    let topic = Arg.(value & pos 0 string "index" & info [] ~docv:"TOPIC") in
    Cmd.v (Cmd.info "guide" ~doc:"Read the same embedded workflow guide used by MCP clients.")
      Term.(const (fun topic -> match Mcp.guide topic with
        | Ok text -> print_endline text; 0
        | Error message -> prerr_endline message; 1) $ topic) in
  let config =
    let format = Arg.(value & opt (enum ["json", `Json; "toml", `Toml]) `Json
      & info ["format"] ~docv:"FORMAT" ~doc:"Configuration format: json (Claude Code, Pi and other clients) or toml (Codex).") in
    Cmd.v (Cmd.info "config" ~doc:"Print MCP configuration for this executable; do not install or modify anything.")
      Term.(const (fun format ->
        print_string (match format with
          | `Json -> Yojson.Safe.pretty_to_string (Mcp_config.json ~monty_command) ^ "\n"
          | `Toml -> Mcp_config.toml ~monty_command); 0) $ format) in
  let setup_pi = Cmd.v (Cmd.info "setup-pi" ~doc:"Ensure Monty's native Pi MCP entry, preserving other servers and refusing conflicts.")
      Term.(const (fun home -> match Mcp_config.ensure_pi ~home ~monty_command with
        | Ok () -> 0
        | Error message -> prerr_endline ("monty: " ^ message); 1) $ home_arg) in
  Cmd.group (Cmd.info "mcp" ~doc:"Portable MCP documentation server and client configuration.")
    [serve; guide; config; setup_pi]
