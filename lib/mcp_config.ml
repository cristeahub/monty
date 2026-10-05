let ( let* ) = Result.bind

let server_fields ~monty_command =
  [ "command", `String monty_command;
    "args", `List [ `String "mcp"; `String "serve" ] ]

let json ~monty_command =
  `Assoc [ "mcpServers", `Assoc [ "monty", `Assoc (server_fields ~monty_command) ] ]

let toml ~monty_command =
  "[mcp_servers.monty]\ncommand = " ^ Codex_trust.toml_basic_string monty_command
  ^ "\nargs = [\"mcp\", \"serve\"]\n"

let codex_args ~monty_command =
  " -c " ^ Shell.quote
    ("mcp_servers.monty={command=" ^ Codex_trust.toml_basic_string monty_command
     ^ ",args=[\"mcp\",\"serve\"],enabled=true}")

let pi_agent_dir () =
  match Sys.getenv_opt "PI_CODING_AGENT_DIR" with
  | Some dir when String.trim dir <> "" -> Shell.abs_path dir |> Shell.normalize
  | _ -> Filename.concat (Option.value ~default:"" (Sys.getenv_opt "HOME")) ".pi/agent"

let ensure_pi_at ~directory ~home ~monty_command =
  let path = Filename.concat directory "mcp.json" in
  let read () =
    let* stat = State_store.lstat path in
    match stat with
    | None -> Ok []
    | Some { Unix.st_kind = Unix.S_REG; st_size; _ } when st_size <= 1_048_576 ->
        let* value = State_store.read_json ~path in
        (match value with
         | Some (`Assoc fields) -> Ok fields
         | _ -> Error (path ^ ": expected a JSON object"))
    | _ -> Error (path ^ ": MCP config must be a regular file of at most 1 MiB (symlinks are not replaced)")
  in
  let merge fields =
    let* servers = match List.assoc_opt "mcpServers" fields with
      | None -> Ok []
      | Some (`Assoc servers) -> Ok servers
      | _ -> Error (path ^ ": mcpServers must be an object") in
    let* entry = match List.assoc_opt "monty" servers with
    | None -> Ok (server_fields ~monty_command)
    | Some (`Assoc entry) when List.assoc_opt "command" entry = Some (`String monty_command)
        && List.assoc_opt "args" entry = Some (`List [ `String "mcp"; `String "serve" ])
        && not (List.mem_assoc "url" entry)
        && (match List.assoc_opt "type" entry with None | Some (`String "stdio") -> true | _ -> false)
        && (match List.assoc_opt "enabled" entry with None | Some (`Bool true) -> true | _ -> false) -> Ok entry
    | Some _ -> Error (path ^ ": existing monty MCP entry conflicts or is disabled; preserve it under another name or explicitly update it using monty mcp config --format json") in
    let* () = match List.assoc_opt "toolExposure" entry with
      | None -> Ok ()
      | Some (`Assoc overrides) when List.for_all (fun (_, value) -> value = `String "direct") overrides -> Ok ()
      | _ -> Error (path ^ ": monty toolExposure must contain only direct overrides; remove it or set its values to direct to expose documentation in restricted headless sessions") in
    match List.assoc_opt "exposure" entry with
    | Some (`String "direct") -> Ok None
    | Some _ -> Error (path ^ ": monty exposure must be direct for restricted headless sessions; explicitly set exposure to direct or preserve this entry under another name")
    | None ->
        let entry = `Assoc (("exposure", `String "direct") :: entry) in
        let servers = ("monty", entry) :: List.remove_assoc "monty" servers in
        Ok (Some (`Assoc (("mcpServers", `Assoc servers)
                         :: List.remove_assoc "mcpServers" fields)))
  in
  let* fields = read () in
  let* change = merge fields in
  match change with
  | None -> Ok ()
  | Some _ ->
      State_store.with_lock ~home (fun () ->
          let* () = State_store.ensure_real_directory ~label:"Pi agent directory" ~mode:0o700 directory in
          (* Different Monty homes can share this Pi profile. Serialize their
             read/merge/write against the config, not just each home's state. *)
          State_store.with_file_lock ~path:(path ^ ".monty.lock") (fun () ->
              let* fields = read () in
              let* change = merge fields in
              match change with None -> Ok () | Some json -> State_store.write_json_atomic ~path json))

let ensure_pi ~home ~monty_command = ensure_pi_at ~directory:(pi_agent_dir ()) ~home ~monty_command

let pi_setup_command ~home ~monty_command =
  Shell.quote monty_command ^ " mcp setup-pi --home " ^ Shell.quote home
