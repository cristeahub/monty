let ( let* ) = Result.bind

let files =
  [ "SKILL.md", "---\n# Managed by Monty.", (String.trim Embedded_skill.text ^ "\n");
    "references/workflows.md", "<!-- Managed by Monty. -->",
      "<!-- Managed by Monty. -->\n" ^ Embedded_manual.text ]

let agent_directory harness =
  let variable, fallback = match harness with
    | Harness.Codex -> "CODEX_HOME", ".codex"
    | Harness.Claude -> "CLAUDE_CONFIG_DIR", ".claude"
    | Harness.Pi -> "PI_CODING_AGENT_DIR", ".pi/agent" in
  match Sys.getenv_opt variable with
  | Some path when String.trim path <> "" -> Ok (Shell.abs_path path)
  | _ -> (match Sys.getenv_opt "HOME" with
      | Some home when String.trim home <> "" -> Ok (Filename.concat home fallback)
      | _ -> Error ("set " ^ variable ^ " or HOME to install the Monty skill"))

(* Only retire the exact entry installed by previous Monty versions. Other MCP
   servers, including an unrelated server named monty, belong to the user. *)
let remove_pi_mcp ~directory ~monty_command =
  let path = Filename.concat directory "mcp.json" in
  State_store.with_file_lock ~path:(path ^ ".monty.lock") (fun () ->
    let* stat = State_store.lstat path in
    match stat with
    | None -> Ok ()
    | Some { Unix.st_kind = Unix.S_REG; st_size; _ } when st_size <= 1_048_576 ->
        let* json = State_store.read_json ~path in
        (match json with
        | Some (`Assoc fields) ->
            (match List.assoc_opt "mcpServers" fields with
             | Some (`Assoc servers) ->
                 (match List.assoc_opt "monty" servers with
                  | Some (`Assoc entry) when
                      List.assoc_opt "command" entry = Some (`String monty_command)
                      && List.assoc_opt "args" entry = Some (`List [`String "mcp"; `String "serve"])
                      && not (List.mem_assoc "url" entry) ->
                      State_store.write_json_atomic ~path
                        (`Assoc (("mcpServers", `Assoc (List.remove_assoc "monty" servers))
                          :: List.remove_assoc "mcpServers" fields))
                  | _ -> Ok ())
             | _ -> Ok ())
        | _ -> Ok ())
    | Some _ -> Error ("refusing unsafe legacy Pi MCP config: " ^ path))

let ensure_at ~home ~harness ~directory ~monty_command =
  State_store.protect_result ~action:"install Monty skill" ~path:directory (fun () ->
    let* directory = State_path.canonicalize directory in
    State_store.with_lock ~home (fun () ->
      let root = Filename.concat directory "skills" in
      let* () = State_store.ensure_real_directory ~label:"harness skills" ~mode:0o700 root in
      (* Profiles can be shared by multiple Monty homes. *)
      State_store.with_file_lock ~path:(Filename.concat root ".monty.lock") (fun () ->
        let target = Filename.concat root "monty" in
        let* () = State_store.ensure_real_directory ~label:"Monty skill" ~mode:0o700 target in
        let* () = State_store.ensure_real_directory ~label:"Monty skill references" ~mode:0o700
            (Filename.concat target "references") in
        let* changes = List.fold_left (fun result (name, marker, contents) ->
          let* changes = result in
          let path = Filename.concat target name in
          let* stat = State_store.lstat path in
          match stat with
          | None -> Ok ((path, contents) :: changes)
          | Some { Unix.st_kind = Unix.S_REG; st_size; _ } when st_size <= 1_048_576 ->
              let current = Shell.read_file path in
              if current = contents then Ok changes
              else if String.starts_with ~prefix:marker current then Ok ((path, contents) :: changes)
              else Error ("Monty will not overwrite an unmanaged skill file: " ^ path)
          | Some _ -> Error ("refusing unsafe Monty skill file: " ^ path)) (Ok []) files in
        let* () = List.fold_left (fun result (path, contents) ->
          let* () = result in State_store.write_file_atomic ~path ~perm:0o600 contents) (Ok ()) changes in
        match harness with
        | Harness.Codex | Harness.Claude -> Ok ()
        | Harness.Pi -> remove_pi_mcp ~directory ~monty_command)))

let ensure ~home ~harness ~monty_command =
  let* directory = agent_directory harness in
  ensure_at ~home ~harness ~directory ~monty_command

let setup_command ~home ~harness ~monty_command =
  Shell.quote monty_command ^ " skills install --harness " ^ Harness.to_string harness
  ^ " --home " ^ Shell.quote home

let command ~home_arg ~harness_arg ~monty_command ~getenv =
  let open Cmdliner in
  let install = Cmd.v (Cmd.info "install" ~doc:"Install or refresh Monty's CLI skill in the selected harness profile.")
    Term.(const (fun home override -> match (let* harness = Settings.effective_harness ~getenv ~home override in
      ensure ~home ~harness ~monty_command) with
      | Ok () -> 0 | Error message -> prerr_endline ("monty: " ^ message); 1) $ home_arg $ harness_arg) in
  Cmd.group (Cmd.info "skills" ~doc:"Manage the CLI skill installed automatically for Monty sessions.") [install]
