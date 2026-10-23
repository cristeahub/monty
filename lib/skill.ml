let ( let* ) = Result.bind

let files =
  [ "SKILL.md", "---\n# Managed by Monty.", (String.trim Embedded_skill.text ^ "\n");
    "references/workflows.md", "<!-- Managed by Monty. -->",
      "<!-- Managed by Monty. -->\n" ^ Embedded_manual.text ]

let agent_directory harness =
  let variable = Harness.account_variable harness in
  match Sys.getenv_opt variable with
  | Some path when String.trim path <> "" -> Ok (Shell.abs_path path)
  | _ -> (match Harness.default_account_directory ~getenv:Sys.getenv_opt harness with
      | Some directory -> Ok directory
      | None -> Error ("set " ^ variable ^ " or HOME to install the Monty skill"))

let ensure_at ~home ~directory =
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
        List.fold_left (fun result (path, contents) ->
          let* () = result in State_store.write_file_atomic ~path ~perm:0o600 contents) (Ok ()) changes)))

let ensure ~home ~harness =
  let* directory = agent_directory harness in
  ensure_at ~home ~directory

let command ~home_arg ~harness_arg ~getenv =
  let open Cmdliner in
  let install = Cmd.v (Cmd.info "install" ~doc:"Install or refresh Monty's CLI skill in the selected harness profile.")
    Term.(const (fun home override -> match (let* harness = Settings.effective_harness ~getenv ~home override in
      ensure ~home ~harness) with
      | Ok () -> 0 | Error message -> prerr_endline ("monty: " ^ message); 1) $ home_arg $ harness_arg) in
  Cmd.group (Cmd.info "skills" ~doc:"Manage the CLI skill installed automatically for Monty sessions.") [install]
