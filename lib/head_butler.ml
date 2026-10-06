type continuation = Picker | Last | Session of string

let command ~monty_command ~home ~harness ~harness_command ~codex_yolo ~name =
  match harness with
  | Harness.Pi ->
      Printf.sprintf "cd %s && exec %s --name %s" (Shell.quote home)
        harness_command (Shell.quote name)
  | Harness.Codex ->
      Printf.sprintf "cd %s && exec %s terminal --home %s --codex-command %s"
        (Shell.quote home) (Shell.quote monty_command) (Shell.quote home) (Shell.quote harness_command)
      ^ (if codex_yolo then " --codex-yolo" else "")

let continuation_command ~monty_command ~home ~harness ~harness_command ~codex_yolo
    continuation =
  match harness with
  | Harness.Pi ->
      let selector =
        match continuation with
        | Picker -> " --resume"
        | Last -> " --continue"
        | Session session -> " --session " ^ Shell.quote session
      in
      Printf.sprintf "cd %s && exec %s%s" (Shell.quote home) harness_command
        selector
  | Harness.Codex ->
      let selector = match continuation with
        | Picker -> " --mode picker" | Last -> " --mode last"
        | Session id -> " --mode exact --session " ^ Shell.quote id in
      Printf.sprintf "cd %s && exec %s terminal --home %s --codex-command %s%s"
        (Shell.quote home) (Shell.quote monty_command) (Shell.quote home) (Shell.quote harness_command) selector
      ^ (if codex_yolo then " --codex-yolo" else "")

let run ~monty_command ~home ~harness ~harness_command command =
  let ( let* ) = Result.bind in
  let home = Shell.normalize (Shell.abs_path home) in
  if not (Sys.file_exists home && Sys.is_directory home) then
    Error (Printf.sprintf "Monty home is not an existing directory: %s" home)
  else
    let* _ = Process.command_exists_with_arguments harness_command in
    let* () = match harness with
      | Harness.Pi -> Skill.ensure ~home ~harness ~monty_command
      | Harness.Codex -> Ok () in
    match Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; command home |] with
    | () -> assert false
    | exception Unix.Unix_error (err, fn, arg) ->
        Error
          (Printf.sprintf "failed to exec %s via %s(%s): %s"
             (Harness.to_string harness) fn arg
             (Unix.error_message err))

let start ~monty_command ~home ~harness ~harness_command ~codex_yolo ~name =
  run ~monty_command ~home ~harness ~harness_command (fun home ->
      command ~monty_command ~home ~harness ~harness_command ~codex_yolo ~name)

let continue ~monty_command ~home ~harness ~harness_command ~codex_yolo continuation =
  run ~monty_command ~home ~harness ~harness_command (fun home ->
      continuation_command ~monty_command ~home ~harness ~harness_command ~codex_yolo
        continuation)
