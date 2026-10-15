type continuation = Picker | Last | Session of string

let terminal ~monty_command ~home ~harness ~harness_command ~codex_yolo =
  Printf.sprintf "cd %s && exec %s terminal --home %s --harness %s --%s-command %s"
    (Shell.quote home) (Shell.quote monty_command) (Shell.quote home)
    (Harness.to_string harness) (Harness.to_string harness) (Shell.quote harness_command)
  ^ (if codex_yolo && harness=Harness.Codex then " --codex-yolo" else "")
let command ~monty_command ~home ~harness ~harness_command ~codex_yolo ~name =
  terminal ~monty_command ~home ~harness ~harness_command ~codex_yolo ^ " --title " ^ Shell.quote name
let continuation_command ~monty_command ~home ~harness ~harness_command ~codex_yolo continuation =
  terminal ~monty_command ~home ~harness ~harness_command ~codex_yolo ^
  (match continuation with Picker->" --mode picker"|Last->" --mode last"
   | Session id->" --mode exact --session " ^ Shell.quote id)

let run ~monty_command:_ ~home ~harness ~harness_command command =
  let ( let* ) = Result.bind in
  let home = Shell.normalize (Shell.abs_path home) in
  if not (Sys.file_exists home && Sys.is_directory home) then
    Error (Printf.sprintf "Monty home is not an existing directory: %s" home)
  else
    let* _ = Process.command_exists_with_arguments harness_command in
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
