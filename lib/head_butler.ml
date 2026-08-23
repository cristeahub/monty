let ( let* ) = Result.bind

type continuation = Picker | Last | Session of string

let codex_options ~codex_yolo =
  let yolo =
    if codex_yolo then " --dangerously-bypass-approvals-and-sandbox" else ""
  in
  Harness_command.codex_effort_arg ^ Harness_command.codex_vim_arg ^ yolo

let command ~home ~harness ~harness_command ~codex_yolo ~name =
  match harness with
  | Harness.Pi ->
      Printf.sprintf "cd %s && exec %s --name %s" (Shell.quote home)
        harness_command (Shell.quote name)
  | Harness.Codex ->
      Printf.sprintf "cd %s && exec %s%s -C ." (Shell.quote home)
        harness_command (codex_options ~codex_yolo)

let continuation_command ~home ~harness ~harness_command ~codex_yolo
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
      let selector =
        match continuation with
        | Picker -> ""
        | Last -> " --last"
        | Session session -> " " ^ Shell.quote session
      in
      Printf.sprintf "cd %s && exec %s resume%s -C .%s" (Shell.quote home)
        harness_command (codex_options ~codex_yolo) selector

let run ~home ~harness command =
  let home = Shell.normalize (Shell.abs_path home) in
  if not (Sys.file_exists home && Sys.is_directory home) then
    Error (Printf.sprintf "Monty home is not an existing directory: %s" home)
  else
    let* () =
      match harness with
      | Harness.Pi -> Ok ()
      | Harness.Codex -> Codex_trust.ensure ~home ~path:home
    in
    match Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; command home |] with
    | () -> assert false
    | exception Unix.Unix_error (err, fn, arg) ->
        Error
          (Printf.sprintf "failed to exec %s via %s(%s): %s"
             (Harness.to_string harness) fn arg
             (Unix.error_message err))

let start ~home ~harness ~harness_command ~codex_yolo ~name =
  run ~home ~harness (fun home ->
      command ~home ~harness ~harness_command ~codex_yolo ~name)

let continue ~home ~harness ~harness_command ~codex_yolo continuation =
  run ~home ~harness (fun home ->
      continuation_command ~home ~harness ~harness_command ~codex_yolo
        continuation)
