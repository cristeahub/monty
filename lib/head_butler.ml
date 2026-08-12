let ( let* ) = Result.bind

let command ~home ~harness ~harness_command ~codex_yolo ~name =
  match harness with
  | Harness.Pi ->
      Printf.sprintf "cd %s && exec %s --name %s" (Shell.quote home)
        harness_command (Shell.quote name)
  | Harness.Codex ->
      Printf.sprintf "cd %s && exec %s%s%s -C ." (Shell.quote home)
        harness_command Harness_command.codex_effort_arg
        (if codex_yolo then " --dangerously-bypass-approvals-and-sandbox"
         else "")

let start ~home ~harness ~harness_command ~codex_yolo ~name =
  let home = Shell.normalize (Shell.abs_path home) in
  if not (Sys.file_exists home && Sys.is_directory home) then
    Error (Printf.sprintf "Monty home is not an existing directory: %s" home)
  else
    let* () =
      match harness with
      | Harness.Pi -> Ok ()
      | Harness.Codex -> Codex_trust.ensure ~home ~path:home
    in
    let command = command ~home ~harness ~harness_command ~codex_yolo ~name in
    match Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; command |] with
    | () -> assert false
    | exception Unix.Unix_error (err, fn, arg) ->
        Error
          (Printf.sprintf "failed to exec %s via %s(%s): %s"
             (Harness.to_string harness) fn arg
             (Unix.error_message err))
