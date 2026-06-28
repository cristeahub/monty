let start ~home ~pi_command ~name =
  let home = Shell.normalize (Shell.abs_path home) in
  if not (Sys.file_exists home && Sys.is_directory home) then
    Error (Printf.sprintf "Monty home is not an existing directory: %s" home)
  else
    let command =
      Printf.sprintf "cd %s && exec %s --name %s" (Shell.quote home) pi_command
        (Shell.quote name)
    in
    match Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; command |] with
    | () -> assert false
    | exception Unix.Unix_error (err, fn, arg) ->
        Error
          (Printf.sprintf "failed to exec pi via %s(%s): %s" fn arg
             (Unix.error_message err))
