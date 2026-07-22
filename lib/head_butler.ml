let extension home = Filename.concat home "pi-extension"

let command ~home ~monty_command ~pi_command ~name =
  Printf.sprintf "cd %s && MONTY_HOME=%s MONTY_COMMAND=%s exec %s --extension %s --name %s"
    (Shell.quote home) (Shell.quote home) (Shell.quote monty_command) pi_command
    (Shell.quote (extension home)) (Shell.quote name)

let start ~home ~monty_command ~pi_command ~name =
  let home = Shell.normalize (Shell.abs_path home) in
  if not (Sys.file_exists home && Sys.is_directory home) then
    Error (Printf.sprintf "Monty home is not an existing directory: %s" home)
  else if not (Sys.file_exists (extension home) && Sys.is_directory (extension home)) then
    Error (Printf.sprintf "Monty Pi extension is missing: %s" (extension home))
  else
    match
      Unix.execv "/bin/sh"
        [| "/bin/sh"; "-c"; command ~home ~monty_command ~pi_command ~name |]
    with
    | () -> assert false
    | exception Unix.Unix_error (err, fn, arg) ->
        Error
          (Printf.sprintf "failed to exec pi via %s(%s): %s" fn arg
             (Unix.error_message err))
