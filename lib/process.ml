type captured = {
  stdout : string;
  status : Bos.OS.Cmd.status;
}

let status_to_string status = Fmt.str "%a" Bos.OS.Cmd.pp_status status

let run_capture ?cwd command =
  let command =
    match cwd with
    | None -> command
    | Some cwd -> Printf.sprintf "cd %s && %s" (Shell.quote cwd) command
  in
  let cmd = Bos.Cmd.(v "/bin/sh" % "-c" % command) in
  match Bos.OS.Cmd.(run_out ~err:err_run_out cmd |> out_string ~trim:false) with
  | Ok (stdout, (_, status)) -> Ok { stdout; status }
  | Error (`Msg msg) -> Error msg

let run_success ?cwd command =
  match run_capture ?cwd command with
  | Error msg -> Error msg
  | Ok { stdout; status = `Exited 0 } -> Ok stdout
  | Ok { stdout; status } ->
      Error
        (Printf.sprintf "command failed with %s:\n%s\n%s"
           (status_to_string status) command stdout)

let run_quiet ?cwd command = run_success ?cwd command |> Result.map (fun _ -> ())

let command_exists command =
  match run_capture (Printf.sprintf "command -v %s" (Shell.quote command)) with
  | Ok { status = `Exited 0; stdout } -> Some (String.trim stdout)
  | _ -> None

let first_command_word command =
  let length = String.length command in
  let buffer = Buffer.create length in
  let rec skip index =
    if index < length && (command.[index] = ' ' || command.[index] = '\t') then
      skip (index + 1)
    else index
  in
  let rec unquoted index =
    if index >= length || command.[index] = ' ' || command.[index] = '\t' then
      index
    else
      match command.[index] with
      | '\'' -> single_quoted (index + 1)
      | '"' -> double_quoted (index + 1)
      | '\\' when index + 1 < length ->
          Buffer.add_char buffer command.[index + 1];
          unquoted (index + 2)
      | character ->
          Buffer.add_char buffer character;
          unquoted (index + 1)
  and single_quoted index =
    if index >= length then length
    else if command.[index] = '\'' then unquoted (index + 1)
    else (
      Buffer.add_char buffer command.[index];
      single_quoted (index + 1))
  and double_quoted index =
    if index >= length then length
    else
      match command.[index] with
      | '"' -> unquoted (index + 1)
      | '\\' when index + 1 < length ->
          Buffer.add_char buffer command.[index + 1];
          double_quoted (index + 2)
      | character ->
          Buffer.add_char buffer character;
          double_quoted (index + 1)
  in
  let start = skip 0 in
  ignore (unquoted start);
  let executable = Buffer.contents buffer in
  if executable = "" then Error "configured command is empty"
  else Ok executable

let executable_file path =
  let path = Shell.abs_path path |> Shell.normalize in
  try
    let stat = Unix.stat path in
    Unix.access path [ Unix.X_OK ];
    if stat.Unix.st_kind = Unix.S_REG then Ok path
    else Error (Printf.sprintf "configured executable is not a regular file: %s" path)
  with Unix.Unix_error (err, fn, arg) ->
    Error
      (Printf.sprintf "configured executable %s failed %s(%s): %s" path fn arg
         (Unix.error_message err))

let command_exists_with_arguments command =
  match first_command_word command with
  | Error _ as error -> error
  | Ok executable ->
      if String.contains executable '/' then executable_file executable
      else
        (match command_exists executable with
        | Some path -> Ok path
        | None ->
            Error
              (Printf.sprintf
                 "required executable %S from configured command %S was not found on PATH"
                 executable command))
