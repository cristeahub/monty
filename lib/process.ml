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
