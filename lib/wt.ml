let create_or_reuse ~wt_command ~repo ~branch =
  let command = Printf.sprintf "%s b %s" wt_command (Shell.quote branch) in
  match Process.run_success ~cwd:repo command with
  | Error msg -> Error msg
  | Ok output -> (
      match Shell.last_non_empty_line output with
      | None -> Error "wt did not print a worktree path"
      | Some path ->
          let path =
            if Filename.is_relative path then Filename.concat repo path else path
          in
          let path = Shell.normalize path in
          if Sys.file_exists path && Sys.is_directory path then Ok path
          else
            Error
              (Printf.sprintf
                 "wt printed %S, but that path is not an existing directory"
                 path))
