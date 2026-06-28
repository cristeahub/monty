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

let status_porcelain ~worktree =
  Process.run_success ~cwd:worktree "git status --porcelain --untracked-files=all"

let ensure_clean ~worktree =
  match status_porcelain ~worktree with
  | Error msg -> Error msg
  | Ok output when String.trim output = "" -> Ok ()
  | Ok output ->
      Error
        (Printf.sprintf
           "worktree has uncommitted or untracked changes: %s\n%s\nUse --force to discard local changes while archiving."
           worktree output)

let force_clean ~worktree =
  let ( let* ) = Result.bind in
  let* () = Process.run_quiet ~cwd:worktree "git reset --hard" in
  Process.run_quiet ~cwd:worktree "git clean -fdx"

let branch_exists ~repo ~branch =
  let ref = "refs/heads/" ^ branch in
  match
    Process.run_capture ~cwd:repo
      (Printf.sprintf "git show-ref --verify --quiet %s" (Shell.quote ref))
  with
  | Ok { Process.status = `Exited 0; _ } -> true
  | _ -> false

let fallback_delete ?worktree ~repo ~branch () =
  let ( let* ) = Result.bind in
  let* () =
    match worktree with
    | Some path when Sys.file_exists path && Sys.is_directory path ->
        Process.run_quiet ~cwd:repo
          (Printf.sprintf "git worktree remove --force %s" (Shell.quote path))
    | _ -> Ok ()
  in
  if branch_exists ~repo ~branch then
    Process.run_quiet ~cwd:repo
      (Printf.sprintf "git branch -D %s" (Shell.quote branch))
  else Ok ()

let delete_worktree_and_branch ?worktree ~wt_command ~repo ~branch ~force () =
  let command = Printf.sprintf "%s db %s" wt_command (Shell.quote branch) in
  match Process.run_quiet ~cwd:repo command with
  | Ok () -> Ok ()
  | Error msg when force -> (
      match fallback_delete ?worktree ~repo ~branch () with
      | Ok () -> Ok ()
      | Error fallback_msg -> Error (msg ^ "\nFallback deletion failed:\n" ^ fallback_msg))
  | Error msg -> Error msg
