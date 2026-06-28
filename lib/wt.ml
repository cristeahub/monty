let prefix text value =
  let text_len = String.length text in
  let value_len = String.length value in
  value_len >= text_len && String.sub value 0 text_len = text

let strip_prefix text value =
  if prefix text value then
    Some (String.sub value (String.length text) (String.length value - String.length text))
  else None

let realpath_if_exists path =
  try Unix.realpath path with Unix.Unix_error _ -> path

let abs_from ~cwd path =
  let path = if Filename.is_relative path then Filename.concat cwd path else path in
  Shell.normalize path |> realpath_if_exists

let git_output ~cwd command =
  Process.run_success ~cwd command |> Result.map String.trim

let git_common_dir ~cwd =
  match git_output ~cwd "git rev-parse --git-common-dir" with
  | Error msg -> Error msg
  | Ok path -> Ok (abs_from ~cwd path)

let same_git_repo ~repo ~worktree =
  match (git_common_dir ~cwd:repo, git_common_dir ~cwd:worktree) with
  | Ok left, Ok right -> String.equal left right
  | _ -> false

let validate_worktree ~repo path =
  let path = Shell.normalize path |> realpath_if_exists in
  if Sys.file_exists path && Sys.is_directory path && same_git_repo ~repo ~worktree:path
  then Ok path
  else
    Error
      (Printf.sprintf "worktree does not belong to repo %s: %s" repo path)

type wt_entry = {
  prompt_index : int option;
  repo_label : string;
  branch : string option;
  path : string;
}

let index_sub needle text =
  let needle_len = String.length needle in
  let text_len = String.length text in
  if needle_len = 0 then Some 0
  else
    let rec loop index =
      if index + needle_len > text_len then None
      else if String.sub text index needle_len = needle then Some index
      else loop (index + 1)
    in
    loop 0

let split_once needle text =
  match index_sub needle text with
  | None -> None
  | Some index ->
      let left = String.sub text 0 index in
      let right_start = index + String.length needle in
      let right = String.sub text right_start (String.length text - right_start) in
      Some (left, right)

let all_digits text =
  String.length text > 0
  && String.for_all (function '0' .. '9' -> true | _ -> false) text

let parse_prompt_entry line =
  let line = String.trim line in
  match split_once ") " line with
  | Some (index, rest) when all_digits index -> (
      match split_once " -> " rest with
      | Some (repo_label, path) ->
          Some
            {
              prompt_index = Some (int_of_string index);
              repo_label = String.trim repo_label;
              branch = None;
              path = Shell.normalize (String.trim path) |> realpath_if_exists;
            }
      | None -> None)
  | _ -> None

let parse_prompt_entries text =
  text |> String.split_on_char '\n' |> List.filter_map parse_prompt_entry

let parse_list text =
  let add_entry repo_label acc line =
    let line = String.trim line in
    match split_once " -> " line with
    | None -> acc
    | Some (branch, path) ->
        {
          prompt_index = None;
          repo_label;
          branch = Some (String.trim branch);
          path = Shell.normalize (String.trim path) |> realpath_if_exists;
        }
        :: acc
  in
  let rec loop repo_label acc = function
    | [] -> List.rev acc
    | line :: rest ->
        let trimmed = String.trim line in
        if trimmed = "" then loop repo_label acc rest
        else if not (prefix " " line) && not (prefix "\t" line) then
          let repo_label =
            match String.ends_with ~suffix:":" trimmed with
            | true -> String.sub trimmed 0 (String.length trimmed - 1)
            | false -> trimmed
          in
          loop repo_label acc rest
        else loop repo_label (add_entry repo_label acc line) rest
  in
  loop "" [] (String.split_on_char '\n' text)

let wt_branch_command ?selection ~wt_command subcommand branch =
  let command = Printf.sprintf "%s %s %s" wt_command subcommand (Shell.quote branch) in
  match selection with
  | None -> command
  | Some selection ->
      Printf.sprintf "printf '%%s\\n' %s | %s"
        (Shell.quote (string_of_int selection)) command

let run_wt_branch ?selection ~wt_command ~repo subcommand branch =
  Process.run_capture ~cwd:repo
    (wt_branch_command ?selection ~wt_command subcommand branch)

let run_wt_list ~wt_command ~repo =
  match Process.run_capture ~cwd:repo (wt_command ^ " list") with
  | Error msg -> Error msg
  | Ok { Process.stdout; status = `Exited 0 } -> Ok (parse_list stdout)
  | Ok { stdout; status } ->
      Error
        (Printf.sprintf "wt list failed with %s:\n%s"
           (Process.status_to_string status) stdout)

let belongs_to_repo ~repo entry =
  Sys.file_exists entry.path && Sys.is_directory entry.path
  && same_git_repo ~repo ~worktree:entry.path

let choose_repo_entry ~repo entries =
  entries |> List.find_opt (belongs_to_repo ~repo)

let entries_for_branch branch entries =
  entries
  |> List.filter (fun entry ->
         match entry.branch with Some value -> String.equal value branch | None -> true)

let path_from_line line =
  let line = String.trim line in
  let candidates =
    [ line ]
    @ (match split_once ": " line with None -> [] | Some (_, path) -> [ String.trim path ])
    @ (match split_once " -> " line with None -> [] | Some (_, path) -> [ String.trim path ])
  in
  candidates
  |> List.map (fun path -> Shell.normalize path |> realpath_if_exists)
  |> List.find_opt (fun path -> Sys.file_exists path && Sys.is_directory path)

let output_path output =
  output |> String.split_on_char '\n' |> List.rev |> List.find_map path_from_line

let select_from_prompt ~repo output =
  match parse_prompt_entries output |> choose_repo_entry ~repo with
  | None -> None
  | Some entry -> entry.prompt_index

let find_existing_for_repo ~wt_command ~repo ~branch =
  match run_wt_list ~wt_command ~repo with
  | Error _ -> None
  | Ok entries -> entries_for_branch branch entries |> choose_repo_entry ~repo

let validate_output_path ~repo output =
  match output_path output with
  | None -> Error "wt did not print a worktree path"
  | Some path -> validate_worktree ~repo path

let create_or_reuse ~wt_command ~repo ~branch =
  let repo = Shell.normalize repo in
  match run_wt_branch ~wt_command ~repo "b" branch with
  | Error msg -> Error msg
  | Ok { stdout; status = `Exited 0 } -> (
      match validate_output_path ~repo stdout with
      | Ok path -> Ok path
      | Error msg -> (
          match find_existing_for_repo ~wt_command ~repo ~branch with
          | Some entry -> Ok entry.path
          | None ->
              Error
                (msg
                ^ "\nwt selected a worktree for a different repo and did not expose a selectable worktree for the requested repo.")))
  | Ok { stdout; status } -> (
      match select_from_prompt ~repo stdout with
      | Some selection -> (
          match run_wt_branch ~selection ~wt_command ~repo "b" branch with
          | Error msg -> Error msg
          | Ok { stdout; status = `Exited 0 } -> validate_output_path ~repo stdout
          | Ok { stdout; status } ->
              Error
                (Printf.sprintf "wt b failed with %s after selecting repo:\n%s"
                   (Process.status_to_string status) stdout))
      | None -> (
          match find_existing_for_repo ~wt_command ~repo ~branch with
          | Some entry -> Ok entry.path
          | None ->
              Error
                (Printf.sprintf "wt b failed with %s:\n%s"
                   (Process.status_to_string status) stdout)))

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

let delete_with_wt ?selection ~wt_command ~repo ~branch () =
  match run_wt_branch ?selection ~wt_command ~repo "db" branch with
  | Error msg -> Error msg
  | Ok { status = `Exited 0; _ } -> Ok ()
  | Ok { stdout; status } ->
      Error
        (Printf.sprintf "wt db failed with %s:\n%s"
           (Process.status_to_string status) stdout)

let delete_worktree_and_branch ?worktree ~wt_command ~repo ~branch ~force:_ () =
  let repo = Shell.normalize repo in
  let worktree = Option.map (fun path -> Shell.normalize path |> realpath_if_exists) worktree in
  let ( let* ) = Result.bind in
  let* entries = run_wt_list ~wt_command ~repo in
  let branch_entries = entries_for_branch branch entries in
  let matching_entries = List.filter (belongs_to_repo ~repo) branch_entries in
  let matching_entries =
    match worktree with
    | None -> matching_entries
    | Some path ->
        let exact =
          List.filter (fun entry -> String.equal entry.path path) matching_entries
        in
        if exact = [] then matching_entries else exact
  in
  match (branch_entries, matching_entries) with
  | [], _ -> delete_with_wt ~wt_command ~repo ~branch ()
  | _, [] ->
      Error
        (Printf.sprintf
           "wt has branch %S in another repo, but not in requested repo %s"
           branch repo)
  | [ _ ], [ _ ] -> delete_with_wt ~wt_command ~repo ~branch ()
  | _ -> (
      match run_wt_branch ~wt_command ~repo "db" branch with
      | Error msg -> Error msg
      | Ok { status = `Exited 0; _ } -> Ok ()
      | Ok { stdout; _ } -> (
          match select_from_prompt ~repo stdout with
          | Some selection -> delete_with_wt ~selection ~wt_command ~repo ~branch ()
          | None ->
              Error
                (Printf.sprintf
                   "wt reported multiple worktrees for %S, but Monty could not select repo %s:\n%s"
                   branch repo stdout)))
