let fetch_github_issues ~repo ~query =
  let query_arg =
    match query with None -> "" | Some value -> " --search " ^ Shell.quote value
  in
  let command =
    Printf.sprintf
      "gh issue list --repo %s --limit 100 --json number,title,state,url,updatedAt%s"
      (Shell.quote repo) query_arg
  in
  match Process.run_success command with
  | Error message -> Error ("failed to fetch GitHub issues for " ^ repo ^ ": " ^ message)
  | Ok output -> (
      try
        match Yojson.Safe.from_string output with
        | `List issues -> Ok issues
        | _ -> Error ("GitHub issue output for " ^ repo ^ " was not a JSON array")
      with Yojson.Json_error message ->
        Error ("invalid GitHub issue JSON for " ^ repo ^ ": " ^ message))
