let print_tool name command =
  match Process.command_exists command with
  | Some path -> Fmt.pr "%s: %s\n" name path
  | None -> Fmt.pr "%s: missing\n" name

let print_command_output name command =
  match Process.run_success command with
  | Ok output -> Fmt.pr "%s: %s\n" name (String.trim output)
  | Error _ -> Fmt.pr "%s: unavailable\n" name

let run ~pi_command ~wt_command =
  let env_home = match Sys.getenv_opt "MONTY_HOME" with Some value -> value | None -> "<unset>" in
  let env_branch_prefix = match Sys.getenv_opt "MONTY_BRANCH_PREFIX" with Some value -> value | None -> "monty" in
  Fmt.pr "Monty home: %s\n" (Home.default ());
  Fmt.pr "MONTY_HOME env: %s\n" env_home;
  Fmt.pr "MONTY_BRANCH_PREFIX: %s\n" env_branch_prefix;
  Fmt.pr "Executable: %s\n" Sys.executable_name;
  print_tool "pi" pi_command;
  print_tool "wt" wt_command;
  print_tool "ghostty" "ghostty";
  print_tool "osascript" "osascript";
  print_tool "sdef" "sdef";
  if Sys.file_exists "/Applications/Ghostty.app" then
    Fmt.pr "Ghostty.app: /Applications/Ghostty.app\n"
  else Fmt.pr "Ghostty.app: missing at /Applications/Ghostty.app\n";
  print_command_output "dune" "dune --version";
  print_command_output "Ghostty AppleScript dictionary"
    "sdef /Applications/Ghostty.app >/dev/null && printf available || printf unavailable";
  Ok ()
