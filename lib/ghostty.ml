let command_for_script script_path =
  "/bin/zsh -l " ^ Shell.quote script_path

let applescript ~target ~workdir ~script_path =
  let command = command_for_script script_path in
  let config =
    Printf.sprintf
      "new surface configuration from {initial working directory:%s, command:%s, wait after command:true}"
      (Shell.applescript_string workdir)
      (Shell.applescript_string command)
  in
  let target_script =
    match target with
    | Terminal.Window ->
        "set montyWindow to new window with configuration montyConfig\n\
         activate window montyWindow"
    | Terminal.Tab ->
        "if (count of windows) = 0 then\n\
         set montyWindow to new window with configuration montyConfig\n\
         activate window montyWindow\n\
         else\n\
         set montyTab to new tab in front window with configuration montyConfig\n\
         select tab montyTab\n\
         end if"
    | Terminal.Split ->
        "if (count of windows) = 0 then\n\
         set montyWindow to new window with configuration montyConfig\n\
         activate window montyWindow\n\
         else\n\
         set montyTerminal to focused terminal of selected tab of front window\n\
         set montyNewTerminal to split montyTerminal direction right with configuration montyConfig\n\
         focus montyNewTerminal\n\
         end if"
  in
  String.concat "\n"
    [ "tell application \"Ghostty\"";
      "activate";
      "set montyConfig to " ^ config;
      target_script;
      "end tell";
      "" ]

let launch ~target ~workdir ~script_path =
  let source = applescript ~target ~workdir ~script_path in
  let script_file = Filename.temp_file "monty-ghostty-" ".applescript" in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists script_file then Sys.remove script_file)
    (fun () ->
      Shell.write_file script_file source;
      Process.run_success ("osascript " ^ Shell.quote script_file)
      |> Result.map (fun _ -> ()))
