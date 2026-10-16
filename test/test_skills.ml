open Test_support

let contains = Monty.Home.contains_substring
let require text needle = check (contains text needle) ("missing " ^ needle ^ " in " ^ text)
let fingerprint paths = List.map (fun path ->
  let stat = Unix.stat path in stat.Unix.st_ino, stat.Unix.st_mtime, read path) paths
let mark_unchanged paths =
  (* A fixed old timestamp makes unintended rewrites observable even on filesystems
     whose timestamp precision is lower than OCaml's float representation. *)
  List.iter (fun path -> Unix.utimes path 1. 1.) paths;
  fingerprint paths

let fixture log args =
  let skill = Sys.getenv "PI_CODING_AGENT_DIR" / "skills/monty/SKILL.md" in
  check (Sys.file_exists skill && not (Sys.is_directory skill)) "native Pi started before installing its skill";
  write log (Yojson.Safe.to_string (`List (List.map string args)))

let suite () = with_temp_dir ~prefix:"monty-skills-" (fun root ->
  let home = root / "monty home" in mkdir home;
  let executable = root / "monty 'quoted' $(touch INJECTED)" in
  write executable (read bin); Unix.chmod executable 0o755;
  let environment = merge_env env
      [ "HOME", root; "MONTY_HOME", home; "MONTY_HARNESS", "codex";
        "CODEX_HOME", root / "codex profile"; "PI_CODING_AGENT_DIR", root / "pi profile";
        "CLAUDE_CONFIG_DIR", root / "claude profile"; "MANTLE_CONTEXT", "";
        "MONTY_CODEX_YOLO", "false" ] in
  let cli ?(ok = true) ?(environment = environment) args =
    let result = run ~cwd:root ~env:environment ~timeout:20. executable args in
    check ((result.code = 0) = ok) (String.concat " " args ^ "\n" ^ result.stdout ^ result.stderr);
    result
  in
  let index = (cli [ "guide" ]).stdout in require index "skills";
  check (not (contains index "mcp:")) "legacy MCP guide remains";
  List.iter (fun args -> ignore (cli ~ok:false args))
    [ [ "mcp"; "guide" ]; [ "mcp"; "guide"; "workers" ];
      [ "headless"; "begin"; "worker" ]; [ "headless"; "finish"; "worker" ];
      [ "guide"; "not-a-topic" ]; [ "mcp"; "serve" ]; [ "mcp"; "setup-pi" ];
      [ "codex-session-capture" ] ];
  ignore (cli [ "skills"; "install"; "--help=plain" ]);
  equal 0 (Array.length (Sys.readdir home));
  List.iter (fun key -> check (not (Sys.file_exists (List.assoc key environment))) "help created native home")
    [ "CODEX_HOME"; "PI_CODING_AGENT_DIR" ];
  let originals = Hashtbl.create 3 in
  List.iter (fun (harness, key) ->
    let agent = List.assoc key environment in
    let target = agent / "skills/monty" in
    let skill = target / "SKILL.md" and manual = target / "references/workflows.md" in
    let install ?environment ?(ok = true) () = cli ?environment ~ok [ "skills"; "install"; "--harness"; harness ] in
    ignore (install ());
    check (String.starts_with ~prefix:"---\n" (read skill)) "skill metadata front matter";
    List.iter (require (read skill)) [ "name: monty\n"; "monty guide" ];
    String.split_on_char '\n' index |> List.iter (fun line ->
      if String.starts_with ~prefix:"- " line then (
        let topic = String.sub line 2 (String.length line - 2) |> String.split_on_char ':' |> List.hd in
        require (read manual) (String.trim (cli [ "guide"; topic ]).stdout)));
    let original = read skill, read manual in Hashtbl.add originals harness original;
    let before = mark_unchanged [ skill; manual ] in ignore (install ());
    equal before (fingerprint [ skill; manual ]);
    write skill "---\n# Managed by Monty.\nname: monty\ndescription: old\n---\n";
    write manual "<!-- Managed by Monty. -->\nold reference\n";
    let unrelated = agent / "skills/custom/SKILL.md" in write unrelated "custom skill";
    ignore (install ()); equal original (read skill, read manual); equal "custom skill" (read unrelated);
    write skill "my own monty skill";
    require (install ~ok:false ()).stderr "unmanaged";
    equal "my own monty skill" (read skill);
    Unix.unlink skill;
    let victim = root / (harness ^ "-victim") in write victim "untouched";
    Unix.symlink victim skill; ignore (install ~ok:false ()); equal "untouched" (read victim);
    Unix.unlink skill; ignore (install ());
    remove_tree target; Unix.symlink root target;
    ignore (install ~ok:false ()); check (not (Sys.file_exists (root / "SKILL.md"))) "followed skill directory symlink";
    Unix.unlink target; ignore (install ());
    let children = List.init 6 (fun number ->
      spawn ~cwd:root ~env:(merge_env environment [ "MONTY_HOME", root / ("other-home-" ^ string_of_int number) ])
        ~timeout:20. executable [ "skills"; "install"; "--harness"; harness ]) in
    let pending = ref children in
    Fun.protect ~finally:(fun () -> List.iter (fun child ->
      kill child;
      try ignore (await ~timeout:5. child) with _ -> ()) !pending) (fun () ->
      List.iter (fun child ->
        let result = await ~timeout:20. child in
        pending := List.tl !pending;
        check (result.code = 0) ("concurrent skill install failed: " ^ result.stderr)) children);
    equal original (read skill, read manual))
    [ "codex", "CODEX_HOME"; "pi", "PI_CODING_AGENT_DIR"; "claude", "CLAUDE_CONFIG_DIR" ];
  let defaults = List.filter (fun (key, _) -> not (List.mem key [ "CODEX_HOME"; "PI_CODING_AGENT_DIR" ])) environment in
  List.iter (fun (harness, directory) ->
    ignore (cli ~environment:defaults [ "skills"; "install"; "--harness"; harness ]);
    equal (fst (Hashtbl.find originals harness)) (read (root / directory / "skills/monty/SKILL.md"));
    let missing_home = List.remove_assoc "HOME" defaults in
    require (cli ~environment:missing_home ~ok:false [ "skills"; "install"; "--harness"; harness ]).stderr "or HOME";
    ignore (cli ~environment:(List.remove_assoc "HOME" environment) [ "skills"; "install"; "--harness"; harness ]))
    [ "codex", ".codex"; "pi", ".pi/agent" ];
  let agent = List.assoc "PI_CODING_AGENT_DIR" environment in
  let config = agent / "mcp.json" in
  write config "{\"mcpServers\":{\"monty\":{\"command\":\"untouched\"}}}";
  let before = mark_unchanged [ config ] in ignore (cli [ "skills"; "install"; "--harness"; "pi" ]);
  equal before (fingerprint [ config ]);
  let log = root / "argv.json" and fake = root / "fake harness" in
  write fake ("#!/bin/sh\nexec " ^ Monty.Shell.quote (Unix.realpath Sys.executable_name)
    ^ " --fixture-pi " ^ Monty.Shell.quote log ^ " \"$@\"\n");
  Unix.chmod fake 0o755;
  let config = root / "server.json" in
  write config (Yojson.Safe.to_string (obj [ "profiles", `List [ obj [ "id", string "pi";
    "harness", string "pi"; "command", string fake; "pi_home", string agent;
    "roots", `List [ string root ] ] ] ]));
  ignore (cli ~ok:false [ "start"; "--harness"; "pi"; "--pi-command"; Monty.Shell.quote fake ]);
  check (not (Sys.file_exists log)) "unavailable service started independent native harness";
  ignore (cli [ "service"; "start"; "--config"; config ]);
  Fun.protect ~finally:(fun () -> ignore (cli [ "service"; "stop" ])) (fun () ->
    ignore (cli [ "start"; "--harness"; "pi"; "--pi-command"; Monty.Shell.quote fake ]);
    wait ~seconds:2. (fun () -> Sys.file_exists log);
    check (List.mem (string "--session-id") (match Yojson.Safe.from_file log with `List values -> values | _ -> failwith "expected argv array"))
      "native start did not use exact session identity";
    check (Sys.file_exists (agent / "skills/monty/SKILL.md")) "native start lost skill");
  List.iter (fun path -> check (not (Sys.file_exists path)) ("unexpected side effect: " ^ path))
    [ root / "INJECTED"; home / ".monty/tasks.local.json"; home / ".monty/runs" ];
  print_endline "CLI skills: installation, atomic refresh, native profiles, concurrency and Pi startup passed")

let () = match Array.to_list Sys.argv with
  | _ :: "--fixture-pi" :: log :: args -> fixture log args
  | _ -> suite ()
