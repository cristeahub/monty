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
let json = Yojson.Safe.from_string
let list = Monty.Json_fields.list
let item kind target = List.find (fun item -> s "kind" item = kind) (xs "items" target)
let target id report = List.find (fun target -> s "id" target = id && field "profile" target = `Null) (xs "targets" report)

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
  let cli ?(ok = true) ?(environment = environment) ?input args =
    let result = run ~cwd:root ~env:environment ?input ~timeout:20. executable args in
    check ((result.code = 0) = ok) (String.concat " " args ^ "\n" ^ result.stdout ^ result.stderr);
    result
  in
  let index = (cli [ "guide" ]).stdout in require index "agents";
  check (not (contains index "mcp:")) "legacy MCP guide remains";
  List.iter (fun args -> ignore (cli ~ok:false args))
    [ [ "mcp"; "guide" ]; [ "skills"; "install" ]; [ "headless"; "begin"; "worker" ];
      [ "guide"; "not-a-topic" ]; [ "mcp"; "serve" ]; [ "codex-session-capture" ] ];
  ignore (cli [ "skill"; "--help=plain" ]); ignore (cli [ "skill"; "setup"; "--help=plain" ]);
  ignore (cli [ "api"; "--help=plain" ]);
  equal 0 (Array.length (Sys.readdir home));
  List.iter (fun key -> check (not (Sys.file_exists (List.assoc key environment))) "help created native home")
    [ "CODEX_HOME"; "PI_CODING_AGENT_DIR"; "CLAUDE_CONFIG_DIR" ];
  (* The guide the stub defers to is the embedded manual, topic by topic. *)
  let shown = (cli [ "skill"; "show" ]).stdout in
  equal (String.trim shown) (String.trim (cli [ "guide"; "all" ]).stdout);
  let topics = json (cli [ "guide"; "--json" ]).stdout |> xs "topics" |> List.map (s "id") in
  check (List.mem "agents" topics && List.mem "workers" topics) "guide --json lacks topics";
  (* Nothing is written by a dry run; every harness is set up by default. *)
  let dry = json (cli [ "skill"; "setup"; "--dry-run"; "--json" ]).stdout in
  check (field "dry_run" dry = `Bool true) "dry run flag";
  equal [ "claude"; "codex"; "pi" ] (List.map (s "id") (xs "targets" dry));
  equal "created" (s "change" (item "skill" (target "codex" dry)));
  equal "unsupported" (s "state" (item "hook" (target "pi" dry)));
  List.iter (fun key -> check (not (Sys.file_exists (List.assoc key environment))) "dry run created native home")
    [ "CODEX_HOME"; "PI_CODING_AGENT_DIR"; "CLAUDE_CONFIG_DIR" ];
  let originals = Hashtbl.create 3 in
  List.iter (fun (harness, key) ->
    let agent = List.assoc key environment in
    let target = agent / "skills/monty" in
    let skill = target / "SKILL.md" and manual = target / "references/workflows.md" in
    let setup ?environment ?(ok = true) args = cli ?environment ~ok ([ "skill"; "setup"; "--harness"; harness ] @ args) in
    let status () = json (cli [ "skill"; "status"; "--harness"; harness; "--json" ]).stdout |> fun report -> List.hd (xs "targets" report) in
    let state kind = s "state" (item kind (status ())) in
    equal "missing" (state "skill");
    let first = setup [] in require first.stdout "created";
    check (String.starts_with ~prefix:"---\n# Managed by Monty." (read skill)) "skill metadata front matter";
    List.iter (require (read skill)) [ "name: monty\n"; "monty guide"; "monty skill show"; "references/workflows.md" ];
    String.split_on_char '\n' index |> List.iter (fun line ->
      if String.starts_with ~prefix:"- " line then (
        let topic = String.sub line 2 (String.length line - 2) |> String.split_on_char ':' |> List.hd in
        require (read manual) (String.trim (cli [ "guide"; topic ]).stdout)));
    equal "current" (state "skill");
    let original = read skill, read manual in Hashtbl.add originals harness original;
    let before = mark_unchanged [ skill; manual ] in require (setup []).stdout "unchanged";
    equal before (fingerprint [ skill; manual ]);
    write skill "---\n# Managed by Monty.\nname: monty\ndescription: old\n---\n";
    write manual "<!-- Managed by Monty. -->\nold reference\n";
    equal "outdated" (state "skill");
    let unrelated = agent / "skills/custom/SKILL.md" in write unrelated "custom skill";
    require (setup []).stdout "updated"; equal original (read skill, read manual); equal "custom skill" (read unrelated);
    write skill "my own monty skill";
    equal "foreign" (state "skill");
    let conflict = setup ~ok:false [] in require conflict.stdout "conflict"; require conflict.stdout "--force";
    equal "my own monty skill" (read skill);
    require (setup [ "--force" ]).stdout "updated"; equal original (read skill, read manual);
    Unix.unlink skill;
    let victim = root / (harness ^ "-victim") in write victim "untouched";
    Unix.symlink victim skill;
    equal "unreadable" (state "skill");
    ignore (setup ~ok:false []); equal "untouched" (read victim);
    Unix.unlink skill; ignore (setup []);
    remove_tree target; Unix.symlink root target;
    ignore (setup ~ok:false []); check (not (Sys.file_exists (root / "SKILL.md"))) "followed skill directory symlink";
    Unix.unlink target; ignore (setup []);
    let children = List.init 6 (fun number ->
      spawn ~cwd:root ~env:(merge_env environment [ "MONTY_HOME", root / ("other-home-" ^ string_of_int number) ])
        ~timeout:20. executable [ "skill"; "setup"; "--harness"; harness ]) in
    let pending = ref children in
    Fun.protect ~finally:(fun () -> List.iter (fun child ->
      kill child;
      try ignore (await ~timeout:5. child) with _ -> ()) !pending) (fun () ->
      List.iter (fun child ->
        let result = await ~timeout:20. child in
        pending := List.tl !pending;
        check (result.code = 0) ("concurrent skill install failed: " ^ result.stderr)) children);
    equal original (read skill, read manual);
    (* The hook joins the agent's own hooks and leaves them alone when removed. *)
    (match harness with
     | "pi" -> equal "unsupported" (state "hook")
     | _ ->
         let file = agent / (if harness = "claude" then "settings.json" else "hooks.json") in
         equal "current" (state "hook");
         let installed = json (read file) in
         let groups = Yojson.Safe.Util.(member "SessionStart" (member "hooks" installed)) |> list in
         equal 1 (List.length groups);
         let command = s "command" (List.hd (xs "hooks" (List.hd groups))) in
         require command ("monty __internal session-start --harness " ^ harness);
         let theirs = `Assoc [ "matcher", string "startup"; "hooks", `List [ obj [ "type", string "command"; "command", string "echo mine" ] ] ] in
         write file (Yojson.Safe.to_string (obj [ "theme", string "dark"; "model", string "opus";
           "hooks", obj [ "PreToolUse", `List []; "SessionStart", `List [ theirs;
             obj [ "hooks", `List [ obj [ "type", string "command"; "command", string "monty __internal session-start --harness old"; "timeout", `Int 1 ] ] ] ] ] ]));
         equal "outdated" (state "hook");
         require (setup []).stdout "updated";
         let merged = json (read file) in
         equal (string "dark") (field "theme" merged); equal (string "opus") (field "model" merged);
         equal (`List []) Yojson.Safe.Util.(member "PreToolUse" (member "hooks" merged));
         equal [ theirs; Monty.Skill.hook_entry (Result.get_ok (Monty.Harness.of_string harness)) ]
           (Yojson.Safe.Util.(member "SessionStart" (member "hooks" merged)) |> list);
         let before = mark_unchanged [ file ] in require (setup []).stdout "unchanged"; equal before (fingerprint [ file ]);
         write file "{not json";
         equal "unreadable" (state "hook");
         ignore (setup ~ok:false []); equal "{not json" (read file);
         write file (Yojson.Safe.to_string (obj [ "hooks", obj [ "SessionStart", `List [ theirs ] ] ]));
         equal "missing" (state "hook");
         ignore (setup []);
         let removed = cli [ "skill"; "remove"; "--harness"; harness ] in require removed.stdout "removed";
         equal [ theirs ] (Yojson.Safe.Util.(member "SessionStart" (member "hooks" (json (read file)))) |> list);
         check (not (Sys.file_exists skill)) "skill survived removal"; equal "custom skill" (read unrelated);
         equal "missing" (state "skill"); equal "missing" (state "hook");
         ignore (setup [])))
    [ "codex", "CODEX_HOME"; "pi", "PI_CODING_AGENT_DIR"; "claude", "CLAUDE_CONFIG_DIR" ];
  (* The hook describes the session it starts in and stays silent elsewhere. *)
  let hook ?(environment = environment) cwd harness =
    cli ~environment ~input:(Printf.sprintf "{\"cwd\":%S,\"hook_event_name\":\"SessionStart\"}" cwd) [ "__internal"; "session-start"; "--harness"; harness ] in
  equal "" (hook root "claude").stdout;
  require (hook home "claude").stdout "Monty control room";
  require (hook home "claude").stdout "No worker handoffs are pending";
  let worker = hook ~environment:(merge_env environment [ "MONTY_JOB_ID", "w-1"; "MONTY_TASK_KEY", "local:9"; "MONTY_JOB_FILE", root / "job.json" ]) root "codex" in
  List.iter (require worker.stdout) [ "Monty worker w-1"; "MONTY_TASK_KEY=local:9"; "monty handoff publish"; "($monty)" ];
  equal 0 (cli ~ok:true ~input:"" [ "__internal"; "session-start"; "--harness"; "nope" ]).code;
  check ((cli ~ok:false [ "__internal"; "bogus" ]).code = 2) "unknown internal command";
  let defaults = List.filter (fun (key, _) -> not (List.mem key [ "CODEX_HOME"; "PI_CODING_AGENT_DIR"; "CLAUDE_CONFIG_DIR" ])) environment in
  List.iter (fun (harness, directory) ->
    ignore (cli ~environment:defaults [ "skill"; "setup"; "--harness"; harness ]);
    equal (fst (Hashtbl.find originals harness)) (read (root / directory / "skills/monty/SKILL.md"));
    let missing_home = List.remove_assoc "HOME" defaults in
    require (cli ~environment:missing_home ~ok:false [ "skill"; "setup"; "--harness"; harness ]).stdout "No agent found";
    ignore (cli ~environment:(List.remove_assoc "HOME" environment) [ "skill"; "setup"; "--harness"; harness ]))
    [ "codex", ".codex"; "pi", ".pi/agent"; "claude", ".claude" ];
  (* Service profiles are reported beside the harness accounts, even without HOME. *)
  let profile_home = root / "work claude" in mkdir profile_home;
  write (home / ".monty/service-profiles.json") (Yojson.Safe.to_string (obj [ "profiles", `List [ obj [ "id", string "claude-work";
    "harness", string "claude"; "command", string "claude"; "claude_home", string profile_home; "roots", `List [ string root ] ] ] ]));
  let report = json (cli [ "skill"; "status"; "--json" ]).stdout in
  let profile = List.find (fun target -> field "profile" target = string "claude-work") (xs "targets" report) in
  equal "missing" (s "state" (item "skill" profile));
  let without_home = cli ~environment:(List.remove_assoc "HOME" defaults) [ "skill"; "setup"; "--harness"; "claude" ] in
  check (not (contains without_home.stdout "Claude Code (found)" || contains without_home.stdout "Claude Code (not found)")) "account set up without HOME";
  check (Sys.file_exists (profile_home / "skills/monty/SKILL.md") && Sys.file_exists (profile_home / "settings.json")) "service profile not set up";
  let agent = List.assoc "PI_CODING_AGENT_DIR" environment in
  let config = agent / "mcp.json" in
  write config "{\"mcpServers\":{\"monty\":{\"command\":\"untouched\"}}}";
  let before = mark_unchanged [ config ] in ignore (cli [ "skill"; "setup"; "--harness"; "pi" ]);
  equal before (fingerprint [ config ]);
  let log = root / "argv.json" and fake = root / "fake harness" in
  write fake ("#!/bin/sh\nexec " ^ Monty.Shell.quote (Unix.realpath Sys.executable_name)
    ^ " --fixture-pi " ^ Monty.Shell.quote log ^ " \"$@\"\n");
  Unix.chmod fake 0o755;
  let config = root / "server.json" in
  write config (Yojson.Safe.to_string (obj [ "profiles", `List [ obj [ "id", string "pi";
    "harness", string "pi"; "command", string fake; "pi_home", string agent;
    "roots", `List [ string root ] ] ] ]));
  (* Skill installs above created account directories under HOME; an empty HOME
     keeps this launch from generating a service configuration out of them. *)
  let no_accounts = root / "no accounts" in mkdir no_accounts;
  ignore (cli ~ok:false ~environment:(merge_env environment [ "HOME", no_accounts ])
    [ "start"; "--harness"; "pi"; "--pi-command"; Monty.Shell.quote fake ]);
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
  print_endline "CLI skill: setup, status, remove, hooks, service profiles, session-start, concurrency and Pi startup passed")

let () = match Array.to_list Sys.argv with
  | _ :: "--fixture-pi" :: log :: args -> fixture log args
  | _ -> suite ()
