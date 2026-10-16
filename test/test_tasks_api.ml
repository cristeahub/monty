open Test_support

let contains = Monty.Home.contains_substring
let require text needle = check (contains text needle) ("missing " ^ needle ^ " in " ^ text)
let rec files root =
  if not (Sys.file_exists root) then [] else
  match (Unix.lstat root).Unix.st_kind with
  | Unix.S_DIR -> Sys.readdir root |> Array.to_list |> List.concat_map (fun name -> files (root / name))
  | _ -> [ root ]
let array = function `List values -> values | _ -> failwith "expected array"
let required key = function `Assoc values -> List.assoc key values | _ -> failwith "expected object"

let suite () = with_temp_dir ~prefix:"monty-tasks-api-" (fun home ->
  let service, repo = Task_fixture.create home in
  Fun.protect ~finally:(fun () -> Service.close service) (fun () ->
    let cli = Task_fixture.cli service in
    let client = ref (Service.client service) in
    let req path = Client.req !client path in
    let cmd ?status key request = Client.cmd ?status !client key request in
    ignore (cli [ "settings"; "set"; "codex-yolo"; "true" ]);
    equal repo (s "repo" (List.hd (array (req "projects"))));
    let create = obj [ "op", string "task_create"; "project", string "repo";
      "title", string "Safe API launch"; "notes", string "Test notes" ] in
    let created = cmd "create" create in
    let key = ref (s "key" created) in
    ignore (Client.req ~data:create !client "commands/create");
    equal 1 (List.length (xs "tasks" (req "inventory"))); equal "Test notes" (s "notes" created);
    let request op extra = obj ([ "op", string op; "task", string !key;
      "revision", field "revision" (req ("tasks/" ^ !key)) ] @ extra) in
    let preflight ?(action = "launch") name = cmd name (request "task_preflight"
      [ "action", string action; "profile", string "local";
        "context", string "Implement the task. Acceptance: all tests pass. No remote actions are authorized." ]) in
    List.iter (fun (name, request) -> ignore (cmd ~status:"rejected" name request)) [
      "invalid-task", obj [ "op", string "task_complete"; "task", string "1"; "confirm", `Bool true ];
      "blank-title", obj [ "op", string "task_create"; "project", string "repo"; "title", string "   " ];
      "long-title", obj [ "op", string "task_create"; "project", string "repo"; "title", string (String.make 501 'x') ];
      "stale", obj [ "op", string "task_complete"; "task", string !key; "revision", string "old"; "confirm", `Bool true ] ];
    http_error 404 (fun () -> req "tasks/local:missing");
    let preview = preflight "preview" in
    equal (`Bool true) (field "ready" preview);
    check (String.starts_with ~prefix:"testprefix/" (s "branch" (List.hd (xs "workspaces" preview)))) "configured branch prefix ignored";
    require (s "stdout" (field "preview" preview)) "[dry-run]";
    check (not (List.exists (fun path -> Filename.basename path = "job.json") (files (home / ".monty/runs")))) "preflight created worker reservation";
    check (not (Sys.file_exists (home / "tools.log"))) "preflight invoked terminal or harness";
    write (home / ".monty/runs/web-preview/context.md") "changed";
    ignore (cmd ~status:"rejected" "stale-context" (obj [ "op", string "task_apply";
      "preflight", string "preview"; "confirm", `Bool true ]));
    let preview = preflight "reviewed" in equal (`Bool true) (field "ready" preview);
    equal "simplified" (s "agent_profile" preview);
    ignore (cmd ~status:"rejected" "unconfirmed" (obj [ "op", string "task_apply"; "preflight", string "reviewed" ]));
    let apply name preflight = cmd name (obj [ "op", string "task_apply"; "preflight", string preflight; "confirm", `Bool true ]) in
    let applied = apply "launch" "reviewed" in equal 0 (i "exit_code" (field "process" applied)); equal (`Bool true) (field "requested" applied);
    let launch = obj [ "op", string "task_apply"; "preflight", string "reviewed"; "confirm", `Bool true ] in
    ignore (Client.req ~data:launch !client "commands/launch");
    ignore (cmd ~status:"rejected" "duplicate-launch" launch);
    let applescripts () = String.split_on_char '\n' (read (home / "tools.log")) |> List.filter (fun line -> contains line "osascript") |> List.length in
    equal 1 (applescripts ());
    Service.stop service; Service.start service; client := Service.client service;
    ignore (Client.req ~data:launch !client "commands/launch"); equal 1 (applescripts ());
    let task = req ("tasks/" ^ !key) in
    equal "launch-requested" (s "status" (field "worker" task)); equal "open" (s "status" task);
    let scripts = files (home / ".monty") |> List.filter (String.ends_with ~suffix:"launch.sh") in
    check (scripts <> []) "launch did not produce scripts";
    List.iter (fun path -> let text = read path in
      check (not (contains text "--codex-yolo")) "service inherited bypass";
      require text "exec env MONTY_CODEX_YOLO=false ") scripts;
    require (Task_fixture.cli ~code:1 service [ "task"; "done"; !key ]) "monty done";
    ignore (cmd ~status:"rejected" "relaunch" (request "task_preflight"
      [ "action", string "launch"; "profile", string "local"; "context", string "Again" ]));
    ignore (cmd ~status:"rejected" "wrong-account" (request "task_preflight"
      [ "action", string "resume"; "profile", string "other" ]));
    equal (`Bool true) (field "ready" (preflight ~action:"resume" "resume-preview"));
    let resumed = apply "resume" "resume-preview" in equal 0 (i "exit_code" (field "process" resumed));
    equal (s "directory" (field "worker" task)) (s "directory" (field "worker" (req ("tasks/" ^ !key)))); equal 2 (applescripts ());
    let worktree = String.trim (cli [ "task"; "workspace"; "ensure"; !key; "--repo"; repo ]) in
    write (worktree / "valuable.txt") "keep me";
    let failed = cmd "dirty-complete" (request "task_complete" [ "confirm", `Bool true ]) in
    check (i "exit_code" (field "process" failed) <> 0) "dirty worktree completion succeeded";
    equal "keep me" (read (worktree / "valuable.txt")); equal "open" (s "status" (req ("tasks/" ^ !key)));
    Unix.unlink (worktree / "valuable.txt");
    let finished = cmd "complete" (request "task_complete" [ "confirm", `Bool true ]) in
    equal 0 (i "exit_code" (field "process" finished)); check (not (Sys.file_exists worktree)) "completion retained worktree";
    equal "done" (s "status" (req ("tasks/" ^ !key))); equal (`List []) (required "tasks" (req "inventory"));
    equal !key (s "key" (List.hd (xs "tasks" (req "inventory?scope=all"))));
    equal (`Bool true) (field "ready" (preflight ~action:"resume" "reopen-preview"));
    let reopened = apply "reopen" "reopen-preview" in equal 0 (i "exit_code" (field "process" reopened));
    equal "open" (s "status" (req ("tasks/" ^ !key)));
    let failed_task = cmd "create-failed" (obj [ "op", string "task_create"; "project", string "repo"; "title", string "Terminal failure" ]) in
    key := s "key" failed_task;
    equal (`Bool true) (field "ready" (preflight "failed-preview"));
    write (home / "fail-terminal") "";
    let failed = apply "failed-launch" "failed-preview" in
    check (i "exit_code" (field "process" failed) <> 0) "terminal failure lost process outcome";
    let process = field "process" failed in require (String.lowercase_ascii (s "stdout" process ^ s "stderr" process)) "recover";
    equal "open" (s "status" (req ("tasks/" ^ !key)));
    let rex_state = home / "rex-state.json" and rex_log = home / "rex.log" in
    let rex = home / "fake-bin/rex" in
    write rex ("#!/bin/sh\nREX_FAKE_STATE=" ^ Monty.Shell.quote rex_state
      ^ " REX_FAKE_LOG=" ^ Monty.Shell.quote rex_log ^ " exec "
      ^ Monty.Shell.quote (Unix.realpath Sys.executable_name) ^ " --fixture-rex \"$@\"\n");
    Unix.chmod rex 0o700;
    let before_applescripts = applescripts () in ignore (cli [ "settings"; "set"; "terminal"; "rex" ]);
    let rex_task = cmd "create-rex" (obj [ "op", string "task_create"; "project", string "repo"; "title", string "Rex API launch" ]) in
    key := s "key" rex_task;
    let preview = preflight "rex-preview" in equal (`Bool true) (field "ready" preview); equal "rex" (s "terminal" preview);
    let rex_calls () = if not (Sys.file_exists rex_log) then [] else
      String.split_on_char '\n' (read rex_log) |> List.filter ((<>) "")
      |> List.map (fun line -> Yojson.Safe.from_string line |> array |> List.map (function `String value -> value | _ -> failwith "expected argv string")) in
    check (not (List.exists (List.mem "run") (rex_calls ()))) "Rex preflight created a terminal";
    ignore (cli [ "settings"; "set"; "terminal"; "ghostty" ]);
    ignore (cmd ~status:"rejected" "rex-stale" (obj [ "op", string "task_apply"; "preflight", string "rex-preview"; "confirm", `Bool true ]));
    ignore (cli [ "settings"; "set"; "terminal"; "rex" ]);
    equal "rex" (s "terminal" (preflight "rex-reviewed"));
    let applied = apply "rex-launch" "rex-reviewed" in equal 0 (i "exit_code" (field "process" applied)); equal "rex" (s "terminal" applied);
    List.iter (fun command -> equal 1 (List.length (List.filter (List.mem command) (rex_calls ())))) [ "run"; "new" ];
    equal before_applescripts (applescripts ());
    let worker = s "directory" (field "worker" (req ("tasks/" ^ !key))) in
    equal (s "session_id" (Yojson.Safe.from_file (home / ".monty/rex-session.json")))
      (s "session_id" (Yojson.Safe.from_file (worker / "rex-terminal.json")));
    equal "open" (s "status" (req ("tasks/" ^ !key)));
    ignore (cli [ "settings"; "set"; "terminal"; "ghostty" ]);
    print_endline "Task API: create, preflight, receipts, launch/resume, dirty completion, reopen/recovery and Rex selection passed"))

let () = match Array.to_list Sys.argv with
  | _ :: "--fixture-rex" :: args -> Fake_rex.run args
  | _ -> suite ()
