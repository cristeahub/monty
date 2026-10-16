open Test_support

let contains = Monty.Home.contains_substring
let require text needle = check (contains text needle) ("missing " ^ needle ^ " in " ^ text)
let json path = Yojson.Safe.from_file path
let json_list = function `List values -> values | _ -> failwith "expected array"
let string_value = function `String value -> value | _ -> failwith "expected string"
let required key = function `Assoc values -> List.assoc key values | _ -> failwith "expected object"
let last values = List.hd (List.rev values)
let option args name = Fake_rex.option args name ""
let strings value = List.map string_value (json_list value)
let doctor_line status key text =
  String.split_on_char '\n' text |> List.exists (fun line ->
    match String.split_on_char ' ' line |> List.filter ((<>) "") with
    | found :: label :: _ -> found = status && label = key
    | _ -> false)
let setting_line key value text =
  String.split_on_char '\n' text |> List.exists (fun line ->
    match String.split_on_char ' ' line |> List.filter ((<>) "") with
    | found :: setting :: _ -> found = key && setting = value
    | _ -> false)

let suite () = with_temp_dir ~prefix:"monty-rex-" (fun home ->
  let repo = home / "repo with ' quotes æ" in
  mkdir repo;
  let base = List.filter (fun (key, _) ->
      not (List.mem key [ "MANTLE_CONTEXT"; "_MANTLE_SELECTED_HOME"; "MONTY_AGENT_PROFILE";
                         "MONTY_TERMINAL"; "MONTY_TARGET"; "REX_SESSION"; "REX_BLOCK";
                         "REX_SERVER"; "REX_CLIENT" ])) env in
  let environment = merge_env base
      [ "HOME", home; "MONTY_HOME", home; "MONTY_CODEX_YOLO", "false";
        "GIT_CONFIG_GLOBAL", "/dev/null"; "GIT_CONFIG_SYSTEM", "/dev/null";
        "GIT_CONFIG_NOSYSTEM", "1"; "GIT_CONFIG_COUNT", "0"; "GIT_TEMPLATE_DIR", "/dev/null" ] in
  let git args = let result = run ~env:environment "git" args in equal 0 result.code in
  git [ "init"; "-q"; repo ];
  git [ "-C"; repo; "-c"; "user.name=Test"; "-c"; "user.email=test@example.invalid";
        "commit"; "-qm"; "Initial"; "--allow-empty" ];
  copy_tree (Filename.dirname (Filename.dirname bin) / "agent-profiles") (home / "agent-profiles");
  let fake = home / "fake-bin" in mkdir fake;
  List.iter (fun name -> write (fake / name) "#!/bin/sh\nexit 0\n"; Unix.chmod (fake / name) 0o700)
    [ "pi"; "gh"; "sdef" ];
  write (fake / "rex") ("#!/bin/sh\nexec " ^ Monty.Shell.quote (Unix.realpath Sys.executable_name) ^ " --fixture \"$@\"\n");
  Unix.chmod (fake / "rex") 0o700;
  let log = home / "rex.log" and state_file = home / "rex-state.json" in
  let environment = merge_env environment
      [ "PATH", fake ^ ":" ^ List.assoc "PATH" environment;
        "REX_FAKE_LOG", log; "REX_FAKE_STATE", state_file ] in
  let state () = json state_file in
  let save value = write state_file (Yojson.Safe.to_string value) in
  let calls () = if not (Sys.file_exists log) then [] else
      read log |> String.split_on_char '\n' |> List.filter ((<>) "")
      |> List.map (fun row -> strings (Yojson.Safe.from_string row)) in
  let mutations () = if Sys.file_exists state_file then List.length (Fake_rex.entries "blocks" (state ())) else 0 in
  let named name = List.filter (List.mem name) (calls ()) in
  let cli ?(code = 0) ?(extra = []) args =
    let result = run ~env:(merge_env environment extra) ~new_session:true ~timeout:20. bin (args @ [ "--home"; home ]) in
    check (result.code = code) (String.concat " " args ^ "\n" ^ result.stdout ^ result.stderr);
    result.stdout ^ result.stderr
  in
  ignore (cli [ "projects"; "add"; "--repo"; repo ]);
  let project = s "id" (List.hd (xs "projects" (json (home / ".monty/projects.json")))) in
  let context = home / "context.md" in write context "# Implement Rex support\n";
  let title = "Rex ' tab $(touch BAD) \"quoted\" æøå; `touch BAD`" in
  let manifest worker =
    let added = cli [ "task"; "add"; "--project"; project; "--title"; worker ] in
    let task = String.split_on_char ' ' added |> List.map String.trim
               |> List.find (String.starts_with ~prefix:"local-") in
    let directory = home / ".monty/runs/rex-test" in mkdir directory;
    let worker_dir = directory / "workers" / worker and path = directory / (worker ^ ".json") in
    write path (Yojson.Safe.to_string (obj [ "jobs", `List [ obj [ "id", string worker;
      "title", string title; "repo", string repo; "branch", string ("test/" ^ worker);
      "context", string context; "worker_dir", string worker_dir; "task_key", string ("local:" ^ task) ] ] ]));
    path, worker_dir
  in
  let launch ?(terminal = "rex") ?(code = 0) ?(extra = []) ?(target = "tab") path =
    cli ~code ~extra [ "launch-many"; "--manifest"; path; "--terminal"; terminal;
      "--target"; target; "--harness"; "pi"; "--worktree"; "never" ] in
  let resume ?(code = 0) ?(extra = []) ?(flags = []) worker =
    cli ~code ~extra ([ "resume"; worker; "--terminal"; "rex"; "--target"; "tab";
                       "--harness"; "pi"; "--worktree"; "never" ] @ flags) in
  let reference worker = json (worker / "rex-terminal.json") in
  let job worker = json (worker / "job.json") in
  let doctor_args = [ "doctor"; "--terminal"; "rex"; "--harness"; "pi"; "--worktree"; "never" ] in
  let doctor = cli doctor_args in
  check (doctor_line "PASS" "rex" doctor && doctor_line "PASS" "rex-server" doctor) doctor;
  require doctor Fake_rex.endpoint;
  let noisy = [ "REX_FAKE_STDERR_BYTES", string_of_int (1024 * 1024) ] in
  let doctor = cli ~extra:noisy doctor_args in
  check (doctor_line "PASS" "rex-server" doctor && not (contains doctor "diagnostic")) doctor;
  require doctor Fake_rex.endpoint;
  let down = cli ~code:1 ~extra:(noisy @ [ "REX_FAKE_DOWN", "1" ]) doctor_args in
  check (doctor_line "FAIL" "rex-server" down) down;
  List.iter (require down) [ "Open the Rex app"; "connection refused"; "diagnostic" ];
  let missing = cli ~code:1 ~extra:[ "PATH", "/usr/bin:/bin" ] doctor_args in
  check (doctor_line "FAIL" "rex" missing && not (contains missing "rex-server")) missing;
  check (List.for_all (List.mem "whoami") (calls ())) "doctor mutated Rex";
  let path, worker = manifest "success" in
  if Sys.file_exists log then Unix.unlink log;
  ignore (launch ~terminal:"dry-run" path);
  check (not (Sys.file_exists log) && not (Sys.file_exists worker)) "dry-run mutated launch state";
  let seed label =
    let result = run ~env:environment (fake / "rex") [ "new"; label; "--json" ] in
    equal 0 result.code; Yojson.Safe.from_string result.stdout in
  let seeded = seed "caller" in
  let caller_session = s "session_id" seeded in
  let caller_block = field "block_ids" (List.hd (xs "initial_windows" seeded)) |> strings |> List.hd in
  let inside = [ "REX_SESSION", caller_session; "REX_BLOCK", caller_block; "REX_SERVER", Fake_rex.endpoint ] in
  Unix.unlink log;
  ignore (launch ~extra:inside path);
  let runs = named "run" in equal 1 (List.length runs); equal 2 (mutations ());
  let args = List.hd runs in
  List.iter (fun (key, value) -> equal value (option args key))
    [ "-S", Fake_rex.endpoint; "-s", caller_session; "-c", repo; "--label", title; "--shell", "none" ];
  check (List.mem "--autostart=false" args && List.mem "--keep-open" args) "Rex run flags";
  (match Fake_rex.tail args with
  | "/bin/zsh" :: "-l" :: script :: _ -> check (String.ends_with ~suffix:"monty-success-launch.sh" script) "launch script identity"
  | _ -> failwith "unexpected native terminal argv");
  check (named "do" = [] && named "client" = []) "caller-relative tab moved app view";
  equal [ caller_session ] (List.map fst (Fake_rex.entries "sessions" (state ())));
  check (not (Sys.file_exists (home / ".monty/rex-session.json"))) "inside tab made home session";
  equal "launch-requested" (s "status" (job worker));
  let ref_ = reference worker in
  List.iter (fun (key, value) -> equal value (s key ref_))
    [ "schema", "monty:rex-terminal:v1"; "session_id", caller_session; "server", Fake_rex.endpoint;
      "target", "tab"; "launch_script", s "launch_script" (job worker) ];
  check (String.starts_with ~prefix:"block:" (s "block_id" ref_)
         && String.starts_with ~prefix:"window:" (s "window_id" ref_)) "Rex reference IDs";
  equal `Null (required "client_id" ref_);
  check (List.for_all (fun (key, _) -> not (String.starts_with ~prefix:"rex" key || String.ends_with ~suffix:"_id" key))
           (Fake_rex.fields (job worker)) && not (contains (Yojson.Safe.to_string (job worker)) "block:")) "runtime IDs leaked into job";
  List.iter (fun directory -> check (not (Sys.file_exists (directory / "BAD"))) "title executed shell") [ repo; home; Sys.getcwd () ];
  equal title (s "label" (List.assoc (s "block_id" ref_) (Fake_rex.entries "blocks" (state ()))));
  ignore (launch ~extra:inside path); equal 2 (mutations ());
  let path, split_worker = manifest "split" in ignore (launch ~target:"split" ~extra:inside path);
  let split = named "split" |> List.filter (fun args -> not (List.mem "inspect" args)) |> last in
  equal caller_block (option split "-b"); equal "right" (option split "--split");
  equal (s "window" (List.assoc caller_block (Fake_rex.entries "blocks" (state ())))) (s "window_id" (reference split_worker));
  equal "split" (s "target" (reference split_worker));
  let reject_launch ?(target = "window") ?(needles = []) name extra needle =
    let path, worker = manifest name in let before = mutations () in
    let out = launch ~target ~code:1 ~extra path in List.iter (require out) (needle :: needles);
    check (not (Sys.file_exists worker) && mutations () = before) ("preflight mutated " ^ name)
  in
  reject_launch ~target:"split" "split-outside" [] "REX_SESSION and REX_BLOCK";
  let path, window_worker = manifest "window" in ignore (launch ~target:"window" ~extra:inside path);
  let args = named "new" |> last in equal ("monty: " ^ title) (option args "new"); equal title (option args "--window");
  let ref_ = reference window_worker in equal Fake_rex.app (s "client_id" ref_); equal "window" (s "target" ref_);
  let selected = xs "selected" (state ()) |> last in
  equal 3 (List.length (Fake_rex.fields selected));
  List.iter (fun (key, value) -> equal value (s key selected))
    [ "client", Fake_rex.app; "session_id", s "session_id" ref_; "window_id", s "window_id" ref_ ];
  equal ("monty: " ^ title) (s "label" (List.assoc (s "session_id" ref_) (Fake_rex.entries "sessions" (state ()))));
  List.iter (fun (name, extra, needle) -> reject_launch name (merge_env inside extra) needle)
    [ "no-app", [ "REX_FAKE_NO_APP", "1" ], "no Rex app is connected";
      "two-apps", [ "REX_FAKE_TWO_APPS", "1" ], "set REX_CLIENT";
      "no-select", [ "REX_FAKE_NO_SELECT", "1" ], "session.select";
      "down", [ "REX_FAKE_DOWN", "1" ], "not reachable" ];
  let path, worker = manifest "chosen-app" in
  ignore (launch ~target:"window" ~extra:(inside @ [ "REX_FAKE_TWO_APPS", "1"; "REX_CLIENT", Fake_rex.other_app ]) path);
  equal Fake_rex.other_app (s "client_id" (reference worker));
  let path, worker = manifest "outside-a" in ignore (launch path);
  let home_session = json (home / ".monty/rex-session.json") in
  equal "monty:rex-session:v1" (s "schema" home_session); equal Fake_rex.endpoint (s "server" home_session);
  let home_id = s "session_id" home_session in
  equal "monty" (s "label" (List.assoc home_id (Fake_rex.entries "sessions" (state ()))));
  let ref_ = reference worker in equal home_id (s "session_id" ref_); equal Fake_rex.app (s "client_id" ref_);
  equal (s "window_id" ref_) (s "window_id" (last (xs "selected" (state ()))));
  let news = List.length (named "new") in
  let path, worker = manifest "outside-b" in ignore (launch path);
  equal news (List.length (named "new")); equal home_id (s "session_id" (reference worker));
  save (Fake_rex.put "sessions" (obj (List.remove_assoc home_id (Fake_rex.entries "sessions" (state ())))) (state ()));
  let path, worker = manifest "outside-c" in ignore (launch path);
  let replaced = s "session_id" (json (home / ".monty/rex-session.json")) in
  check (replaced <> home_id && s "session_id" (reference worker) = replaced) "vanished home session was not replaced";
  equal "" (String.trim (cli [ "settings"; "get"; "rex-session" ])); require (cli [ "settings" ]) "rex-session   (monty-owned session)";
  require (cli ~code:1 [ "settings"; "set"; "rex-session"; "two words" ]) "without spaces";
  equal "rex-session = caller outside any Mantle profile" (String.trim (cli [ "settings"; "set"; "rex-session"; "caller" ]));
  equal "caller" (String.trim (cli [ "settings"; "get"; "rex-session" ])); require (cli [ "settings" ]) "rex-session   caller";
  let doctor = cli doctor_args in check (doctor_line "PASS" "rex-session" doctor) doctor; require doctor ("caller (" ^ caller_session ^ ")");
  let news = List.length (named "new") in
  let path, worker = manifest "setting-session" in ignore (launch path);
  equal news (List.length (named "new"));
  let ref_ = reference worker in equal caller_session (s "session_id" ref_); equal Fake_rex.app (s "client_id" ref_);
  equal (s "window_id" ref_) (s "window_id" (last (xs "selected" (state ()))));
  equal caller_session (s "session" (List.assoc (s "block_id" ref_) (Fake_rex.entries "blocks" (state ()))));
  ignore (cli [ "settings"; "set"; "rex-session"; caller_session ]);
  let path, worker = manifest "setting-session-id" in ignore (launch path); equal caller_session (s "session_id" (reference worker));
  ignore (cli [ "settings"; "set"; "rex-session"; "no-such-session" ]);
  check (doctor_line "FAIL" "rex-session" (cli ~code:1 doctor_args)) "dead configured session doctor";
  reject_launch ~target:"tab" ~needles:[ "set rex-session ''" ] "setting-missing" noisy "rex-session setting no-such-session is not a live session";
  let before = mutations () in
  let path, worker = manifest "setting-inside" in ignore (launch ~extra:inside path);
  equal caller_session (s "session_id" (reference worker)); equal (before + 1) (mutations ());
  require (cli [ "settings"; "set"; "rex-session"; "" ]) "cleared";
  equal "" (String.trim (cli [ "settings"; "get"; "rex-session" ]));
  let path, worker = manifest "setting-cleared" in ignore (launch path);
  equal (s "session_id" (json (home / ".monty/rex-session.json"))) (s "session_id" (reference worker));
  let work = [ "MANTLE_CONTEXT", "work"; "_MANTLE_PREV_STATE", "unset";
               "_MANTLE_SELECTED_HOME", home / "codex-home"; "CODEX_HOME", home / "codex-home" ] in
  require (cli ~extra:work [ "settings" ]) "rex-session   (session labelled work, profile work)";
  equal "" (String.trim (cli ~extra:work [ "settings"; "get"; "rex-session" ]));
  let news = List.length (named "new") in
  let path, worker = manifest "profile-a" in ignore (launch ~extra:work path);
  equal (news + 1) (List.length (named "new"));
  let profile_id = s "session_id" (json (home / ".monty/rex-session.work.json")) in
  equal "work" (s "label" (List.assoc profile_id (Fake_rex.entries "sessions" (state ()))));
  equal profile_id (s "session_id" (reference worker));
  check (s "session_id" (json (home / ".monty/rex-session.json")) <> profile_id) "profile reused plain session";
  let path, worker = manifest "profile-b" in ignore (launch ~extra:work path);
  equal (news + 1) (List.length (named "new")); equal profile_id (s "session_id" (reference worker));
  save (Fake_rex.put "sessions" (obj (List.remove_assoc profile_id (Fake_rex.entries "sessions" (state ())))) (state ()));
  let users_work = s "session_id" (seed "work") in let news = List.length (named "new") in
  let path, worker = manifest "profile-c" in ignore (launch ~extra:work path);
  equal news (List.length (named "new")); equal users_work (s "session_id" (reference worker));
  let path, worker = manifest "profile-off" in ignore (launch path);
  equal (s "session_id" (json (home / ".monty/rex-session.json"))) (s "session_id" (reference worker));
  require (cli ~extra:work [ "settings"; "set"; "rex-session"; "caller" ]) "rex-session = caller for Mantle profile work";
  let persisted = json (home / ".monty/settings.json") in
  equal (obj [ "work", string "caller" ]) (field "rex_sessions" persisted); equal `Null (required "rex_session" persisted);
  equal "caller" (String.trim (cli ~extra:work [ "settings"; "get"; "rex-session" ]));
  equal "" (String.trim (cli [ "settings"; "get"; "rex-session" ])); equal "caller" (String.trim (cli [ "settings"; "get"; "rex-session.work" ]));
  let shown = cli ~extra:work [ "settings" ] in List.iter (require shown) [ "rex-session   caller (profile work)"; "rex-session.work caller" ];
  require (cli ~code:1 [ "settings"; "get"; "rex-session.bad.name" ]) "not a Mantle profile name";
  let path, worker = manifest "profile-mapped" in ignore (launch ~extra:work path); equal caller_session (s "session_id" (reference worker));
  let doctor = cli ~extra:work doctor_args in check (doctor_line "PASS" "rex-session" doctor) doctor;
  require doctor ("caller (" ^ caller_session ^ ") for profile work");
  check (not (contains (cli doctor_args) "rex-session")) "profile leaked into plain doctor";
  require (cli [ "settings"; "set"; "rex-session.work"; "" ]) "cleared for Mantle profile work";
  equal "" (String.trim (cli ~extra:work [ "settings"; "get"; "rex-session" ])); equal (obj []) (field "rex_sessions" (json (home / ".monty/settings.json")));
  List.iter (fun (name, extra, delta) ->
    let path, worker = manifest name in let before = mutations () in
    let out = launch ~code:1 ~extra:(inside @ extra) path in
    equal "launch-requested" (s "status" (job worker));
    check (not (List.mem_assoc "launch_error" (Fake_rex.fields (job worker)))) "failed request added launch_error";
    check (not (Sys.file_exists (worker / "rex-terminal.json"))) "failed request recorded terminal";
    require out (" resume '" ^ name ^ "'"); require out "Recovery:";
    ignore (launch ~extra:inside path); equal (before + delta) (mutations ()))
    [ "invalid", [ "REX_FAKE_INVALID", "run" ], 1; "failed", [ "REX_FAKE_FAIL", "run" ], 0 ];
  let path, worker = manifest "reconnect" in ignore (launch ~extra:inside path);
  let ref_ = reference worker and before = mutations () in
  let out = resume ~extra:inside "reconnect" in require out "still live"; require out (s "block_id" ref_);
  equal before (mutations ()); equal (string (s "block_id" ref_)) (last (xs "focused" (state ())));
  equal ref_ (reference worker); equal "launch-requested" (s "status" (job worker));
  require (resume ~extra:inside ~flags:[ "--fresh" ] "reconnect") "still live"; equal before (mutations ());
  let update_block id changes =
    let state_ = state () in let block = List.assoc id (Fake_rex.entries "blocks" state_) in
    let block = List.fold_left (fun value (key, item) -> Fake_rex.put key item value) block changes in
    save (Fake_rex.put "blocks" (Fake_rex.put id block (field "blocks" state_)) state_) in
  update_block (s "block_id" ref_) [ "child", `Null; "last_exit", obj [ "exit_code", `Int 0 ] ];
  ignore (resume ~extra:inside "reconnect"); equal (before + 1) (mutations ());
  let newer = reference worker in
  check (s "block_id" newer <> s "block_id" ref_ && s "requested_at" newer >= s "requested_at" ref_) "exited terminal not replaced";
  equal caller_session (s "session_id" newer);
  save (Fake_rex.put "blocks" (obj (List.remove_assoc (s "block_id" newer) (Fake_rex.entries "blocks" (state ())))) (state ()));
  let before = mutations () in require (resume ~code:1 ~extra:(inside @ [ "REX_FAKE_DOWN", "1" ]) "reconnect") "cannot verify";
  equal before (mutations ()); equal newer (reference worker);
  ignore (resume ~extra:inside "reconnect"); equal (before + 1) (mutations ()); check (s "block_id" (reference worker) <> s "block_id" newer) "gone block retained";
  let before_calls = List.length (calls ()) in
  ignore (cli [ "resume"; "reconnect"; "--terminal"; "dry-run"; "--harness"; "pi"; "--worktree"; "never" ]);
  equal before_calls (List.length (calls ()));
  let before = mutations () in let out = cli [ "rex"; "status"; "reconnect" ] in
  List.iter (require out) [ "terminal: live (zsh running)"; "status: working app=claude-code"; s "block_id" (reference worker) ]; equal before (mutations ());
  update_block (s "block_id" (reference worker)) [ "child", `Null; "last_exit", obj [ "exit_code", `Int 3 ] ];
  require (cli [ "rex"; "status"; "reconnect" ]) "terminal: exited (3)";
  require (cli ~code:1 [ "rex"; "status"; "failed" ]) "no recorded Rex terminal";
  equal "rex" (String.trim (cli [ "settings"; "get"; "terminal" ]));
  check (setting_line "terminal" "rex" (cli [ "settings" ])) "built-in terminal setting display";
  require (cli ~code:1 [ "settings"; "set"; "terminal"; "dry-run" ]) "must be ghostty or rex";
  ignore (cli [ "settings"; "set"; "terminal"; "rex" ]); equal "rex" (String.trim (cli [ "settings"; "get"; "terminal" ]));
  check (setting_line "terminal" "rex" (cli [ "settings" ])) "persisted terminal setting display";
  let path, worker = manifest "setting-default" in let before = mutations () in
  ignore (cli ~extra:inside [ "launch-many"; "--manifest"; path; "--harness"; "pi"; "--worktree"; "never" ]);
  check (mutations () = before + 1 && Sys.file_exists (worker / "rex-terminal.json")) "persisted terminal not used";
  let path, worker = manifest "env-override" in
  ignore (cli ~extra:(inside @ [ "MONTY_TERMINAL", "dry-run" ]) [ "launch-many"; "--manifest"; path; "--harness"; "pi"; "--worktree"; "never" ]);
  check (not (Sys.file_exists worker) && mutations () = before + 1) "environment terminal override mutated state";
  ignore (cli [ "settings"; "set"; "terminal"; "ghostty" ]);
  print_endline "Rex launch, placement, reconnect, stderr pressure and recovery checks passed")

let () = match Array.to_list Sys.argv with
  | _ :: "--fixture" :: args -> Fake_rex.run args
  | _ -> suite ()
