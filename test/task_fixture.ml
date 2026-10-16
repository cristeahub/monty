open Test_support

let cli ?(code = 0) (service : Service.t) args =
  let result = run ~env:service.env bin (args @ [ "--home"; service.home ]) in
  check (result.code = code) (String.concat " " args ^ "\n" ^ result.stdout ^ result.stderr);
  result.stdout ^ result.stderr

let create home =
  let repo = home / "repo" in mkdir repo;
  List.iter (fun args -> equal 0 (run "git" args).code)
    [ [ "init"; "-q"; repo ]; [ "-C"; repo; "-c"; "user.name=Test";
        "-c"; "user.email=test@example.invalid"; "commit"; "-qm"; "Initial"; "--allow-empty" ] ];
  copy_tree (Filename.dirname (Filename.dirname bin) / "agent-profiles") (home / "agent-profiles");
  let fake = home / "fake-bin" in mkdir fake;
  let actor = Filename.dirname Sys.executable_name / "fake_task_tools.exe" |> Unix.realpath in
  List.iter (fun name ->
    let path = fake / name in
    write path ("#!/bin/sh\nexec " ^ Monty.Shell.quote actor ^ " tool " ^ Monty.Shell.quote home
      ^ " " ^ Monty.Shell.quote name ^ " \"$@\"\n");
    Unix.chmod path 0o700) [ "codex"; "pi"; "ghostty"; "osascript"; "sdef"; "gh" ];
  write (fake / "wt") ("#!/bin/sh\nexec " ^ Monty.Shell.quote actor ^ " wt \"$@\"\n");
  Unix.chmod (fake / "wt") 0o700;
  List.iter (fun name -> mkdir (home / name)) [ "codex"; "other-codex" ];
  let profile id directory = obj [ "id", string id; "harness", string "codex";
    "command", string (fake / "codex"); "codex_home", string (home / directory);
    "roots", `List [ string repo ] ] in
  let config = home / "profiles.json" in
  write config (Yojson.Safe.to_string (obj [ "profiles", `List [ profile "local" "codex"; profile "other" "other-codex" ] ]));
  let service = Service.create ~extra:
      [ "PATH", fake ^ ":" ^ List.assoc "PATH" env; "MONTY_CODEX_YOLO", "true";
        "MONTY_BRANCH_PREFIX", "testprefix" ] home config in
  ignore (cli service [ "projects"; "add"; "--repo"; repo ]);
  ignore (cli service [ "settings"; "set"; "terminal"; "ghostty" ]);
  service, repo
