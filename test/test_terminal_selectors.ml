open Test_support
let fixture = Unix.realpath Sys.argv.(2)
let json = Yojson.Safe.to_string
let lines path = if Sys.file_exists path then read path |> String.split_on_char '\n' |> List.filter((<>) "") |> List.map Yojson.Safe.from_string else []
let first json = List.hd(xs "sessions" json)
let () = with_temp_dir ~prefix:"monty-terminal-selectors-" (fun home ->
  let repos=[home / "repo";home / "other-repo"] in
  List.iter(fun repo->mkdir repo;equal 0 (run "git" ["init";"-q";repo]).code) repos;
  let account=home / "codex" in mkdir account;
  let native=Native.create (home / "native.sock") in
  let log=home / "terminals.jsonl" in
  write (account / "fixture.json") (json(obj["socket",string(home / "native.sock");"terminal_log",string log]));
  let codex=home / "fake-codex" in
  write codex ("#!/bin/sh\nexec " ^ Monty.Shell.quote fixture ^ " \"$@\"\n");Unix.chmod codex 0o700;
  let config=home / "profiles.json" in
  write config(json(obj["profiles",`List[obj["id",string "default";"harness",string "codex";"command",string codex;"codex_home",string account;"roots",`List[string home]]]]));
  let service=Service.create home config in
  Fun.protect ~finally:(fun ()->Service.close service;Native.close native) (fun ()->
    let c=Service.client service in
    let environment=merge_env service.env ["MONTY_CODEX_COMMAND",codex;"MONTY_CODEX_YOLO","false"] in
    let cli ?(environment=environment) ?(cwd=home) ?(code=0) args =
      let result=run ~cwd ~env:environment ~input:"" ~timeout:20. bin (args @ ["--home";home]) in
      check (result.code=code) (String.concat " " args ^ "\n" ^ result.stdout ^ result.stderr);result in
    let start name workspaces bindings =
      Client.cmd c ("start-" ^ name) (obj["op",string "start";"profile",string "default";"terminal",`Bool true;
        "title",string name;"context",obj["workspaces",`List(List.map string workspaces);"environment",`Assoc(List.map(fun (key,value)->key,string value)bindings)]]) |> first |> s "native_id" in
    let attach ?(environment=environment) ?(cwd=home) args selected =
      let before=List.length(lines log) in ignore(cli ~environment ~cwd args);
      let rows=until ~predicate:(fun rows->List.length rows>before) (fun ()->lines log) in
      let row=List.hd(List.rev rows) in equal selected (s "native" row);
      let sess=Client.req c "conversations" |> function `List chats ->
        List.concat_map(xs "sessions") chats |> List.find(fun sess->s "native_id" sess=selected) | _->assert false in
      wait(fun ()->field "running" (Client.req c ("terminals/" ^ s "id" sess))=`Bool false);row in
    let rejected ?(environment=environment) args =
      let before=lines log in let result=cli ~environment ~code:1 args in
      check(Monty.Home.contains_substring result.stderr "no registered session") result.stderr;
      equal before (lines log) in
    let worker name =
      let directory=home / ".monty/runs/selectors/workers" / name in mkdir directory;
      let context=home / (name ^ ".md") in write context "Isolated native selector fixture.\n";
      let job=obj["id",string name;"title",string name;"repo",string(List.hd repos);"branch",string("test/" ^ name);
        "status",string "launch-requested";"worker_dir",string directory;"run_dir",string(Filename.dirname(Filename.dirname directory));
        "context",string context;"worktree_mode",string "never";"last_known_worktree",string(List.hd repos);
        "workspaces",`List(List.map(fun repo->obj["repo",string repo;"branch",string("test/" ^ name);"worktree",string repo])repos)] in
      write(directory / "job.json") (json job);
      directory,["MONTY_JOB_ID",name;"MONTY_WORKER_DIR",directory;"MONTY_JOB_FILE",directory / "job.json"] in
    ignore(cli["settings";"set";"harness";"codex"]);
    let worker_a,a=worker "worker-a" and worker_b,b=worker "worker-b" in
    let worker_env=merge_env environment a in
    let native_b=start "worker-b" repos b in
    let sidecar_b=worker_b / "codex-session-id" in let original_b=read sidecar_b in
    check(not(Sys.file_exists(worker_a / "codex-session-id"))) "A sidecar exists before its session";
    rejected ~environment:worker_env ["terminal";"--worker";"--mode";"picker"];
    write(worker_a / "codex-session-id") native_b;
    rejected ~environment:worker_env ["terminal";"--worker";"--mode";"exact";"--session";native_b];
    equal native_b (read(worker_a / "codex-session-id"));rejected ["continue";native_b];
    let native_a=start "worker-a" repos a in Unix.unlink(worker_a / "codex-session-id");
    let row=attach ~environment:worker_env ["terminal";"--worker";"--mode";"picker"] native_a in
    equal "worker-a" (s "worker" row);equal(List.hd repos)(s "cwd" row);
    equal native_a(read(worker_a / "codex-session-id"));
    ignore(attach ~environment:worker_env ["terminal";"--worker";"--mode";"exact";"--session";native_a] native_a);
    equal original_b(read sidecar_b);
    let path=worker_a / "job.json" in let original=read path in
    let changed=Yojson.Safe.from_string original in let reversed=List.rev(xs "workspaces" changed) in
    write path (json(puts["workspaces",`List reversed;"repo",field "repo" (List.hd reversed);"last_known_worktree",field "worktree" (List.hd reversed)]changed));
    rejected ~environment:worker_env ["terminal";"--worker";"--mode";"exact";"--session";native_a];write path original;
    let head_butler=start "head-butler" [home] [] in
    let unrelated=start "unrelated" [List.nth repos 1] [] in
    ignore(start "newer-worker" repos b);
    let task_free=start "task-free-in-worker-repo" repos [] in
    rejected ~environment:worker_env ["terminal";"--worker";"--mode";"exact";"--session";task_free];
    let jobs=List.map(fun path->path,read path)[worker_a / "job.json";worker_b / "job.json"] in
    let sidecar=read(worker_a / "codex-session-id") in
    ignore(attach ["continue";"--last"] head_butler);ignore(attach ["continue"] head_butler);
    ignore(attach ["continue";unrelated] unrelated);
    List.iter(fun (path,text)->equal text(read path))jobs;equal sidecar(read(worker_a / "codex-session-id"));
    check(not(Sys.file_exists(home / ".monty/tasks.local.json"))) "continuation created tasks";
    check(not(Sys.file_exists(home / ".monty/handoffs"))) "continuation created handoffs";
    let help=(cli["continue";"--help=plain"]).stdout |> String.split_on_char '\n' |> String.concat " " in
    check(Monty.Home.contains_substring help "registered native session ID") help;
    check(not(Monty.Home.contains_substring help "session name"))help;
    print_endline "Terminal selectors: worker identity, sidecars, ordered workspaces and task-free continuation boundaries passed"))
