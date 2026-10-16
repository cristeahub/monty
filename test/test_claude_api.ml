open Test_support
let json=Yojson.Safe.to_string
let first value=List.hd(xs "sessions" value)
let last values=List.hd(List.rev values)
let fixture=Unix.realpath Sys.argv.(2)
let json_lines path=if Sys.file_exists path then read path |> String.split_on_char '\n' |> List.filter((<>) "") |> List.map Yojson.Safe.from_string else []
let () = with_temp_dir ~prefix:"monty-claude-" (fun home ->
  let repo=home / "repo" and second_repo=home / "other" in
  List.iter(fun directory->mkdir directory;equal 0(run "git"["init";"-q";directory]).code)[repo;second_repo];
  copy_tree (Filename.dirname bin / ".." / "agent-profiles") (home / "agent-profiles");
  let account=home / "claude" and other=home / "other-account" in mkdir account;mkdir other;
  let python=sdk_python home in let cli=home / "claude-cli" in
  write cli ("#!/bin/sh\nexec " ^ Monty.Shell.quote fixture ^ " \"$@\"\n");Unix.chmod cli 0o700;
  let config=home / "profiles.json" in
  let profile name directory=obj["id",string name;"harness",string "claude";"command",string cli;"python",string python;
    "claude_home",string directory;"roots",`List[string repo;string second_repo]] in
  write config(json(obj["profiles",`List[profile "claude" account;profile "other" other]]));
  let service=Service.create home config in
  Fun.protect ~finally:(fun ()->Service.close service)(fun ()->
    let c ()=Service.client service in let req path=Client.req(c ())path in
    let cmd ?status key request=Client.cmd ?status (c ())key request in
    let revision cid=Client.revision(c ())cid in
    let conversation cid=req("conversations/" ^ cid) in
    let items cid=Client.history(c ())cid |> xs "sessions" |> List.concat_map(xs "records") |> List.concat_map(xs "items") in
    let calls ()=json_lines(account / "calls.jsonl") in
    let starts ()=List.filter(fun row->field "start" row<>`Null)(calls ()) in
    let context=obj["workspaces",`List[string repo;string second_repo];"instructions",string "Monty trusted instructions and standalone context"] in
    check(List.mem(string "claude")(xs "real_adapters"(req "health"))) "Claude adapter missing";
    http_error 403(fun ()->Client.req ~data:(obj["execution",string "guessed";"nonce",string "guessed";"parent_pid",`Int(Unix.getpid ())])(c ())"supervisors/authorize");
    let forbidden=obj["op",string "start";"profile",string "claude";"context",put "environment"(obj["MONTY_CLAUDE_PRIVATE_KEY",string "must-not-be-persisted"])context] in
    http_error 400(fun ()->Client.req ~data:forbidden(c ())"commands/credential-environment");http_error 404(fun ()->req "commands/credential-environment");
    let invalid_environment key value path=
      http_error 400(fun ()->Client.req ~data:(obj["op",string "start";"profile",string "claude";"context",put "environment"(obj[key,string value])context])(c ())path);
      http_error 404(fun ()->req path) in
    invalid_environment "MONTY_HOME" repo "commands/foreign-home";
    List.iter(fun key->invalid_environment key "/bin/sh -c false"("commands/forbidden-command-" ^ key))
      ["MONTY_WT_COMMAND";"MONTY_CODEX_YOLO";"MONTY_PI_COMMAND";"MONTY_CODEX_COMMAND";"MONTY_CLAUDE_COMMAND";"MONTY_API_PROFILE"];
    let overridden=cmd ~status:"rejected" "worktree-override"(obj["op",string "headless_start";"profile",string "claude";"worker",string "absent";"wt_command",string "/bin/sh -c false"]) in
    equal 409(i "code" overridden);check(Monty.Home.contains_substring(s "message" overridden)"trusted server command") "untrusted wt command accepted";
    let policy=cmd ~status:"rejected" "invalid-headless-policy"(obj["op",string "headless_start";"profile",string "claude";"worker",string "absent";"codex_yolo",string "true"]) in
    equal 400(i "code" policy);check(Monty.Home.contains_substring(s "message" policy)"boolean") "invalid bypass policy accepted";
    let rec json_files path=if Sys.is_directory path then Sys.readdir path |> Array.to_list |> List.concat_map(fun name->json_files(path / name)) else if Filename.check_suffix path ".json" then [path] else [] in
    check(not(List.exists(fun path->Monty.Home.contains_substring(read path)"must-not-be-persisted")(json_files(home / ".monty")))) "credential persisted before rejection";
    let request=obj["op",string "start";"profile",string "claude";"context",context] in
    let chats=Client.parallel_cmd(c ())"start-once" request ~count:4 in
    equal 1(List.map(s "id")chats |> List.sort_uniq String.compare |> List.length);
    let chat=List.hd chats in let cid=s "id" chat in let sess=first chat in let native=s "native_id" sess in
    equal 1(List.length(starts ()));
    let started=List.hd(starts ()) in check(i "pid" started<>Service.pid service && i "ppid" started<>Unix.getpid ()) "SDK process not independently owned";
    equal[string second_repo](xs "add_dirs" started);check(not(Sys.file_exists(other / "calls.jsonl"))) "wrong Claude account started";
    check(Sys.file_exists(account / "skills/monty/SKILL.md")) "Claude skill missing";
    let rejected=cmd ~status:"rejected" "purge-owned-rpc"(obj["op",string "purge";"conversation",string cid;"confirm",`Bool true]) in
    equal 409(i "code" rejected);check(Monty.Home.contains_substring(s "message" rejected)"owned runtimes") "owned Claude purge accepted";
    equal native(s "native_id"(first(conversation cid)));
    ignore(cmd "send-once"(obj["op",string "send";"conversation",string cid;"selected_session",field "id" sess;"text",string "approval"]));
    let pending ()=until ~predicate:(fun rows->rows<>[])(fun ()->xs "pending"(first(conversation cid))) |> List.hd in
    let pending_request=pending () in
    ignore(cmd "detach-busy"(obj["op",string "detach";"conversation",string cid;"revision",revision cid]));equal "running"(s "runtime"(first(conversation cid)));
    let answer=obj["op",string "respond";"conversation",string cid;"pending_id",field "id" pending_request;"pending_revision",`Int 1;"response",obj["decision",string "deny"]] in
    ignore(cmd ~status:"rejected" "invalid-answer"(put "response"(obj["decision",string "bypass"])answer));
    ignore(Client.parallel_req(c ())["commands/answer-0",answer;"commands/answer-1",answer]);
    wait(fun ()->List.map(fun key->s "status"(req("commands/answer-" ^ key)))["0";"1"] |> List.sort String.compare = ["finished";"rejected"]);
    wait(fun ()->List.exists(fun row->field "kind" row=string "agentMessage")(items cid));
    let idle ()=wait(fun ()->s "runtime"(first(conversation cid))="idle") in idle ();
    ignore(cmd "question"(obj["op",string "send";"conversation",string cid;"selected_session",field "id" sess;"text",string "question"]));
    let pending_request=pending () in
    ignore(cmd "answer-question"(obj["op",string "respond";"conversation",string cid;"pending_id",field "id" pending_request;"pending_revision",`Int 1;
      "response",obj["decision",string "allow";"answers",obj["Choose a color",string "Blue"]]]));idle ();
    equal native(s "native_id"(List.hd(xs "data"(req "discovery?profile=claude"))));equal[](xs "data"(req "discovery?profile=other"));
    ignore(cmd "hold"(obj["op",string "send";"conversation",string cid;"selected_session",field "id" sess;"text",string "hold"]));
    ignore(cmd "interrupt"(obj["op",string "interrupt";"conversation",string cid;"selected_session",field "id" sess]));idle ();
    let count=List.length(starts ()) in Service.stop service;Service.start service;pause 0.2;equal count(List.length(starts ()));
    ignore(cmd ~status:"rejected" "stale-answer" answer);
    let resume key=ignore(cmd key(obj["op",string "resume";"conversation",string cid;"revision",revision cid;"detached",`Bool true])) in
    resume "resume-exact";equal native(s "start"(last(starts ())));
    let original_config=read config in let parsed=Yojson.Safe.from_string original_config in
    let profiles=xs "profiles" parsed in Service.stop service;
    write config(json(put "profiles"(`List(put "id"(string "renamed")(List.hd profiles)::List.tl profiles))parsed));Service.start service;
    resume "resume-renamed";equal native(s "start"(last(starts ())));equal "claude"(s "profile"(first(conversation cid)));
    Service.stop service;write config original_config;Service.start service;
    ignore(cmd ~status:"rejected" "bad-root"(obj["op",string "start";"profile",string "claude";"context",obj["workspaces",`List[string home]]]));
    ignore(cmd ~status:"rejected" "absent"(obj["op",string "import";"profile",string "claude";"native_id",string "12345678-1234-4234-8234-123456789abc";"context",context]));
    let invalid=cmd ~status:"rejected" "invalid-native"(obj["op",string "import";"profile",string "claude";"native_id",string "../other-account";"context",context]) in
    equal 400(i "code" invalid);check(Monty.Home.contains_substring(s "message" invalid)"UUID") "invalid native UUID accepted";
    let environment=merge_env service.env["CLAUDE_CONFIG_DIR",account] in
    let terminal_file=account / "terminal.json" in
    let terminal_record predicate=until ~predicate (fun ()->if Sys.file_exists terminal_file then Yojson.Safe.from_file terminal_file else `Null) in
    let terminal_run ?(environment=environment) ?(args=[]) ()=
      let result=run ~cwd:repo ~env:environment ~input:"" ~timeout:15. bin(["terminal";"--harness";"claude";"--home";home] @ args) in
      check(result.code=0)result.stderr in
    let find_chat native=match req "conversations" with `List chats->List.find(fun chat->List.exists(fun sess->field "native_id" sess=string native)(xs "sessions" chat))chats|_->assert false in
    terminal_run ();let record=terminal_record(fun row->row<>`Null) in
    check(field "tty" record=`Bool true && i "pid" record<>Service.pid service) "terminal not server-owned PTY";
    let chat=find_chat(s "native" record) in let sid=s "id"(first chat) and cid=s "id" chat in Unix.kill(i "pid" record)0;
    let rejected=cmd ~status:"rejected" "purge-live-terminal"(obj["op",string "purge";"conversation",string cid;"confirm",`Bool true]) in
    equal 409(i "code" rejected);equal(`Bool true)(field "running"(req("terminals/" ^ sid)));
    let terminal_command key op cid sid fields=cmd key(obj(["op",string op;"conversation",string cid;"selected_session",string sid] @ fields)) in
    let input key cid sid text=ignore(terminal_command key "terminal_input" cid sid["data",string(Base64.encode_exn text)]) in
    let exited sid=wait(fun ()->field "running"(req("terminals/" ^ sid))=`Bool false) in
    ignore(terminal_command "resize" "terminal_resize" cid sid["rows",`Int 37;"cols",`Int 99]);input "input" cid sid "hello\n";
    equal(string "[terminal input omitted]")(field "data"(field "request"(req "commands/input")));
    let out=until ~predicate:(fun value->Monty.Home.contains_substring(Base64.decode_exn(s "data" value))"RECEIVED hello")(fun ()->req("terminals/" ^ sid ^ "?offset=0")) in equal(`Bool true)(field "running" out);
    input "exit" cid sid "/exit\n";exited sid;
    ignore(cmd "purge-ended-terminal"(obj["op",string "purge";"conversation",string cid;"confirm",`Bool true]));http_error 410(fun ()->Client.history(c ())cid);
    let task_context=home / "context.md" in write task_context "Bounded native worker fixture.\n";
    let fake_bin=home / "fake-bin" in mkdir fake_bin;
    List.iter(fun name->write(fake_bin / name)"#!/bin/sh\nexit 0\n";Unix.chmod(fake_bin / name)0o700)["ghostty";"osascript"];
    let worker_env=merge_env environment["PATH",fake_bin ^ ":" ^ List.assoc "PATH" environment] in
    List.iter(fun args->let result=run ~cwd:repo ~env:worker_env bin(args @ ["--home";home])in check(result.code=0)result.stderr)
      [["projects";"add";"--repo";repo];["launch";"--repo";repo;"--title";"Native worker";"--branch";"cto/native-worker";"--context";task_context;"--worktree";"never";"--terminal";"ghostty";"--harness";"claude";"--claude-command";cli]];
    let worker_dir=home / ".monty/runs/manual/workers/native-worker" in
    let worker_env=merge_env worker_env["MONTY_JOB_ID","native-worker";"MONTY_WORKER_DIR",worker_dir;"MONTY_JOB_FILE",worker_dir / "job.json";
      "MONTY_WORKSPACE_1","/stale/other-task";"MONTY_WORKSPACE_2","/stale/extra-task";"MONTY_JOB_WORKTREE","/stale/other-task"] in
    let worker_args=["--worker";"--instructions";worker_dir / "MONTY.md";"--context";task_context] in
    terminal_run ~environment:worker_env ~args:worker_args ();
    let record=terminal_record(fun row->field "native" row<>field "native" record)in let native=s "native" record in
    equal native(String.trim(read(worker_dir / "claude-session-id")));
    let chat=find_chat native in let worker_context=field "context"(first chat) in
    equal[string repo](xs "workspaces" worker_context);equal(string repo)(field "MONTY_WORKSPACE_1"(field "environment" worker_context));equal `Null(field "MONTY_WORKSPACE_2"(field "environment" worker_context));
    let duplicate=cmd "duplicate-worker"(obj["op",string "start";"profile",string "claude";"terminal",`Bool true;"context",worker_context]) in
    let rejected=cmd ~status:"rejected" "duplicate-worker-open"(obj["op",string "terminal_open";"conversation",field "id" duplicate;"selected_session",field "id"(first duplicate);"revision",field "revision" duplicate;"fresh",`Bool true]) in
    equal 409(i "code" rejected);check(Monty.Home.contains_substring(s "message" rejected)"busy") "duplicate worker terminal accepted";
    equal native(String.trim(read(worker_dir / "claude-session-id")));equal native(s "native"(Yojson.Safe.from_file terminal_file));
    let cid=s "id" chat and sid=s "id"(first chat) in input "worker-exit" cid sid "/exit\n";exited sid;
    Service.stop service;Service.start service;
    let retained=req("terminals/" ^ sid) in equal(`Bool false)(field "running" retained);equal(`Int 0)(field "code" retained);
    check(Monty.Home.contains_substring(Base64.decode_exn(s "data" retained))("NATIVE CLAUDE " ^ native)) "terminal bytes lost on restart";
    let rex_env=merge_env worker_env["REX_SESSION","session:01a1-caller";"REX_BLOCK","block:01a1-first";"REX_SERVER","unix:///fake/rex/server.sock";"REX_CLIENT","client:01a1-app"] in
    let attached environment predicate=
      terminal_run ~environment ~args:worker_args ();let record=terminal_record predicate in let chat=find_chat(s "native" record) in
      let bindings=match field "environment"(field "context"(first chat))with `Assoc fields->fields|_->assert false in
      check(not(List.exists(fun (key,_)->String.starts_with ~prefix:"REX_" key)bindings)) "Rex attachment persisted in session";
      let cid=s "id" chat and sid=s "id"(first chat) in input("rex-exit-" ^ s "native" record)cid sid "/exit\n";exited sid;record in
    let record=attached rex_env(fun row->field "rex" row<>`Null && field "rex" row<>obj[]) in
    let rex=match field "rex" record with `Assoc fields->List.sort compare fields|_->assert false in
    equal(List.sort compare ["REX_SESSION",string "session:01a1-caller";"REX_BLOCK",string "block:01a1-first";"REX_SERVER",string "unix:///fake/rex/server.sock";"REX_CLIENT",string "client:01a1-app"])rex;
    let record=attached(merge_env rex_env["REX_BLOCK","block:01a1-second";"REX_CLIENT","junk"])(fun row->field "REX_BLOCK"(field "rex" row)=string "block:01a1-second") in
    equal `Null(field "REX_CLIENT"(field "rex" record));equal(s "native" record)(String.trim(read(worker_dir / "claude-session-id")));
    List.iter(fun (name,bindings)->let rejected=cmd ~status:"rejected" name(obj["op",string "terminal_open";"conversation",string cid;"selected_session",string sid;"revision",revision cid;"fresh",`Bool false;"terminal_environment",bindings])in
      equal 400(i "code" rejected);check(Monty.Home.contains_substring(s "message" rejected)"terminal environment") "invalid terminal environment accepted")
      ["rex-bad-id",obj["REX_BLOCK",string "nope"];"rex-foreign",obj["PATH",string "/tmp"];"rex-control",obj["REX_SESSION",string "session:a b"]];
    equal(`Bool false)(field "running"(req("terminals/" ^ sid)));
    let multi=cmd "multi-start"(obj["op",string "start";"profile",string "claude";"terminal",`Bool true;"context",context])in
    let mcid=s "id" multi and msid=s "id"(first multi)and mnative=s "native_id"(first multi)in
    ignore(cmd "multi-open"(obj["op",string "terminal_open";"conversation",string mcid;"selected_session",string msid;"revision",revision mcid;"fresh",`Bool true;"prompt",string "initial worker prompt";"rows",`Int 40;"cols",`Int 120]));
    let margs=terminal_record(fun row->field "native" row=string mnative)|>xs "args"|>List.map(function `String value->value|_->assert false)in
    equal "initial worker prompt"(last margs);
    let position name=let rec find index=function []->failwith("missing arg " ^ name)|value::rest->if value=name then index else find(index+1)rest in find 0 margs in
    equal second_repo(List.nth margs(position "--add-dir"+1));check(position "--add-dir"<position "--session-id" && position "--session-id"<position "--append-system-prompt") "Claude variadic add-dir swallowed prompt";
    input "multi-exit" mcid msid "/exit\n";exited msid;
    terminal_run ();let crashed=terminal_record(fun row->field "native" row<>string mnative)in let chat=find_chat(s "native" crashed)in let sid=s "id"(first chat)in
    Option.iter kill service.process;Option.iter(fun child->ignore(await ~timeout:5. child))service.process;Service.start service;
    wait ~seconds:10.(fun ()->try Unix.kill(i "pid" crashed)0;false with Unix.Unix_error(Unix.ESRCH,_,_)->true);
    let retained=req("terminals/" ^ sid)in equal(`Bool false)(field "running" retained);equal(string "outcome_unknown")(field "code" retained);
    check(Monty.Home.contains_substring(Base64.decode_exn(s "data" retained))("NATIVE CLAUDE " ^ s "native" crashed)) "crashed terminal bytes lost";
    equal(`Bool false)(field "terminal"(first(conversation(s "id" chat))));
    let released=cmd "purge-candidate-start" request in let released_id=s "id" released in
    ignore(cmd "purge-candidate-release"(obj["op",string "release_runtime";"conversation",string released_id;"revision",revision released_id]));
    ignore(cmd "purge-released-rpc"(obj["op",string "purge";"conversation",string released_id;"confirm",`Bool true]));http_error 410(fun ()->Client.history(c ())released_id);
    let queries=List.filter(fun row->field "prompt" row<>`Null)(calls ())in
    check(queries<>[]) "SDK fixture observed no queries";
    List.iter(fun row->equal ~label:"SDK query selected a different connected session"
      (field "session" row)(field "requested_session" row))queries;
    print_endline "Claude API: scoped SDK, ordered workspaces, receipts, approvals/questions, detach, exact resume, restart and server-owned PTY passed"))
