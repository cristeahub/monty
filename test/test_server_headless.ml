open Test_support
let json=Yojson.Safe.to_string
let fixture=Unix.realpath Sys.argv.(2)
let json_lines path=if Sys.file_exists path then read path |> String.split_on_char '\n' |> List.filter((<>) "") |> List.map Yojson.Safe.from_string else []
let () = with_temp_dir ~prefix:"monty-server-headless-" (fun home ->
  let bootstrap,repo=Task_fixture.create home in Service.close bootstrap;
  let account=home / "claude" in mkdir account;
  let actor=home / "native-agent" in write actor("#!/bin/sh\nexec " ^ Monty.Shell.quote fixture ^ " \"$@\"\n");Unix.chmod actor 0o700;
  let python=match Monty.Process.command_exists_with_arguments "python3"with Ok path->path|Error message->failwith message in
  let config=home / "profiles.json" in
  write config(json(obj["profiles",`List[
    obj["id",string "claude";"harness",string "claude";"command",string actor;"python",string python;"claude_home",string account;"roots",`List[string home]];
    obj["id",string "codex";"harness",string "codex";"command",string actor;"codex_home",string(home / "codex");"roots",`List[string home];"auth_env",obj["OPENAI_API_KEY",string "SERVER_NATIVE_KEY"]]] ]));
  let environment=merge_env bootstrap.env["CLAUDE_CONFIG_DIR",account;"MONTY_CLAUDE_COMMAND",actor;"MONTY_CODEX_COMMAND",actor;
    "MONTY_WT_COMMAND",home / "fake-bin/wt";"SERVER_NATIVE_KEY","scoped-test-key"] in
  let service=Service.create ~extra:environment home config in
  Fun.protect ~finally:(fun ()->Service.close service)(fun ()->
    let c=Service.client service in
    let context=home / "context.md" in write context "Implement the isolated fixture. No remote actions.\n";
    let run_dir=home / ".monty/runs/headless-check" in mkdir run_dir;
    let manifest=run_dir / "jobs.json" in
    let job name profile=obj["id",string name;"title",string name;"branch",string("cto/" ^ name);"repo",string repo;"context",string context;"agent_profile",string profile] in
    let jobs=[job "wait-a" "reviewed";job "wait-b" "reviewed";job "policy" "solo";job "successor" "solo"]in
    let manifest_write path jobs=write path(json(obj["jobs",`List jobs]))in manifest_write manifest jobs;
    let cli ?(overrides=[]) args=
      let result=run ~cwd:repo ~env:(merge_env environment overrides)~timeout:60. bin(args @ ["--home";home])in
      check(result.code=0)(result.stdout ^ result.stderr);Yojson.Safe.from_string result.stdout in
    let phases ()=json_lines(home / "phases.jsonl")in
    let unsafe=run_dir / "unsafe.json"in manifest_write unsafe[puts["id",string "unsafe";"branch",string "cto/unsafe"](List.hd jobs)];
    let denied=run ~env:environment bin["headless";"prepare-many";"--manifest";unsafe;"--harness";"claude";"--claude-command";actor ^ " --mcp-config '{}'";"--home";home]in
    check(denied.code<>0 && Monty.Home.contains_substring denied.stderr "reviewer command cannot include")(denied.stdout ^ denied.stderr);
    check(not(Sys.file_exists(run_dir / "workers/unsafe")))"unsafe reviewer command reserved worker";
    ignore(cli["headless";"prepare-many";"--manifest";manifest;"--harness";"claude";"--claude-command";actor]);
    equal 0(run ~env:environment bin["settings";"set";"codex-yolo";"true";"--home";home]).code;
    ignore(Client.cmd c "policy"(obj["op",string "headless_start";"profile",string "codex";"worker",string "policy";"codex_yolo",`Bool false]));
    let policy=List.find(fun row->field "worker" row=string "policy")(phases ())in
    equal "false"(s "yolo" policy);equal(`Bool true)(field "scoped_auth" policy);
    check(not(List.mem(string "--dangerously-bypass-approvals-and-sandbox")(xs "args" policy)))"API policy inherited bypass";
    let subset=run_dir / "batch.json"in manifest_write subset[List.nth jobs 0;List.nth jobs 1];
    let result=cli ~overrides:["MONTY_CODEX_YOLO","false"]["headless";"run-many";"--wait";"--manifest";subset;"--harness";"claude";"--claude-command";actor]in
    equal(`Bool true)(field "success" result);equal "monty:headless-claude-run-many:v1"(s "schema" result);equal "claude"(s "harness" result);
    List.iter(fun worker->let result=field "result"worker in equal "monty:headless-claude-run:v1"(s "schema"result);equal "claude"(s "harness"result))(xs "results"result);
    let run_worker command=["headless";command;"successor";"--harness";"claude";"--claude-command";actor]in
    let started=cli(run_worker "run")in equal "started"(s "run_status"started);equal `Null(field "harness_call"started);
    let finished=until ~predicate:(fun receipt->List.mem(s "status"receipt)["finished";"failed"])(fun ()->Client.req c("commands/" ^ s "command"started))in
    equal "finished"(s "status"finished);equal(`Int 0)(field "exit_code"(field "result"finished));
    let completed=field "result"finished |> s "stdout" |> Yojson.Safe.from_string in
    equal "monty:headless-claude-run:v1"(s "schema"completed);equal "claude"(s "harness"completed);
    let resumed=cli(run_worker "resume")in equal "started"(s "run_status"resumed);check(field "attempt_id"resumed<>field "attempt_id"started)"resume reused attempt";
    wait(fun ()->s "status"(Client.req c("commands/" ^ s "command"resumed))="finished");
    let rows=phases ()in equal(List.length rows)(List.map(i "pid")rows |> List.sort_uniq compare |> List.length);
    check(List.for_all(fun row->List.mem(`Int(Service.pid service))(xs "parents"row)&&field "private_nonce_cleared"row=`Bool true)rows)"phase escaped server ancestry or retained nonce";
    List.iter(fun job->let name=s "id"job in
      let snapshot=Yojson.Safe.from_file(run_dir / "workers" / name / "agent-profile.json")in
      let profile=match field "headless"snapshot with `Null->snapshot|value->value in
      let reviews=match field "reviews"profile with `List values->values|`Null->[]|_->assert false in
      let expected=(1+List.length reviews+(if field "fix"profile=`Null then 0 else 1))*(if name="successor"then 2 else 1)in
      equal expected(List.filter(fun row->field "worker"row=string name)rows |> List.length))jobs;
    let reviewers=List.filter(fun row->List.mem(string "--tools")(xs "args"row))rows in check(reviewers<>[])"reviewers absent";
    List.iter(fun row->if field "worker"row<>string "policy"then equal(`Bool false)(field "scoped_auth"row))rows;
    List.iter(fun row->let args=xs "args"row in
      let rec value=function key::value::_ when key=string "--tools"->value|_::rest->value rest|[]->`Null in
      equal(string "Read,Grep,Glob")(value args);
      List.iter(fun flag->check(List.mem(string flag)args)("missing reviewer restriction " ^ flag))["--restricted";"--safe-mode";"--strict-mcp-config";"--setting-sources"])reviewers;
    let inventory=Client.req c "inventory" |> xs "tasks"in check(inventory<>[] && List.for_all(fun task->field "status"task=string "open")inventory)"headless chain completed task";
    let failures=run_dir / "failures.json"in manifest_write failures(List.map(fun name->job name "solo")["native-failure";"cancelled"]);
    ignore(cli["headless";"prepare-many";"--manifest";failures;"--harness";"claude";"--claude-command";actor]);
    let failure=Client.cmd ~status:"failed" c "native-failure"(obj["op",string "headless_start";"profile",string "claude";"worker",string "native-failure"])in
    check(i "exit_code"failure<>0 && Sys.file_exists(s "log"failure))"native failure lost process result";check(Option.bind service.process poll=None)"native failure stopped service";ignore(Client.req c "health");
    write(home / "allow-native-failure")"";ignore(cli["headless";"resume";"native-failure";"--wait";"--harness";"claude";"--claude-command";actor]);
    ignore(Client.req ~data:(obj["op",string "headless_start";"profile",string "claude";"worker",string "cancelled"])c "commands/cancelled");
    let started=until ~predicate:(fun receipt->s "status"receipt="running" && field "run_status"(field "result"receipt)=string "started")(fun ()->Client.req c "commands/cancelled")in
    wait(fun ()->Sys.file_exists(home / "cancelled-started"));
    let cancellation=Client.cmd c "cancel-supervisor"(obj["op",string "cancel_execution";"command",string "cancelled";"updated_at",field "updated_at"started])in equal "outcome_unknown"(s "status"cancellation);
    pause 0.1;let cancelled=Client.req c "commands/cancelled"in equal "outcome_unknown"(s "status"cancelled);
    check(Monty.Home.contains_substring(s "reason"(field "result"cancelled))"explicitly cancelled")"late result replaced cancellation";
    check(Option.bind service.process poll=None)"cancellation stopped service";ignore(Client.req c "health");
    write(home / "allow-cancelled")"";ignore(cli["headless";"resume";"cancelled";"--wait";"--harness";"claude";"--claude-command";actor]);
    print_endline "Headless: server ancestry, fresh phases, policy pinning, concurrent batches, failure/cancellation recovery and still-open tasks passed"))
