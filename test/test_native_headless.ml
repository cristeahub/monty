open Test_support

let json=Yojson.Safe.to_string
let first value=List.hd(xs "sessions" value)
let contains=Monty.Home.contains_substring
let rec logs root =
  if not(Sys.file_exists root) then "" else match (Unix.stat root).st_kind with
  | Unix.S_DIR -> Sys.readdir root |> Array.to_list |> List.map(fun name->logs(root/name)) |> String.concat "\n"
  | _ -> let text=read root in root ^ ":\n" ^ String.sub text (max 0 (String.length text-6000)) (min 6000 (String.length text))

let run_suite harness native python = with_temp_dir ~prefix:("monty-" ^ harness ^ "-native-") (fun root ->
  let bootstrap,repo=Task_fixture.create root in Service.close bootstrap;
  let native=Unix.realpath native in
  let account=root/(if harness="codex" then "codex" else "account") in mkdir account;
  let model=Test_native_model.create (if harness="codex" then `Responses else `Claude) in
  Test_native_model.release model;
  let service=ref None in
  Fun.protect ~finally:(fun ()->Fun.protect ~finally:(fun ()->Test_native_model.close model)
    (fun ()->Option.iter Service.close !service)) (fun ()->
    if harness="codex" then write(account/"config.toml")
      ("model=\"monty-fixture\"\nmodel_provider=\"fixture\"\n[model_providers.fixture]\nname=\"Fixture\"\nbase_url=\"" ^ Test_native_model.url model ^ "/v1\"\nwire_api=\"responses\"\nrequires_openai_auth=false\n");
    let profile=["id",string harness;"harness",string harness;"command",string native;
      harness ^ "_home",string account;"roots",`List[string root]] @
      (if harness="codex" then [] else ["python",string(Unix.realpath python);
        "auth_env",obj["ANTHROPIC_API_KEY",string "TEST_CLAUDE_KEY";"ANTHROPIC_BASE_URL",string "TEST_CLAUDE_BASE";
        "CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC",string "TEST_CLAUDE_TRAFFIC"]]) in
    let config=root/"profiles.json" in write config(json(obj["profiles",`List[obj profile]]));
    let extra=merge_env bootstrap.env ["HOME",root;"MONTY_WT_COMMAND",root/"fake-bin/wt";
      "MONTY_CODEX_YOLO","false";"CLAUDE_CONFIG_DIR",account;"TEST_CLAUDE_KEY","synthetic-test-key";
      "TEST_CLAUDE_BASE",Test_native_model.url model;"TEST_CLAUDE_TRAFFIC","1"] in
    let runtime=Service.create ~extra root config in service:=Some runtime;
    if harness="claude" then (
      let c=Service.client runtime in
      let chat=Client.cmd c "start" (obj["op",string "start";"profile",string "claude";
        "context",obj["workspaces",`List[string repo];"instructions",string "Reply with the fake endpoint text. Do not use tools."]]) in
      let cid=s "id" chat in
      ignore(Client.cmd c "send" (obj["op",string "send";"conversation",string cid;
        "selected_session",field "id"(first chat);"text",string "Reply briefly."]));
      let observed=until ~predicate:(fun sess->List.mem(s "runtime" sess)["idle";"outcome_unknown";"disconnected"])
        (fun ()->first(Client.req c("conversations/" ^ cid))) in
      check(s "runtime" observed="idle" && field "runtime_error" observed=`Null) (json observed);
      check(model.requests<>[]) "Claude did not contact the local model";
      wait(fun ()->Client.history c cid |> xs "sessions" |> List.concat_map(xs "records") |> List.concat_map(xs "items")
        |> List.exists(fun item->contains(json item)"Local native protocol verified.")));
    let context=root/"context.md" in write context "Reply briefly with the local fixture text. No edits or tools needed.\n";
    let run_dir=root/".monty/runs/native-review" in mkdir run_dir;
    let manifest=run_dir/"jobs.json" in
    write manifest(json(obj["jobs",`List[obj["id",string "native-review";"title",string "Native review";
      "repo",string repo;"branch",string "cto/native-review";"context",string context;"agent_profile",string "reviewed"]]]));
    List.iter(fun args->
      let result=run ~cwd:repo ~env:runtime.env ~timeout:90. bin
        (["headless"] @ args @ ["--harness";harness;"--" ^ harness ^ "-command";native;"--home";root]) in
      check(result.code=0)(result.stdout ^ result.stderr ^ logs(run_dir/"workers/native-review/artifacts/headless")))
      [["prepare-many";"--manifest";manifest];["run";"--wait";"native-review"]];
    if harness="codex" then (
      check(model.requests<>[]) "Codex did not contact the local model";
      check(List.for_all(fun request->s "model" request="monty-fixture")model.requests) "headless phases lost scoped model")
    else check(List.length model.review_tools>=2) "independent reviewers did not expose restricted tool schemas";
    Printf.printf "Native %s: local protocol, scoped provider/model and reviewed headless phases passed\n%!" harness))
