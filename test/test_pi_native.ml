open Test_support
let json=Yojson.Safe.to_string
let first value=List.hd(xs "sessions" value)
let json_lines path=read path |> String.split_on_char '\n' |> List.filter((<>) "") |> List.map Yojson.Safe.from_string
let () = with_temp_dir ~prefix:"monty-pi-native-" (fun home ->
  let command=match Monty.Process.command_exists_with_arguments "pi"with Ok path->path|Error _->failwith "installed Pi 1.0.2 is required"in
  let repo=home / "repo"in mkdir repo;equal 0(run "git"["init";"-q";repo]).code;
  let pi=home / "pi"and shell_home=home / "shell"in mkdir pi;mkdir shell_home;
  let model=Test_native_model.create `Pi in
  let service=ref None in
  Fun.protect ~finally:(fun ()->
    Test_native_model.release model;
    Fun.protect ~finally:(fun ()->Test_native_model.close model)
      (fun ()->Option.iter Service.close !service))(fun ()->
    write(pi / "models.json")(json(obj["providers",obj["fixture",obj[
      "baseUrl",string(Test_native_model.url model ^ "/v1");"api",string "openai-completions";"apiKey",string "local-fixture";
      "models",`List[obj["id",string "fixture";"name",string "Local fixture";"reasoning",`Bool false;"input",`List[string "text"];
        "cost",obj["input",`Int 0;"output",`Int 0;"cacheRead",`Int 0;"cacheWrite",`Int 0];"contextWindow",`Int 32768;"maxTokens",`Int 512]]]] ]));
    let config=home / "profiles.json"in
    write config(json(obj["profiles",`List[obj["id",string "pi";"harness",string "pi";
      "command",string(command ^ " --offline --provider fixture --model fixture --no-tools --no-extensions --no-skills");
      "pi_home",string pi;"roots",`List[string repo];"tools",`List[]]] ]));
    let runtime=Service.create ~extra:["HOME",shell_home]home config in service:=Some runtime;
    let c=Service.client runtime in
    let cmd key request=Client.cmd c key request in
    let conversation cid=Client.req c("conversations/" ^ cid)in
    let revision cid=Client.revision c cid in
    let entries cid=Client.history c cid |> xs "sessions" |> List.concat_map(xs "records") |> List.concat_map(xs "items")in
    let chat=cmd "real-pi-start"(obj["op",string "start";"profile",string "pi";"modelProvider",string "fixture";"model",string "fixture";
      "context",obj["workspaces",`List[string repo];"instructions",string "This is a local integration fixture."]])in
    let cid=s "id"chat and native=s "native_id"(first chat)and source=s "source_file"(first chat)in
    equal(`Bool true)(field "observed"(first chat));
    ignore(cmd "real-pi-send"(obj["op",string "send";"conversation",string cid;"revision",revision cid;"text",string "Reply with the fixture text."]));
    wait ~seconds:20.(fun ()->List.exists(fun item->field "kind"item=string "agentMessage" && Monty.Home.contains_substring(json item)"Real Pi with a local model fixture.")(entries cid));
    check(Sys.file_exists source)"real Pi did not persist source";equal 1(List.length(Test_native_model.requests model));equal native(s "native_id"(first(conversation cid)));
    let observer=Client.create home in
    ignore(cmd "real-pi-hold"(obj["op",string "send";"conversation",string cid;"revision",revision cid;"text",string "hold for interrupt"]));
    wait ~seconds:10.(fun ()->Test_native_model.started model);
    ignore(Client.cmd observer "real-pi-abort"(obj["op",string "interrupt";"conversation",string cid;"revision",Client.revision observer cid]));
    Test_native_model.release model;wait(fun ()->s "runtime"(first(conversation cid))="idle");
    ignore(cmd "real-pi-detach"(obj["op",string "release_runtime";"conversation",string cid;"revision",revision cid]));
    let result=run ~cwd:repo ~env:(merge_env runtime.env["PI_CODING_AGENT_DIR",pi;"PI_TELEMETRY","0"])~timeout:25. command
      ["--offline";"--no-approve";"--no-tools";"--no-extensions";"--no-skills";"--session";source;"--provider";"fixture";"--model";"fixture";"-p";"native terminal continuation"]in
    check(result.code=0)result.stderr;
    wait(fun ()->List.exists(fun item->Monty.Home.contains_substring(json item)"native terminal continuation")(entries cid));
    equal native(s "id"(List.hd(json_lines source)));
    ignore(cmd "real-pi-resume"(obj["op",string "resume";"conversation",string cid;"revision",revision cid;"detached",`Bool true]));equal native(s "native_id"(first(conversation cid)));
    ignore(cmd "real-pi-terminal"(obj["op",string "terminal_open";"conversation",string cid;"selected_session",field "id"(first chat);"revision",revision cid;
      "prompt",string "server-owned native terminal turn";"rows",`Int 40;"cols",`Int 120]));
    wait ~seconds:20.(fun ()->List.exists(fun item->Monty.Home.contains_substring(json item)"server-owned native terminal turn")(entries cid));
    wait ~seconds:20.(fun ()->List.length(Test_native_model.requests model)>=4);
    wait(fun ()->let latest=json_lines source |> List.rev |> List.hd in field "role"(field "message"latest)=string "assistant");
    ignore(cmd "real-pi-terminal-input"(obj["op",string "terminal_input";"conversation",string cid;"selected_session",field "id"(first chat);
      "data",string(Base64.encode_exn "native terminal byte input\r")]));
    wait ~seconds:20.(fun ()->List.exists(fun item->Monty.Home.contains_substring(json item)"native terminal byte input")(entries cid));equal native(s "id"(List.hd(json_lines source)));
    let fork=cmd "real-pi-fork"(obj["op",string "start";"profile",string "pi";"terminal",`Bool true;"fork",string native;
      "context",obj["workspaces",`List[string repo];"instructions",string "Continue the local fork fixture."]])in
    let fork_id=s "id"fork and fork_session=first fork in
    ignore(cmd "real-pi-fork-terminal"(obj["op",string "terminal_open";"conversation",string fork_id;"selected_session",field "id"fork_session;
      "revision",revision fork_id;"fresh",`Bool true;"fork",string native;"prompt",string "fork terminal turn";"rows",`Int 40;"cols",`Int 120]));
    wait ~seconds:20.(fun ()->List.exists(fun item->Monty.Home.contains_substring(json item)"fork terminal turn")(entries fork_id));
    let state=first(conversation fork_id)in check(s "native_id"state<>native)"fork reused source native identity";
    let fork_file=s "source_file"state in equal(s "native_id"state)(s "id"(List.hd(json_lines fork_file)));
    check(Monty.Home.contains_substring(read fork_file)"server-owned native terminal turn")"fork lost source history";
    check(not(Monty.Home.contains_substring(read source)"fork terminal turn"))"fork mutated source history";
    equal[](xs "tasks"(Client.req c "inventory"));
    print_endline "real Pi 1.0.2: RPC send/interrupt, shared observation, exact server-owned PTY resume and fork identity passed; all inference local"))
