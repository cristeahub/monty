open Test_support
let json=Yojson.Safe.to_string
let first value=List.hd(xs "sessions" value)
let last values=List.hd(List.rev values)
let json_lines path=if Sys.file_exists path then read path |> String.split_on_char '\n' |> List.filter((<>) "") |> List.map Yojson.Safe.from_string else []
let fixture_pi=Unix.realpath Sys.argv.(2)
let fixture_codex=Unix.realpath Sys.argv.(3)
let executable path program=write path ("#!/bin/sh\nexec " ^ Monty.Shell.quote program ^ " \"$@\"\n");Unix.chmod path 0o700
let () = with_temp_dir ~prefix:"monty-pi-" (fun home ->
  let repo=home / "repo" in mkdir repo;equal 0(run "git"["init";"-q";repo]).code;
  let pi=home / "pi" and second=home / "pi-second" and codex_home=home / "codex" in
  mkdir(pi / "sessions");mkdir second;mkdir codex_home;
  let native=Native.create(home / "codex.sock") in
  write(codex_home / "fixture.json")(json(obj["socket",string(home / "codex.sock")]));
  let codex=home / "fake-codex" and fake_pi=home / "fake-pi" in
  executable codex fixture_codex;executable fake_pi fixture_pi;
  let config=home / "profiles.json" in
  write config(json(obj["profiles",`List[
    obj["id",string "pi";"harness",string "pi";"command",string fake_pi;"pi_home",string pi;"roots",`List[string repo]];
    obj["id",string "second";"harness",string "pi";"command",string fake_pi;"pi_home",string second;"roots",`List[string repo]];
    obj["id",string "codex";"harness",string "codex";"command",string codex;"codex_home",string codex_home;"roots",`List[string repo]]]]));
  let service=Service.create ~extra:["MONTY_UNRELATED_WORKER","must-not-inherit"] home config in
  Fun.protect ~finally:(fun ()->Service.close service;Native.close native)(fun ()->
    let c ()=Service.client service in
    let req path=Client.req (c ()) path in
    let cmd ?status key value=Client.cmd ?status (c ()) key value in
    let revision cid=Client.revision (c ()) cid in
    let entries cid=Client.history(c ())cid |> xs "sessions" |> List.concat_map(xs "records") |> List.concat_map(xs "items") in
    let conversation cid=req("conversations/" ^ cid) in
    let status cid=conversation cid |> first |> s "runtime" in
    let calls ()=json_lines(pi / "calls.jsonl") in
    let count key rows=List.filter(fun row->field key row<>`Null)rows |> List.length in
    let stop_gate=home / "stop-gate" in
    let context=obj["workspaces",`List[string repo];"instructions",string "Trusted Monty instructions";
      "environment",obj["MONTY_JOB_TITLE",string "explicit";"MONTY_FIXTURE_STOP_GATE",string stop_gate]] in
    check(List.mem (string "pi")(xs "real_adapters"(req "health"))) "Pi adapter missing";
    let path=pi / "sessions/native.jsonl" in
    let records=[obj["type",string "session";"version",`Int 3;"id",string "native-file";"cwd",string repo];
      obj["type",string "message";"id",string "u1";"parentId",`Null;"message",obj["role",string "user";"content",`List[obj["type",string "image";"data",string(Base64.encode_exn "owned inline image");"mimeType",string "image/png"]]]];
      obj["type",string "message";"id",string "a1";"parentId",string "u1";"message",obj["role",string "assistant";"content",string "abandoned answer";"provider",string "old";"model",string "old-model"]];
      obj["type",string "message";"id",string "u2";"parentId",string "u1";"message",obj["role",string "user";"content",string "new branch"]];
      obj["type",string "compaction";"id",string "c1";"parentId",string "u2";"summary",string "source summary";"firstKeptEntryId",string "u2"];
      obj["type",string "custom";"id",string "x1";"parentId",string "c1";"data",obj["accessToken",string "secret-marker"]]] in
    let original=String.concat ""(List.map(fun row->json row ^ "\n")records) in write path original;
    let request=obj["op",string "import";"profile",string "pi";"session_file",string path;"context",context] in
    let imported=cmd "import-file" request in let cid=s "id" imported in
    check(not(Sys.file_exists(pi / "calls.jsonl"))) "history import spawned Pi";equal original(read path);
    equal (`Bool false)(field "observed"(first imported));equal 5(List.length(entries cid));
    let selected=xs "effective"(first(conversation cid)) |> List.map(function `String value->value|_->assert false) in
    check(not(List.exists(fun value->Monty.Home.contains_substring value "a1")selected) && List.exists(fun value->Monty.Home.contains_substring value "x1")selected) "Pi effective branch lost";
    let attachment=List.find(fun row->field "source_id" row=string "u1")(entries cid) |> xs "attachments" |> List.hd in
    let blob=home / ".monty/conversation-blobs" / s "sha256" attachment in equal "owned inline image"(read blob);
    check(not(Monty.Home.contains_substring(json(Client.history(c ())cid))"secret-marker")) "Pi credential retained";
    ignore(cmd "same-file"(obj["op",string "sync";"conversation",string cid]));equal 5(List.length(entries cid));
    ignore(cmd ~status:"rejected" "unowned-send"(obj["op",string "send";"conversation",string cid;"revision",revision cid;"text",string "no"]));
    let extra=obj["type",string "message";"id",string "a2";"parentId",string "x1";"message",obj["role",string "assistant";"content",string "appended from native TUI"]] in
    write path (original ^ json extra);
    wait(fun ()->conversation cid |> first |> field "coverage" |> field "state" = string "partial");equal 5(List.length(entries cid));
    write path(original ^ json extra ^ "\n");wait(fun ()->List.exists(fun row->field "source_id" row=string "a2")(entries cid));equal 6(List.length(entries cid));
    let replacement=pi / "sessions/native.tmp" in write replacement original;Unix.rename replacement path;
    wait(fun ()->not(List.exists(fun key->Monty.Home.contains_substring(json key)"a2")(xs "effective"(first(conversation cid)))));
    check(List.exists(fun row->field "source_id" row=string "a2")(entries cid)) "source replacement removed retained entry";
    let header=put "id"(string "different-identity")(List.hd records) in
    write path(String.concat ""(List.map(fun row->json row ^ "\n")(header::List.tl records)));
    wait(fun ()->field "state"(field "coverage"(first(conversation cid)))=string "unavailable");write path original;
    let outside=home / "outside.jsonl" in write outside original;
    ignore(cmd ~status:"rejected" "outside"(put "session_file"(string outside)request));
    let alias=pi / "sessions/alias.jsonl" in Unix.symlink outside alias;
    ignore(cmd ~status:"rejected" "symlink"(put "session_file"(string alias)request));
    Unix.unlink path;wait(fun ()->field "state"(field "coverage"(first(conversation cid)))=string "unavailable");equal 6(List.length(entries cid));
    write path original;ignore(cmd "purge-file"(obj["op",string "purge";"conversation",string cid;"confirm",`Bool true]));
    ignore(cmd ~status:"rejected" "purged-reimport" request);check(not(Sys.file_exists blob)) "Pi blob survived purge";
    let start=obj["op",string "start";"profile",string "pi";"context",context] in
    let chats=Client.parallel_cmd (c ()) "rpc-start" start ~count:4 in
    let chat=List.hd chats in let cid=s "id" chat in
    check(Sys.file_exists(pi / "skills/monty/SKILL.md") && Sys.file_exists(pi / "skills/monty/references/workflows.md")) "Pi skill missing";
    check(not(Sys.file_exists(second / "skills/monty"))) "Pi skill installed in other account";
    equal 1(List.map(s "id")chats |> List.sort_uniq String.compare |> List.length);
    equal 1(count "start"(calls ()));
    let started=List.find(fun row->field "start" row<>`Null)(calls ()) in
    equal(string "explicit")(field "MONTY_JOB_TITLE"(field "environment" started));
    equal `Null(field "MONTY_UNRELATED_WORKER"(field "environment" started));
    check(List.mem(string "--no-approve")(xs "argv" started) && List.mem(string "--append-system-prompt")(xs "argv" started)) "Pi safety/context flags missing";
    let send=obj["op",string "send";"conversation",string cid;"revision",revision cid;"text",string "hello"] in
    ignore(Client.parallel_cmd(c ())"rpc-send" send ~count:4);
    wait(fun ()->List.exists(fun row->field "kind" row=string "agentMessage")(entries cid));
    equal 1(List.filter(fun row->field "type"(field "command" row)=string "prompt")(calls ()) |> List.length);
    check(Sys.file_exists(s "source_file"(first(conversation cid)))) "Pi native file missing";
    let pending ()=until ~predicate:(fun rows->rows<>[])(fun ()->xs "pending"(first(conversation cid))) |> List.hd in
    ignore(Client.req ~data:(obj["op",string "send";"conversation",string cid;"revision",revision cid;"text",string "dialog"])(c ())"commands/dialog-send");
    let dialog=pending () in
    let answer=obj["op",string "respond";"conversation",string cid;"revision",revision cid;"pending_id",field "id" dialog;"pending_revision",`Int 1;"response",obj["confirmed",`Bool false]] in
    ignore(cmd ~status:"rejected" "dialog-invalid"(put "response"(obj["value",string "wrong dialog shape"])answer));
    equal(string "pending")(field "state"(List.hd(xs "pending"(first(conversation cid)))));
    ignore(Client.parallel_req(c ()) ["commands/dialog-a",answer;"commands/dialog-b",answer]);
    wait(fun ()->s "status"(req "commands/dialog-send")="finished");
    wait(fun ()->List.map(fun key->s "status"(req("commands/" ^ key)))["dialog-a";"dialog-b"] |> List.sort String.compare = ["finished";"rejected"]);
    equal 1(List.filter(fun row->field "type"(field "command" row)=string "extension_ui_response")(calls ()) |> List.length);
    ignore(Client.req ~data:(obj["op",string "send";"conversation",string cid;"revision",revision cid;"text",string "expiring dialog"])(c ())"commands/expiring-send");
    let dialog=pending () in
    wait(fun ()->Unix.gettimeofday ()>Monty.Json_fields.num(field "expires_at" dialog));
    ignore(cmd ~status:"rejected" "expired-response"(obj["op",string "respond";"conversation",string cid;"revision",revision cid;"pending_id",field "id" dialog;"pending_revision",`Int 1;"response",obj["confirmed",`Bool true]]));
    ignore(cmd "abort-dialog"(obj["op",string "interrupt";"conversation",string cid;"revision",revision cid]));
    wait(fun ()->s "status"(req "commands/expiring-send")="finished");
    ignore(cmd "model"(obj["op",string "set_model";"conversation",string cid;"revision",revision cid;"provider",string "fixture";"model",string "second"]));
    ignore(cmd "hold"(obj["op",string "send";"conversation",string cid;"revision",revision cid;"text",string "hold"]));
    ignore(cmd "steer"(obj["op",string "send";"conversation",string cid;"revision",revision cid;"text",string "queued steering";"delivery",string "steer"]));
    ignore(cmd "ui-detach"(obj["op",string "detach";"conversation",string cid;"revision",revision cid]));equal "running"(status cid);
    ignore(cmd ~status:"rejected" "busy-detach"(obj["op",string "release_runtime";"conversation",string cid;"revision",revision cid]));
    let switch profile=obj["op",string "switch";"conversation",string cid;"revision",revision cid;"mode",string "branch";"profile",string profile;"context",context] in
    ignore(cmd ~status:"rejected" "busy-switch"(switch "second"));
    let stopped=cmd "interrupt"(obj["op",string "interrupt";"conversation",string cid;"revision",revision cid]) in
    equal[`String "queued steering"](xs "steering"(field "cleared_queue" stopped));
    let changed=cmd "to-codex"(switch "codex") in equal 2(List.length(xs "sessions" changed));
    let changed=cmd "to-pi"(switch "second") in equal 3(List.length(xs "sessions" changed));
    wait(fun ()->List.exists(fun row->field "kind" row=string "contextTransfer")(entries cid));
    let second_calls=json_lines(second / "calls.jsonl") in
    let transferred=List.find(fun row->field "type"(field "command" row)=string "prompt")second_calls |> field "command" |> s "message" in
    let boundary=String.index transferred '\n' in
    equal(field "package"(last(xs "transfers" changed)))(Yojson.Safe.from_string(String.sub transferred (boundary+1)(String.length transferred-boundary-1)));
    check(not(List.exists(fun row->field "type"(field "command" row)=string "bash")second_calls)) "switch replayed shell command";
    let starts_before=count "start"(calls ()) in
    write stop_gate "";
    let first_stop=spawn ~env:service.env ~timeout:25. bin["service";"stop";"--home";home] in
    wait(fun ()->List.exists(fun row->field "stopping" row<>`Null)(calls ()));
    let second_stop=spawn ~env:service.env ~timeout:25. bin["service";"stop";"--home";home] in
    Fun.protect ~finally:(fun ()->if Sys.file_exists stop_gate then Unix.unlink stop_gate)(fun ()->pause 1.5;check(poll second_stop=None) "second stop skipped ongoing Pi cleanup");
    equal 0(await first_stop).code;equal 0(await second_stop).code;
    Service.start service;equal starts_before(count "start"(calls ()));check(entries cid<>[]) "restart lost Pi archive";
    ignore(cmd ~status:"rejected" "missing-runtime"(obj["op",string "send";"conversation",string cid;"revision",revision cid;"text",string "no"]));
    ignore(cmd ~status:"rejected" "no-detach-confirmation"(obj["op",string "resume";"conversation",string cid;"revision",revision cid]));
    ignore(cmd "resume"(obj["op",string "resume";"conversation",string cid;"revision",revision cid;"detached",`Bool true]));
    let lost=obj["op",string "send";"conversation",string cid;"revision",revision cid;"text",string "lost response"] in
    ignore(cmd ~status:"outcome_unknown" "lost" lost);ignore(cmd ~status:"outcome_unknown" "lost" lost);
    equal 1(List.filter(fun row->field "message"(field "command" row)=string "lost response")(json_lines(second / "calls.jsonl")) |> List.length);
    wait(fun ()->s "runtime"(last(xs "sessions"(conversation cid)))="disconnected");
    let crash_path=pi / "sessions/crash.jsonl" in
    write crash_path(String.concat ""(List.map(fun row->json row ^ "\n")[put "id"(string "crash-file")(List.hd records);put "id"(string "crash-user")(List.nth records 1)]));
    let starts_before=count "start"(calls ()) in
    Service.stop service;service.env<-merge_env service.env["MONTY_FAULT_INJECT","conversation-after-batch"];Service.start service;
    let crash_request=put "session_file"(string crash_path)request in ignore(cmd ~status:"outcome_unknown" "pi-batch-fault" crash_request);
    let imported=match req "conversations" with `List chats->List.find(fun chat->List.exists(fun sess->field "native_id" sess=string "crash-file")(xs "sessions" chat))chats|_->assert false in
    equal 0(i "checkpoint"(first imported));
    let sessions=home / ".monty/conversations" / s "id" imported / "sessions" in
    let orphans=Sys.readdir sessions |> Array.to_list |> List.concat_map(fun sid->let path=sessions / sid / "batches" in if Sys.file_exists path then Array.to_list(Sys.readdir path) |> List.map(fun name->path / name) else []) in
    check(orphans<>[]) "fault did not leave uncommitted Pi batch";
    Service.stop service;service.env<-List.remove_assoc "MONTY_FAULT_INJECT" service.env;Service.start service;
    wait(fun ()->List.length(entries(s "id" imported))=1);check(List.for_all(fun path->not(Sys.file_exists path))orphans) "orphan Pi batches survived recovery";
    equal starts_before(count "start"(calls ()));
    Service.stop service;
    let profiles=Yojson.Safe.from_file config in
    let values=xs "profiles" profiles in write config(json(put "profiles"(`List(put "id"(string "pi-renamed")(List.hd values)::List.tl values))profiles));
    Service.start service;
    let rebound=cmd "rename-import"(put "profile"(string "pi-renamed")crash_request) in
    equal(s "id" imported)(s "id" rebound);equal "pi-renamed"(s "profile"(first rebound));equal 1(List.length(entries(s "id" imported)));
    let start_with key env=cmd key(obj["op",string "start";"profile",string "pi-renamed";"context",put "environment" env context]) in
    let volatile=start_with "volatile-start"(obj["MONTY_FIXTURE_UNFLUSHED",string "1"]) in
    let volatile_id=s "id" volatile and volatile_path=s "source_file"(first volatile) in
    ignore(cmd "volatile-detach"(obj["op",string "release_runtime";"conversation",string volatile_id;"revision",revision volatile_id]));
    let final_batches=Client.history(c ())volatile_id |> first |> xs "records" |> List.filter(fun batch->field "provenance" batch=string "Pi release_runtime boundary") in
    check(final_batches<>[] && List.exists(fun row->field "data"(field "content" row)=string "unflushed setup")(xs "items"(last final_batches))) "final Pi snapshot omitted unflushed setup";
    check(not(Sys.file_exists volatile_path)) "volatile Pi session unexpectedly created file";equal "disconnected"(status volatile_id);
    let rebound=start_with "rebind-start"(obj["MONTY_FIXTURE_REBIND",string "1"]) in
    let rebound_id=s "id" rebound and source_file=s "source_file"(first rebound) in
    ignore(cmd "rebind-send"(obj["op",string "send";"conversation",string rebound_id;"revision",revision rebound_id;"text",string "persist fixture history"]));
    ignore(cmd "rebind-detach"(obj["op",string "release_runtime";"conversation",string rebound_id;"revision",revision rebound_id]));
    let stops=count "stopped"(calls ()) in
    let result=cmd ~status:"rejected" "rebind-resume"(obj["op",string "resume";"conversation",string rebound_id;"revision",revision rebound_id;"detached",`Bool true]) in
    equal 409(i "code" result);check(Monty.Home.contains_substring(s "message" result)"different session file") "rebound Pi file accepted";
    wait(fun ()->count "stopped"(calls ())=stops+1);equal source_file(s "source_file"(first(conversation rebound_id)));
    let malformed=cmd "malformed-start"(obj["op",string "start";"profile",string "pi-renamed";"context",context]) in
    let malformed_id=s "id" malformed and native_id=s "native_id"(first malformed) in
    let native_pid=List.find(fun row->field "start" row=string native_id)(calls ()) |> i "pid" in
    ignore(cmd ~status:"outcome_unknown" "malformed-send"(obj["op",string "send";"conversation",string malformed_id;"revision",revision malformed_id;"text",string "malformed stream"]));
    wait(fun ()->List.exists(fun row->field "stopped" row=string native_id)(calls ()));
    let stat=(run "ps"["-o";"stat=";"-p";string_of_int native_pid]).stdout |> String.trim in
    check(stat="" || String.starts_with ~prefix:"Z" stat) ("malformed stream left native running: " ^ stat);
    let starts=count "start"(calls ()) in ignore(Client.history(c ())malformed_id);equal starts(count "start"(calls ()));
    equal[](xs "tasks"(req "inventory"));check(not(Sys.file_exists(home / ".monty/tasks.local.json"))) "Pi conversations created tasks";
    print_endline "Pi file mirror, RPC control, dialog races, switching and restart E2E passed"))
