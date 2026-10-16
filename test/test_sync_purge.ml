open Test_support
let json = Yojson.Safe.to_string
let () =
  with_temp_dir ~prefix:"monty-sync-purge-" (fun home ->
    let repo=home / "repo" in mkdir repo;equal 0(run "git"["init";"-q";repo]).code;
    let source=Native.create(home / "native.sock") in
    let config=home / "profiles.json" in
    write config(json(obj["allow_fixture",`Bool true;"profiles",`List[obj["id",string "default";
      "harness",string "fixture";"socket",string(home / "native.sock");"roots",`List[string repo]]]]));
    let service=Service.create home config in
    let gate=Native.Gate.create () in
    Fun.protect ~finally:(fun ()->Native.Gate.release gate;Service.close service;Native.close source) (fun ()->
      let c=Service.client service in
      let request=obj["op",string "start";"profile",string "default";"context",obj["workspaces",`List[string repo]]] in
      let chat=Client.cmd c "observed" request in let cid=s "id" chat in
      source.history_gate<-Some gate;wait ~seconds:5.(fun ()->gate.started);
      ignore(Client.cmd c "purge-in-flight"(obj["op",string "purge";"conversation",string cid;"confirm",`Bool true]));
      http_error 410(fun ()->Client.history c cid);Native.Gate.release gate;
      let survivor=Client.cmd c "survivor" request in let native=s "native_id"(List.hd(xs "sessions" survivor)) in
      let reads ()=List.filter(fun (method_,params)->method_="history" && field "session" params=string native)source.calls |> List.length in
      let before=reads () in wait ~seconds:5.(fun ()->reads ()>before);
      check(Option.bind service.process poll=None) "service stopped after in-flight purge";ignore(Client.req c "health");
      http_error 410(fun ()->Client.history c cid);
      let archive=home / ".monty/conversations" / cid in
      equal [](xs "sessions"(Yojson.Safe.from_file(archive / "conversation.json")));
      check(not(Sys.file_exists(archive / "sessions"))) "late snapshot recreated purged history";
      print_endline "History sync: in-flight purge preserves tombstones and background polling"));
  with_temp_dir ~prefix:"monty-mirror-purge-" (fun home ->
    let repo=home / "repo" in mkdir repo;equal 0(run "git"["init";"-q";repo]).code;
    let account=home / "claude" in mkdir(account / "sessions");
    let python=sdk_python home in
    let config=home / "profiles.json" in
    write config(json(obj["profiles",`List[obj["id",string "claude";"harness",string "claude";
      "command",string "/usr/bin/true";"python",string python;"claude_home",string account;"roots",`List[string repo]]]]));
    let service=Service.create home config in
    Fun.protect ~finally:(fun ()->write(account / "release-history") "";Service.close service)(fun ()->
      let c=Service.client service in
      let imported key native =
        write(account / "sessions" / (native ^ ".json"))(json(obj["cwd",string repo;"entries",`List[]]));
        Client.cmd c key(obj["op",string "import";"profile",string "claude";"native_id",string native;
          "context",obj["workspaces",`List[string repo]]]) in
      let chat=imported "mirror" "12345678-1234-4234-8234-123456789abc" in let cid=s "id" chat in
      write(account / "pause-history") "";wait ~seconds:5.(fun ()->Sys.file_exists(account / "history-started"));
      ignore(Client.cmd c "purge-mirror"(obj["op",string "purge";"conversation",string cid;"confirm",`Bool true]));
      write(account / "release-history") "";
      let native="12345678-1234-4234-8234-123456789abd" in ignore(imported "mirror-survivor" native);
      let reads ()=read(account / "calls.jsonl") |> String.split_on_char '\n' |> List.filter((<>) "")
        |> List.map Yojson.Safe.from_string |> List.filter(fun row->field "history" row=string native) |> List.length in
      let before=reads () in wait ~seconds:5.(fun ()->reads ()>before);
      check(Option.bind service.process poll=None) "Claude mirror watcher stopped service";ignore(Client.req c "health");
      http_error 410(fun ()->Client.history c cid);
      check(not(Sys.file_exists(home / ".monty/conversations" / cid / "sessions"))) "late Claude mirror recreated purged history";
      print_endline "Claude mirror: in-flight purge preserves service and archive deletion"))
