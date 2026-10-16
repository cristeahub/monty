open Lwt.Infix
open Test_support

type terminal = { pid:int; fd:Lwt_unix.file_descr; output:Buffer.t;
  status:Unix.process_status Lwt.t; drain:unit Lwt.t }
let open_terminal ~repo ~environment args =
  let master,slave=Test_pty.open_pair () in
  Test_pty.set_raw slave;Test_pty.resize slave ~rows:40 ~cols:120;
  let child=match Unix.fork () with
    | 0 -> (try
        ignore(Unix.setsid ());Unix.close master;
        List.iter (Unix.dup2 slave) [Unix.stdin;Unix.stdout;Unix.stderr];Unix.close slave;
        Unix.chdir repo;
        Unix.execve bin (Array.of_list(bin::args))
          (Array.of_list(List.map(fun (key,value)->key ^ "=" ^ value)environment))
      with _ -> Unix._exit 127)
    | child -> child in
  Unix.close slave;
  let fd=Lwt_unix.of_unix_file_descr master and output=Buffer.create 8192 in
  let bytes=Bytes.create 8192 in
  let rec drain () = Lwt_unix.read fd bytes 0 (Bytes.length bytes) >>= fun count ->
    if count=0 then Lwt.return_unit else (
      let retained=min count (1_000_000-Buffer.length output) in
      if retained>0 then Buffer.add_subbytes output bytes 0 retained;
      drain ()) in
  let drain=Lwt.catch drain (function Unix.Unix_error((Unix.EIO|Unix.EBADF),_,_)->Lwt.return_unit|error->Lwt.fail error) in
  {pid=child;fd;output;status=(Lwt_unix.waitpid [] child >|= snd);drain}
let send terminal text =
  let rec write offset = if offset=String.length text then Lwt.return_unit else
    Lwt_unix.write_string terminal.fd text offset (String.length text-offset) >>= fun count -> write(offset+count) in
  Lwt_main.run(write 0)
let stop terminal = Lwt_main.run (
  (if Lwt.is_sleeping terminal.status then (
    (try Unix.kill terminal.pid Sys.sigkill with Unix.Unix_error(Unix.ESRCH,_,_)->());
    Lwt.protected terminal.status >|= ignore) else Lwt.return_unit)
  >>= fun () -> Lwt_unix.close terminal.fd >>= fun () ->
  Lwt.catch (fun ()->terminal.drain) (fun _->Lwt.return_unit))
let text terminal = Buffer.contents terminal.output
let contains=Monty.Home.contains_substring
let first value=List.hd(xs "sessions" value)
let json=Yojson.Safe.to_string

let () = with_temp_dir ~prefix:"monty-client-native-" (fun home ->
  let repo=home/"repo" in mkdir repo;equal 0(run "git" ["init";"-q";repo]).code;
  write(repo/".agents/skills/visible-fixture/SKILL.md")
    "---\nname: visible-fixture\ndescription: Isolated skill visibility check.\n---\nOnly act if requested.\n";
  let account=home/"codex" and shell_home=home/"shell" in mkdir account;mkdir shell_home;
  let model=Test_native_model.create `Responses in
  let service=ref None and terminal=ref None in
  Fun.protect ~finally:(fun ()->Test_native_model.release model;
    Fun.protect ~finally:(fun ()->Test_native_model.close model) (fun ()->
      Fun.protect ~finally:(fun ()->Option.iter Service.close !service) (fun ()->Option.iter stop !terminal))) (fun ()->
    write(account/"config.toml") ("model=\"monty-fixture\"\nmodel_provider=\"fixture\"\n[model_providers.fixture]\nname=\"Fixture\"\nbase_url=\"" ^
      Test_native_model.url model ^ "/v1\"\nwire_api=\"responses\"\nrequires_openai_auth=false\n");
    let command=Monty.Process.command_exists_with_arguments "codex" |> Result.get_ok in
    let config=home/"profiles.json" in
    write config(json(obj["profiles",`List[obj["id",string "default";"harness",string "codex";
      "command",string command;"codex_home",string account;"roots",`List[string repo]]]]));
    let runtime=Service.create ~extra:["HOME",shell_home] home config in service:=Some runtime;
    let c=Service.client runtime in
    let chat=Client.cmd c "real-start" (obj["op",string "start";"profile",string "default";
      "codex_yolo",`Bool true;"context",obj["workspaces",`List[string repo]]]) in
    let cid=s "id" chat and sess=first chat in let native=s "native_id" sess in
    let fields=match sess with `Assoc fields->fields|_->assert false in
    check(not(List.mem_assoc "endpoint" fields) && not(List.mem_assoc "socketPath" fields)) "private endpoint leaked";
    let thread=Client.history c cid |> first |> xs "records" |> List.hd |> field "raw" |> field "thread" in
    equal "legacy" (s "historyMode" thread);
    let tui=open_terminal ~repo ~environment:(merge_env runtime.env["TERM","xterm-256color"])
      ["terminal";"--home";home;"--mode";"exact";"--session";native;"--codex-yolo"] in terminal:=Some tui;
    wait(fun ()->contains(text tui)"Vim: Normal");
    check(not(Test_native_model.started model)) "terminal startup triggered inference";
    let offset=Buffer.length tui.output in
    let tail ()=let value=text tui in String.sub value offset (String.length value-offset) in
    let await value=wait(fun ()->contains(tail ())value) in
    send tui "i";await "Vim: Insert";
    send tui "/skills";await "/skills";pause 0.2;send tui "\r";await "Choose an action";
    send tui "\r";await "visible-fixture";send tui "\027";pause 0.2;send tui "\021";
    send tui "Hi";await "Hi";send tui "\r";
    wait ~seconds:15.(fun ()->Test_native_model.started model);
    let observer=Client.create home in
    let active_turn ()=Client.history observer cid |> first |> xs "records" |> List.exists(fun batch->
      match field "turns"(field "thread"(field "raw" batch)) with
      | `Null -> false
      | `List turns -> List.exists(fun turn->s "status" turn="inProgress") turns
      | _ -> failwith "native snapshot has invalid turns") in
    wait active_turn;
    Unix.kill tui.pid Sys.sighup;
    ignore(Lwt_main.run(Lwt_unix.with_timeout 10.(fun ()->Lwt.protected tui.status)));
    check(active_turn ()) "client loss discarded the accepted turn";
    equal (`Bool true) (field "running"(Client.req observer("terminals/" ^ s "id" sess)));
    Test_native_model.release model;
    let history=until ~predicate:(fun history->first history |> xs "records" |> List.exists(fun row->contains(json row)"isolated fixture reply"))
      (fun ()->Client.history c cid) in equal native(s "native_id"(first history));
    ignore(Client.cmd observer "real-second-send"(obj["op",string "send";"conversation",string cid;
      "selected_session",field "id" sess;"text",string "Second fixture reply."]));
    wait(fun ()->Client.history c cid |> first |> xs "effective" |> List.length |> fun count->count>=4);
    print_endline "Native Codex: private runtime, exact TUI, Vim mode, skill discovery, SIGHUP survival and shared history passed; all inference local"))
