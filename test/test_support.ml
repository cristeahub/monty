open Lwt.Infix
type json=Yojson.Safe.t
include Monty.Json_fields
let ( / )=Filename.concat
let obj fields=`Assoc fields
let string value=`String value
let check condition message=if not condition then failwith message
let equal ?(label="values differ") expected actual=check(expected=actual)label
let bin=
  let candidates=(if Array.length Sys.argv>1 && not(String.starts_with ~prefix:"-" Sys.argv.(1))then[Sys.argv.(1)]else[])@
    [Filename.dirname Sys.executable_name / "../bin/main.exe";"../bin/main.exe";Sys.executable_name]in
  List.find Sys.file_exists candidates |>Unix.realpath
let env=Unix.environment() |>Array.to_list |>List.map(fun value->let at=String.index value '='in String.sub value 0 at,String.sub value(at+1)(String.length value-at-1))
let merge_env base overrides=overrides@List.filter(fun(key,_)->not(List.mem_assoc key overrides))base
let mkdir=Monty.Shell.ensure_dir
let read=Monty.Shell.read_file
let write path text=mkdir(Filename.dirname path);Monty.Shell.write_file path text
let rec remove_tree path=match Unix.lstat path with
  | {Unix.st_kind=Unix.S_DIR;_}->Sys.readdir path |>Array.iter(fun name->remove_tree(path/name));Unix.rmdir path
  | _->Unix.unlink path
  | exception Unix.Unix_error((Unix.ENOENT|Unix.ENOTDIR),_,_)->()
let rec copy_tree source destination=match(Unix.lstat source).Unix.st_kind with
  | Unix.S_DIR->mkdir destination;Sys.readdir source |>Array.iter(fun name->copy_tree(source/name)(destination/name))
  | _->write destination(read source);Unix.chmod destination(Unix.stat source).Unix.st_perm
let with_temp_dir ?(prefix="monty-ocaml-") run=
  let path=Filename.temp_file ~temp_dir:"/tmp" prefix ".tmp"in Unix.unlink path;Unix.mkdir path 0o700;
  let path=Unix.realpath path in Fun.protect ~finally:(fun()->remove_tree path)(fun()->run path)
let pause seconds=Lwt_main.run(Lwt_unix.sleep seconds)
let until ?(seconds=12.) ~predicate fn=
  let deadline=Unix.gettimeofday()+.seconds in
  let rec loop ()=let value=fn()in if predicate value then value else
    if Unix.gettimeofday()>=deadline then failwith"condition did not settle"else(pause 0.05;loop())in loop()
let wait ?seconds fn=ignore(until ?seconds ~predicate:Fun.id fn)
type result={code:int;stdout:string;stderr:string}
type child={child_pid:int;new_session:bool;status:Unix.process_status Lwt.t;
  stdout_buffer:Buffer.t;stderr_buffer:Buffer.t;stdin_channel:Lwt_io.output_channel;
  stdout_channel:Lwt_io.input_channel;stderr_channel:Lwt_io.input_channel;
  output:unit Lwt.t;input:unit Lwt.t;timeout:float}
let status_code=function Unix.WEXITED code->code|Unix.WSIGNALED signal|Unix.WSTOPPED signal->if signal<0 then signal else -signal
let spawn ?cwd ?(env=env) ?(input="") ?(timeout=40.) ?log ?(new_session=false) program args=
  let child_in,parent_in=Unix.pipe ~cloexec:true ()in
  let parent_out,child_out=Unix.pipe ~cloexec:true ()in
  let parent_err,child_err=Unix.pipe ~cloexec:true ()in
  let descriptors=[child_in;parent_in;parent_out;child_out;parent_err;child_err]in
  let child_pid=match Unix.fork()with
    | 0->(try
        if new_session then ignore(Unix.setsid());
        Unix.dup2 child_in Unix.stdin;Unix.dup2 child_out Unix.stdout;Unix.dup2 child_err Unix.stderr;
        List.iter Unix.close descriptors;
        Option.iter Unix.chdir cwd;
        Option.iter(fun path->Unix.putenv"PATH"path)(List.assoc_opt"PATH"env);
        Unix.execvpe program(Array.of_list(program::args))(Array.of_list(List.map(fun(k,v)->k^"="^v)env))
      with error->prerr_endline(Printexc.to_string error);Unix._exit 127)
    | child_pid->child_pid
    | exception error->List.iter Unix.close descriptors;raise error in
  List.iter Unix.close[child_in;child_out;child_err];
  let stdin_channel=Lwt_io.of_unix_fd ~mode:Lwt_io.output parent_in
  and stdout_channel=Lwt_io.of_unix_fd ~mode:Lwt_io.input parent_out
  and stderr_channel=Lwt_io.of_unix_fd ~mode:Lwt_io.input parent_err in
  let status=Lwt_unix.waitpid [] child_pid >|=snd in
  let stdout_buffer=Buffer.create 4096 and stderr_buffer=Buffer.create 4096 in
  let capture channel buffer=
    let chunk=Bytes.create 4096 in
    let rec loop ()=Lwt_io.read_into channel chunk 0 4096 >>=fun count->if count=0 then Lwt.return_unit else(
      Buffer.add_subbytes buffer chunk 0 count;
      Option.iter(fun path->let fd=Unix.openfile path[Unix.O_WRONLY;Unix.O_CREAT;Unix.O_APPEND]0o600 in
        Fun.protect ~finally:(fun()->Unix.close fd)(fun()->Monty.State_store.write_all fd(Bytes.sub_string chunk 0 count)))log;
      loop())in loop()in
  let output=Lwt.join[capture stdout_channel stdout_buffer;capture stderr_channel stderr_buffer]in
  let input=Lwt.catch(fun()->Lwt_io.write stdin_channel input >>=fun()->Lwt_io.close stdin_channel)(function Unix.Unix_error((Unix.EPIPE|Unix.EBADF),_,_)|Lwt_io.Channel_closed _->Lwt.return_unit|error->Lwt.fail error)in
  {child_pid;new_session;status;stdin_channel;stdout_channel;stderr_channel;stdout_buffer;stderr_buffer;output;input;timeout}
let pid child=child.child_pid
let poll child=match Lwt.state child.status with Lwt.Sleep->None|Return status->Some(status_code status)|Fail error->raise error
let signal child signal=
  if child.new_session || poll child=None then
    try Unix.kill(if child.new_session then -pid child else pid child)signal
    with Unix.Unix_error(Unix.ESRCH,_,_)->()
let terminate child=signal child Sys.sigterm
let kill child=signal child Sys.sigkill
let await ?timeout child=
  let timeout=Option.value ~default:child.timeout timeout in
  let close ()=Lwt.join(List.map(fun channel->Lwt.catch(fun()->Lwt_io.close channel)(fun _->Lwt.return_unit))[child.stdout_channel;child.stderr_channel]) >>=fun()->
    Lwt.catch(fun()->Lwt_io.close child.stdin_channel)(fun _->Lwt.return_unit)in
  Lwt_main.run(Lwt.catch(fun()->Lwt_unix.with_timeout timeout(fun()->Lwt.both (Lwt.protected child.status)(Lwt.join[child.output;child.input])) >>=fun(status,())->
    close() >|=fun()->{code=status_code status;stdout=Buffer.contents child.stdout_buffer;stderr=Buffer.contents child.stderr_buffer})
    (fun error->kill child;child.status >>=fun _->close() >>=fun()->Lwt.fail error))
let run ?cwd ?env ?input ?timeout ?new_session program args=await(spawn ?cwd ?env ?input ?timeout ?new_session program args)
let sdk_python home=
  let interpreter=Monty.Conversation_store.get(Monty.Process.command_exists_with_arguments"python3")in
  let candidates=[Filename.dirname Sys.executable_name/"fake_claude_sdk";"fake_claude_sdk";"test/fake_claude_sdk"]in
  let sdk=List.find Sys.file_exists candidates |>Unix.realpath in
  let path=home/"sdk-python"in
  write path("#!/bin/sh\nexport PYTHONPATH="^Monty.Shell.quote sdk^"\nexport PYTHONDONTWRITEBYTECODE=1\nexec "^Monty.Shell.quote interpreter^" \"$@\"\n");Unix.chmod path 0o700;path
exception Http_error of int*json
let http_error expected run=match run()with
  | _->failwith("expected HTTP "^string_of_int expected)
  | exception Http_error(code,body)->check(code=expected)(Printf.sprintf"expected HTTP %d, got %d: %s"expected code(Yojson.Safe.to_string body))
module Client=struct
  type t={home:string;url:string;token:string}
  let create home={home;url=s"url"(Yojson.Safe.from_file(home/".monty/client-service.json"));token=read(home/".monty/client-service-token")}
  let raw_lwt ?data ?(headers=[]) ?meth client path=
    let headers=merge_env["Authorization","Bearer "^client.token]headers |>Cohttp.Header.of_list in
    let meth=Option.value ~default:(if data=None then`GET else`POST)meth in
    let body=Cohttp_lwt.Body.of_string(Option.value ~default:""data)in
    Lwt_unix.with_timeout 40.(fun()->Cohttp_lwt_unix.Client.call ~headers ~body meth(Uri.of_string(client.url^path)) >>=fun(response,body)->
      Cohttp_lwt.Body.to_string body >|=fun text->Cohttp.Code.code_of_status(Cohttp.Response.status response),text)
  let raw ?data ?headers ?meth client path=Lwt_main.run(raw_lwt ?data ?headers ?meth client path)
  let req_lwt ?data ?(headers=[]) client path=
    let headers=if data=None then headers else merge_env["Content-Type","application/json"]headers in
    raw_lwt ?data:(Option.map Yojson.Safe.to_string data) ~headers client("/v1/"^path) >|=fun(code,text)->
    let json=try Yojson.Safe.from_string text with Yojson.Json_error _->`String text in
    if code>=400 then raise(Http_error(code,json));json
  let req ?data ?headers client path=Lwt_main.run(req_lwt ?data ?headers client path)
  let cmd_lwt ?(status="finished") client key request=
    req_lwt ~data:request client("commands/"^key) >>=fun _->
    let deadline=Unix.gettimeofday()+.12. in
    let rec wait ()=req_lwt client("commands/"^key) >>=fun receipt->
      if List.mem(s"status"receipt)["accepted";"running"]then(
        if Unix.gettimeofday()>deadline then failwith"command did not settle";
        Lwt_unix.sleep 0.05 >>=wait)
      else(check(s"status"receipt=status)("unexpected command receipt: "^Yojson.Safe.to_string receipt);Lwt.return(field"result"receipt))in wait()
  let cmd ?(status="finished") client key request=
    ignore(req ~data:request client("commands/"^key));
    let receipt=until ~predicate:(fun r->not(List.mem(s"status"r)["accepted";"running"])) (fun()->req client("commands/"^key))in
    check(s"status"receipt=status)("unexpected command receipt: "^Yojson.Safe.to_string receipt);field"result"receipt
  let parallel_cmd ?status client key request ~count=
    Lwt_main.run(Lwt_list.map_p(fun _->cmd_lwt ?status client key request)(List.init count Fun.id))
  let parallel_req client requests=
    Lwt_main.run(Lwt_list.map_p(fun(path,data)->req_lwt ~data client path)requests)
  let revision client cid=field"revision"(req client("conversations/"^cid))
  let history client cid=
    let rec page offset current=
      let more=req client(Printf.sprintf"conversations/%s/history?offset=%d"cid offset)in
      let current=match current with None->more|Some current->
        put"sessions"(`List(List.map2(fun saved fresh->puts["records",`List(xs"records"saved@xs"records"fresh);"next_offset",field"next_offset"fresh]saved)(xs"sessions"current)(xs"sessions"more)))current in
      if List.for_all(fun sess->field"next_offset"sess=`Null)(xs"sessions"more)then current else page(offset+8)(Some current)in page 0 None
end
module Service=struct
  type t={home:string;config:string;mutable env:(string*string)list;mutable process:child option;mutable client_value:Client.t option}
  let client service=Option.get service.client_value
  let install_actor path=
    if not(Monty.Home.contains_substring(read path)"# owned-native-fixture")then(
      let original=path^".original"in write original(read path);Unix.chmod original 0o700;
      let proxy=Filename.dirname Sys.executable_name/"owned_fake_appserver.exe" |>Unix.realpath in
      write path("#!/bin/sh\n# owned-native-fixture\nif [ \"${1-}\" = app-server ] && [ \"${2-}\" = --listen ]; then\n  exec "^Monty.Shell.quote proxy^" "^Monty.Shell.quote original^" \"$3\"\nfi\nexec "^Monty.Shell.quote original^" \"$@\"\n");Unix.chmod path 0o700)
  let start ?port service=
    let state=service.home/".monty/client-service.json"in if Sys.file_exists state then Unix.unlink state;
    let process=spawn ~env:service.env ~log:(service.home/"service.log") bin(["service";"serve";"--home";service.home;"--config";service.config]@(match port with None->[]|Some port->["--port";string_of_int port]))in
    service.process<-Some process;
    wait ~seconds:5.(fun()->if poll process<>None then failwith(read(service.home/"service.log"));Sys.file_exists state);
    service.client_value<-Some(Client.create service.home)
  let create ?(extra=[]) home config=
    Yojson.Safe.from_file config |>xs"profiles" |>List.iter(fun profile->
      let command=match field"command"profile with `String command->command|_->""in
      if s"harness"profile="codex" && Sys.file_exists command && Monty.State_path.is_contained ~root:home command then install_actor command);
    let inherited=List.filter(fun(key,_)->not(String.starts_with ~prefix:"MONTY_"key))env in
    let env=merge_env(merge_env inherited["MONTY_HOME",home;"CODEX_HOME",home/"codex";"MANTLE_CONTEXT",""])extra in
    let service={home;config;env;process=None;client_value=None}in start service;service
  let stop service=Option.iter(fun child->
    if poll child=None then terminate child;
    ignore(await ~timeout:25. child))service.process
  let close=stop
  let pid service=pid(Option.get service.process)
end
module Native=Fake_native
