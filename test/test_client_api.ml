open Test_support
open Lwt.Infix
let str=Monty.Json_fields.str
let list=Monty.Json_fields.list
let contains=Monty.Home.contains_substring
let json=Yojson.Safe.from_string
let json_file=Yojson.Safe.from_file
let has_field name=function `Assoc fields->List.mem_assoc name fields|_->false
let save path value=write path(Yojson.Safe.to_string value)
let first values=List.hd values
let nth values index=List.nth values index
let session chat=first(xs"sessions"chat)
let session_at chat index=nth(xs"sessions"chat)index
let history_items history index=xs"records"(session_at history index) |>List.concat_map(xs"items")
let ok result=check(result.code=0)(result.stderr^result.stdout)
let shell path body=write path("#!/bin/sh\n"^body^"\n");Unix.chmod path 0o700
let cli ?cwd ?env ?input ?(timeout=40.) args=run ?cwd ?env ?input ~timeout bin args
let setup_repo home=let repo=home/"repo"in mkdir repo;ok(run"git"["init";"-q";repo]);repo
let actor_path home=home/"fake-codex"
let actor_config home=home/"fake-codex.json"
let actor home ?(account=false) ?(version="0.160.0") sockets=
  save(actor_config home)(obj["sockets",obj(List.map(fun(key,value)->key,string value)sockets);"account",`Bool account;"version",string version;"home",string home;"mode",string"record"]);
  let path=actor_path home in
  shell path("exec "^Monty.Shell.quote(Unix.realpath Sys.executable_name)^" --fixture codex "^Monty.Shell.quote(actor_config home)^" \"$@\"");path
let codex_profile home repo command= obj["id",string"default";"harness",string"codex";"command",string command;"codex_home",string(home/"codex");"roots",`List(List.map string repo)]
let fixture_profile id socket repo=obj["id",string id;"harness",string"fixture";"socket",string socket;"roots",`List(List.map string repo)]
let set_thread server id update=Native.set_thread server id(update(Hashtbl.find server.Native.threads id))
let latest_call server name=server.Native.calls |>List.rev |>List.find_map(fun(method_name,params)->if method_name=name then Some params else None) |>Option.get
let has_call server name=List.exists(fun(method_name,_)->method_name=name)server.Native.calls
let remove_env environment key=List.remove_assoc key environment
let drop_field name=function `Assoc fields->`Assoc(List.remove_assoc name fields)|_->invalid_arg"object required"
let until_json ?seconds predicate fn=until ?seconds ~predicate fn
let terminal_log home=home/"terminal.json"

let read_byte ()=
  check(Unix.select[Unix.stdin][][]5.<> ([],[],[]))"terminal input timed out";
  let bytes=Bytes.create 1 in check(Unix.read Unix.stdin bytes 0 1=1)"terminal input missing";Bytes.get bytes 0
let actor_fixture ()=
  let config=json_file Sys.argv.(3)in
  let args=Array.to_list Sys.argv |>List.filteri(fun index _->index>=4)in
  let home=s"home"config in
  if List.filteri(fun index _->index<3)args=["app-server";"daemon";"start"]then(
    let account=Sys.getenv"CODEX_HOME"in
    print_endline(Yojson.Safe.to_string(obj["socketPath",field account(field"sockets"config);"appServerVersion",field"version"config])))
  else if s"mode"config="vim"then(
    Test_pty.set_raw Unix.stdin;
    check(Sys.getenv_opt"MONTY_CODEX_TUI_COMMAND"=None)"terminal command override leaked";
    check(Monty.Terminal_size.get Unix.stdin=(40,120))"initial native terminal size";
    let mode=read(home/"fixture-vim-mode")in
    if mode="exit"then(print_endline"startup failed";exit 7);
    print_endline("Vim: "^mode);flush stdout;
    if mode="Insert"then check(read_byte()='\027')"startup Insert was not escaped";
    print_endline("Vim: Insert ready "^mode);flush stdout;
    check(read_byte()='i')"later Insert mode was escaped";
    wait ~seconds:5.(fun()->Monty.Terminal_size.get Unix.stdin=(50,132));exit 7)
  else(
    check(List.filteri(fun index _->index<2)args=["resume";"--remote"])"native terminal was launched standalone";
    check(not(List.mem"--dangerously-bypass-approvals-and-sandbox"args))"terminal bypass leaked";
    let args=`List(List.map string args)in
    save(terminal_log home)(if field"account"config=`Bool true then obj["args",args;"account",string(Sys.getenv"CODEX_HOME")]else args))

let autostart_suite home=
  let repo=setup_repo home in mkdir(home/"codex");
  let server=Native.create(home/"codex.sock")in
  let codex=actor home [home/"codex",home/"codex.sock"]in
  let config=home/"profiles.json"in save config(obj["profiles",`List[codex_profile home[home]codex]]);
  let service=Service.create home config in
  let metadata=home/".monty/client-service.json"and log=home/".monty/client-service.log"in
  let unlocked ()=
    let fd=Unix.openfile(home/".monty/client-service.lock")[Unix.O_RDWR]0 in
    Fun.protect ~finally:(fun()->Unix.close fd)(fun()->try Unix.lockf fd Unix.F_TLOCK 0;true with Unix.Unix_error((Unix.EACCES|Unix.EAGAIN),_,_)->false)in
  let lifecycle ?(target=home) ?(env=service.env) args=cli ~cwd:repo ~env ~timeout:25.(["service"]@args@["--home";target])in
  let launch ()=cli ~cwd:repo ~env:(merge_env service.env["MONTY_CODEX_COMMAND",codex]) ~timeout:25.[]in
  let fresh=home/"fresh"in mkdir fresh;
  Fun.protect ~finally:(fun()->ignore(lifecycle ~target:fresh["stop"]);Service.close service;
    if Sys.file_exists metadata then wait ~seconds:25.(fun()->if unlocked()then true else(
      (try Unix.kill(i"pid"(json_file metadata))Sys.sigterm with Unix.Unix_error(Unix.ESRCH,_,_)->());false));Native.close server)(fun()->
    let stopped=lifecycle ~target:fresh["stop"]in ok stopped;check(contains stopped.stdout"already stopped")"fresh stop";
    check(not(Sys.file_exists(fresh/".monty")))"fresh stop created state";
    let user=fresh/"user"in mkdir user;
    let missing=lifecycle ~target:fresh ~env:(merge_env service.env["HOME",user])["start"]in check(missing.code=1&&contains missing.stderr"service start --config FILE")"missing startup config";
    (* Account directories found under HOME generate Monty's own configuration. *)
    let bin_dir=fresh/"bin"in mkdir bin_dir;shell(bin_dir/"codex")"exit 0";
    List.iter mkdir[user/".codex";user/".local";user/".local/share";user/".local/share/mantle";user/".local/share/mantle/profiles";user/".local/share/mantle/profiles/work";user/".local/share/mantle/profiles/work/codex"];
    ok(lifecycle ~target:fresh ~env:(merge_env service.env["HOME",user;"PATH",bin_dir^":"^Sys.getenv"PATH"])["start"]);
    let generated=json_file(fresh/".monty/service-profiles.json")in
    equal(List.map(fun p->s"id"p,s"codex_home"p,s"command"p,xs"roots"p)(xs"profiles"generated))
      [("codex",user/".codex",bin_dir/"codex",[string(Unix.realpath fresh)]);("codex-work",user/".local/share/mantle/profiles/work/codex",bin_dir/"codex",[string(Unix.realpath fresh)])];
    equal(List.map(s"id")(xs"profiles"(Client.req(Client.create fresh)"health")))["codex";"codex-work"];
    ok(lifecycle ~target:fresh["stop"]);
    List.iter(fun port->let result=lifecycle ~target:fresh["start";"--config";config;"--port="^port]in check(result.code=1&&contains result.stderr"port must be")"invalid service port")["-1";"65536"];
    ok(lifecycle ~target:fresh["start";"--config";config]);
    let client=Client.create fresh in
    equal(field"id"(first(xs"profiles"(Client.req client"health"))))(string"default");
    check(Hashtbl.length server.threads=0 && not(Sys.file_exists(fresh/".monty/tasks.local.json")))"starting service dispatched work";
    let epoch=field"epoch"(Client.req client"health")in equal(Client.req ~data:(obj["epoch",epoch])client"shutdown")(obj["epoch",epoch]);
    ok(lifecycle ~target:fresh["stop"]);
    List.iter(fun(key,value)->ok(cli ~env:service.env["settings";"set";key;value;"--home";home]))["harness","codex";"codex-yolo","true"];
    let configured=json_file metadata in check(s"config_path"configured=config&&i"port"configured>0)"saved startup settings";
    ok(lifecycle["stop"]);Option.iter(fun child->ignore(await ~timeout:5. child))service.process;
    let launch_env=merge_env service.env["MONTY_CODEX_COMMAND",codex]in
    let launches=List.init 4(fun _->spawn ~cwd:repo ~env:launch_env ~timeout:25. bin[])in List.iter(fun child->ok(await child))launches;
    let started=json_file metadata in
    check(field"epoch"started<>field"epoch"configured && field"url"started=field"url"configured && field"port"started=field"port"configured)"restart reused wrong startup state";
    check(Hashtbl.length server.threads=4)"concurrent plain launches lost/duplicated threads";
    let occurrences text needle=String.split_on_char '\n' text |>List.filter(fun line->contains line needle)|>List.length in
    check(occurrences(read log)"Monty client API "=1)"multiple detached services started";
    List.iter(fun(method_name,p)->if method_name="thread/start"then check(field"approvalPolicy"p=string"never"&&field"sandbox"p=string"danger-full-access")"plain launch lost selected bypass")server.calls;
    check((Unix.stat log).Unix.st_perm land 0o777=0o600)"service log permissions";
    check(not(Sys.file_exists(home/".monty/tasks.local.json")))"plain launch created task";
    ok(launch());equal(json_file metadata)started;
    let count=Hashtbl.length server.threads in
    let starts=List.init 4(fun _->spawn ~cwd:repo ~env:service.env bin["service";"start";"--home";home])in List.iter(fun child->ok(await child))starts;
    equal(Hashtbl.length server.threads)count;equal(json_file metadata)started;
    let alternate=home/"alternate.json"in write alternate(read config);
    List.iter(fun args->let result=lifecycle args in check(result.code=1&&contains result.stderr"service stop first")"live startup setting override accepted")[["start";"--config";alternate];["start";"--port";"8765"]];
    let client=Client.create home in http_error 409(fun()->Client.req ~data:(obj["epoch",field"epoch"configured])client"shutdown");equal(field"epoch"(Client.req client"health"))(field"epoch"started);
    let token=home/".monty/client-service-token"in let original=read token in
    Fun.protect ~finally:(fun()->write token original)(fun()->write token"invalid-token";
      List.iter(fun result->check(result.code=1&&contains result.stderr"bearer authentication required")"authentication failure restarted service")[launch();lifecycle["start"];lifecycle["stop"]];
      equal(Hashtbl.length server.threads)count;equal(json_file metadata)started);
    save metadata(put"pid"(`Int(Unix.getpid()))started);
    Fun.protect ~finally:(fun()->save metadata started)(fun()->
      let stops=List.init 4(fun _->spawn ~cwd:repo ~env:service.env bin["service";"stop";"--home";home])in List.iter(fun child->ok(await child))stops);
    check(unlocked()&&Hashtbl.length server.threads=count)"authenticated stop signalled wrong PID";
    let stopped=lifecycle["stop"]in ok stopped;check(contains stopped.stdout"already stopped")"repeat service stop";
    let original=read config in Fun.protect ~finally:(fun()->write config original)(fun()->
      write config"{\"profiles\":[]}";let before=read log in let result=launch()in
      check(result.code=1&&contains result.stderr"configure one to 32 profiles")"invalid saved config";check(unlocked()&&read log=before)"invalid config launched process");
    ok(launch());check(field"epoch"(json_file metadata)<>field"epoch"started)"service epoch did not change";
    check(occurrences(read log)"Monty client API "=2)"unexpected detached startup count";ok(lifecycle["stop"]);
    let listener=Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Lwt_main.run(Lwt_unix.bind listener(Unix.ADDR_INET(Unix.inet_addr_loopback,0)));Lwt_unix.listen listener 1;
    let port=match Lwt_unix.getsockname listener with Unix.ADDR_INET(_,port)->port|_->assert false in
    save metadata(puts["port",`Int 0;"url",string("http://127.0.0.1:"^string_of_int port)](json_file metadata));
    Lwt.async(fun()->Lwt.bind(Lwt_unix.accept listener)(fun(connection,_)->Lwt_unix.setsockopt_optint connection Unix.SO_LINGER(Some 0);Lwt_unix.close connection));
    Fun.protect ~finally:(fun()->Lwt_main.run(Lwt_unix.close listener))(fun()->ok(launch()));
    print_endline"client service autostart, startup reuse, concurrency, authentication and stale metadata recovery passed")

let scope_suite home=
  let repo=setup_repo home in let source=Native.create(home/"source.sock")and other=Native.create(home/"other.sock")in
  let config=home/"profiles.json"in
  let profiles=ref[fixture_profile"source"(home/"source.sock")[repo];fixture_profile"alias"(home/"source.sock")[repo]]in
  let save_config()=save config(obj["allow_fixture",`Bool true;"profiles",`List !profiles])in save_config();
  let service=Service.create home config in
  Fun.protect ~finally:(fun()->Service.close service;Native.close source;Native.close other)(fun()->
    let c=Service.client service in let request=obj["op",string"start";"profile",string"source";"context",obj["workspaces",`List[string repo]]]in
    let started=Client.cmd c"start-scoped"request in let cid=s"id"started and sess=session started in
    equal(field"scope"sess)(string(Digestif.SHA256.(to_hex(digest_string(home/"source.sock")))));
    Service.stop service;profiles:=put"socket"(string(home/"other.sock"))(first !profiles)::List.tl !profiles;save_config();Service.start service;
    let c=Service.client service in ignore(Client.cmd c"check-changed-scope"(obj["op",string"sync";"conversation",string cid]));
    wait(fun()->s"state"(field"coverage"(session(Client.req c("conversations/"^cid))))="unavailable");
    ignore(Client.cmd ~status:"rejected"c"wrong-scope"(obj["op",string"send";"conversation",string cid;"revision",Client.revision c cid;"text",string"must not send"]));
    equal other.calls [];check(xs"records"(session(Client.history c cid))<>[])"scope change lost archive";
    let attached=Client.cmd c"attach-alias"(puts["op",string"attach";"profile",string"alias";"native_id",field"native_id"sess]request)in
    check(s"id"attached=cid&&List.length(xs"sessions"attached)=1&&s"profile"(session attached)="alias")"profile alias duplicated source";
    ignore(Client.cmd c"purge-scope"(obj["op",string"purge";"conversation",string cid;"confirm",`Bool true]));
    Service.stop service;profiles:=put"socket"(string(home/"source.sock"))(first !profiles)::List.tl !profiles;save_config();Service.start service;
    let before=List.length source.calls in ignore(Client.cmd ~status:"rejected"(Service.client service)"reimport-renamed"(puts["op",string"import";"native_id",field"native_id"sess]request));
    equal(List.length source.calls)before;print_endline"client API account scope and tombstones passed")

let recovery_suite home=
  let repo=setup_repo home in let source=Native.create(home/"source.sock")and dest=Native.create(home/"dest.sock")in
  let config=home/"profiles.json"in save config(obj["allow_fixture",`Bool true;"profiles",`List[fixture_profile"source"source.path[repo];fixture_profile"dest"dest.path[repo]]]);
  let service=Service.create ~extra:["MONTY_FAULT_INJECT","conversation-after-batch"]home config in
  Fun.protect ~finally:(fun()->Service.close service;Native.close source;Native.close dest)(fun()->
    let c=Service.client service in let request=obj["op",string"start";"profile",string"source";"context",obj["workspaces",`List[string repo]]]in
    ignore(Client.cmd ~status:"outcome_unknown"c"batch-fault"request);
    let cid=s"id"(first(list(Client.req c"conversations")))in let before=Client.req c("conversations/"^cid)in
    equal(i"checkpoint"(session before))0;
    let directory=home/".monty/conversations"/cid/"sessions"in
    let orphans=Sys.readdir directory |>Array.to_list |>List.concat_map(fun sid->let batches=directory/sid/"batches"in Sys.readdir batches |>Array.to_list |>List.map(fun name->batches/name))in
    check(orphans<>[])"batch fault did not retain orphan evidence";
    Service.stop service;service.env<-remove_env service.env"MONTY_FAULT_INJECT";Service.start service;
    let c=Service.client service in check(source.clients=[])"restart recreated native connection";
    ignore(Client.cmd c"recover-batch"(obj["op",string"sync";"conversation",string cid]));
    wait(fun()->i"checkpoint"(session(Client.req c("conversations/"^cid)))>0);
    check(List.for_all(fun path->not(Sys.file_exists path))orphans)"uncommitted orphan survived recovery";
    ignore(Client.cmd ~status:"outcome_unknown"c"batch-fault"request);equal(Hashtbl.length source.threads)1;
    Service.stop service;service.env<-merge_env service.env["MONTY_FAULT_INJECT","conversation-after-destination"];Service.start service;
    let c=Service.client service in
    let switch=obj["op",string"switch";"conversation",string cid;"revision",Client.revision c cid;"mode",string"branch";"profile",string"dest";"context",obj["workspaces",`List[string repo]]]in
    ignore(Client.cmd ~status:"outcome_unknown"c"destination-fault"switch);
    let intermediate=Client.req c("conversations/"^cid)in
    check(List.length(xs"sessions"intermediate)=2&&field"selected_session"intermediate=field"id"(session intermediate))"uncertain transfer selected destination";
    equal(s"status"(first(xs"transfers"intermediate)))"accepted";
    Service.stop service;service.env<-remove_env service.env"MONTY_FAULT_INJECT";Service.start service;
    let c=Service.client service in ignore(Client.cmd ~status:"outcome_unknown"c"destination-fault"switch);equal(Hashtbl.length dest.threads)1;
    let recovered=Client.cmd c"recover-existing"(obj["op",string"recover_switch";"conversation",string cid;"revision",Client.revision c cid;"transfer",field"id"(first(xs"transfers"intermediate));"confirm_branch",`Bool true])in
    check(field"selected_session"recovered=field"id"(session_at recovered 1)&&Hashtbl.length dest.threads=1)"recovery created duplicate destination";
    dest.drop_open<-true;
    let request=put"revision"(Client.revision c cid)switch in
    ignore(Client.cmd ~status:"outcome_unknown"c"unknown-destination"request);equal(Hashtbl.length dest.threads)2;
    ignore(Client.cmd ~status:"outcome_unknown"c"unknown-destination"request);equal(Hashtbl.length dest.threads)2;
    ignore(Client.cmd c"restart-receipt"(obj["op",string"sync";"conversation",string cid]));
    Native.emit dest(obj["id",`Int 902;"method",string"item/commandExecution/requestApproval";"params",obj["threadId",field"native_id"(session_at intermediate 1)]]);
    wait(fun()->match field"pending"(session_at(Client.req c("conversations/"^cid))1)with `List(_::_)->true|_->false);
    Service.stop service;
    let receipt=home/".monty/conversation-commands/restart-receipt.json"in save receipt(put"status"(string"running")(json_file receipt));
    let count=List.length source.calls+List.length dest.calls in Service.start service;
    let c=Service.client service in equal(s"status"(Client.req c"commands/restart-receipt"))"outcome_unknown";
    wait(fun()->field"pending"(session_at(Client.req c("conversations/"^cid))1)=`List[]);
    check(count<=List.length source.calls+List.length dest.calls)"native call history lost";
    print_endline"client API durable crash recovery and transfer recovery passed")

let terminal_profiles_suite home=
  let repo=setup_repo home in let personal=home/"codex"and work=home/"work"in mkdir personal;mkdir work;
  let servers=[Native.create(home/"personal.sock");Native.create(home/"work.sock")]in
  let sockets=[personal,home/"personal.sock";work,home/"work.sock"]in
  let codex=actor home ~account:true sockets in
  let profiles=ref(List.map(fun(id,account)->puts["id",string id;"codex_home",string account](codex_profile home[home]codex))["default",personal;"company",work])in
  let config=home/"profiles.json"in let save_config()=save config(obj["profiles",`List !profiles])in save_config();
  let service=Service.create home config in
  let environment=merge_env service.env["MONTY_CODEX_COMMAND",codex]in
  let work_env=merge_env environment["CODEX_HOME",work;"MANTLE_CONTEXT","work";"_MANTLE_SELECTED_HOME",work;"_MANTLE_PREV_STATE","export";"_MANTLE_PREV_HOME",personal]in
  let launch ?(env=environment) args=cli ~cwd:repo ~env ~timeout:25.(args@["--home";home])in
  let attached result account=
    ok result;let recorded=json_file(terminal_log home)in let args=xs"args"recorded |>List.map str in
    equal(s"account"recorded)account;
    check(List.filteri(fun index _->index<2)args=["resume";"--remote"]&&String.starts_with ~prefix:"unix://"(nth args 2)&&not(List.mem(nth args 2)(List.map(fun(_,socket)->"unix://"^socket)sockets)))"terminal attached shared socket";
    check(Sys.file_exists(account/"skills/monty/SKILL.md"))"account skill missing";List.hd(List.rev args)in
  let rejected args env message=
    let before=List.map(fun server->Hashtbl.length server.Native.threads)servers in let result=launch ~env args in
    check(result.code=1&&contains result.stderr message)("wrong account selection accepted: "^result.stderr);
    equal(List.map(fun server->Hashtbl.length server.Native.threads)servers)before in
  let terminal=["terminal";"--codex-command";codex]in
  Fun.protect ~finally:(fun()->Service.close service;List.iter Native.close servers)(fun()->
    ok(cli ~env:environment["settings";"set";"harness";"codex";"--home";home]);
    let personal_id=attached(launch[])personal and work_id=attached(launch ~env:work_env[])work in
    equal personal_id work_id;ignore(attached(launch ~env:work_env["continue";work_id])work);ignore(attached(launch["continue";personal_id])personal);
    equal(List.map(fun server->Hashtbl.length server.Native.threads)servers)[1;1];
    let shell_home=home/"shell"in mkdir shell_home;Unix.symlink personal(shell_home/".codex");
    ignore(attached(launch ~env:(merge_env(remove_env environment"CODEX_HOME")["HOME",shell_home])[])personal);
    let alias=home/"work-alias"in Unix.symlink work alias;
    ignore(attached(launch ~env:(merge_env work_env["CODEX_HOME",alias])terminal)work);
    rejected(terminal@["--profile";"default"])work_env"terminal account scope does not match";
    rejected[](merge_env work_env["MONTY_API_PROFILE","default"])"terminal account scope does not match";
    rejected(terminal@["--profile";"missing"])work_env"terminal account scope does not match";
    ignore(attached(launch ~env:(merge_env work_env["MONTY_API_PROFILE","default"])(terminal@["--profile";"company"]))work);
    let unknown=home/"unknown"in mkdir unknown;rejected[](merge_env work_env["CODEX_HOME",unknown])"no service profile matches";
    check(not(Sys.file_exists(home/".monty/tasks.local.json")))"account switching created task";
    Service.stop service;profiles:= !profiles@[put"id"(string"company-alias")(nth !profiles 1)];save_config();Service.start service;
    rejected[]work_env"multiple service profiles match";
    ignore(attached(launch ~env:(merge_env work_env["MONTY_API_PROFILE","company"])terminal)work);
    print_endline"terminal account scope, Mantle switching, home aliases and explicit profile selection passed")

let warm_archive_suite home=
  let repo=setup_repo home in mkdir(home/"codex");let server=Native.create(home/"native.sock")in
  let codex=actor home[home/"codex",server.path]in let config=home/"profiles.json"in save config(obj["profiles",`List[codex_profile home[repo]codex]]);
  let service=Service.create home config in
  Fun.protect ~finally:(fun()->Service.close service;Native.close server)(fun()->
    let result=Client.cmd(Service.client service)"start"(obj["op",string"start";"profile",string"default";"context",obj["workspaces",`List[string repo]]])in
    let cid=s"id"result in Service.stop service;
    let path=home/".monty/conversations"/cid/"conversation.json"in let manifest=json_file path in let sess=session manifest in
    let directory=Filename.dirname path/"sessions"/s"id"sess/"batches"in
    let batch=obj["schema",`Int 1;"normalizer",`Int 1;"raw",obj["padding",string(String.make(1024*1024)'x')];"items",`List[]]in
    let ids=List.init 192(fun index->"archive-"^string_of_int index)in List.iter(fun bid->save(directory/(bid^".json"))batch)ids;
    let batches=xs"batches"sess@List.map string ids in save path(put"sessions"(`List[puts["batches",`List batches;"checkpoint",`Int(List.length batches)]sess])manifest);
    let native=s"native_id"sess in
    let update text=set_thread server native(put"turns"(`List[obj["id",string"turn-1";"status",string"completed";"items",`List[obj["id",string"message";"type",string"agentMessage";"text",string text]]]]))in update"first";
    let started=Unix.gettimeofday()in Service.start service;let c=Service.client service in ignore(Client.cmd c"cold"(obj["op",string"sync";"conversation",string cid]));
    let cold=Unix.gettimeofday()-.started in
    let warm=List.init 3(fun index->let started=Unix.gettimeofday()in ignore(Client.cmd c("warm-"^string_of_int index)(obj["op",string"sync";"conversation",string cid]));
      ok(cli ~cwd:repo ~env:service.env ~timeout:15.["terminal";"--home";home;"--codex-command";codex]);Unix.gettimeofday()-.started)in
    check(List.for_all(fun elapsed->elapsed<max 0.7(cold/.3.))warm)(Printf.sprintf"warm archive latency regressed: cold=%f warm=%s"cold(String.concat","(List.map string_of_float warm)));
    let latest_revision()=let current=session(json_file path)in let bid=List.hd(List.rev(xs"batches"current)) |>str in
      i"revision"(first(xs"items"(json_file(directory/(bid^".json")))))in
    equal(latest_revision())1;update"second";ignore(Client.cmd c"changed"(obj["op",string"sync";"conversation",string cid]));equal(latest_revision())2;
    Service.stop service;update"third";Service.start service;ignore(Client.cmd(Service.client service)"restarted"(obj["op",string"sync";"conversation",string cid]));equal(latest_revision())3;
    print_endline"large archive warm launch latency and revision recovery passed")

let sse_replay client cursor needle=
  let uri=Uri.of_string client.Client.url in let port=Option.get(Uri.port uri)in
  Lwt_main.run(Lwt_io.open_connection(Unix.ADDR_INET(Unix.inet_addr_loopback,port)) >>=fun(ic,oc)->
    Lwt.finalize(fun()->
      Lwt_io.write oc(Printf.sprintf"GET /v1/events HTTP/1.1\r\nHost: 127.0.0.1:%d\r\nAuthorization: Bearer %s\r\nLast-Event-ID: %s\r\n\r\n"port client.token cursor) >>=fun()->Lwt_io.flush oc >>=fun()->
      Lwt_unix.with_timeout 10.(fun()->let rec lines()=Lwt_io.read_line_opt ic >>=function
        | None->Lwt.fail(Failure"SSE closed before replaying native event")
        | Some line->if String.starts_with ~prefix:"data:"line&&contains line needle then Lwt.return_unit else lines()in lines()))
      (fun()->Monty.Codex_rpc.close_channels ic oc))

let tty_client home repo environment native initial=
  write(home/"fixture-vim-mode")initial;
  let master,slave=Test_pty.open_pair()in Test_pty.resize slave ~rows:40 ~cols:120;
  let child=match Unix.fork()with
    | 0->(try ignore(Unix.setsid());Unix.dup2 slave Unix.stdin;Unix.dup2 slave Unix.stdout;Unix.dup2 slave Unix.stderr;
        Unix.close master;Unix.close slave;Unix.chdir repo;
        Unix.execve bin[|bin;"continue";native;"--home";home|](Array.of_list(List.map(fun(k,v)->k^"="^v)environment))
      with _->Unix._exit 127)
    | child->child in
  Unix.close slave;Unix.set_nonblock master;
  let status=Lwt_unix.waitpid[]child in let output=Buffer.create 4096 in
  Fun.protect ~finally:(fun()->
    (match Lwt.state status with Lwt.Sleep->(try Unix.kill(-child)Sys.sigkill with Unix.Unix_error(Unix.ESRCH,_,_)->());ignore(Lwt_main.run status)|_->());Unix.close master)(fun()->
    let marker=if initial="exit"then"startup failed"else"ready "^initial in
    wait ~seconds:10.(fun()->
      let bytes=Bytes.create 8192 in
      (try let count=Unix.read master bytes 0 8192 in if count>0 then Buffer.add_subbytes output bytes 0 count
       with Unix.Unix_error((Unix.EAGAIN|Unix.EWOULDBLOCK|Unix.EIO),_,_)->());
      contains(Buffer.contents output)marker);
    if initial<>"exit"then(Test_pty.resize master ~rows:50 ~cols:132;Unix.kill child(Monty.Terminal_size.winch());ignore(Unix.write_substring master"i"0 1));
    let _,status=Lwt_main.run(Lwt_unix.with_timeout 10.(fun()->Lwt.protected status))in
    check(status=Unix.WEXITED 7)("native terminal exit propagation: "^Buffer.contents output))

let rec fake_suite home=
  let repo=setup_repo home in let native_home=home/"codex"in mkdir native_home;
  let server=Native.create(home/"codex.sock")and second=Native.create(home/"second.sock")in
  let codex=actor home ~version:"0.999.0"[native_home,server.path]in
  let config=home/"profiles.json"in save config(obj["allow_fixture",`Bool true;"profiles",`List[
    codex_profile home[repo;home]codex;fixture_profile"second"second.path[repo];fixture_profile"filesystem-root"server.path["/"]]]);
  let service=Service.create home config in
  Fun.protect ~finally:(fun()->Service.close service;Native.close server;Native.close second)(fun()->
    let c=ref(Service.client service)in let req ?data ?headers path=Client.req ?data ?headers !c path in
    let cmd ?status key value=Client.cmd ?status !c key value in
    let history cid=Client.history !c cid in let revision cid=Client.revision !c cid in
    let chat cid=req("conversations/"^cid)in let pending cid=match field"pending"(session(chat cid))with`List values->values|_->[]in
    http_error 401(fun()->req ~headers:["Authorization","no"]"health");
    http_error 403(fun()->req ~headers:["Origin","https://evil.example"]"health");
    http_error 403(fun()->req ~headers:["Host","evil.example"]"health");
    check(not(has_field"storage_scope"(req"health")))"removed storage scope advertised";
    let port=Option.get(Uri.port(Uri.of_string !c.url))in
    http_error 401(fun()->req ~headers:["Authorization","";"Cookie",Printf.sprintf"monty_session_%d=%s"port !c.token;"Origin",!c.url]"health");
    List.iter(fun route->http_error 404(fun()->req ~data:(obj[])route))["browser/pair";"browser/session";"browser/logout"];
    List.iter(fun path->let status,_=Client.raw ~headers:["Authorization",""]!c path in equal status 401;
      let status,_=Client.raw !c path in equal status 404)["/";"/main.bc.js"];
    List.iter(fun args->let result=cli ~env:service.env args in check(result.code<>0)"removed dashboard CLI accepted")[["web";"--help=plain";"--home";home];["guide";"web-client"]];
    equal(xs"tasks"(req"inventory"))[];check(not(Sys.file_exists(home/".monty/tasks.json")))"inventory created task state";
    let removed=cmd ~status:"rejected""removed-classify"(obj["op",string"classify";"profile",string"default";"text",string"This feature has been removed.";"seconds",`Int 30])in
    check(i"code"removed=400&&s"message"removed="unknown command")"classification command remained";
    check(Hashtbl.length server.threads=0&&not(Sys.file_exists(home/".monty/conversation-executions")))"removed command dispatched native process";
    let request=obj["op",string"start";"profile",string"default";"context",obj["workspaces",`List[string repo];"instructions",string"trusted current instructions";"environment",obj["MONTY_JOB_TITLE",string"isolated"]]]in
    equal(xs"codex_tested_versions"(req"health"))(`List[string"0.160.0";string"0.160.1"]|>list);
    let receipts=Client.parallel_cmd !c"start-once"request ~count:8 in
    let opened=first receipts in let cid=s"id"opened and native=s"native_id"(session opened)in
    check(List.for_all(fun receipt->s"id"receipt=cid)receipts&&Hashtbl.length server.threads=1)"idempotent start duplicated native thread";
    http_error 409(fun()->req ~data:(put"title"(string"different")request)"commands/start-once");
    let params=latest_call server"thread/start"in
    equal(s"historyMode"params)"legacy";check(Sys.file_exists(native_home/"skills/monty/SKILL.md"))"Codex skill missing";
    check(has_call server"skills/list")"skills not reloaded";
    let config=field"config"params in
    check(not(List.exists(fun(key,_)->String.starts_with ~prefix:"mcp_servers.monty"key)(match config with`Assoc fields->fields|_->[])))"legacy MCP config projected";
    equal(field"MONTY_JOB_TITLE"(field"shell_environment_policy.set"config))(string"isolated");
    check(s"approvalPolicy"params="on-request"&&s"sandbox"params="workspace-write")"unsafe default execution policy";
    check(xs"records"(session(history cid))<>[])"initial history missing";
    let send=obj["op",string"send";"conversation",string cid;"revision",`Int 0;"selected_session",field"selected_session"opened;"text",string"one turn"]in
    ignore(Client.parallel_cmd !c"send-once"send ~count:8);equal server.sends 1;
    ignore(cmd ~status:"rejected""wrong-selected-session"(put"selected_session"(string"another-session")send));
    ignore(cmd ~status:"rejected""legacy-stale-send"(drop_field"selected_session"send));equal server.sends 1;
    let cursor=s"cursor"(req"health")in
    let delta=obj["method",string"item/agentMessage/delta";"params",obj["threadId",string native;"delta",string"SSE fixture"]]in
    Native.emit server delta;wait(fun()->s"cursor"(req"health")<>cursor);sse_replay !c cursor"SSE fixture";
    for _=1 to 300 do Native.emit server delta done;
    let sequence value=String.split_on_char ':'value |>List.rev |>List.hd |>int_of_string in
    wait(fun()->sequence(s"cursor"(req"health"))>sequence cursor+256);
    http_error 409(fun()->req ~headers:["Last-Event-ID",cursor]"events");
    ignore(cmd"initial-sync"(obj["op",string"sync";"conversation",string cid]));
    let snapshot=obj["thread",Hashtbl.find server.threads native]in
    let raw_count()=xs"records"(session(history cid)) |>List.filter(fun batch->field"raw"batch=snapshot)|>List.length in
    let initial_raw_count=raw_count()in check(initial_raw_count>0)"native snapshot not retained";
    List.iter(fun item->Native.emit server(obj["method",string"item/completed";"params",obj["threadId",string native;"turnId",string"turn-1";"item",item]]))[
      obj["id",string"a-1";"type",string"agentMessage";"text",string"provisional"];
      obj["id",string"abandoned-live";"type",string"agentMessage";"text",string"not in snapshot"]];
    wait(fun()->List.exists(fun item->s"source_id"item="abandoned-live")(history_items(history cid)0));
    ignore(cmd"identical-sync"(obj["op",string"sync";"conversation",string cid]));
    let reconciled=session(history cid)in let latest=Hashtbl.create 16 in List.iter(fun item->Hashtbl.replace latest(s"source_id"item)item)(List.concat_map(xs"items")(xs"records"reconciled));
    let answer=Hashtbl.find latest"a-1"in
    check(s"text"(field"content"answer)="hello"&&field"provisional_order"answer<>`Bool true)"identical snapshot did not reconcile provisional revision";
    check(List.for_all(fun key->not(contains(str key)"abandoned-live"))(xs"effective"reconciled))"abandoned live item remained effective";
    equal(raw_count())initial_raw_count;
    let image=repo/"temporary.png"in write image"owned attachment bytes";
    set_thread server native(fun thread->put"turns"(`List(xs"turns"thread@[obj["id",string"terminal-turn";"status",string"completed";"items",`List[
      obj["id",string"terminal";"type",string"userMessage";"content",`List[obj["type",string"localImage";"path",string image]]];
      obj["id",string"compact";"type",string"contextCompaction"];
      obj["id",string"opaque";"type",string"futureType";"data",obj(List.map(fun key->key,string"do-not-store")["access_token";"accessToken";"refreshToken";"idToken"])]]]]))thread);
    Native.emit server(obj["method",string"item/completed";"params",obj["threadId",string native;"item",obj["id",string"terminal"]]]);
    let items()=history_items(history cid)0 in
    let captured=until ~predicate:(List.exists(fun item->s"source_id"item="terminal"))items in
    let attachment=List.find(fun item->s"source_id"item="terminal")captured |>xs"attachments" |>first in
    Unix.unlink image;equal(read(home/".monty/conversation-blobs"/s"sha256"attachment))"owned attachment bytes";
    check(not(contains(Yojson.Safe.to_string(history cid))"do-not-store"))"credential aliases leaked into owned archive";
    set_thread server native(fun thread->let turns=xs"turns"thread in let last=List.hd(List.rev turns)in
      let edited=put"items"(`List(put"content"(`List[obj["type",string"text";"text",string"edited"]])(first(xs"items"last))::List.tl(xs"items"last)))last in
      put"turns"(`List(List.rev(List.tl(List.rev turns))@[edited]))thread);
    wait(fun()->List.length(List.filter(fun item->s"source_id"item="terminal")(items()))=2);
    set_thread server native(fun thread->put"turns"(`List[first(xs"turns"thread)])thread);
    wait(fun()->List.for_all(fun key->not(contains(str key)"terminal"))(xs"effective"(session(chat cid))));
    equal(List.length(List.filter(fun item->s"source_id"item="terminal")(items())))2;
    Native.emit server(obj["id",`Int 900;"method",string"item/commandExecution/requestApproval";"params",obj["threadId",string native;"turnId",string"turn-1";"itemId",string"a-1"]]);
    let current=until ~predicate:(fun _->pending cid<>[])(fun()->chat cid)in let approval=first(pending cid)in
    let answer=obj["op",string"respond";"conversation",string cid;"revision",field"revision"current;"pending_id",field"id"approval;"pending_revision",`Int 1;"response",obj["decision",string"decline"]]in
    ignore(cmd ~status:"rejected""invalid-approval"(put"response"(obj["decision",string"allow-everything"])answer));
    equal(s"state"(first(pending cid)))"pending";equal server.answers [];
    Native.emit server(obj["method",string"item/completed";"params",obj["threadId",string native;"turnId",string"turn-1";"item",obj["id",string"while-pending";"type",string"agentMessage";"text",string"waiting for approval"]]]);
    wait(fun()->revision cid>field"revision"current);
    ignore(req ~data:answer"commands/answer-a");ignore(req ~data:answer"commands/answer-b");
    wait(fun()->List.length server.answers=1);wait(fun()->s"status"(req"commands/answer-b")="rejected");
    Native.emit server(obj["method",string"serverRequest/resolved";"params",obj["threadId",string native;"requestId",`Int 900]]);wait(fun()->pending cid=[]);
    Native.emit server(obj["id",`Int 901;"method",string"item/commandExecution/requestApproval";"params",obj["threadId",string native;"turnId",string"turn-1";"itemId",string"a-1"]]);
    let disconnected_pending=until ~predicate:((<>)[])(fun()->pending cid) |>first in
    server.drop_next<-true;let uncertain=obj["op",string"send";"conversation",string cid;"revision",revision cid;"text",string"lost reply"]in
    ignore(cmd ~status:"outcome_unknown""uncertain"uncertain);equal server.sends 2;equal(pending cid)[];
    ignore(cmd ~status:"rejected""stale-disconnected-response"(puts["revision",revision cid;"pending_id",field"id"disconnected_pending]answer));
    Service.stop service;Service.start service;c:=Service.client service;
    ignore(cmd ~status:"outcome_unknown""uncertain"uncertain);equal server.sends 2;
    http_error 409(fun()->req ~headers:["Last-Event-ID","old:1"]"events");check(server.clients=[])"restart silently recreated runtime";
    ignore(cmd"interrupt"(obj["op",string"interrupt";"conversation",string cid;"revision",revision cid;"turn_id",string"turn-2"]));
    ignore(cmd"resume-after-unknown"(obj["op",string"resume";"conversation",string cid;"revision",revision cid;"detached",`Bool true]));wait(fun()->server.clients<>[]);
    ignore(cmd ~status:"rejected""handoff"(obj["op",string"handoff";"conversation",string cid]));
    let switched=cmd"switch"(obj["op",string"switch";"mode",string"branch";"conversation",string cid;"revision",revision cid;"profile",string"second";"context",obj["workspaces",`List[string repo];"instructions",string"current trusted instructions";"objective",string"continue"]])in
    check(s"id"switched=cid&&List.length(xs"sessions"switched)=2)"cross-harness switch changed conversation identity";
    let supplied=field"package"(latest_call second"open")in equal supplied(field"package"(first(xs"transfers"switched)));
    equal(field"trust"(first(xs"historical_data"supplied)))(string"historical_data");check(second.sends=0&&server.sends=2)"transfer dispatched implementation prompt";
    wait(fun()->xs"records"(session_at(history cid)1)<>[]);
    let picker=cli ~cwd:repo ~env:service.env["terminal";"--home";home;"--profile";"default";"--mode";"last"]in
    check(picker.code=1&&contains picker.stderr"no registered session")"picker selected wrong harness";
    let returned=cmd"switch-back"(obj["op",string"switch";"mode",string"branch";"conversation",string cid;"revision",revision cid;"profile",string"default";"context",obj["workspaces",`List[string repo]]])in
    let transferred=until ~predicate:(fun sess->List.exists(fun item->s"kind"item="contextTransfer"&&field"provisional_order"item<>`Bool true)(List.concat_map(xs"items")(xs"records"sess)))(fun()->session_at(history cid)2)in
    let boundaries=List.concat_map(xs"items")(xs"records"transferred) |>List.filter(fun item->s"kind"item="contextTransfer")in
    check(List.exists(fun item->field"provisional_order"item=`Bool true)boundaries)"live context boundary missing";
    List.iter(fun item->equal(field"content"item)(obj["transfer",field"id"(nth(xs"transfers"returned)1)]))boundaries;
    fake_terminal_checks home repo service server second !c cid native codex request attachment)

and fake_terminal_checks home repo service server second client cid native codex request attachment=
  let req path=Client.req client path and cmd ?status key value=Client.cmd ?status client key value in
  let worker_dir=home/"ambient-worker"in mkdir worker_dir;
  let ambient_sidecar=worker_dir/"codex-session-id"in write ambient_sidecar"existing-worker-thread\n";
  let environment=merge_env service.Service.env["MONTY_API_PROFILE","default";"MONTY_TASK_KEY","local:must-not-link";"MONTY_JOB_TITLE","worker title";"MONTY_WORKER_DIR",worker_dir;"MONTY_WORKSPACE_1",home/"missing-workspace"]in
  ok(cli ~cwd:repo ~env:environment["terminal";"--home";home;"--codex-command";codex;"--request-id";"terminal-start"]);
  let terminal_args()=json_file(terminal_log home)|>list |>List.map str in
  let args=until ~predicate:((<>)[])(fun()->if Sys.file_exists(terminal_log home)then terminal_args()else[])in
  let private_endpoint args=List.filteri(fun index _->index<2)args=["resume";"--remote"]&&String.starts_with ~prefix:"unix://"(nth args 2)&&nth args 2<>"unix://"^server.Native.path in
  check(private_endpoint args)"terminal used shared endpoint";check(has_call server"config/value/write")"Vim mode default not configured";
  check(Hashtbl.mem server.threads(List.hd(List.rev args)))"terminal selected unregistered thread";
  let task_free=field"result"(req"commands/terminal-start")in
  equal(s"title"task_free)"Monty conversation";equal(xs"task_keys"task_free)[];
  equal(xs"workspaces"(field"context"(session task_free)))[string repo];equal(field"environment"(field"context"(session task_free)))(obj[]);
  equal(read ambient_sidecar)"existing-worker-thread\n";check(not(List.mem"--dangerously-bypass-approvals-and-sandbox"args))"terminal bypass flag";
  List.iter(fun(key,value)->ok(cli ~env:service.env["settings";"set";key;value;"--home";home]))["harness","codex";"codex-yolo","true"];
  let launch_env=merge_env service.env["MONTY_CODEX_COMMAND",codex]in
  let last_args=ref args in
  List.iter(fun(command,extra,yolo)->
    ok(cli ~cwd:repo ~env:(merge_env launch_env extra)(command@["--home";home]));
    let params=latest_call server"thread/start"in
    check(s"approvalPolicy"params=(if yolo then"never"else"on-request")&&s"sandbox"params=(if yolo then"danger-full-access"else"workspace-write"))"selected Codex approval policy not forwarded";
    let args=terminal_args()in last_args:=args;check(private_endpoint args&&not(List.mem"--dangerously-bypass-approvals-and-sandbox"args))"native terminal bypass or endpoint changed")
    [[],[],true;["start"],["MONTY_CODEX_YOLO","false"],false;["start";"--codex-yolo"],["MONTY_CODEX_YOLO","false"],true;["terminal";"--codex-command";codex],[],true];
  ok(cli ~cwd:repo ~env:(merge_env launch_env["REX_SESSION","session:01a1-caller";"REX_BLOCK","block:01a1-caller"])["terminal";"--codex-command";codex;"--home";home]);
  let params=latest_call server"thread/start"in equal(field"REX_BLOCK"(field"shell_environment_policy.set"(field"config"params)))(string"block:01a1-caller");
  let rex_thread=List.hd(List.rev(terminal_args()))in
  let rex_chat=list(req"conversations") |>List.find(fun chat->s"native_id"(session chat)=rex_thread)in
  check(not(has_field"REX_BLOCK"(field"environment"(field"context"(session rex_chat)))))"Rex pane leaked into durable context";
  let resumed=List.hd(List.rev !last_args)and count=Hashtbl.length server.threads in
  Unix.unlink(home/"codex/skills/monty/SKILL.md");
  ok(cli ~env:launch_env["continue";resumed;"--home";home]);equal(Hashtbl.length server.threads)count;
  check(Sys.file_exists(home/"codex/skills/monty/SKILL.md")&&has_call server"skills/list")"continue did not refresh skill";
  let args=terminal_args()in check(List.hd(List.rev args)=resumed&&not(List.mem"--dangerously-bypass-approvals-and-sandbox"args))"continue selected wrong thread";
  let rec after=function flag::value::_ when flag="-c"->value|_::rest->after rest|[]->failwith"missing terminal config"in
  equal(after args)"tui.vim_mode_default=true";
  let worker_bin=home/"worker-bin"in mkdir worker_bin;
  let profiles=Filename.dirname Sys.executable_name/"../agent-profiles"in copy_tree profiles(home/"agent-profiles");
  List.iter(fun name->shell(worker_bin/name)"exit 0")["ghostty";"osascript"];
  let worker_env=merge_env launch_env["PATH",worker_bin^":"^List.assoc"PATH"launch_env]in
  let context=home/"context.md"in write context"Isolated structured worker.\n";
  List.iter(fun args->ok(cli ~cwd:repo ~env:worker_env(args@["--home";home])))
    [["projects";"add";"--repo";repo];["launch";"--repo";repo;"--title";"Structured worker";"--branch";"cto/structured-worker";"--context";context;"--worktree";"never";"--terminal";"ghostty";"--harness";"codex";"--codex-command";codex]];
  let job_file=home/".monty/runs/manual/workers/structured-worker/job.json"in
  let context=obj["workspaces",`List[string repo];"environment",obj["MONTY_JOB_ID",string"structured-worker";"MONTY_WORKER_DIR",string(Filename.dirname job_file);"MONTY_JOB_FILE",string job_file]]in
  let owned=cmd"worker-chat"(obj["op",string"start";"profile",string"default";"context",context])in
  let owned_id=s"id"owned and sidecar=Filename.dirname job_file/"codex-session-id"in let saved=read sidecar in
  let rejected=cmd ~status:"rejected""purge-owned-worker"(obj["op",string"purge";"conversation",string owned_id;"confirm",`Bool true])in
  check(i"code"rejected=409&&contains(s"message"rejected)"owned runtimes")"purged live worker";
  equal(s"native_id"(session(req("conversations/"^owned_id))))saved;
  let before=Hashtbl.length server.threads in
  let duplicate=cmd ~status:"rejected""duplicate-worker-chat"(obj["op",string"start";"profile",string"default";"context",context])in
  check(i"code"duplicate=409&&Hashtbl.length server.threads=before&&read sidecar=saved)"duplicate structured worker dispatched";
  let release key=ignore(cmd key(obj["op",string"release_runtime";"conversation",string owned_id;"revision",Client.revision client owned_id]))in
  release"worker-release";
  ignore(cmd"worker-resume"(obj["op",string"resume";"conversation",string owned_id;"revision",Client.revision client owned_id;"detached",`Bool true]));equal(read sidecar)saved;
  release"worker-release-again";ignore(cmd"purge-released-worker"(obj["op",string"purge";"conversation",string owned_id;"confirm",`Bool true]));
  http_error 410(fun()->Client.history client owned_id);check(read sidecar=saved&&s"status"(json_file job_file)="launch-requested")"archive purge changed worker lifecycle";
  let count=Hashtbl.length server.threads in
  save(actor_config home)(put"mode"(string"vim")(json_file(actor_config home)));
  List.iter(fun initial->tty_client home repo launch_env resumed initial;
    let recorded=list(req"conversations") |>List.concat_map(xs"sessions") |>List.find(fun sess->s"native_id"sess=resumed&&s"profile"sess="default")in
    check(field"terminal"recorded=`Bool false&&field"terminal_code"recorded=`Int 7)"terminal final state not persisted")["Insert";"Normal";"exit"];
  ignore(cmd ~status:"rejected""invalid-yolo"(put"codex_yolo"(string"true")request));equal(Hashtbl.length server.threads)count;
  ok(cli ~env:service.env["settings";"set";"codex-yolo";"false";"--home";home]);
  Native.close second;
  wait(fun()->s"state"(field"coverage"(session_at(req("conversations/"^cid))1))="unavailable");
  check(xs"records"(session(Client.history client cid))<>[])"source loss erased archive";
  let inventory=xs"tasks"(req"inventory")in check(List.length inventory=1&&s"status"(first inventory)="open")"runtime failure completed task";
  ignore(cmd"archive"(obj["op",string"archive";"conversation",string cid]));equal(field"archived"(req("conversations/"^cid)))(`Bool true);
  ignore(cmd"purge"(obj["op",string"purge";"conversation",string cid;"confirm",`Bool true]));http_error 410(fun()->Client.history client cid);
  ignore(cmd ~status:"rejected""reimport"(obj["op",string"import";"profile",string"default";"native_id",string native;"context",obj["workspaces",`List[string repo]]]));
  check(not(Sys.file_exists(home/".monty/conversation-blobs"/s"sha256"attachment)))"purged attachment blob survived GC";
  let cli args=let result=cli ~env:service.env(args@["--home";home])in ok result;result.stdout in
  let other=home/"other"in mkdir other;ok(run"git"["init";"-q";other]);
  ignore(cli["projects";"add";"--repo";other]);ignore(cli["task";"add";"--project";"repo";"--title";"API inventory first"]);
  ignore(cli["task";"workspace";"add";"local:local-002";"--repo";repo;"--branch";"cto/api-one"]);
  ignore(cli["task";"workspace";"add";"local:local-002";"--repo";other;"--branch";"cto/api-two"]);
  ignore(cli["task";"add";"--project";"repo";"--title";"API inventory second"]);
  let tasks_file=home/".monty/tasks.local.json"in let before=read tasks_file in
  let view=req"inventory?all_groups=true"in let native_table=cli["list";"--no-sync";"--all-groups"]in
  equal(s"banner"view)"Groups: all (--all-groups)\n";
  equal(List.map(s"key")(xs"tasks"view))["local:local-001";"local:local-002";"local:local-003"];
  equal(field"workspaces"(nth(xs"tasks"view)1))(field"workspaces"(nth(xs"tasks"(json before))1));
  equal(List.length(xs"workspaces"(nth(xs"tasks"view)1)))2;
  check(List.for_all(fun row->contains native_table(s"title"row)&&contains native_table(s"status"row))(xs"tasks"view))"API inventory diverged from native CLI";
  equal(read tasks_file)before;
  Native.emit server(`List[string"not-a-json-rpc-object"]);
  wait(fun()->let live=list(req"conversations") |>List.find(fun chat->field"purged"chat<>`Bool true)in s"state"(field"coverage"(session live))="unavailable");
  equal(read tasks_file)before;
  let root_chat=cmd"root-workspace"(obj["op",string"start";"profile",string"filesystem-root";"context",obj["workspaces",`List[string repo]]])in
  equal(xs"workspaces"(field"context"(session root_chat)))[string repo];
  print_endline"client API receipts, events, history, blobs, approvals, transfers, worker ownership, PTY, purge and inventory passed"

let ()=
  if Array.length Sys.argv>2 && Sys.argv.(1)="--fixture"then actor_fixture()
  else List.iter(fun(prefix,suite)->with_temp_dir ~prefix suite)
    ["mca-",fake_suite;"mcar-",recovery_suite;"mcas-",scope_suite;"mcab-",autostart_suite;"mcap-",terminal_profiles_suite;"mcaw-",warm_archive_suite]
