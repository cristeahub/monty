open Lwt.Infix
open Monty.Json_fields
type json=Yojson.Safe.t
module Ws=Monty.Codex_rpc.Ws
let obj fields=`Assoc fields
let string value=`String value
module Gate = struct
  type t={mutable started:bool;promise:unit Lwt.t;wake:unit Lwt.u}
  let create ()=let promise,wake=Lwt.wait () in {started=false;promise;wake}
  let release gate=if Lwt.is_sleeping gate.promise then Lwt.wakeup_later gate.wake ()
end
type t={path:string;threads:(string,json)Hashtbl.t;loaded:(string,unit)Hashtbl.t;
  mutable calls:(string*json)list;mutable answers:json list;mutable sends:int;
  mutable drop_next:bool;mutable drop_open:bool;mutable history_gate:Gate.t option;
  mutable clients:(Lwt_io.input_channel*Lwt_io.output_channel*Lwt_mutex.t)list;
  mutable order:string list;mutable server:Lwt_io.server option;mutable errors:exn list}
let check condition message=if not condition then failwith message
let set_thread t id value=if not(Hashtbl.mem t.threads id) then t.order<-t.order@[id];Hashtbl.replace t.threads id value
let send (_,oc,lock) value=Lwt_mutex.with_lock lock (fun () ->
  let buffer=Buffer.create 128 in
  Ws.write_frame_to_buf ~mode:Ws.Server buffer (Websocket.Frame.create ~content:(Yojson.Safe.to_string value) ());
  Lwt_io.write oc (Buffer.contents buffer) >>=fun ()->Lwt_io.flush oc)
let emit_lwt t value=Lwt_list.iter_p(fun client->Lwt.catch(fun()->send client value)(fun _->Lwt.return_unit)) t.clients
let emit t value=Lwt_main.run(emit_lwt t value)
let handle t ic oc =
  let input=Cohttp_lwt_unix.Private.Input_channel.create ic in
  Ws.Request.read input >>=function
  | `Ok request ->
    let key=Option.get(Cohttp.Header.get(Cohttp.Request.headers request)"sec-websocket-key") in
    let accept=Websocket.b64_encoded_sha1sum(key^Websocket.websocket_uuid) in
    Lwt_io.write oc ("HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: "^accept^"\r\n\r\n") >>=fun()->Lwt_io.flush oc >>=fun()->
    let client=ic,oc,Lwt_mutex.create () in t.clients<-client::t.clients;
    let read=Ws.make_read_frame ~mode:Ws.Server input oc in
    let rec loop ()=read () >>=fun frame->
      if frame.Websocket.Frame.opcode=Websocket.Frame.Opcode.Close then Lwt.return_unit else
      let request=Yojson.Safe.from_string frame.content in
      match field "method" request with
      | `Null ->t.answers<-t.answers@[request];loop ()
      | `String method_name ->
        let params=match field "params" request with `Null->obj[]|value->value in
        t.calls<-t.calls@[method_name,params];
        if method_name="initialized" then loop () else
        let native=match field "threadId" params,field "session" params with `String value,_|_,`String value->value|_->""in
        let result = match method_name with
        | "initialize"->Lwt.return(obj["userAgent",string"fake-0.160.0"])
        | "thread/start"|"open" ->
          let id="thread-"^string_of_int(Hashtbl.length t.threads+1) in
          let value=obj["id",string id;"turns",`List[]] in
          set_thread t id value;Hashtbl.replace t.loaded id ();
          Lwt.return(if method_name="thread/start" then obj["thread",value] else obj["session",string id;"entries",`List[]])
        | "skills/list"->check(field"forceReload"params=`Bool true && xs"cwds"params<>[])"invalid skill reload";Lwt.return(obj["data",`List[]])
        | "thread/name/set"->Lwt.return(obj[])
        | "config/value/write"->
          check((match params with `Assoc fields->List.length fields=3|_->false) && field"keyPath"params=string"tui.vim_mode_default" && field"value"params=`Bool true && field"mergeStrategy"params=string"replace")"invalid vim config write";
          Lwt.return(obj["status",string"ok"])
        | "thread/loaded/list"->Lwt.return(obj["data",`List(Hashtbl.to_seq_keys t.loaded |>List.of_seq |>List.sort String.compare |>List.map string);"nextCursor",`Null])
        | "thread/list"->
          check(field"modelProviders"params=`List[] && List.mem(string"exec")(xs"sourceKinds"params))"invalid discovery parameters";
          let ids=t.order @ (Hashtbl.to_seq_keys t.threads |>List.of_seq |>List.filter(fun id->not(List.mem id t.order)) |>List.sort String.compare) in
          let threads=ids |>List.map(Hashtbl.find t.threads) |>List.filter(fun thread->(field"archived"thread=`Bool true)=(field"archived"params=`Bool true)) in
          let offset=match field"cursor"params with `String value when value<>""->int_of_string value|_->0 in
          let limit=i"limit"params in
          Lwt.return(obj["data",`List(List.filteri(fun index _->index>=offset && index<offset+limit)threads);
            "nextCursor",(if offset+limit<List.length threads then string(string_of_int(offset+limit))else`Null)])
        | "thread/read"|"thread/resume"|"history"|"join"->
          if not(Hashtbl.mem t.threads native)then Lwt.fail Not_found else
          (match method_name,t.history_gate with
          | ("thread/read"|"history"),Some gate->t.history_gate<-None;gate.started<-true;Lwt_unix.with_timeout 10.(fun()->gate.promise)
          | _->Lwt.return_unit) >|=fun()->
          if List.mem method_name["thread/resume";"join"]then Hashtbl.replace t.loaded native ();
          if List.mem method_name["history";"join"]then obj["session",string native;"leaf",string"entry-1";"entries",`List[
            obj["uuid",string"entry-1";"parent",`Null;"exchange",string"x";"kind",string"message";"payload",obj["text",string"different shape"]]]]
          else obj["thread",Hashtbl.find t.threads native]
        | "turn/start"|"send"->
          t.sends<-t.sends+1;
          let suffix=string_of_int t.sends in
          let turn=obj["id",string("turn-"^suffix);"status",string"completed";"items",`List[
            obj["id",string("u-"^suffix);"type",string"userMessage";"content",(match field"input"params with `Null->`List[]|value->value)];
            obj["id",string("a-"^suffix);"type",string"agentMessage";"text",string"hello"]]] in
          let thread=Hashtbl.find t.threads native in
          set_thread t native(put"turns"(`List(xs"turns"thread@[turn]))thread);
          Lwt_list.iter_s(fun item->emit_lwt t(obj["method",string"item/completed";"params",obj["threadId",string native;"turnId",field"id"turn;"item",item]]))(xs"items"turn) >|=fun()->obj["turn",turn]
        | "turn/interrupt"|"abort"->Lwt.return(obj[])
        | name->Lwt.fail(Failure("unexpected fake-native method: "^name)) in
        Lwt.catch(fun()->result >>=fun result->
          let drop=(List.mem method_name["thread/start";"open"]&&t.drop_open)||(List.mem method_name["turn/start";"send"]&&t.drop_next) in
          if drop then(t.drop_open<-false;t.drop_next<-false;Lwt_io.close oc >>=fun()->Lwt_io.close ic)
          else send client(obj["id",field"id"request;"result",result]) >>=loop)
          (function Not_found->send client(obj["id",field"id"request;"error",obj["code",`Int(-32602);"message",string"unknown thread"]]) >>=loop|error->Lwt.fail error)
      | _->Lwt.fail(Failure"invalid native request") in
    Lwt.finalize loop (fun()->t.clients<-List.filter(fun current->current!=client)t.clients;Lwt.return_unit)
  | _ ->Lwt.fail(Failure"invalid WebSocket upgrade")
let create path=
  let t={path;threads=Hashtbl.create 16;loaded=Hashtbl.create 16;calls=[];answers=[];sends=0;drop_next=false;drop_open=false;history_gate=None;clients=[];order=[];server=None;errors=[]}in
  t.server<-Some(Lwt_main.run(Lwt_io.establish_server_with_client_address(Unix.ADDR_UNIX path)(fun _ (ic,oc)->
    Lwt.catch(fun()->handle t ic oc)(function End_of_file|Lwt_io.Channel_closed _|Unix.Unix_error((Unix.ECONNRESET|Unix.EPIPE|Unix.EBADF),_,_)->Lwt.return_unit|error->t.errors<-error::t.errors;Lwt.return_unit))));t
let close t=
  Lwt_main.run(Lwt_list.iter_p(fun(ic,oc,_)->Monty.Codex_rpc.close_channels ic oc)t.clients >>=fun()->
    match t.server with None->Lwt.return_unit|Some server->t.server<-None;Lwt_io.shutdown_server server);
  (try Unix.unlink t.path with Unix.Unix_error(Unix.ENOENT,_,_)->());
  match t.errors with []->()|error::_->raise error
