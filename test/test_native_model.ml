open Lwt.Infix
open Test_support

type protocol = [ `Responses | `Pi | `Claude ]
type t = {
  url : string;
  mutable requests : json list;
  mutable started : bool;
  mutable review_tools : string list list;
  mutable errors : exn list;
  release_gate : unit Lwt.t;
  release_wake : unit Lwt.u;
  stop : unit Lwt.t;
  stop_wake : unit Lwt.u;
  mutable server : unit Lwt.t;
}

let url model = model.url
let requests model = model.requests
let started model = model.started
let release model = if Lwt.is_sleeping model.release_gate then Lwt.wakeup_later model.release_wake ()
let json = Yojson.Safe.to_string
let event name value = "event: " ^ name ^ "\ndata: " ^ json value ^ "\n\n"
let data value = "data: " ^ json value ^ "\n\n"
let response_events () =
  let text = "isolated fixture reply" in
  let item = obj ["id",string "msg-fixture";"type",string "message";"role",string "assistant";
    "status",string "completed";"content",`List [obj ["type",string "output_text";
      "text",string text;"annotations",`List []]]] in
  let frame kind fields = event kind (obj (("type",string kind)::fields)) in
  [frame "response.output_item.added" ["output_index",`Int 0;"item",puts ["status",string "in_progress";"content",`List []] item];
   frame "response.output_text.delta" ["item_id",string "msg-fixture";"output_index",`Int 0;"content_index",`Int 0;"delta",string text];
   frame "response.output_item.done" ["output_index",`Int 0;"item",item];
   frame "response.completed" ["response",obj ["id",string "resp-fixture";"status",string "completed";
     "output",`List [item];"usage",obj ["input_tokens",`Int 1;"output_tokens",`Int 1;"total_tokens",`Int 2]]]]

let pi_chunk delta finish = data (obj ["id",string "chatcmpl-fixture";"object",string "chat.completion.chunk";
  "created",`Int 1;"model",string "fixture";"choices",`List [obj ["index",`Int 0;"delta",delta;"finish_reason",finish]]])
let claude_events request =
  let frame kind fields = event kind (obj (("type",string kind)::fields)) in
  [frame "message_start" ["message",obj ["id",string "msg_fixture";"type",string "message";"role",string "assistant";
     "content",`List [];"model",field "model" request;"stop_reason",`Null;"stop_sequence",`Null;
     "usage",obj ["input_tokens",`Int 10;"output_tokens",`Int 1]]];
   frame "content_block_start" ["index",`Int 0;"content_block",obj ["type",string "text";"text",string ""]];
   frame "content_block_delta" ["index",`Int 0;"delta",obj ["type",string "text_delta";"text",string "Local native protocol verified."]];
   frame "content_block_stop" ["index",`Int 0];
   frame "message_delta" ["delta",obj ["stop_reason",string "end_turn";"stop_sequence",`Null];"usage",obj ["output_tokens",`Int 5]];
   frame "message_stop" []]

let create protocol =
  let socket=Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  Lwt_main.run (Lwt_unix.bind socket (Unix.ADDR_INET(Unix.inet_addr_loopback,0)));
  Lwt_unix.listen socket 32;
  let port=match Lwt_unix.getsockname socket with Unix.ADDR_INET(_,port)->port|_->assert false in
  let release_gate,release_wake=Lwt.wait () and stop,stop_wake=Lwt.wait () in
  let model={url=Printf.sprintf "http://127.0.0.1:%d" port;requests=[];started=false;
    review_tools=[];errors=[];release_gate;release_wake;stop;stop_wake;server=Lwt.return_unit} in
  let callback _ request body = Lwt.catch (fun () ->
    if Cohttp.Request.meth request=`GET then Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"{}" ()
    else Cohttp_lwt.Body.to_string body >>= fun text ->
    let value=Yojson.Safe.from_string text in
    if Monty.Home.contains_substring (Uri.path(Cohttp.Request.uri request)) "count_tokens" then
      Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"{\"input_tokens\":10}" ()
    else (
      model.requests<-model.requests@[value];
      if protocol=`Claude then (
        equal (Some "synthetic-test-key") (Cohttp.Header.get (Cohttp.Request.headers request) "x-api-key");
        if Monty.Home.contains_substring text "Independently review Monty worker" then (
          let names=xs "tools" value |> List.map(s "name") in
          check (names<>[] && List.for_all(fun name->List.mem name ["Read";"Grep";"Glob"]) names) "reviewer exposed write tools";
          model.review_tools<-names::model.review_tools));
      let held=protocol=`Responses || protocol=`Pi && Monty.Home.contains_substring text "hold for interrupt" in
      if held then model.started<-true;
      let initial,remaining=match protocol with
        | `Responses -> event "response.created" (obj ["type",string "response.created";
            "response",obj ["id",string "resp-fixture";"status",string "in_progress";"output",`List []]]),response_events ()
        | `Pi -> pi_chunk (obj ["role",string "assistant";"content",string ""]) `Null,
            [pi_chunk (obj ["content",string "Real Pi with a local model fixture."]) `Null;
             pi_chunk (obj []) (string "stop");"data: [DONE]\n\n"]
        | `Claude -> "",claude_events value in
      let stream,push=Lwt_stream.create () in
      if initial<>"" then push(Some initial);
      Lwt.async (fun () ->
        (if held then Lwt.pick [Lwt.protected release_gate;Lwt_unix.sleep 20.] else Lwt.return_unit)
        >|= fun () -> List.iter(fun frame->push(Some frame)) remaining;push None);
      let headers=Cohttp.Header.init_with "content-type" "text/event-stream" in
      Cohttp_lwt_unix.Server.respond ~headers ~status:`OK ~body:(Cohttp_lwt.Body.of_stream stream) ()))
    (fun error -> model.errors<-error::model.errors;
      Cohttp_lwt_unix.Server.respond_string ~status:`Internal_server_error ~body:(Printexc.to_string error) ()) in
  model.server<-Cohttp_lwt_unix.Server.create ~stop ~mode:(`TCP (`Socket socket)) (Cohttp_lwt_unix.Server.make ~callback ());
  model

let close model =
  release model;
  if Lwt.is_sleeping model.stop then Lwt.wakeup_later model.stop_wake ();
  Lwt_main.run model.server;
  match model.errors with [] -> () | error::_ -> raise error
