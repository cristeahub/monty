open Lwt.Infix
open Conversation_store
module Io = struct
  include Cohttp_lwt_unix.Private.IO
  let read ic count =
    if count > 16_777_216 then Lwt.fail (Failure (502,"native frame exceeds 16 MiB"))
    else read ic count
end
module Ws = Websocket.Make(Io)
type t = { identity:string; ic:Lwt_io.input_channel; oc:Lwt_io.output_channel;
  pending:(int, Yojson.Safe.t Lwt.u) Hashtbl.t; mutable next:int;
  write_lock:Lwt_mutex.t; mutable alive:bool; mutable notify:Yojson.Safe.t -> unit }
let rng n = let ic = open_in_bin "/dev/urandom" in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () -> really_input_string ic n)
let frame c frame = Lwt_mutex.with_lock c.write_lock (fun () ->
  let b = Buffer.create 256 in Ws.write_frame_to_buf ~mode:(Ws.Client rng) b frame;
  Lwt_io.write c.oc (Buffer.contents b) >>= fun () -> Lwt_io.flush c.oc)
let send c json = frame c (Websocket.Frame.create ~content:(Yojson.Safe.to_string json) ())
let call c meth params =
  if not c.alive then Lwt.fail (Failure (503,"native connection unavailable")) else
  let id = c.next in c.next <- id+1;
  let promise,wake = Lwt.wait () in Hashtbl.add c.pending id wake;
  Lwt.finalize (fun () ->
    send c (obj ["id",`Int id;"method",string meth;"params",params]) >>= fun () ->
    Lwt_unix.with_timeout 30. (fun () -> promise) >>= fun value ->
    if Json_fields.field "error" value <> `Null then
      Lwt.fail (Failure (502,"native request rejected: " ^ Yojson.Safe.to_string (redact (Json_fields.field "error" value))))
    else Lwt.return (Json_fields.field "result" value))
    (fun () -> Hashtbl.remove c.pending id;Lwt.return_unit)
let connect path =
  Lwt_io.open_connection (Unix.ADDR_UNIX path) >>= fun (ic,oc) ->
  let key = Base64.encode_exn (rng 16) in
  let request = Cohttp.Request.make ~headers:(Cohttp.Header.of_list ["host","localhost";"upgrade","websocket";
    "connection","Upgrade";"sec-websocket-version","13";"sec-websocket-key",key]) (Uri.of_string "http://localhost/") in
  Ws.Request.write ~flush:true (fun _ -> Lwt.return_unit) request oc >>= fun () -> Lwt_io.flush oc >>= fun () ->
  let input = Cohttp_lwt_unix.Private.Input_channel.create ic in
  Ws.Response.read input >>= (function
    | `Ok r when Cohttp.Response.status r = `Switching_protocols &&
        Cohttp.Header.get (Cohttp.Response.headers r) "sec-websocket-accept" = Some (Websocket.b64_encoded_sha1sum (key ^ Websocket.websocket_uuid)) -> Lwt.return_unit
    | _ -> Lwt.fail (Failure (502,"Codex Unix WebSocket upgrade failed"))) >>= fun () ->
  let c = {identity=random_id ();ic;oc;pending=Hashtbl.create 16;next=1;write_lock=Lwt_mutex.create ();alive=true;notify=(fun _ -> ())} in
  let read_frame = Ws.make_read_frame ~mode:(Ws.Client rng) input oc in
  let buffer = Buffer.create 4096 in
  let rec read () = read_frame () >>= fun f ->
    match f.Websocket.Frame.opcode with
    | Websocket.Frame.Opcode.Close -> Lwt.fail End_of_file
    | Ping -> frame c (Websocket.Frame.create ~opcode:Pong ~content:f.content ()) >>= read
    | Pong -> read ()
    | Text | Continuation ->
        if Buffer.length buffer + String.length f.content > 16_777_216 then Lwt.fail (Failure (502,"native frame exceeds 16 MiB")) else (
        Buffer.add_string buffer f.content;
        if f.final then (
          let j = Yojson.Safe.from_string (Buffer.contents buffer) in Buffer.clear buffer;
          match Json_fields.field "id" j, Json_fields.field "method" j with
          | `Int id,`Null -> (match Hashtbl.find_opt c.pending id with Some wake -> Lwt.wakeup_later wake j | _ -> ())
          | _,`String _ -> c.notify j
          | _ -> fail 502 "malformed native JSON-RPC event");
        read ())
    | _ -> Lwt.fail (Failure (502,"unsupported native frame")) in
  Lwt.async (fun () -> Lwt.catch read (fun _ ->
      c.alive <- false;
      Hashtbl.iter (fun _ wake -> Lwt.wakeup_later_exn wake (Failure (503,"native connection lost; outcome may be unknown"))) c.pending;
      Hashtbl.clear c.pending;
      c.notify (obj ["method",string "monty/disconnected"]);
      Lwt.catch (fun () -> Lwt_io.close ic >>= fun () -> Lwt_io.close oc) (fun _ -> Lwt.return_unit)));
  call c "initialize" (obj ["clientInfo",obj ["name",string "monty";"version",string "1"];
    "capabilities",obj ["experimentalApi",`Bool true]]) >>= fun _ ->
  send c (obj ["method",string "initialized"]) >|= fun () -> c
