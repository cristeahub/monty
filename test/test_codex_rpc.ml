open Lwt.Infix
open Monty
open Json_fields

module Ws = Codex_rpc.Ws

let assert_true label value = if not value then failwith label

let upgrade_request ic =
  Lwt_io.read_line ic >>= fun _ ->
  let rec headers values = Lwt_io.read_line ic >>= fun line ->
    let line=String.trim line in
    if line="" then Lwt.return values else
    let boundary=String.index line ':' in
    headers ((String.lowercase_ascii(String.sub line 0 boundary),
      String.trim(String.sub line (boundary+1) (String.length line-boundary-1))) :: values) in
  headers []

let upgrade ic oc accepted =
  upgrade_request ic >>= fun values ->
  let response=if accepted then
    "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: " ^
    Websocket.b64_encoded_sha1sum(List.assoc "sec-websocket-key" values ^ Websocket.websocket_uuid) ^ "\r\n\r\n"
    else "HTTP/1.1 400 Bad Request\r\nContent-Length: 0\r\n\r\n" in
  Lwt_io.write oc response >>= fun () -> Lwt_io.flush oc

let frame value =
  let buffer=Buffer.create 128 in
  Ws.write_frame_to_buf ~mode:Ws.Server buffer
    (Websocket.Frame.create ~content:(Yojson.Safe.to_string value) ());
  Buffer.contents buffer

let send oc values =
  Lwt_io.write oc (String.concat "" (List.map frame values)) >>= fun () -> Lwt_io.flush oc

let read_frames ic oc =
  let read=Ws.make_read_frame ~mode:Ws.Server (Cohttp_lwt_unix.Private.Input_channel.create ic) oc in
  fun () -> read () >|= fun value -> Yojson.Safe.from_string value.Websocket.Frame.content

let expect_eof ic = Lwt_unix.with_timeout 2. (fun () -> Lwt_io.read_char_opt ic) >|= fun value ->
  assert_true "failed or closed Codex connection retained its socket" (value=None)

let with_server ?(timeout=5.) serve run =
  let path=Filename.temp_file "monty-rpc-" ".sock" in
  Unix.unlink path;
  let complete,wake=Lwt.wait () in
  Lwt_io.establish_server_with_client_address (Unix.ADDR_UNIX path) (fun _ (ic,oc) ->
    Lwt.catch (fun () -> serve ic oc >|= fun () -> Lwt.wakeup_later wake ())
      (fun error -> Lwt.wakeup_later_exn wake error;Lwt.return_unit)) >>= fun server ->
  Lwt.finalize
    (fun () -> Lwt_unix.with_timeout timeout (fun () ->
      Lwt.both (run path) complete >|= fun _ -> ()))
    (fun () -> Lwt_io.shutdown_server server >|= fun () ->
      try Unix.unlink path with Unix.Unix_error(Unix.ENOENT,_,_)->())

let expect_rejected promise =
  Lwt.catch (fun () -> promise >>= fun _ -> Lwt.fail(Failure "expected rejected Codex connection"))
    (function Conversation_store.Failure(502,_) -> Lwt.return_unit | error -> Lwt.fail error)

let test_upgrade_failure () = with_server
  (fun ic oc -> upgrade ic oc false >>= fun () -> expect_eof ic)
  (fun path -> expect_rejected(Codex_rpc.connect path))

let test_upgrade_timeout () = with_server ~timeout:15.
  (fun ic _ ->
    upgrade_request ic >>= fun _ ->
    (* Keep the peer open without replying, then observe client ownership end. *)
    Lwt_unix.with_timeout 14. (fun () -> Lwt_io.read_char_opt ic) >|= fun value ->
    assert_true "timed-out WebSocket upgrade retained its socket" (value=None))
  (fun path ->
    let started=Unix.gettimeofday () in
    Lwt.catch (fun () -> Codex_rpc.connect path >>= fun rpc ->
      Codex_rpc.close rpc >>= fun () -> Lwt.fail(Failure "expected stalled upgrade to time out"))
      (function Lwt_unix.Timeout ->
        assert_true "stalled upgrade exceeded its startup budget" (Unix.gettimeofday ()-.started<14.);
        Lwt.return_unit
        | error -> Lwt.fail error))

let test_initialize_failure () = with_server
  (fun ic oc ->
    upgrade ic oc true >>= fun () ->
    let read=read_frames ic oc in
    read () >>= fun request ->
    assert_true "expected initialize" (field "method" request=`String "initialize");
    send oc [`Assoc["id",field "id" request;"error",`Assoc["code",`Int(-32000);"message",`String "fixture rejection"]]] >>= fun () ->
    expect_eof ic)
  (fun path -> expect_rejected(Codex_rpc.connect path))

let initialize ic oc =
  upgrade ic oc true >>= fun () ->
  let read=read_frames ic oc in
  read () >>= fun request ->
  send oc [`Assoc["id",field "id" request;"result",`Assoc[]]] >>= fun () ->
  read () >|= fun initialized ->
  assert_true "expected initialized notification" (field "method" initialized=`String "initialized");read

let test_duplicate_response () = with_server
  (fun ic oc ->
    initialize ic oc >>= fun read ->
    read () >>= fun request ->
    let response=`Assoc["id",field "id" request;"result",`String "first"] in
    send oc [response;response] >>= fun () ->
    read () >>= fun request ->
    send oc [`Assoc["id",field "id" request;"result",`String "second"]] >>= fun () ->
    expect_eof ic)
  (fun path ->
    Codex_rpc.connect path >>= fun rpc ->
    Lwt.finalize (fun () ->
      Codex_rpc.call rpc "first" (`Assoc[]) >>= fun first ->
      assert_true "first response" (first=`String "first");
      Codex_rpc.call rpc "second" (`Assoc[]) >|= fun second ->
      assert_true "duplicate response interrupted the next request" (second=`String "second"))
      (fun () -> Codex_rpc.close rpc))

let test_notification_failure_still_closes () = with_server
  (fun ic oc -> initialize ic oc >>= fun _ -> expect_eof ic)
  (fun path ->
    Codex_rpc.connect path >>= fun rpc ->
    rpc.notify<-(fun _ -> failwith "fixture notification failed");
    Lwt.catch (fun () -> Codex_rpc.close rpc >>= fun () -> Lwt.fail(Failure "expected notification failure"))
      (function Failure message when message="fixture notification failed" -> Lwt.return_unit | error -> Lwt.fail error))

let () = Lwt_main.run (
  test_upgrade_failure () >>= fun () ->
  test_upgrade_timeout () >>= fun () ->
  test_initialize_failure () >>= fun () ->
  test_duplicate_response () >>= fun () ->
  test_notification_failure_still_closes () >|= fun () ->
  print_endline "Codex RPC: failed/stalled handshakes close sockets, duplicate responses settle once, and notification errors preserve cleanup")
