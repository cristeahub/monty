open Lwt.Infix
open Json_fields
open Conversation_store

(* One plain HTTP/1.1 exchange per request, on a socket this chain owns.
   cohttp-lwt 6.3's clients run a reader and a writer beside the request:
   its default client re-raises a transport failure from inside Lwt's
   resolution loop, so a reset mid-response escaped every handler, and its
   connection machinery marks a connection Failed when the reader meets a reset
   or a connect fails without failing the requests queued on it, so a client of
   a service that just died waited out its whole timeout instead of being
   refused. Here the connect, the request, the response and the body are one
   promise chain: whatever fails rejects that promise, the socket is closed in
   every case, and cohttp is used only to format and parse the messages. Every
   transport failure becomes a Monty 503, so callers see the same error for a
   service that is absent, refused or gone mid-request, and no cohttp or Unix
   exception reaches a command line. *)
let transport_failure ~label = function
  | Failure _ | Lwt_unix.Timeout | Lwt.Canceled as error -> Lwt.fail error
  | Unix.Unix_error (code, call, _) ->
      Lwt.fail (Failure (503, Printf.sprintf "Monty service is not reachable for %s (%s: %s)" label call (Unix.error_message code)))
  | End_of_file | Lwt_io.Channel_closed _ ->
      Lwt.fail (Failure (503, Printf.sprintf "Monty service closed the connection before answering %s" label))
  | error -> Lwt.fail (Failure (503, Printf.sprintf "Monty service connection failed for %s (%s)" label (Printexc.to_string error)))

module Request = Cohttp_lwt_unix.Request
module Response = Cohttp_lwt_unix.Response

(* The response and its whole body as text. *)
let call ~label ~headers ~body meth uri =
  Lwt.catch (fun () ->
    let host = Uri.host_with_default ~default:"127.0.0.1" uri and port = Option.value ~default:80 (Uri.port uri) in
    Lwt_io.open_connection (Unix.ADDR_INET (Unix.inet_addr_of_string host, port)) >>= fun (ic, oc) ->
    Lwt.finalize (fun () ->
      Cohttp_lwt.Body.to_string body >>= fun payload ->
      let headers = Cohttp.Header.add_unless_exists headers "connection" "close" in
      let encoding = Cohttp.Transfer.Fixed (Int64.of_int (String.length payload)) in
      let request = Cohttp.Request.make ~meth ~headers ~encoding uri in
      Request.write ~flush:true (fun writer -> Request.write_body writer payload) request oc >>= fun () ->
      let input = Cohttp_lwt_unix.Private.Input_channel.create ic in
      Response.read input >>= function
      | `Eof -> Lwt.fail (Failure (503, Printf.sprintf "Monty service closed the connection before answering %s" label))
      | `Invalid reason -> Lwt.fail (Failure (503, Printf.sprintf "Monty service sent an invalid response to %s (%s)" label reason))
      | `Ok response ->
          let reader = Response.make_body_reader response input in
          let buffer = Buffer.create 4096 in
          let rec drain () = Response.read_body_chunk reader >>= function
            | Cohttp.Transfer.Chunk data -> Buffer.add_string buffer data; drain ()
            | Cohttp.Transfer.Final_chunk data -> Buffer.add_string buffer data; Lwt.return_unit
            | Cohttp.Transfer.Done -> Lwt.return_unit in
          drain () >|= fun () -> response, Buffer.contents buffer)
      (fun () -> Codex_rpc.close_channels ic oc))
    (transport_failure ~label)

(* How long one request may wait for the service's answer. The service answers
   loopback requests in milliseconds, so a wait this long means its event loop
   is blocked, not that the request is slow. MONTY_CLIENT_TIMEOUT shortens it
   for tests and experiments. *)
let default_timeout = 35.
let timeout () =
  match Sys.getenv_opt "MONTY_CLIENT_TIMEOUT" with
  | Some value -> (match float_of_string_opt (String.trim value) with Some seconds when seconds > 0. -> seconds | _ -> default_timeout)
  | None -> default_timeout

let log_path ~home = file home ["client-service.log"]

(* Client-side observations share the service log so one file tells the story
   of a stall: the service's own watchdog lines next to the requests that gave
   up waiting. Logging must never fail the command it describes. *)
let note ~home message =
  try
    ignore (get (State_store.ensure_state_dir ~home));
    let fd = get (State_store.open_verified_lock (log_path ~home) [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND]) in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      Unix.fchmod fd 0o600;
      let stamp =
        let t = Unix.gmtime (Unix.gettimeofday ()) in
        Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min t.tm_sec in
      State_store.write_all fd (Printf.sprintf "%s monty client [%d]: %s\n" stamp (Unix.getpid ()) message))
  with _ -> ()

(* A request that outlives the timeout fails like any other service error, as
   HTTP 504, so every caller's Failure handling applies and no Lwt exception
   escapes to the command line. *)
let timed ~home ~label f =
  let seconds = timeout () in
  Lwt.catch (fun () -> Lwt_unix.with_timeout seconds f)
    (function
      | Lwt_unix.Timeout ->
          let message = Printf.sprintf "Monty service did not answer %s within %.0fs; its process is alive but its event loop is not serving requests (see %s)" label seconds (log_path ~home) in
          note ~home message;
          Lwt.fail (Failure (504, message))
      | error -> Lwt.fail error)

let endpoint ~home =
  let metadata_path=file home ["client-service.json"] in
  if not(Sys.file_exists metadata_path) then fail 503 "Monty client service is not running; run monty service start --config FILE";
  let state=read metadata_path in
  let base=s "url" state in
  let uri=Uri.of_string base in
  if Uri.scheme uri<>Some "http" || Uri.host uri<>Some "127.0.0.1" then fail 500 "unsafe service address";
  base,String.trim (Shell.read_file (file home ["client-service-token"]))

let request ~home meth path body =
  let base,token=endpoint ~home in
  let headers=Cohttp.Header.of_list ["authorization","Bearer " ^ token;"content-type","application/json"] in
  let body=Cohttp_lwt.Body.of_string (Yojson.Safe.to_string body) in
  let label=Printf.sprintf "%s %s at %s" (Cohttp.Code.string_of_method meth) path base in
  timed ~home ~label (fun () ->
    call ~label ~headers ~body meth (Uri.of_string (base ^ path)) >|= fun (response,text) ->
    let json=Yojson.Safe.from_string text in
    let code=Cohttp.Code.code_of_status (Cohttp.Response.status response) in
    if code>=400 then fail code (Yojson.Safe.to_string json);json)
