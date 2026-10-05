(* One owned Pi subprocess per session. HTTP observers never own these pipes. *)
open Lwt.Infix
open Context_store
open Conversation_store

type t = { identity:string; process:Lwt_process.process_full;
  pending:(string,Yojson.Safe.t Lwt.t * Yojson.Safe.t Lwt.u) Hashtbl.t; write_lock:Lwt_mutex.t;
  mutable next:int; mutable alive:bool; mutable busy:bool; notify:string -> Yojson.Safe.t -> unit }

let send c json =
  if not c.alive then Lwt.fail (Failure(409,"Pi runtime is unavailable; explicitly resume after inspecting its history")) else
  Lwt_mutex.with_lock c.write_lock (fun () ->
    Lwt_io.write c.process#stdin (Yojson.Safe.to_string json ^ "\n") >>= fun () -> Lwt_io.flush c.process#stdin)
let call c command params =
  let id=c.identity ^ "-" ^ string_of_int c.next in c.next<-c.next+1;
  let promise,wake=Lwt.wait () in Hashtbl.add c.pending id (promise,wake);
  Lwt.finalize (fun () ->
    send c (puts ["type",string command;"id",string id] params) >>= fun () ->
    Lwt_unix.with_timeout 30. (fun () -> promise) >|= fun response ->
    if field "success" response<>`Bool true then fail 502 "Pi rejected the command; inspect retained events and native configuration";
    field "data" response)
    (fun () -> Hashtbl.remove c.pending id;Lwt.return_unit)
let disconnected c =
  if c.alive then (
    c.alive<-false;
    Hashtbl.iter (fun _ (promise,wake) -> if Lwt.is_sleeping promise then
      Lwt.wakeup_later_exn wake (Failure(503,"Pi process disconnected; command outcome may be unknown"))) c.pending;
    Hashtbl.clear c.pending;
    c.notify c.identity (obj ["type",string "monty/disconnected"]))
let close c =
  disconnected c;
  Lwt.catch (fun () -> Lwt_io.close c.process#stdin) (fun _ -> Lwt.return_unit) >>= fun () ->
  Lwt.catch (fun () -> Lwt_unix.with_timeout 5. (fun () -> c.process#status) >|= ignore)
    (fun _ -> c.process#terminate; c.process#status >|= ignore) >>= fun () ->
  c.process#close >|= ignore
let start ~env ~cwd ~command ~args ~notify =
  Lwt_unix.with_timeout 10. (fun () ->
    Lwt_process.pread ~env (command,[|command;"--version"|])) >>= fun version ->
  if String.trim version<>"1.0.2" then fail 501 "Pi RPC adapter is tested with Pi 1.0.2";
  let process=Lwt_process.open_process_full ~env ~cwd (command,Array.of_list (command::args)) in
  let c={identity=random_id ();process;pending=Hashtbl.create 16;write_lock=Lwt_mutex.create ();
    next=1;alive=true;busy=false;notify} in
  (* Strict LF framing; bound a record before parsing it, including partial lines. *)
  let buffer=Buffer.create 4096 and chunk=Bytes.create 4096 in
  let deliver () =
    let json=Yojson.Safe.from_string (Buffer.contents buffer) in Buffer.clear buffer;
    match field "type" json with
    | `String "response" -> (match field "id" json with
        | `String id -> (match Hashtbl.find_opt c.pending id with
            | Some (promise,wake) when Lwt.is_sleeping promise -> Lwt.wakeup_later wake json | _ -> ())
        | _ -> ())
    | `String kind ->
        if kind="agent_start" then c.busy<-true else if kind="agent_settled" then c.busy<-false;
        c.notify c.identity json
    | _ -> fail 502 "malformed Pi event" in
  let rec read () = Lwt_io.read_into process#stdout chunk 0 (Bytes.length chunk) >>= fun count ->
    if count=0 then Lwt.fail End_of_file else (
      for n=0 to count-1 do
        if Bytes.get chunk n='\n' then deliver () else (
          if Buffer.length buffer>=16_777_216 then fail 502 "Pi record exceeds 16 MiB";
          Buffer.add_char buffer (Bytes.get chunk n))
      done;read ()) in
  Lwt.async (fun () -> Lwt.catch read (fun _ ->
    disconnected c;process#terminate;Lwt.return_unit));
  (* Drain diagnostics without retaining credentials or allowing pipe backpressure. *)
  Lwt.async (fun () -> Lwt.catch (fun () ->
    let bytes=Bytes.create 4096 in let rec drain () =
      Lwt_io.read_into process#stderr bytes 0 4096 >>= fun n -> if n=0 then Lwt.return_unit else drain () in drain ())
    (fun _ -> Lwt.return_unit));
  Lwt.return c
