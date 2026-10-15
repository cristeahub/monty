open Lwt.Infix
open Context_store
open Conversation_store

type t = {process:Lwt_process.process_full; path:string; mutable code:Yojson.Safe.t; mutable alive:bool}
let start ~rows ~cols ~vim ~env ~cwd ~lock ~locks ~path ~notify args =
  let fd=get(State_store.open_verified_lock path [Unix.O_WRONLY;Unix.O_CREAT;Unix.O_APPEND]) in
  Unix.fchmod fd 0o600;
  let process=try Owned_process.start ~mode:(Printf.sprintf "pty:%d:%d%s" rows cols (if vim then ":vim" else "")) ~lock ~locks ~env ~cwd args
    with error->Unix.close fd;raise error in
  Lwt.async (fun () -> Owned_process.drain process#stderr);
  Lwt.catch (fun () -> Lwt_unix.with_timeout 10. (fun () -> Lwt_io.read_line_opt process#stdout) >>= function
  | Some line when field "type" (Yojson.Safe.from_string line)=string "ready" -> Lwt.return_unit
  | _->fail 503 "native terminal could not claim its ownership locks; inspect existing execution before retrying")
    (fun error->Unix.close fd;Owned_process.close process >>= fun ()->Lwt.fail error) >>= fun () ->
  let t={process;path;code=`Null;alive=true} in
  let rec read () = Lwt_io.read_line_opt process#stdout >>= function
    | None -> Lwt.return_unit
    | Some line ->
        if String.length line>65536 then fail 502 "terminal frame too large";
        let json=Yojson.Safe.from_string line in
        if s "type" json="output" then (
          let data=Base64.decode_exn (bounded_string 65536(field "data" json)) in
          let bytes=Bytes.of_string data in
          let rec write offset = if offset<Bytes.length bytes then
            let count=Unix.write fd bytes offset (Bytes.length bytes-offset) in
            if count=0 then fail 500 "terminal log write failed" else write (offset+count) in
          write 0;notify json)
        else if s "type" json="exit" then t.code<-field "code" json
        else fail 502 "invalid terminal frame";
        read () in
  Lwt.async (fun () -> Lwt.finalize
    (fun () -> Lwt.catch read (fun _ -> t.code<-string "outcome_unknown";Lwt.return_unit))
    (fun () -> t.alive<-false;Unix.close fd;notify(obj ["type",string "terminal_closed";"code",t.code]);Owned_process.close process));
  Lwt.return t
let send t request =
  if not t.alive then fail 409 "terminal runtime ended; inspect its history before explicit resume";
  Lwt_io.write t.process#stdin (Yojson.Safe.to_string request ^ "\n") >>= fun () -> Lwt_io.flush t.process#stdin
let output_file ~path ~running ~code offset =
  if offset<0 then fail 400 "invalid terminal offset";
  let fd=get(State_store.open_verified_lock path [Unix.O_RDONLY]) in
  let ic=Unix.in_channel_of_descr fd in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
    let size=in_channel_length ic in
    if offset>size then fail 409 "terminal cursor exceeds retained output";
    seek_in ic offset;
    let data=really_input_string ic (min 32768 (size-offset)) in
    obj ["offset",`Int(offset+String.length data);"data",string(Base64.encode_exn data);
      "running",`Bool running;"code",code])
let output t offset = output_file ~path:t.path ~running:t.alive ~code:t.code offset
