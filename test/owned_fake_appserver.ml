open Lwt.Infix
let ()=
  if Array.length Sys.argv<>3 then failwith"owned app-server requires original command and destination";
  let original=Sys.argv.(1)and destination=Sys.argv.(2)in
  let destination=if String.starts_with ~prefix:"unix://"destination then String.sub destination 7(String.length destination-7)else destination in
  Lwt_main.run(
    Lwt_process.pread(original,[|original;"app-server";"daemon";"start"|]) >>=fun text->
    let source=Monty.Json_fields.s"socketPath"(Yojson.Safe.from_string text)in
    Lwt_io.establish_server_with_client_address(Unix.ADDR_UNIX destination)(fun _ (client_in,client_out)->
      Lwt_io.with_connection(Unix.ADDR_UNIX source)(fun(native_in,native_out)->
        let proxy input output=
          let bytes=Bytes.create 65536 in
          let rec loop ()=Lwt_io.read_into input bytes 0(Bytes.length bytes) >>=fun n->if n=0 then Lwt.return_unit else Lwt_io.write_from_exactly output bytes 0 n >>=fun()->Lwt_io.flush output >>=loop in loop()in
        Lwt.pick[proxy client_in native_out;proxy native_in client_out])) >>=fun _->
    Unix.chmod destination 0o600;
    fst(Lwt.wait()))
