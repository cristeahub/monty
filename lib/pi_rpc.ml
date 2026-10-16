open Lwt.Infix
open Conversation_store

include Native_rpc

let start ?(lock="") ?(locks=[]) ~env ~cwd ~command ~args ~notify () =
  Lwt_unix.with_timeout 10. (fun () ->
    Lwt_process.pread ~env ("",Array.of_list(Native_command.argv command ["--version"]))) >>= fun version ->
  if String.trim version<>"1.0.2" then fail 501 "Pi RPC adapter is tested with Pi 1.0.2";
  Native_rpc.start ~lock ~locks ~env ~cwd ~command ~args ~notify ()
