open Lwt.Infix
open Conversation_store

let start ?(mode="pipe") ?(lock="") ?(locks=[]) ~env ~cwd args =
  let python=get(Process.command_exists_with_arguments "python3") in
  Lwt_process.open_process_full ~env
    (python,Array.of_list ([python;"-u";"-c";Embedded_runtime.text;mode;cwd;
      Yojson.Safe.to_string(`List(List.filter((<>) "")(lock::locks) |> List.map string))] @ args))

let drain channel =
  let bytes=Bytes.create 4096 in
  let rec loop () = Lwt_io.read_into channel bytes 0 4096 >>= fun n ->
    if n=0 then Lwt.return_unit else loop () in
  Lwt.catch loop (fun _ -> Lwt.return_unit)

let close (process:Lwt_process.process_full) =
  Lwt.catch (fun () -> Lwt_io.close process#stdin) (fun _ -> Lwt.return_unit) >>= fun () ->
  (* Keep a noisy child from blocking the guard before it can observe owner EOF. *)
  Lwt.join [drain process#stdout;drain process#stderr;
    Lwt.catch (fun () -> Lwt_unix.with_timeout 20. (fun () -> process#status) >|= ignore)
      (fun _ -> process#terminate;process#status >|= ignore)] >>= fun () -> process#close >|= ignore
