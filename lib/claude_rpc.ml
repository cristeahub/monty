open Lwt.Infix
open Json_fields
open Conversation_store
let uuid () =
  let value=random_id () in
  String.sub value 0 8 ^ "-" ^ String.sub value 8 4 ^ "-4" ^ String.sub value 13 3
  ^ "-8" ^ String.sub value 17 3 ^ "-" ^ String.sub value 20 12
let python p = match field "python" p with `String path->path|_->get(Process.command_exists_with_arguments "python3")
let preflight ~env p =
  let command=python p in
  Lwt.catch(fun ()->Lwt_unix.with_timeout 5.(fun ()->
    Lwt_process.with_process_full ~env (command,[|command;"-c";"import importlib.metadata; print(importlib.metadata.version('claude-agent-sdk'))"|]) (fun child ->
      Lwt.both(Lwt_io.read child#stdout)(Owned_process.drain child#stderr) >>= fun (version,())->
      child#status >|= fun status->
      if status<>Unix.WEXITED 0 || String.trim version<>"0.2.166" then fail 501 "Claude server adapter requires Python 3.10+ with claude-agent-sdk==0.2.166; install helpers/claude-requirements.txt into the configured profile's Python environment")))
    (function Failure _ as error->Lwt.fail error|_->Lwt.fail(Failure(501,"Claude SDK dependency check failed; inspect the configured Python path and helpers/claude-requirements.txt")))
let start ?(lock="") ?(locks=[]) ~env ~cwd p ~notify () =
  Native_rpc.start ~lock ~locks ~env ~cwd ~command:(python p) ~args:["-u";"-c";Embedded_claude.text] ~notify ()
let read ~env ~cwd p command params =
  start ~env ~cwd p ~notify:(fun _ _ -> ()) () >>= fun rpc ->
  Lwt.finalize (fun () -> Native_rpc.call rpc command params) (fun () -> Native_rpc.close rpc)
