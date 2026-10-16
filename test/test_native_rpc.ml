open Lwt.Infix
open Monty
open Json_fields

let assert_true label value = if not value then failwith label

let fixture = {python|
import json,sys,time
for line in sys.stdin:
    request=json.loads(line)
    if request['type']=='break_transport':
        print('invalid JSON',flush=True)
        time.sleep(60)
    response=json.dumps({'type':'response','id':request['id'],'success':True,'data':request['type']})+'\n'
    sys.stdout.write(response*(2 if request['type']=='duplicate' else 1))
    sys.stdout.flush()
|python}

let close_safely rpc = Lwt.catch (fun () -> Native_rpc.close rpc) (fun _ -> Lwt.return_unit)

let with_runtime notify run =
  let directory=Filename.temp_file "monty-native-rpc-" ".tmp" in
  Unix.unlink directory;Unix.mkdir directory 0o700;
  let lock=Filename.concat directory "runtime.lock" in
  Lwt.finalize (fun () ->
    Native_rpc.start ~lock ~env:(Unix.environment ()) ~cwd:directory
      ~command:"python3" ~args:["-u";"-c";fixture] ~notify () >>= fun rpc ->
    Lwt.finalize (fun () ->
      Native_rpc.call rpc "ready" (`Assoc[]) >>= fun ready ->
      assert_true "fixture did not become ready" (ready=`String "ready");
      run rpc >>= fun () ->
      Lwt_unix.with_timeout 8. (fun () -> rpc.process#status) >|= fun _ ->
      let fd=Unix.openfile lock [Unix.O_RDWR] 0 in
      Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
        Worker_lock.try_lock fd))
      (fun () -> close_safely rpc))
    (fun () ->
      (try Unix.unlink lock with Unix.Unix_error(Unix.ENOENT,_,_)->());
      Unix.rmdir directory;Lwt.return_unit)

let failing_notification _ json =
  if field "type" json=`String "monty/disconnected" then
    failwith "fixture disconnection notification failed"

let test_explicit_close () = with_runtime failing_notification (fun rpc ->
  Lwt.catch (fun () -> Native_rpc.close rpc >>= fun () -> Lwt.fail(Failure "expected notification failure"))
    (function Failure message when message="fixture disconnection notification failed" -> Lwt.return_unit | error -> Lwt.fail error))

let test_async_close () =
  let unexpected=ref [] and previous= !Lwt.async_exception_hook in
  Lwt.async_exception_hook:=(fun error -> unexpected:=error :: !unexpected);
  Lwt.finalize (fun () ->
    with_runtime failing_notification (fun rpc ->
      Lwt.catch (fun () -> Native_rpc.call rpc "break_transport" (`Assoc[]) >>= fun _ ->
        Lwt.fail(Failure "expected transport failure"))
        (function Conversation_store.Failure(503,_) -> Lwt.return_unit | error -> Lwt.fail error)) >|= fun () ->
    assert_true "native RPC disconnect escaped its async boundary" (!unexpected=[]))
    (fun () -> Lwt.async_exception_hook:=previous;Lwt.return_unit)

let test_duplicate_response () = with_runtime (fun _ _ -> ()) (fun rpc ->
  Native_rpc.call rpc "duplicate" (`Assoc[]) >>= fun first ->
  assert_true "first response changed" (first=`String "duplicate");
  Native_rpc.call rpc "next" (`Assoc[]) >>= fun next ->
  assert_true "duplicate response interrupted next request" (next=`String "next");
  Native_rpc.close rpc)

let () = Lwt_main.run (
  test_explicit_close () >>= fun () ->
  test_async_close () >>= fun () ->
  test_duplicate_response () >|= fun () ->
  print_endline "Native RPC: throwing notifications release ownership, async failures settle safely, and duplicate replies settle once")
