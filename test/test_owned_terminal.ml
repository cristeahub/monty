open Lwt.Infix
open Monty
open Json_fields

let assert_true label value = if not value then failwith label

let with_directory run =
  let directory=Filename.temp_file "monty-terminal-" ".tmp" in
  Unix.unlink directory;Unix.mkdir directory 0o700;
  Lwt.finalize (fun () -> run directory) (fun () ->
    Sys.readdir directory |> Array.iter (fun name -> Unix.unlink(Filename.concat directory name));
    Unix.rmdir directory;Lwt.return_unit)

let with_terminal directory notify args run =
  Owned_terminal.start ~rows:24 ~cols:80 ~vim:false ~env:(Unix.environment ())
    ~cwd:directory ~lock:(Filename.concat directory "runtime.lock") ~locks:[]
    ~path:(Filename.concat directory "terminal.log") ~notify args >>= fun terminal ->
  Lwt.finalize (fun () -> run terminal)
    (fun () -> Owned_process.close terminal.process)

let released_lock directory =
  let fd=Unix.openfile (Filename.concat directory "runtime.lock") [Unix.O_RDWR] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> Worker_lock.try_lock fd)

let native_command directory body =
  let python=Conversation_store.get(Process.command_exists_with_arguments "python3") in
  [python;"-u";"-c";
   "import os,sys,time;from pathlib import Path;Path(sys.argv[1]).write_text(str(os.getpid()));" ^ body;
   Filename.concat directory "native.pid"]

let exited_native directory =
  let pid=Shell.read_file(Filename.concat directory "native.pid") |> int_of_string in
  match Unix.kill pid 0 with
  | () -> failwith "native terminal process survived closure"
  | exception Unix.Unix_error(Unix.ESRCH,_,_) -> ()

let closed_notice () =
  let closed,wake=Lwt.wait () in
  let notify json=if field "type" json=`String "terminal_closed" && Lwt.is_sleeping closed then
    Lwt.wakeup_later wake json in
  closed,notify

let wait_closed closed = Lwt_unix.with_timeout 8. (fun () -> closed)

let test_exit_result () = with_directory (fun directory ->
  let closed,notify=closed_notice () in
  with_terminal directory notify (native_command directory "print('retained output',flush=True);sys.exit(7)") (fun terminal ->
    wait_closed closed >>= fun notice ->
    assert_true "terminal exit code was lost" (field "code" notice=`Int 7 && terminal.code=`Int 7);
    assert_true "terminal remains available after closure" (not terminal.alive);
    let output=Owned_terminal.output terminal 0 in
    let retained=Base64.decode_exn(s "data" output) in
    assert_true "terminal output was not retained" (Home.contains_substring retained "retained output");
    released_lock directory;exited_native directory;Lwt.return_unit))

let test_throwing_notifications () = with_directory (fun directory ->
  let closed,record_closed=closed_notice () in
  let notify json =
    record_closed json;
    if List.mem (s "type" json) ["output";"terminal_closed"] then
      failwith "fixture terminal notification failed" in
  with_terminal directory notify (native_command directory "print('running terminal',flush=True);time.sleep(60)") (fun terminal ->
    wait_closed closed >>= fun notice ->
    assert_true "broken output stream was treated as a known exit"
      (field "code" notice=`String "outcome_unknown" && terminal.code=`String "outcome_unknown");
    Lwt_unix.with_timeout 2. (fun () -> terminal.process#status) >|= fun _ ->
    assert_true "broken terminal remains available" (not terminal.alive);
    released_lock directory;exited_native directory))

let test_eof_without_exit () = with_directory (fun directory ->
  (* This fake ownership guard ends after ready, omitting its exit receipt. *)
  let guard=Filename.concat directory "python3" in
  Shell.write_file guard "#!/bin/sh\nprintf '%s\\n' '{\"type\":\"ready\"}'\n";
  Unix.chmod guard 0o700;
  let previous_path=Sys.getenv "PATH" in
  Lwt.finalize (fun () ->
    Unix.putenv "PATH" (directory ^ ":" ^ previous_path);
    let closed,notify=closed_notice () in
    with_terminal directory notify ["/usr/bin/true"] (fun terminal ->
      wait_closed closed >|= fun notice ->
      assert_true "EOF without an exit receipt retained a null result"
        (field "code" notice=`String "outcome_unknown" && terminal.code=`String "outcome_unknown")))
    (fun () -> Unix.putenv "PATH" previous_path;Lwt.return_unit))

let () =
  let unexpected=ref [] and previous= !Lwt.async_exception_hook in
  Lwt.async_exception_hook:=(fun error -> unexpected:=error :: !unexpected);
  Fun.protect ~finally:(fun () -> Lwt.async_exception_hook:=previous) (fun () ->
    Lwt_main.run (
      test_exit_result () >>= fun () ->
      test_throwing_notifications () >>= fun () ->
      test_eof_without_exit () >>= fun () ->
      Lwt.pause () >|= fun () ->
      assert_true "terminal cleanup escaped its async boundary" (!unexpected=[]));
    print_endline "Owned terminals: exit results, retained output, throwing notifications, process/lock cleanup and incomplete EOF passed")
