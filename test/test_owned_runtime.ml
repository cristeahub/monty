open Lwt.Infix
open Test_support

let self = Unix.realpath Sys.executable_name
let contains = Monty.Home.contains_substring
let occurrences needle hay =
  let n = String.length needle in
  let rec count from acc =
    if from + n > String.length hay then acc
    else if String.sub hay from n = needle then count (from + n) (acc + 1) else count (from + 1) acc in
  count 0 0
let fixture mode root = match mode with
  | "tree" ->
      let child = Unix.create_process "sleep" [|"sleep";"60"|] Unix.stdin Unix.stdout Unix.stderr in
      write (root/"child") (Printf.sprintf "%d %d" (Unix.getpid ()) child);
      print_endline "ready"; Unix.sleepf 60.
  | "target" -> print_endline "runtime ready"; Unix.sleepf 60.
  | "terminal" ->
      Printf.printf "native tty %b\n%!" (Unix.isatty Unix.stdin);
      print_endline (read_line ())
  | "probe" ->
      (* Behave like a harness: raw mode, an OSC 7501 support probe split across writes,
         then a status report; finally say how many probe replies came back. *)
      let attrs = Unix.tcgetattr Unix.stdin in
      Unix.tcsetattr Unix.stdin Unix.TCSANOW {attrs with c_icanon=false;c_echo=false;c_isig=false;c_opost=false;c_vmin=1;c_vtime=0};
      let out text = ignore (Unix.write_substring Unix.stdout text 0 (String.length text)) in
      out "before\027]7501;?\007middle\027]7501;"; Unix.sleepf 0.3;
      out "?\027\\after\027]7501;state=working:app=test\027\\";
      let got = Buffer.create 256 and deadline = Unix.gettimeofday () +. 2. in
      let replies () = occurrences "7501;?:states=" (Buffer.contents got) in
      while Unix.gettimeofday () < deadline && replies () < 2 do
        match Unix.select [Unix.stdin] [] [] 0.1 with
        | [], _, _ -> ()
        | _ -> let chunk = Bytes.create 4096 in
            let n = Unix.read Unix.stdin chunk 0 4096 in Buffer.add_subbytes got chunk 0 n
      done;
      out (Printf.sprintf "replies=%d" (replies ()))
  | "duplex" ->
      let size = 256 * 1024 in
      output_string stdout (String.make size 'o'); flush stdout;
      if really_input_string stdin size <> String.make size 'i' then failwith "fixture prompt was corrupted";
      print_endline "complete"
  | _ -> failwith "unknown ownership fixture"

let run helper = with_temp_dir ~prefix:"monty-owned-" (fun root ->
  let python = Monty.Process.command_exists_with_arguments "python3" |> Result.get_ok in
  let lock = root/"lock" in
  let spawn mode lock actor =
    Lwt_process.open_process_full (python,Array.of_list(python::helper::mode::root::lock::self::"--fixture"::actor::root::[])) in
  let close process = Monty.Owned_process.close process in
  let wait_exit process = Lwt_unix.with_timeout 8. (fun () -> Lwt.protected process#status) in
  let ready process = Lwt_unix.with_timeout 8. (fun () -> Lwt_io.read_line process#stdout) in
  let rec wait_condition attempts condition =
    if condition () then Lwt.return_unit
    else if attempts=0 then Lwt.fail (Failure "ownership condition did not settle")
    else Lwt_unix.sleep 0.02 >>= fun () -> wait_condition (attempts-1) condition in
  let alive pid = try Unix.kill pid 0;true with Unix.Unix_error(Unix.ESRCH,_,_) -> false in
  let conflict actor =
    let process=Lwt_process.open_process_full (python,Array.of_list[python;helper;"pipe";root;lock;self;"--fixture";actor;root]) in
    Lwt.finalize (fun () -> Lwt_unix.with_timeout 5. (fun () ->
      Lwt.both (Lwt_io.read process#stdout) (Lwt_io.read process#stderr) >>= fun _ ->
      Lwt.protected process#status) >|= fun status ->
      check (status<>Unix.WEXITED 0) "two runtimes claimed the same lock")
      (fun () -> close process) in
  Lwt_main.run (
    let owner=spawn "pipe" lock "tree" in
    Lwt.finalize (fun () ->
      ready owner >>= fun ready ->
      check (ready="ready") "owned process never became ready";
      let parent,child = Scanf.sscanf (read (root/"child")) "%d %d" (fun parent child -> parent,child) in
      conflict "tree" >>= fun () ->
      Lwt_io.close owner#stdin >>= fun () -> wait_exit owner >>= fun _ ->
      wait_condition 200 (fun () -> not (alive parent)) >>= fun () ->
      Lwt_process.pread ("ps",[|"ps";"-o";"stat=";"-p";string_of_int child|]) >|= fun state ->
      let state=String.trim state in
      check (state="" || String.starts_with ~prefix:"Z" state) "descendant survived native ownership cleanup")
      (fun () -> close owner) >>= fun () ->
    let target=spawn "pipe" "" "target" in
    Lwt.finalize (fun () ->
      ready target >>= fun ready_text -> check (ready_text="runtime ready") "lease target missing";
      let mode=Yojson.Safe.to_string(obj["mode",string "lease";"target_pid",`Int target#pid]) in
      let lease=Lwt_process.open_process_full (python,[|python;helper;mode;root;lock|]) in
      Lwt.finalize (fun () ->
        ready lease >>= fun ready_text ->
        check (Yojson.Safe.from_string ready_text=obj["type",string "ready"]) "lease did not claim its fence";
        Lwt_io.close lease#stdin >>= fun () -> Lwt_unix.sleep 0.1 >>= fun () ->
        check (lease#state=Lwt_process.Running) "server loss released the worker before native cleanup";
        conflict "target" >>= fun () ->
        Lwt_io.close target#stdin >>= fun () -> wait_exit target >>= fun _ -> wait_exit lease >|= fun status ->
        check (status=Unix.WEXITED 0) "lease did not exit after native cleanup")
        (fun () -> if lease#state=Lwt_process.Running then lease#terminate;lease#close >|= ignore))
      (fun () -> close target) >>= fun () ->
    let terminal=spawn "pty" lock "terminal" in
    Lwt.finalize (fun () ->
      ready terminal >>= fun ready_text ->
      check (Yojson.Safe.from_string ready_text=obj["type",string "ready"]) "PTY ownership was not ready";
      let output=Buffer.create 256 and code=ref `Null in
      let rec drain () = Lwt_io.read_line_opt terminal#stdout >>= function
        | None -> Lwt.return_unit
        | Some line ->
            let frame=Yojson.Safe.from_string line in
            (match s "type" frame with
            | "output" -> Buffer.add_string output (Base64.decode_exn(s "data" frame))
            | "exit" -> code:=field "code" frame
            | _ -> failwith "invalid terminal fixture frame");drain () in
      ready terminal >>= fun first ->
      let first=Yojson.Safe.from_string first in
      check (contains (Base64.decode_exn(s "data" first)) "native tty true") "native process did not own a PTY";
      let send frame=Lwt_io.write_line terminal#stdin (Yojson.Safe.to_string frame) >>= fun () -> Lwt_io.flush terminal#stdin in
      send (obj["type",string "resize";"rows",`Int 37;"cols",`Int 101]) >>= fun () ->
      send (obj["type",string "input";"data",string(Base64.encode_exn "exact input\n")]) >>= fun () ->
      Lwt_unix.with_timeout 8. drain >>= fun () -> wait_exit terminal >|= fun status ->
      check (contains (Buffer.contents output) "exact input" && !code=`Int 0 && status=Unix.WEXITED 0) "terminal input or native exit was lost")
      (fun () -> close terminal) >>= fun () ->
    (* A display that supports the Program Status Protocol is answered by the owned terminal
       itself: probes never reach the display, a probe split across writes is still answered,
       and status reports pass through untouched. *)
    let status_mode=Yojson.Safe.to_string(obj["mode",string "pty:24:80";"program_status",`Bool true]) in
    let probing=spawn status_mode "" "probe" in
    Lwt.finalize (fun () ->
      ready probing >>= fun ready_text ->
      check (Yojson.Safe.from_string ready_text=obj["type",string "ready"]) "probe terminal was not ready";
      let output=Buffer.create 256 and code=ref `Null in
      let rec drain () = Lwt_io.read_line_opt probing#stdout >>= function
        | None -> Lwt.return_unit
        | Some line ->
            let frame=Yojson.Safe.from_string line in
            (match s "type" frame with
            | "output" -> Buffer.add_string output (Base64.decode_exn(s "data" frame))
            | "exit" -> code:=field "code" frame
            | _ -> failwith "invalid terminal fixture frame");drain () in
      Lwt_unix.with_timeout 8. drain >>= fun () -> wait_exit probing >|= fun status ->
      let output=Buffer.contents output in
      check (status=Unix.WEXITED 0 && !code=`Int 0) "probe fixture did not exit cleanly";
      check (not (contains output "\027]7501;?")) "support probe reached the display";
      check (String.starts_with ~prefix:"beforemiddleafter\027]7501;state=working:app=test\027\\" output) "probe was not stripped cleanly or the report was altered";
      check (String.ends_with ~suffix:"replies=2" output) "the owned terminal did not answer both probes")
      (fun () -> close probing) >>= fun () ->
    let duplex=spawn "pipe" "" "duplex" in
    Lwt.finalize (fun () ->
      let sending=Lwt_io.write duplex#stdin (String.make (256*1024) 'i') >>= fun () -> Lwt_io.flush duplex#stdin in
      Lwt_unix.with_timeout 8. (fun () -> Lwt.both sending (Lwt_io.read duplex#stdout)) >>= fun (_,output) ->
      wait_exit duplex >|= fun status ->
      check (status=Unix.WEXITED 0 && output=String.make (256*1024) 'o' ^ "complete\n") "full duplex pipe backpressure lost input or output")
      (fun () -> close duplex));
  print_endline "Owned runtimes: lock fences, descendants, EOF, PTY input/resize, program status probe and full duplex passed")

let () = match Array.to_list Sys.argv with
  | _::"--fixture"::mode::root::[] -> fixture mode root
  | _::_binary::helper::[] -> run (Unix.realpath helper)
  | _ -> failwith "usage: test_owned_runtime BIN OWNED_RUNTIME_HELPER"
