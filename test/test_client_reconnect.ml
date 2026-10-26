open Lwt.Infix
open Test_support
let contains=Monty.Home.contains_substring
let fixture=Unix.realpath Sys.argv.(2)
(* Where each occurrence of [needle] starts in [text]. *)
let positions needle text=
  let size=String.length needle in
  List.init (max 0 (String.length text-size+1)) Fun.id |> List.filter(fun index->String.sub text index size=needle)
let count needle text=List.length(positions needle text)

(* A terminal client on its own PTY, with standard output on a pipe: the pipe carries exactly
   the native bytes the client forwards, the PTY its input and Monty's own notices. *)
type client={pid:int;master:Unix.file_descr;slave:Unix.file_descr;screen:Buffer.t;stdout:Buffer.t;status:Unix.process_status Lwt.t}
let attach ~env ~cwd program args=
  let master,slave=Test_pty.open_pair () in Test_pty.resize slave ~rows:40 ~cols:120;
  let out,into=Unix.pipe ~cloexec:true () in
  let pid=match Unix.fork () with
    | 0 -> (try ignore(Unix.setsid ());
        Unix.dup2 slave Unix.stdin;Unix.dup2 into Unix.stdout;Unix.dup2 slave Unix.stderr;Unix.chdir cwd;
        Unix.execve program (Array.of_list(program::args)) (Array.of_list(List.map(fun (key,value)->key ^ "=" ^ value)env))
      with _ -> Unix._exit 127)
    | pid -> pid in
  Unix.close into;
  let screen=Buffer.create 4096 and stdout=Buffer.create 4096 in
  let drain fd buffer=
    let fd=Lwt_unix.of_unix_file_descr fd and bytes=Bytes.create 8192 in
    let rec loop ()=Lwt_unix.read fd bytes 0 8192 >>= fun count ->
      if count=0 then Lwt.return_unit else (Buffer.add_subbytes buffer bytes 0 count;loop ()) in
    Lwt.async(fun ()->Lwt.catch loop (fun _->Lwt.return_unit)) in
  drain master screen;drain out stdout;
  {pid;master;slave;screen;stdout;status=Lwt_unix.waitpid [] pid >|= snd}
let send client text=ignore(Unix.write_substring client.master text 0 (String.length text))
let output client=Buffer.contents client.stdout
let screen client=Buffer.contents client.screen
let running client=Lwt.is_sleeping client.status
let finished ?(seconds=40.) client=
  match Lwt_main.run(Lwt_unix.with_timeout seconds (fun ()->Lwt.protected client.status)) with
  | Unix.WEXITED code -> code | _ -> -1
let stop client=if running client then (try Unix.kill(-client.pid)Sys.sigkill with Unix.Unix_error _->());
  ignore(Lwt_main.run(Lwt.protected client.status))
let alive pid=try Unix.kill pid 0;true with Unix.Unix_error(Unix.ESRCH,_,_)->false
let signal pid number=try Unix.kill pid number with Unix.Unix_error(Unix.ESRCH,_,_)->()
(* A client run under a shell that reports the terminal modes it leaves behind; the shell
   keeps the PTY's session alive, so the modes stay readable after the client exits. *)
let reporting args=["/bin/sh";"-c";"\"$@\"; code=$?; stty -a; exit $code";"sh"] @ args
let restored client=
  let words=String.split_on_char '\n' (output client) |> List.concat_map(String.split_on_char ' ') in
  List.mem "icanon" words && List.mem "echo" words

let () = with_temp_dir ~prefix:"monty-reconnect-" (fun home ->
  let repo=home / "repo" and account=home / "claude" in mkdir account;mkdir repo;
  List.iter(fun args->equal 0(run ~cwd:repo "git" args).code)
    [["init";"-q"];["config";"user.email";"monty@example.invalid"];["config";"user.name";"Monty"];["commit";"-q";"--allow-empty";"-m";"initial"]];
  copy_tree (Filename.dirname bin / ".." / "agent-profiles") (home / "agent-profiles");
  let cli=home / "claude-cli" in write cli ("#!/bin/sh\nexec " ^ Monty.Shell.quote fixture ^ " \"$@\"\n");Unix.chmod cli 0o700;
  let config=home / "profiles.json" in
  write config(Yojson.Safe.to_string(obj["profiles",`List[obj["id",string "claude";"harness",string "claude";"command",string cli;
    "claude_home",string account;"roots",`List[string repo;string home]]]]));
  let service=Service.create home config in
  let clients=ref [] in
  let attach ?(timeout="2") ?(cwd=repo) args=
    let env=merge_env service.env["CLAUDE_CONFIG_DIR",account;"MONTY_TEST_CLIENT_TIMEOUT",timeout;"TERM","xterm-256color";
      "PATH",(home / "fake-bin") ^ ":" ^ List.assoc "PATH" service.env] in
    let client=attach ~env ~cwd (List.hd args) (List.tl args) in clients:=client :: !clients;client in
  let terminal ?timeout args=attach ?timeout (bin :: args @ ["--home";home]) in
  let native ()=Yojson.Safe.from_file(account / "terminal.json") in
  let session native=match Client.req(Service.client service)"conversations" with
    | `List chats->List.find_map(fun chat->List.find_opt(fun sess->s "native_id" sess=native)(xs "sessions" chat) |> Option.map(fun sess->s "id" chat,s "id" sess))chats |> Option.get
    | _->assert false in
  let retained native=let cid,sid=session native in read(home / ".monty/conversations" / cid / ("terminal-" ^ sid ^ ".output")) in
  let started previous=until ~predicate:(fun row->row<>`Null && field "native" row<>previous)(fun ()->
    if Sys.file_exists(account / "terminal.json") then native () else `Null) in
  Fun.protect ~finally:(fun ()->Option.iter(fun child->signal(pid child)Sys.sigcont)service.process;
    List.iter stop !clients;List.iter(fun client->Unix.close client.slave;Unix.close client.master) !clients;Service.close service) (fun ()->

    (* A service stalled past the request timeout: the client keeps its terminal, delivers
       the input typed meanwhile exactly once and resumes output at the right offset. *)
    let client=terminal ["terminal";"--harness";"claude"] in
    let first=started `Null in let id=s "native" first in
    wait(fun ()->contains(output client)("NATIVE CLAUDE " ^ id));
    send client "one\r";wait(fun ()->contains(output client)"RECEIVED one");
    signal(Service.pid service)Sys.sigstop;
    send client "two\r";pause 6.;
    check(running client)("client died while the service was stalled: " ^ screen client);
    check(contains(screen client)"reconnecting to the Monty service")"no reconnect notice";
    signal(Service.pid service)Sys.sigcont;
    wait ~seconds:20.(fun ()->contains(output client)"RECEIVED two");
    (* The reconnect notice is redrawn over by a one-column size change of the 120-column PTY. *)
    let receipts=home / ".monty/conversation-commands" in
    wait(fun ()->Sys.readdir receipts |> Array.exists(fun name->let request=field "request"(Yojson.Safe.from_file(receipts / name)) in
      field "op" request=string "terminal_resize" && field "cols" request=`Int 119));
    send client "three\r";wait(fun ()->contains(output client)"RECEIVED three");
    wait(fun ()->output client=retained id);
    let bytes=retained id in
    let received=List.map(fun word->positions("RECEIVED " ^ word)bytes)["one";"two";"three"] in
    check(List.for_all(fun found->List.length found=1)received)("input delivered twice or lost: " ^ String.escaped bytes);
    check(List.concat received=List.sort compare(List.concat received))"input reordered";

    (* `monty continue` reattaches a head-butler session to the same running native. *)
    stop client;check(alive(i "pid" first))"client loss ended the native";
    let again=attach (reporting(bin :: ["continue";id;"--harness";"claude";"--claude-command";cli;"--home";home])) in
    wait(fun ()->contains(output again)"RECEIVED three");send again "four\r";wait(fun ()->contains(output again)"RECEIVED four");
    equal ~label:"reattach started another native"(i "pid" first)(i "pid"(native ()));equal 1(count "NATIVE CLAUDE"(retained id));

    (* A replaced service no longer owns the terminal: the client stops cleanly with guidance. *)
    signal(-again.pid) Sys.sigstop;
    Option.iter kill service.process;Option.iter(fun child->ignore(await ~timeout:10. child))service.process;Service.start service;
    signal(-again.pid) Sys.sigcont;
    let code=finished again in
    equal ~label:("replaced service exit: " ^ screen again)69 code;
    List.iter(fun text->check(contains(screen again)text)("missing replacement guidance " ^ text ^ ": " ^ screen again))
      ["lost the Monty service: it was restarted or replaced";"may still be running in the Monty service";"monty continue '" ^ id ^ "' --harness claude"];
    check(not(contains(screen again)"internal error"))"internal error surfaced";check(restored again)"terminal mode not restored";

    (* A stopped service released its lock: no waiting for it to come back. *)
    let stopped=attach (reporting[bin;"terminal";"--harness";"claude";"--home";home]) in
    let second=started(string id) in wait(fun ()->contains(output stopped)("NATIVE CLAUDE " ^ s "native" second));
    signal(-stopped.pid) Sys.sigstop;
    Option.iter kill service.process;Option.iter(fun child->ignore(await ~timeout:10. child))service.process;
    signal(-stopped.pid) Sys.sigcont;
    equal ~label:("stopped service exit: " ^ screen stopped)69(finished stopped);
    check(contains(screen stopped)"lost the Monty service: it stopped")("missing stop reason: " ^ screen stopped);
    check(restored stopped)"terminal mode not restored";
    Service.start service;

    (* A service that holds its lock but never answers is given up after a bounded window. *)
    let silent=terminal ~timeout:"1" ["terminal";"--harness";"claude"] in
    let third=started(field "native" second) in let id=s "native" third in
    wait(fun ()->contains(output silent)("NATIVE CLAUDE " ^ id));
    signal(Service.pid service)Sys.sigstop;
    let code=finished ~seconds:60. silent in signal(Service.pid service)Sys.sigcont;
    equal ~label:("silent service exit: " ^ screen silent)69 code;
    check(contains(screen silent)"lost the Monty service: it stopped answering")("missing silence reason: " ^ screen silent);
    check(alive(i "pid" third))"giving up ended the native";
    let back=attach (bin :: ["continue";id;"--harness";"claude";"--claude-command";cli;"--home";home]) in
    wait(fun ()->contains(output back)("NATIVE CLAUDE " ^ id));send back "/exit\r";
    equal ~label:"native exit after reattach"0(finished back);
    print_endline "terminal client survives a stalled service, delivers input once, reattaches with monty continue and stops cleanly when the service is replaced, stopped or silent";

    (* A worker client dies while its runtime keeps the worker lock: `monty resume` attaches a
       new client to that runtime, `monty done` completes it from inside that session, and the
       lock still fences everything else. *)
    let fake_bin=home / "fake-bin" in
    List.iter(fun name->write(fake_bin / name)"#!/bin/sh\nexit 0\n";Unix.chmod(fake_bin / name)0o700)["ghostty";"osascript"];
    write(fake_bin / "wt")(String.concat "\n"["#!/bin/sh";"set -eu";"root=" ^ Monty.Shell.quote(home / "worktrees");
      "worktree=\"$root/$(printf '%s' \"${2-}\" | tr '/ ' '__')\"";"case \"$1\" in";
      "  b) [ -d \"$worktree\" ] || { mkdir -p \"$root\"; git worktree add -q -b \"$2\" \"$worktree\"; }";
      "    printf '%s\\n' \"$worktree\" ;;";
      "  list) printf '%s\\n' 'repo:'";
      "    for path in \"$root\"/*; do if [ -d \"$path\" ]; then printf '  %s -> %s\\n' \"$(git -C \"$path\" symbolic-ref --short HEAD)\" \"$path\"; fi; done ;;";
      "  db) git worktree remove -f \"$worktree\"; git branch -D \"$2\" >/dev/null ;;";
      "  *) exit 92 ;;";"esac";""]);
    Unix.chmod(fake_bin / "wt")0o700;
    let context=home / "context.md" in write context "Reattach fixture.\n";
    let env=merge_env service.env["CLAUDE_CONFIG_DIR",account;"PATH",fake_bin ^ ":" ^ List.assoc "PATH" service.env] in
    let monty args=run ~cwd:repo ~env bin(args @ ["--home";home]) in
    let options=["--terminal";"ghostty";"--harness";"claude";"--claude-command";cli;"--wt-command";fake_bin / "wt"] in
    List.iter(fun args->let result=monty args in check(result.code=0)(result.stderr ^ result.stdout))
      [["projects";"add";"--repo";repo];["launch";"--repo";repo;"--title";"Reattached worker";"--branch";"cto/reattached";"--context";context;"--worktree";"always"] @ options];
    let job=home / ".monty/runs/manual/workers/reattached/job.json" in
    let script ()=s "launch_script"(Yojson.Safe.from_file job) in
    let worker=attach ["/bin/sh";script ()] in
    let fourth=started(string id) in let id=s "native" fourth in
    wait(fun ()->contains(output worker)("NATIVE CLAUDE " ^ id));stop worker;check(alive(i "pid" fourth))"worker client loss ended the native";
    List.iter(fun (args,refusal)->let result=monty args in
      check(result.code<>0 && contains result.stderr refusal)("live worker runtime was not fenced: " ^ String.concat " " args ^ ": " ^ result.stderr))
      [["resume";"reattached";"--fresh"] @ options,"busy";["done";"reattached";"--wt-command";fake_bin / "wt"],"run monty done inside that session"];
    let resumed=monty(["resume";"reattached"] @ options) in check(resumed.code=0)("resume did not reattach: " ^ resumed.stderr ^ resumed.stdout);
    let rewritten=read(script ()) in
    check(contains rewritten "--mode exact" && contains rewritten("--session '" ^ id ^ "'"))"resume did not target the live session";
    let reattached=attach ["/bin/sh";script ()] in
    wait(fun ()->contains(output reattached)("NATIVE CLAUDE " ^ id));send reattached "after resume\r";
    wait(fun ()->contains(output reattached)"RECEIVED after resume");
    equal ~label:"resume started another native"(i "pid" fourth)(i "pid"(native ()));equal 1(count "NATIVE CLAUDE"(retained id));
    let session=["MONTY_JOB_ID","reattached";"MONTY_WORKER_DIR",Filename.dirname job;"MONTY_JOB_FILE",job] in
    let completed=run ~cwd:repo ~env:(merge_env env session) bin["done";"--home";home;"--wt-command";fake_bin / "wt"] in
    check(completed.code=0 && contains completed.stdout "Archived")("monty done inside the live worker failed: " ^ completed.stderr ^ completed.stdout);
    check(not(Sys.file_exists job) && not(Sys.file_exists(home / "worktrees/cto_reattached")))"completion left the worker active";
    check(alive(i "pid" fourth))"completion ended the native session";
    send reattached "/exit\r";equal ~label:"worker native exit"0(finished reattached);
    print_endline "monty resume reattaches a worker client to its live runtime and monty done completes it from inside, while the worker lock still fences fresh resumes and outside lifecycle operations"))
