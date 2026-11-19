(* A worker whose Rex tab closed keeps running in the Monty service. This drives it
   the way a user meets it: a real service and checkout binary, a fake native
   Claude, a fake rex, and the launch script run in a real PTY as a Rex tab would. *)
open Lwt.Infix
open Test_support

let contains = Monty.Home.contains_substring
let require text needle = check (contains text needle) ("missing " ^ needle ^ " in " ^ text)
let json path = Yojson.Safe.from_file path

type tab = { pid : int; fd : Lwt_unix.file_descr; output : Buffer.t; status : Unix.process_status Lwt.t }

(* What Rex does for a block: a new session on a PTY running the launch script. *)
let open_tab ~cwd ~environment script =
  let master, slave = Test_pty.open_pair () in
  Test_pty.set_raw slave;
  Test_pty.resize slave ~rows:40 ~cols:120;
  let pid =
    match Unix.fork () with
    | 0 -> (
        try
          ignore (Unix.setsid ());
          Unix.close master;
          List.iter (Unix.dup2 slave) [ Unix.stdin; Unix.stdout; Unix.stderr ];
          Unix.close slave;
          Unix.chdir cwd;
          Unix.execve "/bin/sh" [| "/bin/sh"; script |]
            (Array.of_list (List.map (fun (key, value) -> key ^ "=" ^ value) environment))
        with _ -> Unix._exit 127)
    | pid -> pid
  in
  Unix.close slave;
  let fd = Lwt_unix.of_unix_file_descr master and output = Buffer.create 8192 in
  let bytes = Bytes.create 8192 in
  let rec drain () =
    Lwt_unix.read fd bytes 0 (Bytes.length bytes) >>= fun count ->
    if count = 0 then Lwt.return_unit else (Buffer.add_subbytes output bytes 0 count; drain ())
  in
  Lwt.async (fun () -> Lwt.catch drain (fun _ -> Lwt.return_unit));
  { pid; fd; output; status = Lwt_unix.waitpid [] pid >|= snd }

let text tab = Buffer.contents tab.output

let type_into tab text =
  ignore (Lwt_main.run (Lwt_unix.write_string tab.fd text 0 (String.length text)))

(* Closing a Rex tab hangs up its session and closes the PTY. *)
let close_tab tab =
  (try Unix.kill (-tab.pid) Sys.sighup with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
  ignore (Lwt_main.run (Lwt_unix.with_timeout 10. (fun () -> Lwt.protected tab.status)));
  Lwt_main.run (Lwt.catch (fun () -> Lwt_unix.close tab.fd) (fun _ -> Lwt.return_unit))

let alive pid = try Unix.kill pid 0; true with Unix.Unix_error (Unix.ESRCH, _, _) -> false

let suite claude = with_temp_dir ~prefix:"monty-reattach-" (fun home ->
  let repo = home / "repo" and account = home / "claude" and fake = home / "fake-bin" in
  List.iter mkdir [ repo; account; fake ];
  let cli_wrapper = home / "claude-cli" in
  write cli_wrapper ("#!/bin/sh\nexec " ^ Monty.Shell.quote claude ^ " \"$@\"\n");
  Unix.chmod cli_wrapper 0o700;
  write (fake / "rex") ("#!/bin/sh\nexec " ^ Monty.Shell.quote (Unix.realpath Sys.executable_name) ^ " --fixture \"$@\"\n");
  Unix.chmod (fake / "rex") 0o700;
  copy_tree (Filename.dirname (Filename.dirname bin) / "agent-profiles") (home / "agent-profiles");
  let config = home / "profiles.json" in
  write config (Yojson.Safe.to_string (obj [ "profiles", `List [ obj [
    "id", string "claude"; "harness", string "claude"; "command", string cli_wrapper;
    "python", string (sdk_python home); "claude_home", string account; "roots", `List [ string repo ] ] ] ]));
  (* Nothing from the caller's own Rex, Mantle or Monty session may leak into the fixture. *)
  let base = List.filter (fun (key, _) ->
      not (String.starts_with ~prefix:"REX_" key || String.starts_with ~prefix:"MONTY_" key
           || String.starts_with ~prefix:"MANTLE" key || String.starts_with ~prefix:"_MANTLE" key
           || key = "CLAUDE_CONFIG_DIR" || key = "CODEX_HOME")) env in
  let state_file = home / "rex-state.json" and log = home / "rex.log" in
  let environment = merge_env base
      [ "HOME", home; "MONTY_HOME", home; "MANTLE_CONTEXT", ""; "CLAUDE_CONFIG_DIR", account; "CODEX_HOME", home / "codex";
        "PATH", fake ^ ":" ^ List.assoc "PATH" base; "REX_FAKE_STATE", state_file; "REX_FAKE_LOG", log;
        "GIT_CONFIG_GLOBAL", "/dev/null"; "GIT_CONFIG_SYSTEM", "/dev/null"; "GIT_CONFIG_NOSYSTEM", "1";
        "TERM", "xterm-256color" ] in
  equal 0 (run ~env:environment "git" [ "init"; "-q"; repo ]).code;
  equal 0 (run ~env:environment "git"
    [ "-C"; repo; "-c"; "user.name=Test"; "-c"; "user.email=test@example.invalid"; "commit"; "-qm"; "Initial"; "--allow-empty" ]).code;
  let service = Service.create ~extra:environment home config in
  let tabs = ref [] in
  Fun.protect ~finally:(fun () ->
      List.iter (fun tab -> if Lwt.is_sleeping tab.status then close_tab tab) !tabs;
      Service.close service;
      (* A guard the service started just before stopping, such as the Claude bridge
         reading final history, treats the service's EOF as owner death and finishes
         its bounded cleanup on its own; let it end before the home is removed. *)
      wait ~seconds:15. (fun () ->
          not (List.exists (fun line -> contains line home)
                 (String.split_on_char '\n' (run "ps" [ "-axo"; "command=" ]).stdout)))) (fun () ->
    let cli ?(code = 0) ?(extra = []) args =
      let result = run ~env:(merge_env environment extra) ~new_session:true ~timeout:30. bin (args @ [ "--home"; home ]) in
      check (result.code = code) (String.concat " " args ^ "\n" ^ result.stdout ^ result.stderr);
      result.stdout ^ result.stderr in
    let rex args =
      let result = run ~env:environment (fake / "rex") args in
      equal 0 result.code; Yojson.Safe.from_string result.stdout in
    let state () = json state_file in
    let runs () =
      if not (Sys.file_exists log) then 0 else
      read log |> String.split_on_char '\n' |> List.filter (fun row -> contains row "\"run\"") |> List.length in
    (* The caller works in a Rex tab of its own session, as the head butler does. *)
    let caller = rex [ "new"; "caller"; "--json" ] in
    let session = s "session_id" caller in
    let inside = [ "REX_SESSION", session; "REX_SERVER", Fake_rex.endpoint;
                   "REX_BLOCK", Monty.Json_fields.str (List.hd (xs "block_ids" (List.hd (xs "initial_windows" caller)))) ] in
    ignore (cli [ "projects"; "add"; "--repo"; repo ]);
    let project = s "id" (List.hd (xs "projects" (json (home / ".monty/projects.json")))) in
    let added = cli [ "task"; "add"; "--project"; project; "--title"; "Reattach" ] in
    let task = String.split_on_char ' ' added |> List.map String.trim |> List.find (String.starts_with ~prefix:"local-") in
    let context = home / "context.md" in write context "# Keep working after the tab closes\n";
    let worker = "reattach" in
    let run_dir = home / ".monty/runs/reattach-test" in
    let worker_dir = run_dir / "workers" / worker and manifest = run_dir / "jobs.json" in
    write manifest (Yojson.Safe.to_string (obj [ "jobs", `List [ obj [
      "id", string worker; "title", string "Reattach"; "repo", string repo; "branch", string "test/reattach";
      "context", string context; "worker_dir", string worker_dir; "task_key", string ("local:" ^ task) ] ] ]));
    let terminal_args = [ "--terminal"; "rex"; "--target"; "tab"; "--harness"; "claude";
                          "--claude-command"; cli_wrapper; "--worktree"; "never" ] in
    let doctor () = cli [ "doctor"; "--terminal"; "rex"; "--harness"; "claude"; "--claude-command"; cli_wrapper; "--worktree"; "never" ] in
    ignore (cli ~extra:inside ([ "launch-many"; "--manifest"; manifest ] @ terminal_args));
    (* Rex runs the launch script in the block it created. *)
    let show () =
      let reference = json (worker_dir / "rex-terminal.json") in
      let tab = open_tab ~cwd:repo ~environment:(merge_env environment
          [ "REX_SESSION", s "session_id" reference; "REX_BLOCK", s "block_id" reference; "REX_SERVER", Fake_rex.endpoint ])
          (s "launch_script" (json (worker_dir / "job.json"))) in
      tabs := tab :: !tabs; tab, s "block_id" reference in
    let first, first_block = show () in
    let started = until ~predicate:(fun row -> row <> `Null) (fun () ->
        if Sys.file_exists (account / "terminal.json") then json (account / "terminal.json") else `Null) in
    let native = s "native" started and pid = i "pid" started in
    wait (fun () -> contains (text first) ("NATIVE CLAUDE " ^ native));
    let conversation = Client.req (Service.client service) "conversations" |> Monty.Json_fields.list
      |> List.find (fun c -> List.exists (fun sess -> s "native_id" sess = native) (xs "sessions" c)) in
    let sid = s "id" (List.hd (xs "sessions" conversation)) in
    let running () = field "running" (Client.req (Service.client service) ("terminals/" ^ sid)) = `Bool true in
    equal (`Int pid) (field "terminal_pid" (List.hd (xs "sessions" conversation)));
    (* How every message names the running worker. *)
    let running_in_service = Printf.sprintf "(claude session %s, pid %d)" native pid in
    (* The user closes the tab: the client hangs up, the worker keeps running. *)
    close_tab first;
    let blocks = Fake_rex.entries "blocks" (state ()) in
    write state_file (Yojson.Safe.to_string (Fake_rex.put "blocks" (obj (List.remove_assoc first_block blocks)) (state ())));
    check (running () && alive pid) "closing the tab ended the worker";
    List.iter (require (cli [ "rex"; "status"; worker ]))
      [ "terminal: gone (block no longer exists)";
        "worker: still running in the Monty service " ^ running_in_service ^ "; monty resume 'reattach'" ];
    (* Output produced while no tab is attached is retained, not left in a pipe. *)
    ignore (Client.cmd (Service.client service) "detached-input" (obj [ "op", string "terminal_input";
      "conversation", field "id" conversation; "selected_session", string sid;
      "data", string (Base64.encode_exn "while-detached\n") ]));
    let retained () =
      let rec read offset acc =
        let page = Client.req (Service.client service) (Printf.sprintf "terminals/%s?offset=%d" sid offset) in
        let data = Base64.decode_exn (s "data" page) in
        if data = "" then acc else read (i "offset" page) (acc ^ data) in
      read 0 "" in
    wait (fun () -> contains (retained ()) "RECEIVED while-detached");
    List.iter (require (doctor ()))
      [ Printf.sprintf "running in the Monty service %s, but its Rex terminal %s no longer exists" running_in_service first_block;
        "monty resume 'reattach'" ];
    (* Lifecycle commands name the interactive terminal that holds the worker, not a headless run. *)
    let refused = cli ~code:1 [ "done"; worker ] in
    List.iter (require refused) [ "busy: its interactive terminal " ^ running_in_service; "monty resume 'reattach'" ];
    check (not (contains refused "headless run")) refused;
    require (cli ~code:1 ~extra:inside ([ "resume"; worker; "--fresh" ] @ terminal_args)) ("busy: its interactive terminal " ^ running_in_service);
    (* Resume opens a new tab attached to the same runtime and conversation. *)
    let before = runs () in
    let resumed = cli ~extra:inside ([ "resume"; worker ] @ terminal_args) in
    require resumed ("still running in the Monty service " ^ running_in_service);
    equal (before + 1) (runs ());
    let second, second_block = show () in
    check (second_block <> first_block) "resume reused the vanished block";
    let script = read (s "launch_script" (json (worker_dir / "job.json"))) in
    check (contains script " --mode exact " && contains script (" --session '" ^ native ^ "'")) "resume script does not attach to the recorded session";
    wait (fun () -> contains (text second) ("NATIVE CLAUDE " ^ native) && contains (text second) "RECEIVED while-detached");
    type_into second "hello\n";
    wait (fun () -> contains (text second) "RECEIVED hello");
    equal pid (i "pid" (json (account / "terminal.json")));
    check (alive pid && running ()) "reattaching restarted the worker";
    (* With its tab live again, resume focuses it instead of opening another. *)
    let before = runs () in
    require (cli ~extra:inside ([ "resume"; worker ] @ terminal_args)) "still live";
    equal before (runs ());
    check (not (contains (doctor ()) ("worker " ^ worker))) "doctor flagged a visible running worker";
    type_into second "/exit\n";
    equal (Unix.WEXITED 0) (Lwt_main.run (Lwt_unix.with_timeout 15. (fun () -> Lwt.protected second.status)));
    wait (fun () -> not (running ()));
    equal `Null (field "terminal_pid" (List.hd (xs "sessions" (Client.req (Service.client service) ("conversations/" ^ s "id" conversation)))));
    print_endline "Worker reattach: a closed Rex tab leaves the worker running, doctor and lifecycle errors name it, and resume attaches a new tab to the same runtime"))

let () =
  match Array.to_list Sys.argv with
  | _ :: "--fixture" :: args -> Fake_rex.run args
  | [ _; _binary; claude ] -> suite (Unix.realpath claude)
  | _ -> failwith "usage: test_worker_reattach BIN FAKE_CLAUDE"
