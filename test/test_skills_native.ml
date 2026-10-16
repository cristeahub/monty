open Test_support
open Lwt.Infix

(* Opt-in installed Codex/Pi discovery only: fresh homes and no inference. *)
let with_native ~root ~environment harness args f =
  let log = root / (harness ^ ".log") in
  let errors = Unix.openfile log [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600 in
  let process =
    Fun.protect ~finally:(fun () -> Unix.close errors) (fun () ->
      Lwt_process.open_process ~cwd:root
        ~env:(Array.of_list (List.map (fun (key, value) -> key ^ "=" ^ value) environment))
        ~stderr:(`FD_copy errors) (harness, Array.of_list (harness :: args)))
  in
  let send value =
    Lwt_io.write_line process#stdin (Yojson.Safe.to_string value) >>= fun () ->
    Lwt_io.flush process#stdin
  in
  let reply identity =
    let rec next () =
      Lwt_io.read_line_opt process#stdout >>= function
      | None -> Lwt.fail_with (harness ^ " exited before discovery: " ^ read log)
      | Some line ->
          let value = try Some (Yojson.Safe.from_string line) with Yojson.Json_error _ -> None in
          (match value with Some value when field "id" value = identity -> Lwt.return value | _ -> next ())
    in
    Lwt.pick [ next (); (Lwt_unix.sleep 30. >>= fun () -> Lwt.fail_with (harness ^ " discovery timed out")) ]
  in
  let stop () =
    (try process#kill Sys.sigterm with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
    Lwt.pick [ (Lwt.protected process#status >|= fun _ -> true); (Lwt_unix.sleep 10. >|= fun () -> false) ]
    >>= fun stopped ->
    if not stopped then (try process#kill Sys.sigkill with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
    process#close >|= fun _ -> ()
  in
  Lwt_main.run (Lwt.finalize (fun () -> f send reply) stop)

let suite () = with_temp_dir ~prefix:"monty-skill-native-" (fun root ->
  let environment =
    merge_env (List.filter (fun (key, _) -> List.mem key [ "PATH"; "LANG"; "TMPDIR" ]) env)
      [ "HOME", root; "CODEX_HOME", root / "codex"; "PI_CODING_AGENT_DIR", root / "pi";
        "MONTY_HOME", root / "monty"; "PI_TELEMETRY", "0" ]
  in
  List.iter (fun placement -> List.iter (fun harness ->
    let installed = run ~cwd:root ~env:environment bin [ "skills"; "install"; "--harness"; harness ] in
    check (installed.code = 0) (installed.stdout ^ installed.stderr);
    if placement = "shared" then (
      let key = if harness = "codex" then "CODEX_HOME" else "PI_CODING_AGENT_DIR" in
      let source = List.assoc key environment / "skills/monty" and target = root / ".agents/skills/monty" in
      mkdir (Filename.dirname target);
      if Sys.file_exists target then remove_tree target;
      Unix.rename source target);
    let args = if harness = "codex" then [ "app-server" ] else
      [ "--mode"; "rpc"; "--offline"; "--no-approve"; "--no-tools";
        "--no-extensions"; "--no-prompt-templates"; "--no-context-files" ] in
    with_native ~root ~environment harness args (fun send reply ->
      (if harness = "codex" then (
        send (obj [ "id", `Int 1; "method", string "initialize";
          "params", obj [ "clientInfo", obj [ "name", string "monty-skill-test"; "version", string "1" ];
            "capabilities", obj [ "experimentalApi", `Bool true ] ] ]) >>= fun () ->
        reply (`Int 1) >>= fun initialized ->
        check (not (List.mem_assoc "error" (match initialized with `Assoc values -> values | _ -> []))) "Codex initialize failed";
        send (obj [ "method", string "initialized" ]) >>= fun () ->
        send (obj [ "id", `Int 2; "method", string "skills/list";
          "params", obj [ "cwds", `List [ string root ]; "forceReload", `Bool true ] ]) >>= fun () ->
        reply (`Int 2) >|= fun result ->
        let skills = List.concat_map (xs "skills") (xs "data" (field "result" result)) in
        check (List.exists (fun skill -> s "name" skill = "monty" && field "enabled" skill = `Bool true) skills)
          ("Codex did not discover installed Monty skill: " ^ Yojson.Safe.to_string result))
      else (
        send (obj [ "id", string "skills"; "type", string "get_commands" ]) >>= fun () ->
        reply (string "skills") >|= fun result ->
        equal (`Bool true) (field "success" result);
        check (List.exists (fun command -> s "name" command = "skill:monty") (xs "commands" (field "data" result)))
          ("Pi did not discover installed Monty skill: " ^ Yojson.Safe.to_string result)))
      >|= fun () -> Printf.printf "%s (%s): native discovery of automatically installed Monty skill passed\n%!" harness placement))
    [ "codex"; "pi" ]) [ "profile"; "shared" ])

let () = suite ()
