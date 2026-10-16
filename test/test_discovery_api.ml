open Test_support

let required key = function `Assoc values -> List.assoc key values | _ -> failwith "expected object"
let query path values =
  path ^ "?" ^ Uri.encoded_of_query (List.map (fun (key, value) -> key, [ value ]) values)

let suite () = with_temp_dir ~prefix:"monty-discovery-" (fun home ->
  let repo = home / "repo" in mkdir repo;
  equal 0 (run "git" [ "init"; "-q"; repo ]).code;
  let native = Native.create (home / "native.sock") in
  let pi = home / "pi" in let sessions = pi / "sessions" in mkdir sessions;
  let config = home / "profiles.json" in
  write config (Yojson.Safe.to_string (obj [ "allow_fixture", `Bool true; "profiles", `List [
    obj [ "id", string "local"; "harness", string "fixture";
          "socket", string (home / "native.sock"); "roots", `List [ string repo ] ];
    obj [ "id", string "pi"; "harness", string "pi"; "command", string "/must-not-run";
          "pi_home", string pi; "roots", `List [ string repo ] ] ] ]));
  let service = Service.create home config in
  Fun.protect ~finally:(fun () -> Service.close service; Native.close native) (fun () ->
    let client = Service.client service in
    for number = 0 to 25 do
      let id = string_of_int number in
      Native.set_thread native id (obj [ "id", string id; "name", string ("Native chat " ^ id);
        "preview", string "Saved message"; "cwd", string repo; "updatedAt", `Int number; "turns", `List [] ]);
      let records = [
        obj [ "type", string "session"; "version", `Int 3; "id", string id; "cwd", string repo ];
        obj [ "type", string "message"; "id", string "u";
              "message", obj [ "role", string "user"; "content", string "Saved message" ] ];
        obj [ "type", string "session_info"; "id", string "title"; "name", string ("Pi chat " ^ id) ] ] in
      write (sessions / Printf.sprintf "%02d.jsonl" number)
        (String.concat "\n" (List.map Yojson.Safe.to_string records) ^ "\n")
    done;
    Native.set_thread native "archived" (obj [ "id", string "archived"; "name", string "Old chat";
      "archived", `Bool true; "turns", `List [] ]);
    let zero = Hashtbl.find native.threads "0" in
    Native.set_thread native "0" (puts [ "name", `Null;
      "preview", string (String.make 499 'a' ^ String.concat "" (List.init 5 (fun _ -> "🦊"))) ] zero);
    write (sessions / "25.jsonl")
      (String.concat "\n" (List.map Yojson.Safe.to_string [
        obj [ "type", string "session"; "version", `Int 3; "id", string "25"; "cwd", string repo ];
        obj [ "type", string "session_info"; "id", string "info"; "name", string "Metadata only" ] ]) ^ "\n");
    let snapshot () =
      Sys.readdir sessions |> Array.to_list |> List.sort String.compare
      |> List.map (fun name -> name, read (sessions / name)) in
    let originals = snapshot () in
    List.iter (fun profile ->
      let page = Client.req client (query "discovery" [ "profile", profile ]) in
      if profile = "local" then equal (String.make 499 'a' ^ "…") (s "title" (List.hd (xs "data" page)));
      equal 25 (List.length (xs "data" page)); check (s "next_cursor" page <> "") "missing discovery cursor";
      check (List.for_all (fun row -> required "conversation" row = `Null) (xs "data" page)) "discovery imported a chat";
      let more = Client.req client (query "discovery" [ "profile", profile; "cursor", s "next_cursor" page ]) in
      equal 1 (List.length (xs "data" more)); equal `Null (required "next_cursor" more);
      equal (`List []) (required "errors" page); equal (`List []) (required "errors" more);
      let history = Client.req client (query "discovery/history"
          [ "profile", profile; "native_id", "0"; "session_file", sessions / "00.jsonl" ]) in
      equal (`Bool true) (field "read_only" history)) [ "local"; "pi" ];
    equal (`List []) (Client.req client "conversations");
    equal (`List []) (required "tasks" (Client.req client "inventory"));
    equal originals (snapshot ());
    check (List.for_all (fun (method_, _) -> List.mem method_ [ "initialize"; "initialized"; "thread/list"; "thread/read" ]) native.calls)
      "read-only discovery invoked a mutating native method";
    let archived = Client.req client "discovery?profile=local&archived=true" in
    equal "archived" (s "native_id" (List.hd (xs "data" archived)));
    write (sessions / "00.jsonl") "broken\n";
    let partial = Client.req client "discovery?profile=pi" in
    equal "partial" (s "coverage" partial); check (xs "errors" partial <> []) "invalid native file lost coverage error";
    http_error 400 (fun () -> Client.req client "discovery?profile=unknown");
    http_error 403 (fun () -> Client.req client (query "discovery/history"
      [ "profile", "pi"; "session_file", home / "secret.jsonl" ]));
    print_endline "Discovery: both sources, pagination, read-only history, archive and partial coverage passed"))

let () = suite ()
