open Monty

let require condition message = if not condition then failwith message
let must = function Ok value -> value | Error message -> failwith message

let error_contains expected = function
  | Ok _ -> failwith ("expected error containing " ^ expected)
  | Error message ->
      require (Home.contains_substring message expected)
        (Printf.sprintf "expected error containing %S, got %S" expected message)

let test_claude_result_fields () =
  let events = Filename.temp_file "monty-claude-boundary-" ".jsonl" in
  Fun.protect ~finally:(fun () -> Sys.remove events) (fun () ->
      let fields =
        [ "type", `String "result";
          "subtype", `String "success";
          "is_error", `Bool false;
          "permission_denials", `List [];
          "session_id", `String "12345678-1234-4234-8234-123456789abc";
          "result", `String "  Validated handoff.  " ]
      in
      let result = `Assoc fields in
      let parse records =
        Shell.write_file events
          (String.concat "\n" (List.map Yojson.Safe.to_string records) ^ "\n");
        Headless.claude_final_message events
      in
      require (must (parse [ result ]) = "Validated handoff.\n")
        "valid Claude result changed";
      List.iter
        (fun (name, values) ->
          List.iter
            (fun value ->
              let invalid = `Assoc ((name, value) :: List.remove_assoc name fields) in
              require (Result.is_error (parse [ invalid ]))
                ("malformed Claude field accepted: " ^ name))
            values)
        [ "type", [ `Null; `Int 1; `List [] ];
          "subtype", [ `Null; `Int 1; `Assoc [] ];
          "is_error", [ `Null; `Int 0; `String "false"; `Bool true ];
          "permission_denials", [ `Int 0; `String "none"; `List [ `Assoc [] ] ];
          "session_id", [ `Null; `Int 1; `Assoc []; `String ""; `String "invalid" ];
          "result", [ `Null; `Int 1; `List []; `String "  " ] ];
      List.iter
        (fun trailing ->
          require (Result.is_error (parse [ result; trailing ]))
            "Claude accepted an event after its terminal result")
        [ `Null; `String "trailing"; `List []; `Assoc [ "type", `String "assistant" ] ];
      require (Result.is_error (parse [ result; result ]))
        "Claude accepted duplicate terminal results";
      let without_denials = `Assoc (List.remove_assoc "permission_denials" fields) in
      require (must (parse [ without_denials ]) = "Validated handoff.\n")
        "Claude result without optional permission denials was rejected")

let test_child_results () =
  let text : string Headless.spawned_result =
    Headless.spawn_result (fun () -> Ok "typed text")
  in
  let count : int Headless.spawned_result =
    Headless.spawn_result (fun () -> Ok 42)
  in
  let json : Yojson.Safe.t Headless.spawned_result =
    Headless.spawn_result (fun () ->
        Ok (`Assoc [ "retained", `String (String.make 131_072 'x') ]))
  in
  require (must (Headless.await_result text) = "typed text") "text child result changed";
  require (must (Headless.await_result count) = 42) "integer child result changed";
  (match must (Headless.await_result json) with
  | `Assoc [ "retained", `String value ] ->
      require (String.length value = 131_072) "large child payload was truncated"
  | _ -> failwith "JSON child result changed");
  Headless.spawn_result (fun () -> Error "reported phase failure")
  |> Headless.await_result |> error_contains "reported phase failure";
  Headless.spawn_result (fun () -> failwith "unexpected phase failure")
  |> Headless.await_result |> error_contains "unexpected phase failure";
  Headless.spawn_result (fun () -> Unix._exit 7)
  |> Headless.await_result |> error_contains "exited with code 7";
  Headless.spawn_result (fun () -> Unix._exit 0)
  |> Headless.await_result |> error_contains "returned no result";
  Headless.run_parallel
    [ (fun () -> Ok ());
      (fun () -> Error "first failed review");
      (fun () -> Error "second failed review") ]
  |> fun result ->
  error_contains "first failed review" result;
  error_contains "second failed review" result

let () =
  test_claude_result_fields ();
  test_child_results ();
  print_endline "Headless result boundaries and typed child transport passed"
