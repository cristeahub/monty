(* A documentation-only stdio MCP server. No shell, state, or network access.
   Workflow text is embedded at build time; command help uses Cmdliner's parser. *)
let field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let topics =
  let finish title lines acc =
    if title = "" then acc
    else (Slug.of_title title, title, String.concat "\n" (List.rev lines)) :: acc
  in
  let title, lines, acc =
    Embedded_manual.text |> String.split_on_char '\n'
    |> List.fold_left
         (fun (title, lines, acc) line ->
           if String.starts_with ~prefix:"## " line then
             (String.sub line 3 (String.length line - 3), [ line ], finish title lines acc)
           else (title, line :: lines, acc))
         ("", [], [])
  in
  List.rev (finish title lines acc)

let guide topic =
  if topic = "index" then
    Ok
      ("Monty workflow topics (pass the topic id to monty_help):\n"
      ^ String.concat "\n" (List.map (fun (id, title, _) -> "- " ^ id ^ ": " ^ title) topics)
      ^ "\nUse monty_command_help for exact commands and options. Help does not authorize actions; follow your assigned role and user instructions.")
  else
    match List.find_opt (fun (id, _, _) -> id = topic) topics with
    | Some (_, _, text) -> Ok text
    | None -> Error ("Unknown topic: " ^ topic ^ ". Use topic=index.")

let schema properties =
  `Assoc [ "type", `String "object"; "properties", `Assoc properties;
           "additionalProperties", `Bool false ]

let tool name description input_schema =
  `Assoc
    [ "name", `String name; "description", `String description;
      "inputSchema", input_schema;
      "annotations", `Assoc [ "readOnlyHint", `Bool true;
        "destructiveHint", `Bool false; "idempotentHint", `Bool true;
        "openWorldHint", `Bool false ] ]

let tools =
  [ tool "monty_help"
      "Read Monty's workflow guide before operating its CLI. Omit topic for an index. Covers tasks, launch, headless, handoffs, workspaces, findings and recovery. Documentation only; never executes an operation."
      (schema [ "topic", `Assoc [ "type", `String "string";
        "enum", `List (`String "index" :: List.map (fun (id, _, _) -> `String id) topics) ] ]);
    tool "monty_command_help"
      "Read exact help from this Monty binary's CLI parser. command is an array of subcommand names, e.g. [\"task\",\"workspace\",\"add\"]. Omit it for top-level help. Do not pass flags or positional values. Documentation only."
      (schema [ "command", `Assoc [ "type", `String "array"; "maxItems", `Int 4;
        "items", `Assoc [ "type", `String "string"; "pattern", `String "^[a-z][a-z0-9-]{0,47}$" ] ] ]) ]

let valid_command_word word =
  String.length word > 0 && String.length word <= 48
  && word.[0] >= 'a' && word.[0] <= 'z'
  && String.for_all (function 'a' .. 'z' | '0' .. '9' | '-' -> true | _ -> false) word

let text_result = function
  | Ok text -> `Assoc [ "content", `List [ `Assoc [ "type", `String "text"; "text", `String text ] ] ]
  | Error message -> `Assoc [ "isError", `Bool true;
      "content", `List [ `Assoc [ "type", `String "text"; "text", `String message ] ] ]

let call ~command_help params =
  let argument name default = match field "arguments" params with
    | None | Some (`Assoc []) -> Some default
    | Some (`Assoc [key, value]) when key = name -> Some value
    | _ -> None
  in
  match field "name" params with
  | Some (`String "monty_help") ->
      (match argument "topic" (`String "index") with
       | Some (`String topic) when String.length topic <= 100 -> Ok (text_result (guide topic))
       | _ -> Error (-32602, "expected only an optional string topic"))
  | Some (`String "monty_command_help") ->
      (match argument "command" (`List []) with
       | Some (`List words) when List.length words <= 4
           && List.for_all (function `String w -> valid_command_word w | _ -> false) words ->
           let words = List.map Yojson.Safe.Util.to_string words in
           Ok (text_result (command_help words))
       | _ -> Error (-32602, "command must contain at most four subcommand names, without flags or arguments"))
  | _ -> Error (-32602, "unknown tool")

let response id result =
  `Assoc ([ "jsonrpc", `String "2.0"; "id", id ] @
    match result with
    | Ok value -> [ "result", value ]
    | Error (code, message) -> [ "error", `Assoc [ "code", `Int code; "message", `String message ] ])

let serve ~command_help () =
  let initialized = ref false in
  let ready = ref false in
  let dispatch method_name params =
    match method_name with
    | "initialize" when not !initialized ->
        (match field "protocolVersion" params, field "capabilities" params, field "clientInfo" params with
         | Some (`String requested), Some (`Assoc _), Some info
           when (match field "name" info, field "version" info with
             | Some (`String _), Some (`String _) -> true
             | _ -> false) ->
             initialized := true;
             let version = if List.mem requested [ "2024-11-05"; "2025-03-26"; "2025-06-18"; "2025-11-25" ]
               then requested else "2025-11-25" in
             Ok (`Assoc [ "protocolVersion", `String version;
               "capabilities", `Assoc [ "tools", `Assoc [] ];
               "serverInfo", `Assoc [ "name", `String "monty"; "version", `String "1" ];
               "instructions", `String "Monty CLI documentation. Use monty_help to discover and read the relevant workflow, then monty_command_help for exact syntax. These tools only return documentation. Execute authorized operations through the installed monty CLI. Your role and user instructions still govern actions." ])
         | _ -> Error (-32602, "initialize requires protocolVersion, capabilities and clientInfo"))
    | "ping" -> Ok (`Assoc [])
    | _ when not !ready -> Error (-32002, "initialize and send notifications/initialized first")
    | "tools/list" -> Ok (`Assoc [ "tools", `List tools ])
    | "tools/call" -> call ~command_help params
    | _ -> Error (-32601, "method not found")
  in
  let handle json =
    match json with
    | `Assoc fields when field "jsonrpc" json = Some (`String "2.0") ->
        (match field "method" json, List.assoc_opt "id" fields with
         | Some (`String method_name), None ->
             if method_name = "notifications/initialized" && !initialized then ready := true;
             None
         | Some (`String method_name), Some ((`Int _ | `Intlit _ | `String _) as id) ->
             let params = Option.value ~default:(`Assoc []) (field "params" json) in
             Some (response id (match params with
               | `Assoc _ -> dispatch method_name params
               | _ -> Error (-32602, "params must be an object")))
         | _ -> Some (response `Null (Error (-32600, "invalid request"))))
    | _ -> Some (response `Null (Error (-32600, "invalid request")))
  in
  (* Bound even unterminated frames before parsing. Oversized input closes this
     client process; no daemon or other sessions are affected. *)
  let read_line () =
    let buffer = Buffer.create 512 in
    let rec read () =
      match input_char stdin with
      | '\n' -> Some (Buffer.contents buffer)
      | char ->
          if Buffer.length buffer >= 65_536 then failwith "MCP request exceeds 64 KiB";
          Buffer.add_char buffer char; read ()
      | exception End_of_file -> if Buffer.length buffer = 0 then None else Some (Buffer.contents buffer)
    in read ()
  in
  let rec loop () =
    match read_line () with
    | None -> ()
    | Some line ->
        let result = try handle (Yojson.Safe.from_string line) with
          | Yojson.Json_error _ -> Some (response `Null (Error (-32700, "invalid JSON"))) in
        Option.iter (fun json -> output_string stdout (Yojson.Safe.to_string json ^ "\n"); flush stdout) result;
        loop ()
  in
  try loop (); 0 with
  | Failure message | Sys_error message -> prerr_endline ("monty mcp: " ^ message); 1
