(* `monty api list|describe|call`: discover and call the service API from the
   command line, as `rex api` does for Rex. The catalog comes from the running
   service when there is one, so it matches that service's version; without a
   service, this binary's own catalog is shown and said so. *)
open Lwt.Infix
open Json_fields
open Conversation_store

type source = Service | Binary

let catalog ~home =
  match Lwt_main.run (Client_terminal.health ~home) with
  | Some _ -> (
      match Lwt_main.run (Client_http.request ~home `GET "/v1/help" `Null) with
      | json -> (Api_catalog.of_json json, Service)
      | exception Failure _ -> (Api_catalog.operations, Binary))
  | None -> (Api_catalog.operations, Binary)
  | exception Failure _ -> (Api_catalog.operations, Binary)

let note_source = function
  | Service -> ()
  | Binary -> prerr_endline "monty: the service is not running; this is the catalog of the installed monty binary"

let print_json json =
  print_string ((if Unix.isatty Unix.stdout then Yojson.Safe.pretty_to_string ~std:true json else Yojson.Safe.to_string json) ^ "\n")

(* ---- Arguments ---------------------------------------------------------- *)

let replace text pattern replacement =
  let length = String.length pattern in
  let buffer = Buffer.create (String.length text) in
  let rec loop index =
    if index > String.length text - length then Buffer.add_string buffer (String.sub text index (String.length text - index))
    else if String.sub text index length = pattern then (
      Buffer.add_string buffer replacement;
      loop (index + length))
    else (
      Buffer.add_char buffer text.[index];
      loop (index + 1))
  in
  loop 0;
  Buffer.contents buffer

let read_all channel =
  let buffer = Buffer.create 4096 in
  let chunk = Bytes.create 4096 in
  let rec loop () =
    let count = input channel chunk 0 (Bytes.length chunk) in
    if count > 0 then (
      Buffer.add_subbytes buffer chunk 0 count;
      loop ())
  in
  loop ();
  Buffer.contents buffer

let arguments = function
  | None | Some "" -> `Assoc []
  | Some "-" -> Yojson.Safe.from_string (read_all stdin)
  | Some text when String.length text > 1 && text.[0] = '@' ->
      Yojson.Safe.from_file (String.sub text 1 (String.length text - 1))
  | Some text -> Yojson.Safe.from_string text

let placeholders path =
  let rec collect start acc =
    match String.index_from_opt path start '{' with
    | None -> List.rev acc
    | Some left -> (
        match String.index_from_opt path left '}' with
        | None -> List.rev acc
        | Some right -> collect (right + 1) (String.sub path (left + 1) (right - left - 1) :: acc))
  in
  collect 0 []

let scalar = function
  | `String value -> value
  | `Int value -> string_of_int value
  | `Float value -> Printf.sprintf "%g" value
  | `Bool value -> string_of_bool value
  | value -> Yojson.Safe.to_string value

(* Fills {param} path segments from the arguments and returns what is left. *)
let fill_path (operation : Api_catalog.operation) (fields : (string * Yojson.Safe.t) list) =
  List.fold_left
    (fun (path, fields) name ->
      match List.assoc_opt name fields with
      | Some value ->
          let encoded = Uri.pct_encode ~component:`Path (scalar value) in
          (replace path ("{" ^ name ^ "}") encoded, List.remove_assoc name fields)
      | None -> fail 400 ("missing path argument " ^ name))
    (operation.path, fields) (placeholders operation.path)

(* ---- Raw HTTP (text and bytes, not only JSON) ---------------------------- *)

let raw ~home meth path body =
  let base, token = Client_http.endpoint ~home in
  let headers =
    Cohttp.Header.of_list
      ([ ("authorization", "Bearer " ^ token) ] @ match body with Some _ -> [ ("content-type", "application/json") ] | None -> [])
  in
  let body = Cohttp_lwt.Body.of_string (match body with Some json -> Yojson.Safe.to_string json | None -> "") in
  Lwt_unix.with_timeout 35. (fun () ->
      Client_http.call ~headers ~body meth (Uri.of_string (base ^ path)) >>= fun (response, body) ->
      Cohttp_lwt.Body.to_string body >|= fun text ->
      (Cohttp.Code.code_of_status (Cohttp.Response.status response), Cohttp.Header.get (Cohttp.Response.headers response) "content-type", text))

let settled status = not (List.mem status [ "accepted"; "running" ])

let wait_receipt ~home ~timeout id =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec poll () =
    Client_http.request ~home `GET ("/v1/commands/" ^ id) `Null >>= fun receipt ->
    if settled (s "status" receipt) || Unix.gettimeofday () >= deadline then Lwt.return receipt
    else Lwt_unix.sleep 0.2 >>= poll
  in
  poll ()

let call ~home ~name ~args ~request_id ~wait ~timeout =
  let operations, source = catalog ~home in
  note_source source;
  let operation =
    match List.find_opt (fun (operation : Api_catalog.operation) -> operation.name = name) operations with
    | Some operation -> operation
    | None -> fail 404 ("unknown operation " ^ name ^ "; run monty api list")
  in
  let fields = match arguments args with `Assoc fields -> fields | _ -> fail 400 "arguments must be a JSON object" in
  if name = "events" then fail 400 "events is a server-sent event stream; read it with curl -N and Authorization: Bearer";
  (* A command's ID is the caller's receipt key: --request-id, an id argument,
     or a generated one. Other path parameters come from the arguments. *)
  let is_command = String.starts_with ~prefix:"/v1/commands/" operation.path && operation.meth = "POST" in
  let fields, path =
    if is_command then
      let id =
        match (request_id, List.assoc_opt "id" fields) with
        | Some id, _ -> id
        | None, Some (`String id) -> id
        | None, _ -> random_id ()
      in
      (List.remove_assoc "id" fields, replace operation.path "{id}" (component id))
    else (fields, operation.path)
  in
  let path, fields = fill_path { operation with path } fields in
  match operation.meth with
  | "GET" ->
      let query = List.map (fun (key, value) -> (key, [ scalar value ])) fields in
      let path = if query = [] then path else path ^ "?" ^ Uri.encoded_of_query query in
      let code, content_type, text = Lwt_main.run (raw ~home `GET path None) in
      if code >= 400 then fail code text;
      (match content_type with
       | Some kind when String.starts_with ~prefix:"application/json" kind -> print_json (Yojson.Safe.from_string text)
       | _ -> print_string text);
      0
  | _ ->
      let fields =
        match operation.op with
        | Some op -> (
            match List.assoc_opt "op" fields with
            | Some (`String given) when given <> op -> fail 400 ("op must be " ^ op ^ " for " ^ name)
            | _ -> ("op", `String op) :: List.remove_assoc "op" fields)
        | None -> fields
      in
      let code, _, text = Lwt_main.run (raw ~home `POST path (Some (`Assoc fields))) in
      if code >= 400 then fail code text;
      let receipt = Yojson.Safe.from_string text in
      let receipt =
        if is_command && wait && not (settled (s "status" receipt)) then
          Lwt_main.run (wait_receipt ~home ~timeout (Filename.basename path))
        else receipt
      in
      print_json receipt;
      if is_command && List.mem (s "status" receipt) [ "rejected"; "outcome_unknown" ] then 1 else 0

(* ---- Commands ----------------------------------------------------------- *)

let error_message = function
  | Failure (code, message) ->
      let message =
        match Yojson.Safe.from_string message with
        | `Assoc fields -> ( match List.assoc_opt "error" fields with Some (`String text) -> text | _ -> message)
        | _ -> message
        | exception _ -> message
      in
      Printf.sprintf "%s (status=%d)" message code
  | Yojson.Json_error message -> "invalid JSON: " ^ message
  | Sys_error message | Invalid_argument message -> message
  | Unix.Unix_error (error, fn, _) -> fn ^ ": " ^ Unix.error_message error
  | error -> Printexc.to_string error

let run f = try f () with error -> prerr_endline ("Error: " ^ error_message error); 1

let command ~home_arg =
  let open Cmdliner in
  let json = Arg.(value & flag & info [ "json" ] ~doc:"Print the catalog entries as JSON, with schemas and examples.") in
  let quiet = Arg.(value & flag & info [ "q"; "quiet" ] ~doc:"Print only operation names.") in
  let list =
    Cmd.v (Cmd.info "list" ~doc:"List the operations the service exposes, with a short description each.")
      Term.(const (fun home json quiet -> run (fun () ->
              let operations, source = catalog ~home in
              note_source source;
              if json then print_json (`Assoc [ ("http", Api_catalog.http); ("operations", `List (List.map Api_catalog.operation_json operations)) ])
              else if quiet then List.iter (fun (operation : Api_catalog.operation) -> print_endline operation.name) operations
              else print_string (Api_catalog.table operations);
              0)) $ home_arg $ json $ quiet)
  in
  let name = Arg.(required & pos 0 (some string) None & info [] ~docv:"OPERATION" ~doc:"An operation name from monty api list.") in
  let describe =
    Cmd.v (Cmd.info "describe" ~doc:"Describe one operation: what it does, its input and output schemas, examples and validation notes.")
      Term.(const (fun home json name -> run (fun () ->
              let operations, source = catalog ~home in
              note_source source;
              match List.find_opt (fun (operation : Api_catalog.operation) -> operation.name = name) operations with
              | Some operation -> if json then print_json (Api_catalog.operation_json operation) else print_string (Api_catalog.describe operation); 0
              | None -> fail 404 ("unknown operation " ^ name ^ "; run monty api list"))) $ home_arg $ json $ name)
  in
  let args = Arg.(value & pos 1 (some string) None & info [] ~docv:"ARGS" ~doc:"A JSON object, @FILE, or - for standard input. Path parameters, query parameters and body fields all come from it.") in
  let request_id = Arg.(value & opt (some string) None & info [ "request-id" ] ~docv:"ID" ~doc:"Command ID for a durable receipt; generated when omitted.") in
  let no_wait = Arg.(value & flag & info [ "no-wait" ] ~doc:"Print the accepted receipt without waiting for the command to settle.") in
  let timeout = Arg.(value & opt float 30. & info [ "timeout" ] ~docv:"SECONDS" ~doc:"How long to wait for a command to settle before printing its receipt as it stands.") in
  let call =
    Cmd.v (Cmd.info "call" ~doc:"Call an operation and print its result as JSON; a command prints its receipt once it settles.")
      Term.(const (fun home name args request_id no_wait timeout -> run (fun () -> call ~home ~name ~args ~request_id ~wait:(not no_wait) ~timeout))
            $ home_arg $ name $ args $ request_id $ no_wait $ timeout)
  in
  Cmd.group
    (Cmd.info "api" ~doc:"Inspect and call the service API."
       ~man:
         [ `S Manpage.s_description;
           `P "The service documents its own API. Use list to discover operations, describe to read one operation's schemas, examples and validation notes, and call to run it. The catalog comes from the running service, so it matches that service's version; the same document is served at /v1/help and, as Markdown, /v1/llms.txt.";
           `S Manpage.s_examples;
           `Pre "  monty api list\n  monty api describe command.send\n  monty api call inventory '{\"scope\":\"all\"}'\n  monty api call command.sync '{\"conversation\":\"c1\"}'" ])
    [ list; describe; call ]
