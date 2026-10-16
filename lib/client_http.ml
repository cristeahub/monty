open Lwt.Infix
open Json_fields
open Conversation_store
let request ~home meth path body =
  let metadata_path=file home ["client-service.json"] in
  if not(Sys.file_exists metadata_path) then fail 503 "Monty client service is not running; run monty service start --config FILE";
  let state=read metadata_path in
  let base=s "url" state in
  let uri=Uri.of_string base in
  if Uri.scheme uri<>Some "http" || Uri.host uri<>Some "127.0.0.1" then fail 500 "unsafe service address";
  let token=String.trim (Shell.read_file (file home ["client-service-token"])) in
  let headers=Cohttp.Header.of_list ["authorization","Bearer " ^ token;"content-type","application/json"] in
  let body=Cohttp_lwt.Body.of_string (Yojson.Safe.to_string body) in
  Lwt_unix.with_timeout 35. (fun () ->
    Cohttp_lwt_unix.Client.call ~headers ~body meth (Uri.of_string (base ^ path)) >>= fun (response,body) ->
    Cohttp_lwt.Body.to_string body >|= fun text ->
    let json=Yojson.Safe.from_string text in
    let code=Cohttp.Code.code_of_status (Cohttp.Response.status response) in
    if code>=400 then fail code (Yojson.Safe.to_string json);json)
