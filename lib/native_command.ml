open Conversation_store
let shell command = if Sys.file_exists command then Shell.quote command else command
let argv command args =
  if Sys.file_exists command then command::args else
  ["/bin/sh";"-c";"exec " ^ command ^ " \"$@\"";"monty-native"] @ args
let validate command =
  let path=if Sys.file_exists command then command else fst(get(Process.command_word command)) in
  if Filename.is_relative path then fail 400 "native command must name an absolute executable";
  command
let executable ~home command =
  if Sys.file_exists command then command else
  let directory=file home["runtime";"commands"] in
  let path=Filename.concat directory (sha command ^ ".sh") in
  locked home(fun ()->
    get(State_store.ensure_real_directory ~label:"native command wrappers" ~mode:0o700 directory);
    get(State_store.write_file_atomic ~path ~perm:0o700 ("#!/bin/sh\nexec " ^ command ^ " \"$@\"\n")));
  path

let codex_model_config config =
  let open Json_fields in
  let provider=s "model_provider" config in
  let* ()=if provider="" || String.for_all(function 'a'..'z'|'A'..'Z'|'0'..'9'|'_'|'-'->true|_->false) provider then Ok ()
    else Error "Codex reviewer provider id cannot be projected safely" in
  let definition=field provider (field "model_providers" config) in
  let* ()=if field "experimental_bearer_token" definition<>`Null ||
    (match field "http_headers" definition with `Null|`Assoc []->false|_->true) then
    Error "Codex reviewers require provider credentials through environment references, not inline header/token config"
    else Ok () in
  let fields=List.filter_map (fun key -> match field key config with `Null->None|value->Some(key,value)) ["model";"model_provider"] in
  let fields=fields @ List.filter_map (fun key -> match field key definition with `Null->None|value->
    Some("model_providers." ^ provider ^ "." ^ key,value))
    ["name";"base_url";"wire_api";"env_key";"env_key_instructions";"env_http_headers";
     "requires_openai_auth";"supports_websockets";"request_max_retries";"stream_max_retries";"stream_idle_timeout_ms"] in
  Ok (`Assoc fields)

let codex_model_arguments json =
  let rec toml = function
    | `String value -> Codex_trust.toml_basic_string value
    | `Int _ | `Bool _ as value -> Yojson.Safe.to_string value
    | `Assoc fields -> "{" ^ String.concat "," (List.map(fun (key,value)->Codex_trust.toml_basic_string key ^ "=" ^ toml value) fields) ^ "}"
    | _ -> invalid_arg "invalid server-owned Codex reviewer model config" in
  match json with
  | `Null -> ""
  | `Assoc fields -> String.concat "" (List.map(fun (key,value)->" -c " ^ Shell.quote(key ^ "=" ^ toml value)) fields)
  | _ -> invalid_arg "invalid server-owned Codex reviewer model config"
