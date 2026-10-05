(* Read only explicitly selected native files. Pi remains their sole writer. *)
open Context_store
open Conversation_store

let source_path profile path =
  if Filename.is_relative path || not (Filename.check_suffix path ".jsonl") then fail 400 "Pi session_file must be an absolute .jsonl path";
  let canonical=get(State_path.canonicalize path) in
  if canonical<>path || not(List.exists (fun r -> within (str r) path) (xs "session_roots" profile)) then
    fail 403 "Pi session file is outside configured session roots or uses a symlink";
  path
let read_file profile path =
  let path=source_path profile path in
  let st=try Unix.lstat path with Unix.Unix_error((Unix.ENOENT|Unix.ENOTDIR),_,_)->fail 404 "Pi source file is unavailable" in
  if st.Unix.st_kind<>Unix.S_REG then fail 400 "Pi session must be a regular file";
  if st.st_size>16_777_216 then fail 413 "Pi session exceeds the 16 MiB import limit";
  let ic=open_in_bin path in
  let bytes=Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
    if not(State_store.same_file st (Unix.fstat (Unix.descr_of_in_channel ic))) then fail 409 "Pi source changed during open";
    really_input_string ic st.st_size) in
  let boundary=match String.rindex_opt bytes '\n' with None->0|Some n->n+1 in
  let lines=String.sub bytes 0 boundary |> String.split_on_char '\n' |> List.filter ((<>) "") in
  let records=try List.map Yojson.Safe.from_string lines with _ -> fail 422 "malformed completed Pi JSONL record; previous archive retained" in
  let header,entries=match records with
    | h::entries when field "type" h=string "session" && List.mem (field "version" h) [`Int 2;`Int 3] -> h,entries
    | _ -> fail 422 "Pi session requires a complete version 2 or 3 header" in
  ignore(required "id" header);
  let ids=Hashtbl.create 64 in
  List.iter (fun e -> let id=required "id" e in
    if field "type" e=string "session" || Hashtbl.mem ids id then fail 422 "duplicate Pi entry identity";
    Hashtbl.add ids id ();
    match field "parentId" e with `Null|`String _->()|_->fail 422 "invalid Pi entry parent") entries;
  obj ["header",header;"entries",`List entries;
    "leafId",(match List.rev entries with e::_->field "id" e|[]->`Null);
    "source_file",string path;"complete_bytes",`Int boundary;"partial_tail",`Bool (boundary<String.length bytes);
    "leaf_source",string "last persisted entry; unsaved native navigation is unknown"]
let verify_session sess raw =
  if field "id" (field "header" raw)<>field "native_id" sess then fail 409 "Pi source session identity changed; import it separately";
  raw
