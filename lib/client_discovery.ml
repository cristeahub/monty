(* Discovery never registers a conversation or resumes a native writer. *)
open Json_fields
open Conversation_store
let optional key json = match field key json with `String value -> value | _ -> ""
let short_text value =
  if String.length value<=500 then value else
  let boundary=ref 500 in
  while !boundary>0 && Char.code value.[!boundary] land 0xc0=0x80 do decr boundary done;
  String.sub value 0 !boundary ^ "…"

let linked home p native_id =
  match existing home ~harness:(s "harness" p) ~scope:(s "scope" p) native_id with
  | Some c when field "purged" c<>`Bool true -> field "id" c
  | _ -> `Null

let row ~home p ~native_id fields =
  obj (["profile",field "id" p;"harness",field "harness" p;"scope",field "scope" p;
    "native_id",string native_id;"conversation",linked home p native_id] @ fields)

let pi_files p =
  let paths=ref [] and errors=ref [] and visited=ref 0 in
  let problem path message =
    if List.length !errors<100 then errors:=obj ["path",string path;"message",string message] :: !errors in
  let rec walk depth path =
    incr visited;
    (* ponytail: rescan at most 100k directory entries per page; add an index only
       if native catalogs reach that size. Coverage remains visibly partial. *)
    if !visited>100_000 || depth>16 then problem path "session scan limit reached" else
    try
      if get(State_path.canonicalize path)<>path then problem path "symlink skipped" else
      let stat=Unix.lstat path in
      match stat.Unix.st_kind with
      | Unix.S_DIR -> Sys.readdir path |> Array.iter (fun name ->
          if !visited<=100_000 then walk (depth+1) (Filename.concat path name))
      | Unix.S_REG when Filename.check_suffix path ".jsonl" -> paths:=path :: !paths
      | Unix.S_LNK -> problem path "symlink skipped"
      | _ -> ()
    with _ -> problem path "session directory or file is unavailable" in
  List.iter (fun root -> walk 0 (str root)) (xs "session_roots" p);
  List.sort_uniq String.compare !paths,List.rev !errors

let pi_page ~home p cursor =
  let paths,errors=pi_files p in
  let paths=List.filter (fun path -> path>cursor) paths in
  let page=take 25 paths in
  let errors=ref errors in
  let data=List.filter_map (fun path ->
    try
      let raw=Pi_history.read_file p path in
      let header=field "header" raw and entries=array "entries" raw in
      let title=List.rev entries |> List.find_opt (fun e -> optional "type" e="session_info" && optional "name" e<>"") in
      let preview=List.find_opt (fun e -> optional "role" (field "message" e)="user") entries in
      let preview=match preview with None->""|Some e ->
        (match field "content" (field "message" e) with
        | `String text -> text
        | `List xs -> xs |> List.filter_map (fun x -> if optional "type" x="text" then Some(optional "text" x) else None) |> String.concat "\n"
        | _ -> "") in
      let preview=short_text preview in
      Some(row ~home p ~native_id:(required "id" header)
        ["title",(match title with None->string (if preview="" then "Pi conversation" else preview)|Some e->string(short_text (s "name" e)));
         "preview",string preview;"cwd",field "cwd" header;"session_file",string path;
         "updated_at",`Float (Unix.stat path).Unix.st_mtime;"partial_tail",field "partial_tail" raw])
    with Failure(_,message) -> errors:=obj ["path",string path;"message",string message] :: !errors;None
       | _ -> errors:=obj ["path",string path;"message",string "session file unavailable"] :: !errors;None) page in
  obj ["data",`List data;"next_cursor",(if List.length paths>25 then string(List.hd(List.rev page)) else `Null);
    "errors",`List !errors;"coverage",string (if !errors=[] then "page" else "partial")]

let codex_params uri =
  let param name=match Uri.get_query_param uri name with None->`Null|Some value->string value in
  obj ["cursor",param "cursor";"limit",`Int 25;"sortKey",string "updated_at";
    "archived",`Bool (Uri.get_query_param uri "archived"=Some "true");
    "modelProviders",`List [];"searchTerm",param "search";
    "sourceKinds",`List (List.map string ["cli";"vscode";"exec";"appServer";"subAgent";
      "subAgentReview";"subAgentCompact";"subAgentThreadSpawn";"subAgentOther";"unknown"])]

let codex_page ~home p result =
  let data=array "data" result |> List.map (fun thread ->
    row ~home p ~native_id:(required "id" thread)
      ["title",string(short_text(if optional "name" thread<>"" then s "name" thread else optional "preview" thread));
       "preview",string(short_text(optional "preview" thread));"cwd",field "cwd" thread;
       "updated_at",field "updatedAt" thread;"status",field "status" thread]) in
  obj ["data",`List data;"next_cursor",field "nextCursor" result;
    "errors",`List [];"coverage",string "page"]

let history p raw =
  let sess=obj ["id",string "native";"profile",field "id" p;"harness",field "harness" p] in
  let raw=redact raw in
  let items=normalized sess raw in
  let sess=puts ["effective",`List(effective_keys sess raw items);
    "records",`List[obj ["items",`List items]];
    "coverage",obj ["state",string "native history";"gaps",`List (
      [string "Read-only native snapshot; import to retain history in Monty."] @
      (if s "harness" p="pi" then [string "The last persisted branch is shown; unsaved native navigation is unavailable."] else []) @
      (if field "partial_tail" raw=`Bool true then [string "The source has an incomplete trailing record."] else []))]] sess in
  obj ["harness",field "harness" p;"scope",field "scope" p;
    "read_only",`Bool true;"snapshot",raw;"sessions",`List[sess]]
