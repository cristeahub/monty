let ( let* ) = Result.bind
let sidecar ~harness worker_dir = Filename.concat worker_dir (Harness.to_string harness ^ "-session-id")
let validate ~harness id =
  let valid_uuid=String.length id=36 && (String.to_seqi id |> Seq.for_all(fun (n,c)->
    if List.mem n [8;13;18;23] then c='-'
    else (c>='0' && c<='9') || (c>='a' && c<='f') || (c>='A' && c<='F'))) in
  if harness=Harness.Claude && not valid_uuid then Error "Claude exact session id must be a UUID" else Ok id
let read ~harness worker_dir =
  let* value = Codex_session.read_regular (sidecar ~harness worker_dir) in
  match value with None -> Ok None | Some value ->
    let* id=Codex_session.validate_id ~label:"native session id" (String.trim value) in
    validate ~harness id |> Result.map Option.some
let resume_mode ~harness ~fresh ~worker_dir =
  if fresh then Ok Codex_session.Fresh else
  let* selected=read ~harness worker_dir in
  match selected with None->Ok Codex_session.Picker|Some id->
    let* id=validate ~harness id in Ok(Codex_session.Exact id)
let write ~home ~harness ~worker_dir id =
  let* id=Codex_session.validate_id ~label:"native session id" id in
  let* id=validate ~harness id in
  State_store.with_lock ~home (fun () ->
    let path=sidecar ~harness worker_dir in
    let* _=Codex_session.read_regular path in
    State_store.write_file_atomic ~path ~perm:0o600 id)
