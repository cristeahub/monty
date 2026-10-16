type mode = Fresh | Picker | Exact of string

let sidecar worker_dir = Filename.concat worker_dir "codex-session-id"
let max_id_length = 1024

let validate_id ~label value =
  if String.trim value = "" then Error (label ^ " must not be empty")
  else if String.length value > max_id_length then
    Error
      (Printf.sprintf "%s is too long (%d bytes; maximum %d)" label
         (String.length value) max_id_length)
  else if
    String.exists
      (fun character ->
        let code = Char.code character in
        code < 0x20 || code = 0x7f)
      value
  then Error (label ^ " must not contain control characters")
  else Ok value

let read_regular path =
  let ( let* ) = Result.bind in
  let* existing = State_store.lstat path in
  match existing with
  | None -> Ok None
  | Some { Unix.st_kind = Unix.S_LNK; _ } ->
      Error (Printf.sprintf "Codex session sidecar must not be a symlink: %s" path)
  | Some { Unix.st_kind = Unix.S_REG; _ } ->
      State_store.protect_result ~action:"read Codex session sidecar" ~path
        (fun () ->
          let fd = Unix.openfile path [ Unix.O_RDONLY ] 0 in
          Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
              let opened = Unix.fstat fd in
              let* current = State_store.lstat path in
              let* () =
                match current with
                | Some stat
                  when stat.Unix.st_kind = Unix.S_REG
                       && State_store.same_file stat opened ->
                    Ok ()
                | _ ->
                    Error
                      (Printf.sprintf
                         "Codex session sidecar changed while being opened: %s"
                         path)
              in
              if opened.Unix.st_size > max_id_length then
                Error
                  (Printf.sprintf
                     "Codex session sidecar is too large (%d bytes; maximum %d): %s"
                     opened.Unix.st_size max_id_length path)
              else
                let contents = Bytes.create opened.Unix.st_size in
                let rec read offset =
                  if offset = Bytes.length contents then Ok ()
                  else
                    let count =
                      Unix.read fd contents offset (Bytes.length contents - offset)
                    in
                    if count = 0 then
                      Error
                        (Printf.sprintf
                           "Codex session sidecar ended before its recorded size: %s"
                           path)
                    else read (offset + count)
                in
                let* () = read 0 in
                Ok (Some (Bytes.unsafe_to_string contents))))
  | Some _ ->
      Error
        (Printf.sprintf "Codex session sidecar is not a regular file: %s" path)

let read worker_dir =
  let ( let* ) = Result.bind in
  let path = sidecar worker_dir in
  let* value = read_regular path in
  match value with
  | None -> Ok None
  | Some value ->
      validate_id ~label:("invalid Codex session id in " ^ path) value
      |> Result.map Option.some

let resume_mode ~fresh ~worker_dir =
  if fresh then Ok Fresh
  else
    read worker_dir
    |> Result.map (function None -> Picker | Some session_id -> Exact session_id)
