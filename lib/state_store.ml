let ( let* ) = Result.bind

let error_of_unix action path err fn arg =
  Printf.sprintf "%s %s failed via %s(%s): %s" action path fn arg
    (Unix.error_message err)

let protect_result ~action ~path f =
  try f () with
  | Sys_error msg -> Error (Printf.sprintf "%s %s failed: %s" action path msg)
  | Unix.Unix_error (err, fn, arg) -> Error (error_of_unix action path err fn arg)
  | Invalid_argument msg -> Error (Printf.sprintf "%s %s failed: %s" action path msg)

let lstat path =
  try Ok (Some (Unix.lstat path)) with
  | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> Ok None
  | Unix.Unix_error (err, fn, arg) ->
      Error (error_of_unix "inspect" path err fn arg)

let rec ensure_real_directory ~label ~mode path =
  let* existing = lstat path in
  match existing with
  | Some { Unix.st_kind = Unix.S_DIR; _ } -> Ok ()
  | Some { Unix.st_kind = Unix.S_LNK; _ } ->
      Error
        (Printf.sprintf
           "unsafe %s is a symlink: %s; replace it with a real directory before retrying"
           label path)
  | Some _ -> Error (Printf.sprintf "%s is not a directory: %s" label path)
  | None ->
      let parent = Filename.dirname path in
      let* () =
        if String.equal parent path then Ok ()
        else ensure_real_directory ~label:"parent directory" ~mode:0o755 parent
      in
      let* () =
        try
          Unix.mkdir path mode;
          Ok ()
        with
        | Unix.Unix_error (Unix.EEXIST, _, _) -> Ok ()
        | Unix.Unix_error (err, fn, arg) ->
            Error (error_of_unix ("create " ^ label) path err fn arg)
      in
      let* created = lstat path in
      (match created with
      | Some { Unix.st_kind = Unix.S_DIR; _ } -> Ok ()
      | Some { Unix.st_kind = Unix.S_LNK; _ } ->
          Error
            (Printf.sprintf
               "unsafe %s became a symlink while it was being created: %s"
               label path)
      | Some _ -> Error (Printf.sprintf "%s is not a directory after creation: %s" label path)
      | None -> Error (Printf.sprintf "%s disappeared while it was being created: %s" label path))

let ensure_state_dir ~home =
  protect_result ~action:"prepare state directory" ~path:home (fun () ->
      let* home = State_path.canonicalize home in
      let* () = ensure_real_directory ~label:"Monty home" ~mode:0o755 home in
      let monty_dir = Filename.concat home ".monty" in
      let* () =
        ensure_real_directory ~label:"Monty state parent" ~mode:0o700 monty_dir
      in
      let* canonical_monty = State_path.canonicalize monty_dir in
      if String.equal canonical_monty monty_dir then Ok canonical_monty
      else
        Error
          (Printf.sprintf
             "unsafe Monty state parent uses a symlink alias: %s resolves to %s; repair it explicitly"
             monty_dir canonical_monty))

let same_file left right =
  left.Unix.st_dev = right.Unix.st_dev && left.Unix.st_ino = right.Unix.st_ino

let verify_lock_identity lock_path fd =
  let* current = lstat lock_path in
  match current with
  | Some ({ Unix.st_kind = Unix.S_REG; _ } as path_stat) ->
      let fd_stat = Unix.fstat fd in
      if same_file path_stat fd_stat then Ok ()
      else
        Error
          (Printf.sprintf
             "Monty state lock changed while it was being opened: %s; retry after repairing the state directory"
             lock_path)
  | Some { Unix.st_kind = Unix.S_LNK; _ } ->
      Error
        (Printf.sprintf
           "unsafe Monty state lock is a symlink: %s; remove it and retry"
           lock_path)
  | Some _ -> Error (Printf.sprintf "Monty state lock is not a regular file: %s" lock_path)
  | None -> Error (Printf.sprintf "Monty state lock disappeared while opening: %s" lock_path)

let open_verified_lock lock_path flags =
  protect_result ~action:"open state lock" ~path:lock_path (fun () ->
      let fd = Unix.openfile lock_path flags 0o600 in
      match verify_lock_identity lock_path fd with
      | Ok () -> Ok fd
      | Error _ as error ->
          Unix.close fd;
          error)

let rec open_lock_file lock_path =
  let* existing = lstat lock_path in
  match existing with
  | Some { Unix.st_kind = Unix.S_LNK; _ } ->
      Error
        (Printf.sprintf
           "unsafe Monty state lock is a symlink: %s; remove it and retry"
           lock_path)
  | Some { Unix.st_kind = Unix.S_REG; _ } ->
      open_verified_lock lock_path [ Unix.O_RDWR ]
  | Some _ -> Error (Printf.sprintf "Monty state lock is not a regular file: %s" lock_path)
  | None -> (
      match
        open_verified_lock lock_path
          [ Unix.O_CREAT; Unix.O_EXCL; Unix.O_RDWR ]
      with
      | Error _ when Sys.file_exists lock_path -> open_lock_file lock_path
      | result -> result)

let with_lock_path lock_path f =
  let* fd = open_lock_file lock_path in
  protect_result ~action:"lock state" ~path:lock_path (fun () ->
      Fun.protect
        ~finally:(fun () ->
          (try Unix.lockf fd Unix.F_ULOCK 0 with _ -> ());
          Unix.close fd)
        (fun () ->
          Unix.lockf fd Unix.F_LOCK 0;
          let* () = verify_lock_identity lock_path fd in
          f ()))

let with_lock ~home f =
  let* state_dir = ensure_state_dir ~home in
  with_lock_path (Filename.concat state_dir "state.lock") f

let with_named_lock ~home ~name f =
  let* name = State_path.safe_component ~label:"state lock name" name in
  let* state_dir = ensure_state_dir ~home in
  with_lock_path (Filename.concat state_dir name) f

let read_json ~path =
  if not (Sys.file_exists path) then Ok None
  else
    protect_result ~action:"read JSON" ~path (fun () ->
        try Ok (Some (Yojson.Safe.from_file path)) with
        | Yojson.Json_error msg -> Error ("invalid JSON in " ^ path ^ ": " ^ msg))

let write_all fd contents =
  let bytes = Bytes.unsafe_of_string contents in
  let rec loop offset =
    if offset = Bytes.length bytes then ()
    else
      let written = Unix.single_write fd bytes offset (Bytes.length bytes - offset) in
      if written = 0 then raise (Sys_error "short write") else loop (offset + written)
  in
  loop 0

let temp_counter = ref 0

let next_temp_path path =
  incr temp_counter;
  let dir = Filename.dirname path in
  let base = Filename.basename path in
  Filename.concat dir
    (Printf.sprintf ".%s.monty-tmp-%d-%d" base (Unix.getpid ()) !temp_counter)

let before_rename_hook : (unit -> (unit, string) result) ref = ref (fun () -> Ok ())
let set_before_rename_hook hook = before_rename_hook := hook
let reset_before_rename_hook () = before_rename_hook := (fun () -> Ok ())

let check_before_rename_fault () =
  match Sys.getenv_opt "MONTY_FAULT_INJECT" with
  | Some "state-store-before-rename" ->
      Error "fault injected before atomic state rename"
  | _ -> !before_rename_hook ()

let fsync_directory dir =
  let fd = Unix.openfile dir [ Unix.O_RDONLY ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> Unix.fsync fd)

let fsync_regular_file path =
  let fd = Unix.openfile path [ Unix.O_RDONLY ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> Unix.fsync fd)

let write_file_atomic ~path ~perm contents =
  protect_result ~action:"atomically write file" ~path (fun () ->
      let dir = Filename.dirname path in
      Shell.ensure_dir dir;
      let temp_path = next_temp_path path in
      let fd =
        Unix.openfile temp_path
          [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL ] perm
      in
      let renamed = ref false in
      Fun.protect
        ~finally:(fun () ->
          (try Unix.close fd with _ -> ());
          if (not !renamed) && State_path.path_exists temp_path then
            try Unix.unlink temp_path with _ -> ())
        (fun () ->
          write_all fd contents;
          Unix.fchmod fd perm;
          Unix.fsync fd;
          Unix.close fd;
          Unix.rename temp_path path;
          renamed := true;
          fsync_directory dir;
          Ok ()))

let write_json_atomic ~path json =
  let serialized =
    protect_result ~action:"serialize JSON" ~path (fun () ->
        Ok (Yojson.Safe.to_string json ^ "\n"))
  in
  let* contents = serialized in
  protect_result ~action:"atomically write JSON" ~path (fun () ->
      let dir = Filename.dirname path in
      Shell.ensure_dir dir;
      let temp_path = next_temp_path path in
      let existing_perm =
        try (Unix.stat path).Unix.st_perm land 0o777 with Unix.Unix_error _ -> 0o600
      in
      let fd =
        Unix.openfile temp_path
          [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL ] existing_perm
      in
      let renamed = ref false in
      Fun.protect
        ~finally:(fun () ->
          (try Unix.close fd with _ -> ());
          if (not !renamed) && State_path.path_exists temp_path then
            try Unix.unlink temp_path with _ -> ())
        (fun () ->
          write_all fd contents;
          Unix.fsync fd;
          let* () = check_before_rename_fault () in
          Unix.close fd;
          Unix.rename temp_path path;
          renamed := true;
          fsync_directory dir;
          Ok ()))

let write_json ~home ~path json =
  with_lock ~home (fun () -> write_json_atomic ~path json)

let update_json ~home ~path f =
  with_lock ~home (fun () ->
      let* current = read_json ~path in
      let* json, result = f current in
      let* () = write_json_atomic ~path json in
      Ok result)
