let ( let* ) = Result.bind

let try_lock = Process.try_worker_lock

(* When another process holds the lock, [runtime] may prove that the holder is this
   worker's live service-owned terminal and that the operation belongs to it (attaching
   a client, or completing the worker from inside that session), and return its result.
   That needs no lock of its own: the runtime's lock still excludes a second runtime,
   headless runs and other lifecycle operations. None keeps the busy error. *)
let with_lock ?(runtime = fun () -> None) ~home ~(record : Job_store.record) operation =
  let directory = Filename.concat record.run_dir ".worker-locks" in
  let path = Filename.concat directory (record.id ^ ".lock") in
  let* () =
    State_store.with_lock ~home (fun () ->
        let* canonical = State_path.canonicalize directory in
        if not (String.equal canonical directory) then
          Error ("unsafe worker lock directory: " ^ directory)
        else State_store.ensure_real_directory ~label:"worker lock directory"
            ~mode:0o700 directory)
  in
  let* fd = State_store.open_lock_file path in
  let locked =
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
        match try_lock fd with
        | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EACCES), _, _) -> None
        | exception Unix.Unix_error (err, fn, arg) ->
            Some (Error (State_store.error_of_unix "lock worker" path err fn arg))
        | () ->
            Some
              (let* () = State_store.verify_lock_identity path fd in
               (* Children keep the same lock if the supervisor is killed. Close our
                  descriptor on exit; an explicit unlock would also unlock live children. *)
               Unix.clear_close_on_exec fd;
               operation ()))
  in
  match locked with
  | Some result -> result
  | None ->
      Option.value (runtime ())
        ~default:(Error (Printf.sprintf "worker %s is busy with a headless run or lifecycle operation" record.id))
