let ( let* ) = Result.bind

external try_lock : Unix.file_descr -> unit = "monty_worker_flock"

let with_lock ~home ~(record : Job_store.record) operation =
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
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      let* () =
        try try_lock fd; Ok () with
        | Unix.Unix_error ((Unix.EAGAIN | Unix.EACCES), _, _) ->
            Error (Printf.sprintf "worker %s is busy with a headless run or lifecycle operation" record.id)
        | Unix.Unix_error (err, fn, arg) ->
            Error (State_store.error_of_unix "lock worker" path err fn arg)
      in
      let* () = State_store.verify_lock_identity path fd in
      (* Children keep the same lock if the supervisor is killed. Close our
         descriptor on exit; an explicit unlock would also unlock live children. *)
      Unix.clear_close_on_exec fd;
      operation ())
