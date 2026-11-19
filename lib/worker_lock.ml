let ( let* ) = Result.bind

let try_lock = Process.try_worker_lock

let directory (record : Job_store.record) = Filename.concat record.run_dir ".worker-locks"

let path (record : Job_store.record) = Filename.concat (directory record) (record.id ^ ".lock")

(* The worker's own native terminal, run by the Monty service. pid is the native
   program, which holds the lock together with its ownership guard. *)
type terminal = { harness : string; native_id : string; pid : int }

let describe_terminal t = Printf.sprintf "%s session %s, pid %d" t.harness t.native_id t.pid

(* The worker's own live terminal in the Monty service: the holder of a busy lock
   when it is not a headless run or lifecycle command. The service records a live
   terminal and its PID in the worker's conversation session and clears both when
   the terminal ends or the service restarts. Reads conversation files, so call it
   outside the one-home lock. *)
let terminal ~home (record : Job_store.record) =
  let open Json_fields in
  let alive pid =
    match Unix.kill pid 0 with
    | () -> true
    | exception Unix.Unix_error (Unix.EPERM, _, _) -> true
    | exception Unix.Unix_error _ -> false
  in
  let own sess =
    let environment = field "environment" (field "context" sess) in
    field "MONTY_JOB_ID" environment = `String record.id
    && field "MONTY_WORKER_DIR" environment = `String record.worker_dir
    && field "MONTY_JOB_FILE" environment = `String record.path
  in
  let sessions =
    (* An unreadable conversation only means its terminal cannot be named. *)
    try
      Conversation_store.list home
      |> List.concat_map (fun c -> if field "purged" c = `Bool true then [] else xs "sessions" c)
    with Conversation_store.Failure _ | Yojson.Json_error _ | Sys_error _ | Unix.Unix_error _ -> []
  in
  List.find_map
    (fun sess ->
      match (field "terminal" sess, field "terminal_pid" sess) with
      | `Bool true, `Int pid when own sess && alive pid ->
          Some { harness = s "harness" sess; native_id = s "native_id" sess; pid }
      | _ -> None)
    sessions

(* Names the holder found by terminal, with a recovery that works for it. *)
let busy_message ~home (record : Job_store.record) = function
  | Some terminal ->
      Printf.sprintf
        "worker %s is busy: its interactive terminal (%s) is running in the Monty service; show it with monty resume %s --home %s, or exit it at its prompt before retrying"
        record.id (describe_terminal terminal) (Shell.quote record.id) (Shell.quote home)
  | None -> Printf.sprintf "worker %s is busy with a headless run or lifecycle operation" record.id

(* Runs operation while holding the worker's lock. When another process holds it,
   on_busy decides instead; by default the attempt fails naming the holder. *)
let with_lock ~home ~(record : Job_store.record)
    ?(on_busy = fun () -> Error (busy_message ~home record (terminal ~home record))) operation =
  let directory = directory record and path = path record in
  let* () =
    State_store.with_lock ~home (fun () ->
        let* canonical = State_path.canonicalize directory in
        if not (String.equal canonical directory) then
          Error ("unsafe worker lock directory: " ^ directory)
        else State_store.ensure_real_directory ~label:"worker lock directory"
            ~mode:0o700 directory)
  in
  let* fd = State_store.open_lock_file path in
  let attempt =
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
        match try_lock fd with
        | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EACCES), _, _) -> `Busy
        | exception Unix.Unix_error (err, fn, arg) ->
            `Done (Error (State_store.error_of_unix "lock worker" path err fn arg))
        | () ->
            `Done
              (let* () = State_store.verify_lock_identity path fd in
               (* Children keep the same lock if the supervisor is killed. Close our
                  descriptor on exit; an explicit unlock would also unlock live children. *)
               Unix.clear_close_on_exec fd;
               operation ()))
  in
  match attempt with `Done result -> result | `Busy -> on_busy ()

(* Whether another process currently holds the lock. The probe takes and
   immediately releases the lock, so call it before, never inside, the one-home
   state lock. A lock file nobody created yet is free, and the probe leaves it so. *)
let is_busy ~home ~(record : Job_store.record) =
  if not (Sys.file_exists (path record)) then Ok false
  else with_lock ~home ~record ~on_busy:(fun () -> Ok true) (fun () -> Ok false)
