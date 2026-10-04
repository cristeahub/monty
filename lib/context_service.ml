open Context_store

let label home = "dev.monty.context." ^ String.sub (hash home) 0 12
let domain () = "gui/" ^ string_of_int (Unix.getuid ())
let target home = domain () ^ "/" ^ label home
let plist_path home =
  let directory = match Sys.getenv_opt "MONTY_CONTEXT_LAUNCH_AGENTS" with
    | Some path -> path
    | None -> Filename.concat (Sys.getenv "HOME") "Library/LaunchAgents" in
  Filename.concat directory (label home ^ ".plist")
let launchctl () = Option.value ~default:"launchctl" (Sys.getenv_opt "MONTY_LAUNCHCTL_COMMAND")
let call home args =
  let* () = State_store.ensure_real_directory ~label:"context directory" ~mode:0o700 (root home) in
  Context_process.run ~seconds:15 ~cwd:(root home)
    ~command:(String.concat " " (List.map Shell.quote (launchctl () :: args))) ~input:"" ()
let actual home =
  let command = String.concat " " (List.map Shell.quote [launchctl ();"print";target home]) in
  match Context_process.run ~seconds:5 ~cwd:(Filename.get_temp_dir_name ())
          ~diagnostic_failure:true ~command ~input:"" () with
  | Ok stdout when Home.contains_substring stdout "state =" ->
      if Home.contains_substring stdout "state = running" then "running" else "loaded"
  | Ok stdout when Home.contains_substring stdout "Could not find service" || Home.contains_substring stdout "service not found" -> "unloaded"
  | _ -> "unknown"
let xml value = String.to_seq value |> Seq.map (function
    | '&' -> "&amp;" | '<' -> "&lt;" | '>' -> "&gt;" | '"' -> "&quot;" | '\'' -> "&apos;" | c -> String.make 1 c)
    |> List.of_seq |> String.concat ""
let plist home =
  let executable = Unix.realpath Sys.executable_name in
  let env = ["MONTY_HOME", home; "PATH",Option.value ~default:"/usr/bin:/bin" (Sys.getenv_opt "PATH")]
    @ List.filter_map (fun key -> Option.map (fun value -> key,value) (Sys.getenv_opt key))
        ["MONTY_HARNESS";"MONTY_PI_COMMAND";"MONTY_CODEX_COMMAND";"CODEX_HOME";"PI_CODING_AGENT_DIR"] in
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n<plist version=\"1.0\"><dict>"
  ^ "<key>Label</key><string>" ^ xml (label home) ^ "</string>"
  ^ "<key>ProgramArguments</key><array>"
  ^ String.concat "" (List.map (fun v -> "<string>" ^ xml v ^ "</string>") [executable;"context";"tick";"--home";home]) ^ "</array>"
  ^ "<key>EnvironmentVariables</key><dict>"
  ^ String.concat "" (List.map (fun (k,v) -> "<key>" ^ k ^ "</key><string>" ^ xml v ^ "</string>") env) ^ "</dict>"
  (* A calendar minute trigger coalesces missed wakeups. Source due times enforce
     configurable cadence; RunAtLoad also catches up after login. *)
  ^ "<key>StartCalendarInterval</key><dict></dict>"
  ^ "<key>RunAtLoad</key><true/><key>ProcessType</key><string>Background</string>"
  ^ "<key>ThrottleInterval</key><integer>30</integer><key>ExitTimeOut</key><integer>5</integer>"
  ^ "<key>StandardOutPath</key><string>/dev/null</string><key>StandardErrorPath</key><string>/dev/null</string>"
  ^ "</dict></plist>\n"
let with_runtime_lock ~home name f =
  let* () = State_store.with_lock ~home (fun () ->
      let* _ = safe_path home in
      State_store.ensure_real_directory ~label:"context directory" ~mode:0o700 (root home)) in
  let* fd = State_store.open_lock_file (Filename.concat (root home) (name ^ ".lock")) in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      let* () = try Worker_lock.try_lock fd; Ok () with
        | Unix.Unix_error ((Unix.EAGAIN | Unix.EACCES),_,_) -> Error "context operation already running"
        | Unix.Unix_error _ -> Error "cannot acquire context operation lock" in
      Unix.clear_close_on_exec fd;
      f ())
let switch ~home enabled = with_runtime_lock ~home "service" (fun () ->
    let* settings = Settings.load ~home in
    let* () = if settings.context_enabled = enabled then Ok () else (
      let* () = Settings.set_context_enabled ~home enabled in
      mutate ~home (fun state ->
        let generation = i "generation" state + 1 in
        let connections = xs "connections" state |> List.map (fun c -> puts
            ["generation",`Int generation] c) in
        Ok (puts ["generation",`Int generation;"connections",`List connections] state,()))) in
    let service = actual home in
    if enabled then (
      let path = plist_path home in
      let contents = plist home in
      let changed = match State_store.lstat path with
        | Ok (Some {Unix.st_kind=Unix.S_REG;_}) -> Shell.read_file path <> contents
        | _ -> true in
      let* canonical = State_path.canonicalize path in
      let* () = if canonical = path then Ok () else Error "unsafe LaunchAgent path" in
      let* _ = call home ["enable";target home] in
      let* () = if changed && List.mem service ["loaded";"running"] then
        call home ["bootout";target home] |> Result.map (fun _ -> ()) else Ok () in
      (* Preserve the old plist until its loaded service is stopped. Otherwise
         a failed bootout would make a retry mistake the new file for applied state. *)
      let* () = State_store.with_lock ~home (fun () ->
          let* canonical = State_path.canonicalize path in
          if canonical <> path then Error "unsafe LaunchAgent path" else
          let* () = State_store.ensure_real_directory ~label:"LaunchAgents directory" ~mode:0o700 (Filename.dirname path) in
          State_store.write_file_atomic ~path ~perm:0o600 contents) in
      let* () = if not changed && List.mem service ["loaded";"running"] then Ok () else
        call home ["bootstrap";domain ();path] |> Result.map (fun _ -> ()) in
      if List.mem (actual home) ["loaded";"running"] then Ok ()
      else Error "context desired on, but launchd state is unverified; retry context on")
    else (
      let* _ = call home ["disable";target home] in
      let* () = if service = "unloaded" then Ok () else
        call home ["bootout";target home] |> Result.map (fun _ -> ()) in
      if actual home <> "unloaded" then Error "context desired off, but launchd stop is unverified; retry context off"
      else
        let* state = load ~home in
        let rec stopped attempts =
          let busy = List.exists (fun c -> match with_runtime_lock ~home ("source-" ^ s "id" c) (fun () -> Ok ()) with Ok () -> false | Error _ -> true) (xs "connections" state) in
          if not busy then Ok () else if attempts = 0 then Error "context disabled; active child has not stopped yet, retry context off"
          else (ignore (Unix.select [] [] [] 0.1); stopped (attempts - 1)) in
        stopped 60))
