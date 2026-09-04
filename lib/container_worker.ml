let ( let* ) = Result.bind

let schema = "monty:container-worker:v1"
let image_schema = "monty:container-worker-image:v1"
let image = "monty-apple-worker:1"
let image_label = "com.monty.image"
let image_version = "apple-worker-v1"
let owner_label = "com.monty.worker"
let guest_repo = "/monty/repos/task"

let now_utc () =
  let tm = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

type t = {
  image : string;
  image_digest : string;
  container : string;
  volume : string;
  owner : string;
}

type inspection = {
  output : string;
  was_running : bool;
  ready_seconds : float;
  result_seconds : float;
}

type change_summary = {
  files : string list;
  insertions : int;
  deletions : int;
}

let command () =
  match Sys.getenv_opt "MONTY_CONTAINER_COMMAND" with
  | Some value when String.trim value <> "" -> value
  | _ -> "container"

let shell_command args =
  command () ^ " " ^ String.concat " " (List.map Shell.quote args)

let capture args = Process.run_capture (shell_command args)

let run args =
  match capture args with
  | Error message -> Error message
  | Ok { Process.status = `Exited 0; stdout } -> Ok stdout
  | Ok { status; stdout } ->
      Error
        (Printf.sprintf "Apple container command %s: %s"
           (Process.status_to_string status) stdout)

let run_stdin args input =
  match Process.run_capture_with_stdin (shell_command args) input with
  | Error message -> Error message
  | Ok { Process.status = `Exited 0; stdout } -> Ok stdout
  | Ok { status; _ } ->
      Error
        (Printf.sprintf "Apple container stdin command %s"
           (Process.status_to_string status))

let sha256 value =
  String.length value = 71
  && String.starts_with ~prefix:"sha256:" value
  && String.for_all
       (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
       (String.sub value 7 64)

let safe_resource name =
  String.starts_with ~prefix:"monty-worker-" name
  && String.length name <= 128
  && String.for_all
       (function
         | 'a' .. 'z' | '0' .. '9' | '-' -> true
         | _ -> false)
       name

let validate value =
  if value.image <> image then
    Error (Printf.sprintf "unsupported container worker image %S" value.image)
  else if not (sha256 value.image_digest) then
    Error "container worker image digest must be a complete lowercase SHA-256"
  else if not (safe_resource value.container) then
    Error (Printf.sprintf "unsafe container worker name %S" value.container)
  else if
    not
      (safe_resource value.volume
      && String.ends_with ~suffix:"-volume" value.volume)
  then Error (Printf.sprintf "unsafe container worker volume %S" value.volume)
  else if
    String.length value.owner <> 35
    || not (String.starts_with ~prefix:"v1-" value.owner)
    || not
         (String.sub value.owner 3 32
         |> String.for_all (function
              | '0' .. '9' | 'a' .. 'f' -> true
              | _ -> false))
  then
    Error (Printf.sprintf "unsafe container worker owner %S" value.owner)
  else Ok value

let to_json value =
  `Assoc
    [ ("schema", `String schema);
      ("image", `String value.image);
      ("image_digest", `String value.image_digest);
      ("container", `String value.container);
      ("volume", `String value.volume);
      ("owner", `String value.owner) ]

let of_json json =
  let string name =
    match Yojson.Safe.Util.member name json with
    | `String value when String.trim value <> "" -> Ok value
    | _ -> Error (Printf.sprintf "container worker field %S must be a string" name)
  in
  let* found_schema = string "schema" in
  let* image = string "image" in
  let* image_digest = string "image_digest" in
  let* container = string "container" in
  let* volume = string "volume" in
  let* owner = string "owner" in
  if found_schema <> schema then
    Error (Printf.sprintf "unsupported container worker schema %S" found_schema)
  else validate { image; image_digest; container; volume; owner }

let token ~worker_dir ~id =
  let parent = Filename.dirname worker_dir in
  let stable_worker_dir =
    match Filename.basename parent with
    | "workers" -> worker_dir
    | "archive" ->
        Filename.concat
          (Filename.concat (Filename.dirname parent) "workers") id
    | _ -> worker_dir
  in
  Digest.string (Shell.normalize stable_worker_dir ^ "\000" ^ id)
  |> Digest.to_hex

let identity ~worker_dir ~id image_digest =
  let token = token ~worker_dir ~id in
  validate
    {
      image;
      image_digest;
      container = "monty-worker-" ^ token;
      volume = "monty-worker-" ^ token ^ "-volume";
      owner = "v1-" ^ token;
    }

let validate_identity ~worker_dir ~id value =
  let* expected = identity ~worker_dir ~id value.image_digest in
  if expected = value then Ok ()
  else
    Error
      (Printf.sprintf
         "persisted container worker identity is not deterministic for this worker: expected %s/%s, found %s/%s"
         expected.container expected.volume value.container value.volume)

let with_worker_lock ?(wait = true) ~worker_dir operation =
  let path = Filename.concat worker_dir ".container-worker.lock" in
  let* fd = State_store.open_lock_file path in
  Fun.protect
    ~finally:(fun () ->
      (try Unix.lockf fd Unix.F_ULOCK 0 with _ -> ());
      Unix.close fd)
    (fun () ->
      let acquired =
        try
          Unix.lockf fd (if wait then Unix.F_LOCK else Unix.F_TLOCK) 0;
          Ok ()
        with
        | Unix.Unix_error ((Unix.EACCES | Unix.EAGAIN), _, _) ->
            Error "container worker is busy with a headless run"
        | Unix.Unix_error (err, fn, arg) ->
            Error
              (Printf.sprintf "%s(%s): %s" fn arg (Unix.error_message err))
      in
      let* () = acquired in
      let* () = State_store.verify_lock_identity path fd in
      operation ())

let image_record_path ~home =
  Filename.concat (Filename.concat home ".monty") "container-worker-image.json"

let image_digest_of_json json =
  let open Yojson.Safe.Util in
  try
    let digest = json |> index 0 |> member "configuration" |> member "descriptor"
                 |> member "digest" |> to_string
    in
    if sha256 digest then Ok digest
    else Error "Apple image inspect returned an invalid digest"
  with Type_error _ -> Error "Apple image inspect did not contain an image digest"

let image_config json =
  let open Yojson.Safe.Util in
  try
    let variant =
      json |> index 0 |> member "variants" |> to_list
      |> List.find (fun variant ->
             variant |> member "platform" |> member "architecture" |> to_string
             = "arm64"
             && variant |> member "platform" |> member "os" |> to_string
                = "linux")
    in
    Some (variant |> member "config" |> member "config")
  with Not_found | Type_error _ -> None

let validate_image_json json =
  let open Yojson.Safe.Util in
  let* digest = image_digest_of_json json in
  match image_config json with
  | None -> Error "container worker image has no Linux ARM64 configuration"
  | Some config ->
      let label = config |> member "Labels" |> member image_label in
      let user = config |> member "User" in
      let command = config |> member "Cmd" in
      let leaked_secret =
        (match config |> member "Env" with `List values -> values | _ -> [])
        |> List.exists (function
             | `String value ->
                 String.starts_with ~prefix:"CODEX_API_KEY=" value
                 || String.starts_with ~prefix:"OPENAI_API_KEY=" value
                 || String.starts_with ~prefix:"CODEX_HOME_AUTH=" value
             | _ -> false)
      in
      if label <> `String image_version then
        Error "container worker image has the wrong Monty image label"
      else if user <> `String "monty" then
        Error "container worker image must run as user monty"
      else if command <> `List [ `String "sleep"; `String "infinity" ] then
        Error "container worker image must have inert sleep infinity as its command"
      else if leaked_secret then
        Error "container worker image configuration contains a credential variable"
      else Ok digest

let check_runtime () =
  let* _ = Process.command_exists_with_arguments (command ()) in
  match capture [ "system"; "status" ] with
  | Ok { Process.status = `Exited 0; _ } -> Ok ()
  | Ok _ ->
      Error
        "Apple container service must already be running; Monty will not start or reconfigure it"
  | Error message -> Error message

let inspect_image () =
  let* raw = run [ "image"; "inspect"; image ] in
  try Yojson.Safe.from_string raw |> validate_image_json
  with Yojson.Json_error message ->
    Error ("invalid Apple image inspection JSON: " ^ message)

let load_registered_image ~home =
  let record_path = image_record_path ~home in
  let* state = State_store.lstat record_path in
  let* () =
    match state with
    | Some { Unix.st_kind = Unix.S_REG; _ } -> Ok ()
    | Some { Unix.st_kind = Unix.S_LNK; _ } ->
        Error (Printf.sprintf "unsafe container image record is a symlink: %s" record_path)
    | Some _ -> Error (Printf.sprintf "container image record is not regular: %s" record_path)
    | None ->
        Error
          (Printf.sprintf
             "container worker image is not registered; build %s explicitly, then run monty container-image register"
             image)
  in
  let* json = State_store.read_json ~path:record_path in
  match json with
  | None -> Error (Printf.sprintf "container image record disappeared: %s" record_path)
  | Some json ->
      let open Yojson.Safe.Util in
      (match (member "schema" json, member "image" json, member "digest" json) with
      | `String found_schema, `String found_image, `String digest
        when found_schema = image_schema && found_image = image && sha256 digest ->
          Ok digest
      | _ -> Error (Printf.sprintf "invalid container image record: %s" record_path))

let register_image ~home =
  let* () = check_runtime () in
  let* digest = inspect_image () in
  let json =
    `Assoc
      [ ("schema", `String image_schema); ("image", `String image);
        ("digest", `String digest);
        ("registered_at", `String (now_utc ())) ]
  in
  let* () =
    State_store.with_lock ~home (fun () ->
        State_store.write_json_atomic ~path:(image_record_path ~home) json)
  in
  Ok digest

let preflight_image ~home =
  let* () = check_runtime () in
  let* expected = load_registered_image ~home in
  let* actual = inspect_image () in
  if actual = expected then Ok actual
  else
    Error
      (Printf.sprintf
         "container worker image mismatch: registered %s@%s, found %s; launch will not build, pull, or replace it"
         image expected actual)

let preflight_new ~home ~worker_dir ~id =
  let* digest = preflight_image ~home in
  identity ~worker_dir ~id digest

let preflight_existing ~home value =
  let* digest = preflight_image ~home in
  if digest = value.image_digest then Ok ()
  else Error "persisted container worker image no longer matches the registered image"

let lines output =
  output |> String.split_on_char '\n' |> List.map String.trim
  |> List.filter (fun value -> value <> "")

let exists args name =
  let* output = run (args @ [ "--quiet" ]) in
  Ok (List.mem name (lines output))

let labels json =
  let open Yojson.Safe.Util in
  try json |> index 0 |> member "configuration" |> member "labels" |> to_assoc
  with Type_error _ -> []

let owner_of_json json =
  match List.assoc_opt owner_label (labels json) with
  | Some (`String value) -> Ok value
  | _ -> Error "Apple resource inspect omitted the Monty ownership label"

let inspect_json args =
  let* output = run args in
  try Ok (Yojson.Safe.from_string output)
  with Yojson.Json_error message -> Error ("invalid Apple resource inspection JSON: " ^ message)

let ensure_owner value kind json =
  let* owner = owner_of_json json in
  if owner = value.owner then Ok ()
  else
    Error
      (Printf.sprintf "refusing %s %S owned by %S instead of %S" kind
         (if kind = "volume" then value.volume else value.container)
         owner value.owner)

let validate_volume value =
  let* json = inspect_json [ "volume"; "inspect"; value.volume ] in
  ensure_owner value "volume" json

let validate_container value =
  let open Yojson.Safe.Util in
  let* json = inspect_json [ "inspect"; value.container ] in
  let* () = ensure_owner value "container" json in
  let rec mount_records = function
    | `Assoc fields ->
        fields
        |> List.concat_map (fun (name, child) ->
               (if String.lowercase_ascii name = "mounts" then
                  match child with
                  | `List mounts ->
                      List.filter
                        (function `Assoc _ -> true | _ -> false)
                        mounts
                  | _ -> []
                else [])
               @ mount_records child)
    | `List values -> List.concat_map mount_records values
    | _ -> []
  in
  let mount_identity mount =
    let destination = member "destination" mount in
    match member "type" mount with
    | `Assoc [ ("volume", volume) ] ->
        ("volume", destination, member "name" volume)
    | `Assoc [ ("tmpfs", _) ] -> ("tmpfs", destination, `Null)
    | `String kind -> (String.lowercase_ascii kind, destination, `Null)
    | `Assoc ((kind, _) :: _) ->
        (String.lowercase_ascii kind, destination, `Null)
    | _ -> ("", destination, `Null)
  in
  try
    let config = json |> index 0 |> member "configuration" in
    let digest = config |> member "image" |> member "descriptor" |> member "digest" |> to_string in
    let no_host_integration name expected = member name config = expected in
    let init = member "initProcess" config in
    let uid = init |> member "user" |> member "id" |> member "uid" in
    let gid = init |> member "user" |> member "id" |> member "gid" in
    let executable = init |> member "executable" in
    let arguments = init |> member "arguments" in
    let environment =
      match member "environment" init with `List values -> values | _ -> []
    in
    let forbidden_environment =
      List.exists
        (function
          | `String value ->
              String.starts_with ~prefix:"CODEX_API_KEY=" value
              || String.starts_with ~prefix:"OPENAI_API_KEY=" value
              || String.starts_with ~prefix:"CODEX_HOME_AUTH=" value
          | _ -> true)
        environment
    in
    let mounts = mount_records json |> List.map mount_identity in
    let expected_mounts =
      [ ("tmpfs", `String "/run/monty-secrets", `Null);
        ("tmpfs", `String "/tmp", `Null);
        ("volume", `String "/monty", `String value.volume) ]
      |> List.sort compare
    in
    if digest <> value.image_digest then
      Error "owned container uses a different image digest"
    else if
      member "platform" config |> member "architecture" <> `String "arm64"
      || member "platform" config |> member "os" <> `String "linux"
      || member "useInit" config <> `Bool true
    then Error "owned container is not a native ARM64 inert init container"
    else if
      uid <> `Int 10001 || gid <> `Int 10001
      || executable |> to_string |> Filename.basename <> "sleep"
      || arguments <> `List [ `String "infinity" ]
      || forbidden_environment
      || member "capAdd" config
         <> `List [ `String "CAP_CHOWN"; `String "CAP_DAC_OVERRIDE" ]
      || member "capDrop" config <> `List [ `String "ALL" ]
    then Error "owned container does not run the inert process as unprivileged monty"
    else if
      not
        (no_host_integration "publishedPorts" (`List [])
        && no_host_integration "publishedSockets" (`List [])
        && no_host_integration "rosetta" (`Bool false)
        && no_host_integration "ssh" (`Bool false)
        && no_host_integration "virtualization" (`Bool false))
    then Error "owned container has a forbidden broad host integration"
    else if List.sort compare mounts <> expected_mounts then
      Error "owned container does not have exactly the private volume and two tmpfs mounts"
    else Ok ()
  with Type_error _ -> Error "owned container inspection is incomplete"

let running value = exists [ "list" ] value.container

let create_arguments value =
  [ "create"; "--name"; value.container; "--cpus"; "4"; "--memory"; "8g";
    "--platform"; "linux/arm64"; "--label";
    owner_label ^ "=" ^ value.owner; "--init"; "--cap-drop"; "ALL";
    "--cap-add"; "CAP_CHOWN"; "--cap-add"; "CAP_DAC_OVERRIDE"; "--user";
    "monty"; "--env"; "HOME=/monty/home"; "--env";
    "CODEX_HOME=/monty/home/.codex"; "--mount";
    "type=volume,source=" ^ value.volume ^ ",target=/monty"; "--mount";
    "type=tmpfs,target=/tmp,size=1G,mode=1777"; "--mount";
    "type=tmpfs,target=/run/monty-secrets,size=16M,mode=0700"; "--entrypoint";
    "sleep"; value.image ^ "@" ^ value.image_digest; "infinity" ]

let ensure_resources value =
  let* volume_exists = exists [ "volume"; "list" ] value.volume in
  let* () =
    if volume_exists then validate_volume value
    else
      let* _ =
        run
          [ "volume"; "create"; "--label"; owner_label ^ "=" ^ value.owner;
            "--opt"; "size=8g"; value.volume ]
      in
      validate_volume value
  in
  let* container_exists = exists [ "list"; "--all" ] value.container in
  if container_exists then validate_container value
  else
    let* _ = run (create_arguments value) in
    validate_container value

let auth_path () =
  let path =
    match Sys.getenv_opt "CODEX_HOME" with
    | Some home when String.trim home <> "" -> Filename.concat home "auth.json"
    | _ -> (
        match Sys.getenv_opt "HOME" with
        | Some home -> Filename.concat (Filename.concat home ".codex") "auth.json"
        | None -> "")
  in
  if path = "" || Filename.is_relative path then
    Error "Codex auth.json path must be absolute"
  else
    try
      let stat = Unix.lstat path in
      if stat.Unix.st_kind <> Unix.S_REG then
        Error (Printf.sprintf "Codex auth.json is not a regular file: %s" path)
      else if stat.Unix.st_uid <> Unix.getuid () || stat.Unix.st_perm land 0o077 <> 0 then
        Error (Printf.sprintf "Codex auth.json must be owned by this user with mode 0600: %s" path)
      else Ok path
    with Unix.Unix_error _ ->
      Error (Printf.sprintf "Codex auth.json is unavailable: %s" path)

let credential_values contents =
  let rec collect acc = function
    | `String value when value <> "" -> value :: acc
    | `Assoc fields ->
        List.fold_left (fun acc (_, value) -> collect acc value) acc fields
    | `List values -> List.fold_left collect acc values
    | _ -> acc
  in
  try Ok (Yojson.Safe.from_string contents |> collect [] |> List.sort_uniq String.compare)
  with Yojson.Json_error _ -> Error "Codex auth.json must contain valid JSON"

let load_auth () =
  let* path = auth_path () in
  let* contents =
    try Ok (Shell.read_file path)
    with Sys_error message -> Error ("could not read Codex auth.json: " ^ message)
  in
  let* credentials = credential_values contents in
  Ok (contents, credentials)

let reject_credentials credentials contents =
  if
    List.exists
      (fun credential -> Home.contains_substring contents credential)
      credentials
  then Error "container output contains a Codex credential value"
  else Ok ()

let inject_auth value =
  let* contents, credentials = load_auth () in
  let* _ =
    run
      [ "exec"; "--user"; "root"; value.container; "sh"; "-c";
        "set -eu; mkdir -p /monty/home/.codex; chown monty:monty /monty/home /monty/home/.codex /run/monty-secrets" ]
  in
  let script =
    "set -eu; umask 077; cat > /run/monty-secrets/auth.json; "
    ^ "chmod 600 /run/monty-secrets/auth.json; "
    ^ "ln -sfn /run/monty-secrets/auth.json /monty/home/.codex/auth.json"
  in
  let* _ =
    run_stdin
      [ "exec"; "--interactive"; "--user"; "monty"; value.container; "sh";
        "-c"; script ]
      contents
  in
  Ok credentials

let stop_started value result =
  match run [ "stop"; "--time"; "1"; value.container ] with
  | Ok _ -> result
  | Error stop_error -> (
      match result with
      | Ok _ -> Error ("failed to restore stopped container state: " ^ stop_error)
      | Error message ->
          Error
            (message ^ "; additionally failed to restore stopped state: "
           ^ stop_error))

let validate_worker_capabilities value =
  run
    [ "exec"; "--user"; "monty"; value.container; "sh"; "-c";
      "set -eu; for field in CapInh CapPrm CapEff CapAmb; do value=$(sed -n \"s/^$field:[[:space:]]*//p\" /proc/self/status); test -n \"$value\"; case \"$value\" in *[!0]*) exit 1;; esac; done" ]
  |> Result.map (fun _ -> ())

let start_existing ~probe ~authenticate value =
  let started = Unix.gettimeofday () in
  let* was_running = running value in
  let result =
    let* _ = if was_running then Ok "" else run [ "start"; value.container ] in
    let* _ =
      if probe && not was_running then run [ "exec"; value.container; "true" ]
      else Ok ""
    in
    let* () = if probe then validate_worker_capabilities value else Ok () in
    let* _ = if authenticate then inject_auth value else Ok [] in
    Ok ()
  in
  match (result, was_running) with
  | Ok (), _ -> Ok (was_running, Unix.gettimeofday () -. started)
  | Error _ as error, true -> error
  | Error _ as error, false -> stop_started value error

let ensure_running value =
  let* () = ensure_resources value in
  start_existing ~probe:true ~authenticate:true value

let wake value =
  let* volume_exists = exists [ "volume"; "list" ] value.volume in
  let* container_exists = exists [ "list"; "--all" ] value.container in
  if not volume_exists then
    Error (Printf.sprintf "container worker volume is missing: %s" value.volume)
  else if not container_exists then
    Error (Printf.sprintf "container worker is missing: %s" value.container)
  else
    let* () = validate_volume value in
    let* () = validate_container value in
    start_existing ~probe:true ~authenticate:true value

let start_for_inspection value =
  let* volume_exists = exists [ "volume"; "list" ] value.volume in
  let* container_exists = exists [ "list"; "--all" ] value.container in
  if not volume_exists then
    Error (Printf.sprintf "container worker volume is missing: %s" value.volume)
  else if not container_exists then
    Error (Printf.sprintf "container worker is missing: %s" value.container)
  else
    let* () = validate_volume value in
    let* () = validate_container value in
    start_existing ~probe:false ~authenticate:false value

let encoded_branch branch =
  let buffer = Buffer.create (String.length branch) in
  String.iter
    (function
      | '_' -> Buffer.add_string buffer "__"
      | '/' -> Buffer.add_char buffer '_'
      | character -> Buffer.add_char buffer character)
    branch;
  Buffer.contents buffer

let guest_worktree branch =
  "/monty/worktrees/task/" ^ encoded_branch branch

let write_guest_file value path contents =
  let script =
    "set -eu; umask 077; cat > " ^ Shell.quote path
  in
  run_stdin
    [ "exec"; "--interactive"; "--user"; "monty"; value.container; "sh";
      "-c"; script ]
    contents
  |> Result.map (fun _ -> ())

let guest_instructions ~id ~title =
  String.concat "\n"
    [ "# Monty container worker instructions"; "";
      "You are running in a private Apple container volume.";
      "Worker: " ^ id ^ " — " ^ title; "";
      "The authoritative workspace map is /monty/context/job.json.";
      "MONTY_JOB_FILE=/monty/context/job.json";
      "The task context is /monty/context/task.md.";
      "Modify only the worktree recorded in that workspace map.";
      "Do not write Monty host state, stage or commit, push, or start another agent.";
      "Return discoveries, validation, risks, and the final handoff in your response."; "" ]

let guest_job_json ~id ~title ~branch ~worktree =
  `Assoc
    [ ("id", `String id); ("title", `String title); ("repo", `String guest_repo);
      ("branch", `String branch); ("context", `String "/monty/context/task.md");
      ("workspaces",
       `List
         [ `Assoc
             [ ("repo", `String guest_repo); ("branch", `String branch);
               ("worktree", `String worktree) ] ]) ]
  |> Yojson.Safe.pretty_to_string

let seed_revision repo branch =
  let branch_ref = "refs/heads/" ^ branch in
  match
    Process.run_capture
      (String.concat " "
         [ "git"; "-C"; Shell.quote repo; "show-ref"; "--verify"; "--quiet";
           Shell.quote branch_ref ])
  with
  | Ok { status = `Exited 0; _ } ->
      Process.run_success
        (String.concat " "
           [ "git"; "-C"; Shell.quote repo; "rev-parse"; Shell.quote branch_ref ])
      |> Result.map (fun head -> (String.trim head, Some branch_ref))
  | Ok { status = `Exited 1; _ } ->
      Process.run_success
        (String.concat " " [ "git"; "-C"; Shell.quote repo; "rev-parse"; "HEAD" ])
      |> Result.map (fun head -> (String.trim head, None))
  | Ok { status; stdout } ->
      Error
        (Printf.sprintf "could not inspect requested task branch (%s): %s"
           (Process.status_to_string status) stdout)
  | Error _ as error -> error

let with_temp_bundle repo branch_ref operation =
  let path = Filename.temp_file "monty-container-seed-" ".bundle" in
  Fun.protect
    ~finally:(fun () ->
      try Unix.unlink path with Unix.Unix_error (Unix.ENOENT, _, _) -> ())
    (fun () ->
      let* _ =
        Process.run_success
          (String.concat " "
             ([ "git"; "-C"; Shell.quote repo; "bundle"; "create";
                Shell.quote path; "HEAD" ]
             @
             match branch_ref with
             | None -> []
             | Some branch_ref -> [ Shell.quote branch_ref ]))
      in
      let* _ =
        Process.run_success
          (String.concat " " [ "git"; "bundle"; "verify"; Shell.quote path ])
      in
      operation path)

let seed value ~repo ~branch ~branch_ref ~expected_head =
  let worktree = guest_worktree branch in
  with_temp_bundle repo branch_ref (fun bundle ->
      let* _ =
        run [ "copy"; bundle; value.container ^ ":/monty-seed.bundle" ]
      in
      let* _ =
        run
          [ "exec"; "--user"; "root"; value.container; "sh"; "-c";
            "set -eu; mkdir -p /monty/home/.cache /monty/home/.codex /monty/repos /monty/worktrees /monty/context /monty/outbox /monty/artifacts /monty/home/.local/share; chown -R monty:monty /monty" ]
      in
      let script =
        String.concat " "
          [ "set -eu;";
            "if [ ! -e /monty/home/.local/share/wt ]; then ln -s /monty/worktrees /monty/home/.local/share/wt; fi;";
            "test \"$(readlink /monty/home/.local/share/wt)\" = /monty/worktrees;";
            "if [ ! -f /monty/context/seeded ]; then";
            "cp /monty-seed.bundle /monty/context/source.bundle;";
            "if [ ! -d /monty/repos/task/.git ]; then git clone /monty/context/source.bundle /monty/repos/task; fi;";
            "cd /monty/repos/task && wt b \"$1\" >/tmp/monty-wt.out;";
            "test -d \"$2\";";
            "test \"$(git -C \"$2\" rev-parse HEAD)\" = \"$3\";";
            "test \"$(git -C \"$2\" branch --show-current)\" = \"$1\";";
            "printf '%s\\n' \"$2\" > /monty/context/worktree;";
            "printf '%s\\n' \"$1\" > /monty/context/branch;";
            "printf '%s\\n' \"$3\" > /monty/context/seeded;";
            "else test -d \"$2\"; test \"$(cat /monty/context/seeded)\" = \"$3\"; test \"$(cat /monty/context/branch)\" = \"$1\"; fi" ]
      in
      let* _ =
        run
          [ "exec"; "--user"; "monty"; value.container; "sh"; "-c"; script;
            "monty-seed"; branch; worktree; expected_head ]
      in
      let* _ =
        run
          [ "exec"; "--user"; "root"; value.container; "rm"; "-f";
            "/monty-seed.bundle" ]
      in
      Ok worktree)

let ensure_prepared value ~id ~title ~repo ~branch ~context =
  let* was_running, _ready = ensure_running value in
  let result =
    let* expected_head, branch_ref = seed_revision repo branch in
    let* worktree = seed value ~repo ~branch ~branch_ref ~expected_head in
    let* context_contents =
      try Ok (Shell.read_file context) with Sys_error message -> Error message
    in
    let* () = write_guest_file value "/monty/context/task.md" context_contents in
    let* () =
      write_guest_file value "/monty/context/MONTY.md"
        (guest_instructions ~id ~title)
    in
    let* () =
      write_guest_file value "/monty/context/job.json"
        (guest_job_json ~id ~title ~branch ~worktree)
    in
    let* _ =
      run
        [ "exec"; "--user"; "root"; value.container; "sh"; "-c";
          "set -eu; test ! -L /monty; test ! -L /monty/context; test ! -L /monty/repos; test ! -L /monty/worktrees; test ! -L /monty/worktrees/task; chown root:root /monty /monty/context /monty/repos /monty/worktrees /monty/worktrees/task; chown -R root:root /monty/context; chmod 0755 /monty /monty/repos /monty/worktrees /monty/worktrees/task; chmod 0555 /monty/context; find /monty/context -type f -exec chmod 0444 {} +" ]
    in
    Ok worktree
  in
  match (result, was_running) with
  | Ok _, _ | Error _, true -> result
  | Error _ as error, false -> stop_started value error

let remove_partial path =
  match State_store.lstat path with
  | Ok None -> Ok ()
  | Ok (Some { Unix.st_kind = (Unix.S_REG | Unix.S_LNK); _ }) -> (
      try Unix.unlink path; Ok () with Unix.Unix_error (err, fn, arg) ->
        Error (Printf.sprintf "%s(%s): %s" fn arg (Unix.error_message err)))
  | Ok (Some _) -> Error (Printf.sprintf "refusing non-file collection target: %s" path)
  | Error _ as error -> error

let cleanup_error original label = function
  | Ok () -> original
  | Error message -> original ^ "; additionally failed to clean " ^ label ^ ": " ^ message

let collect_file value ~credentials ~guest ~host =
  let stage =
    "/monty-export-" ^ Digest.to_hex (Digest.string (value.owner ^ "\000" ^ guest))
  in
  let partial = host ^ ".partial" in
  let remove_stage () =
    run [ "exec"; "--user"; "root"; value.container; "rm"; "-f"; stage ]
    |> Result.map (fun _ -> ())
  in
  let fail message =
    let message = cleanup_error message "container staging file" (remove_stage ()) in
    Error (cleanup_error message "partial host output" (remove_partial partial))
  in
  let validate_script =
    "set -eu; test -f \"$1\"; test ! -L \"$1\"; "
    ^ "size=$(wc -c < \"$1\"); test \"$size\" -le 5242880; "
    ^ "cp \"$1\" \"$2\"; chmod 600 \"$2\""
  in
  let result =
    let* _ =
      run
        [ "exec"; "--user"; "root"; value.container; "sh"; "-c";
          validate_script; "monty-collect"; guest; stage ]
    in
    let* () = remove_partial partial in
    let* _ = run [ "copy"; value.container ^ ":" ^ stage; partial ] in
    let* state = State_store.lstat partial in
    let* () =
      match state with
      | Some { Unix.st_kind = Unix.S_REG; st_size; _ }
        when st_size <= 5_242_880 -> Ok ()
      | Some _ ->
          Error (Printf.sprintf "unsafe collected container output: %s" partial)
      | None ->
          Error (Printf.sprintf "container output was not collected: %s" partial)
    in
    let* contents =
      try Ok (Shell.read_file partial)
      with Sys_error message -> Error ("could not read collected output: " ^ message)
    in
    let* () = reject_credentials credentials contents in
    let* () = remove_stage () in
    try
      Unix.rename partial host;
      Ok ()
    with Unix.Unix_error (err, fn, arg) ->
      Error (Printf.sprintf "%s(%s): %s" fn arg (Unix.error_message err))
  in
  match result with Ok () -> Ok () | Error message -> fail message

let run_codex_phase value ~attempt ~name ~worktree ~codex_yolo ~writable ~prompt
    ~output ~events ~progress =
  let* is_running = running value in
  let* () =
    if is_running then Ok ()
    else Error "container worker stopped before its Codex phase started"
  in
  let* _ = State_path.safe_component ~label:"headless attempt id" attempt in
  let* _ = State_path.safe_component ~label:"headless phase name" name in
  let outbox = "/monty/outbox/" ^ attempt in
  let guest_output = outbox ^ "/" ^ name ^ ".md" in
  let guest_events = outbox ^ "/" ^ name ^ ".jsonl" in
  let guest_progress = outbox ^ "/" ^ name ^ ".log" in
  let permission =
    if codex_yolo then " --dangerously-bypass-approvals-and-sandbox"
    else if writable then " --sandbox workspace-write"
    else " --sandbox read-only"
  in
  let codex =
    "codex exec -c " ^ Shell.quote "model_reasoning_effort=\"xhigh\""
    ^ Codex_trust.argument worktree
    ^ " --ephemeral --json --color never" ^ permission ^ " -C "
    ^ Shell.quote worktree ^ " --output-last-message " ^ Shell.quote guest_output
    ^ " - > " ^ Shell.quote guest_events ^ " 2> " ^ Shell.quote guest_progress
  in
  let script =
    "set -eu; umask 077; mkdir -p " ^ Shell.quote outbox ^ "; " ^ codex
  in
  let result =
    let* credentials = inject_auth value in
    Process.run_capture_with_stdin
      (shell_command
         [ "exec"; "--interactive"; "--user"; "monty"; "--workdir";
           worktree; value.container; "sh"; "-c"; script ])
      prompt
    |> Result.map (fun result -> (credentials, result))
  in
  let collect credentials guest host =
    collect_file value ~credentials ~guest ~host
  in
  match result with
  | Error message ->
      let _ = message in
      Error message
  | Ok (credentials, { Process.status = `Exited 0; _ }) ->
      let* () = collect credentials guest_events events in
      let* () = collect credentials guest_progress progress in
      collect credentials guest_output output
  | Ok (credentials, { status; _ }) ->
      let _ = collect credentials guest_events events in
      let _ = collect credentials guest_progress progress in
      Error
        (Printf.sprintf "containerized Codex %s phase %s" name
           (Process.status_to_string status))

let split_nul value =
  value |> String.split_on_char '\000' |> List.filter (fun item -> item <> "")

let safe_git_path path =
  path <> "" && Filename.is_relative path
  &&
  path |> String.split_on_char '/'
  |> List.for_all (fun part -> part <> "" && part <> "." && part <> "..")

let parse_change_summary output =
  let* newline =
    match String.index_opt output '\n' with
    | Some index -> Ok index
    | None -> Error "container change summary omitted its header"
  in
  let header = String.sub output 0 newline in
  let* numstat_size, tracked_size, untracked_size =
    match
      String.split_on_char ' ' header |> List.filter (fun field -> field <> "")
    with
    | [ "monty-changes-v1"; numstat; tracked; untracked ] -> (
        match
          (int_of_string_opt numstat, int_of_string_opt tracked,
           int_of_string_opt untracked)
        with
        | Some numstat, Some tracked, Some untracked ->
            Ok (numstat, tracked, untracked)
        | _ -> Error "container change summary has invalid section sizes")
    | _ -> Error "container change summary has an invalid header"
  in
  let payload_offset = newline + 1 in
  let* () =
    if
      numstat_size < 0 || tracked_size < 0 || untracked_size < 0
      || List.exists (fun size -> size > 5_242_880)
           [ numstat_size; tracked_size; untracked_size ]
    then Error "container change summary has invalid section sizes"
    else Ok ()
  in
  let total = numstat_size + tracked_size + untracked_size in
  let* () =
    if total = String.length output - payload_offset then Ok ()
    else Error "container change summary has invalid section sizes"
  in
  let section offset length = String.sub output (payload_offset + offset) length in
  let numstat = section 0 numstat_size |> split_nul in
  let tracked = section numstat_size tracked_size |> split_nul in
  let untracked =
    section (numstat_size + tracked_size) untracked_size |> split_nul
  in
  let files = List.sort_uniq String.compare (tracked @ untracked) in
  let* () =
    if List.for_all safe_git_path files then Ok ()
    else Error "container change summary contains an unsafe Git path"
  in
  let insertions, deletions =
    numstat
    |> List.fold_left
         (fun (insertions, deletions) record ->
           match String.split_on_char '\t' record with
           | inserted :: deleted :: _ ->
               let count value = Option.value ~default:0 (int_of_string_opt value) in
               (insertions + count inserted, deletions + count deleted)
           | _ -> (insertions, deletions))
         (0, 0)
  in
  Ok { files; insertions; deletions }

let collect_changes value ~worktree ~branch =
  let* _, credentials = load_auth () in
  let* captured =
    capture
      [ "exec"; value.container; "monty-inspect"; "--changes"; worktree;
        branch ]
  in
  let* () = reject_credentials credentials captured.stdout in
  match captured.status with
  | `Exited 0 -> parse_change_summary captured.stdout
  | status ->
      Error
        (Printf.sprintf "container change inspection %s"
           (Process.status_to_string status))

let inspect value ~worktree ~branch =
  let* was_running, ready_seconds = start_for_inspection value in
  let started = Unix.gettimeofday () in
  let result =
    run [ "exec"; value.container; "monty-inspect"; worktree; branch ]
  in
  (match (result, was_running) with
  | Ok output, _ ->
      Ok
        { output; was_running; ready_seconds;
          result_seconds = Unix.gettimeofday () -. started }
  | Error _ as error, true -> error
  | Error _ as error, false -> stop_started value error)

let stop value =
  let started = Unix.gettimeofday () in
  let* exists = exists [ "list"; "--all" ] value.container in
  let* () = if exists then validate_container value else Ok () in
  let* is_running = if exists then running value else Ok false in
  let* () =
    if is_running then
      run [ "stop"; "--time"; "1"; value.container ] |> Result.map (fun _ -> ())
    else Ok ()
  in
  Ok (Unix.gettimeofday () -. started)

let ensure_clean value ~worktree ~branch =
  let* was_running, _ready = start_for_inspection value in
  let result =
    match
      capture
        [ "exec"; value.container; "monty-inspect"; "--dirty"; worktree;
          branch ]
    with
    | Ok { Process.status = `Exited 0; _ } -> Ok ()
    | Ok { Process.status = `Exited 3; _ } ->
        Error
          "containerized worker has tracked or untracked changes; use monty inspect, preserve the work, or rerun monty done --force to discard it"
    | Ok { status; stdout } ->
        Error
          (Printf.sprintf "container workspace cleanliness check %s: %s"
             (Process.status_to_string status) stdout)
    | Error message -> Error message
  in
  if was_running then result else stop_started value result

let remove value =
  let* container_exists = exists [ "list"; "--all" ] value.container in
  let* () = if container_exists then validate_container value else Ok () in
  let* () =
    if container_exists then
      let* _ = stop value in
      let* _ = run [ "delete"; value.container ] in
      Ok ()
    else Ok ()
  in
  let* volume_exists = exists [ "volume"; "list" ] value.volume in
  let* () = if volume_exists then validate_volume value else Ok () in
  let* () =
    if volume_exists then
      run [ "volume"; "delete"; value.volume ] |> Result.map (fun _ -> ())
    else Ok ()
  in
  let* container_remains = exists [ "list"; "--all" ] value.container in
  let* volume_remains = exists [ "volume"; "list" ] value.volume in
  if container_remains || volume_remains then
    Error "exact container worker cleanup left an owned resource behind"
  else Ok ()
