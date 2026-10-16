type t = {
  harness : Harness.t option;
  codex_yolo : bool;
  context_enabled : bool;
  branch_prefix : string option;
  agent_profile : string option;
  terminal : Terminal.backend option;
  rex_session : string option;  (* Rex session for tabs requested outside Rex, no Mantle profile *)
  rex_sessions : (string * string) list;  (* the same per Mantle profile *)
}

let empty =
  { harness = None; context_enabled = false; codex_yolo = false;
    branch_prefix = None; agent_profile = None; terminal = None; rex_session = None;
    rex_sessions = [] }
let path ~home = Filename.concat (Filename.concat home ".monty") "settings.json"

let parse json =
  State_store.decode_json ~path:"settings" (fun () ->
    let open Yojson.Safe.Util in
    let ( let* ) = Result.bind in
    let* harness =
      match member "harness" json with
      | `Null -> Ok None
      | `String value ->
          Harness.of_string value
          |> Result.map_error (fun (`Msg message) -> message)
          |> Result.map Option.some
      | _ -> Error "settings field \"harness\" must be a string"
    in
    let* context_enabled =
      match member "context_enabled" json with
      | `Null -> Ok false
      | `Bool value -> Ok value
      | _ -> Error "context_enabled must be a boolean"
    in
    let* codex_yolo =
      match member "codex_yolo" json with
      | `Null -> Ok false
      | `Bool value -> Ok value
      | _ -> Error "settings field \"codex_yolo\" must be a boolean"
    in
    let* branch_prefix =
      match member "branch_prefix" json with
      | `Null -> Ok None
      | `String value -> Ok (Some value)
      | _ -> Error "settings field \"branch_prefix\" must be a string"
    in
    let* agent_profile =
      match member "agent_profile" json with
      | `Null -> Ok None
      | `String value ->
          State_path.safe_component ~label:"agent profile setting" value
          |> Result.map Option.some
      | _ -> Error "settings field \"agent_profile\" must be a string"
    in
    let* terminal =
      match member "terminal" json with
      | `Null -> Ok None
      | `String value ->
          Terminal.backend_of_string value
          |> Result.map_error (fun (`Msg message) -> message)
          |> Result.map Option.some
      | _ -> Error "settings field \"terminal\" must be a string"
    in
    let* rex_session =
      match member "rex_session" json with
      | `Null -> Ok None
      | `String value when String.trim value = "" -> Ok None
      | `String value -> Ok (Some value)
      | _ -> Error "settings field \"rex_session\" must be a string"
    in
    let* rex_sessions =
      match member "rex_sessions" json with
      | `Null -> Ok []
      | `Assoc entries ->
          List.fold_left
            (fun acc (profile, value) ->
              let* acc = acc in
              let* profile = Mantle.valid_name profile in
              match value with
              | `String value when String.trim value <> "" -> Ok (acc @ [ (profile, value) ])
              | `String _ | `Null -> Ok acc
              | _ -> Error (Printf.sprintf "settings field \"rex_sessions.%s\" must be a string" profile))
            (Ok []) entries
      | _ -> Error "settings field \"rex_sessions\" must be an object"
    in
    Ok { harness; context_enabled; codex_yolo; branch_prefix; agent_profile; terminal;
         rex_session; rex_sessions })

let load ~home =
  let settings_path = path ~home in
  let ( let* ) = Result.bind in
  let* existing = State_store.lstat settings_path in
  let* () =
    match existing with
    | None | Some { Unix.st_kind = Unix.S_REG; _ } -> Ok ()
    | Some { Unix.st_kind = Unix.S_LNK; _ } ->
        Error
          (Printf.sprintf "unsafe Monty settings file is a symlink: %s"
             settings_path)
    | Some _ ->
        Error
          (Printf.sprintf "Monty settings path is not a regular file: %s"
             settings_path)
  in
  let* json = State_store.read_json ~path:settings_path in
  match json with
  | None -> Ok empty
  | Some json ->
      parse json
      |> Result.map_error (fun message ->
             Printf.sprintf "invalid Monty settings in %s: %s" settings_path
               message)

let to_json settings =
  `Assoc
    [ ( "harness",
        match settings.harness with
        | None -> `Null
        | Some harness -> `String (Harness.to_string harness) );
      ("context_enabled", `Bool settings.context_enabled);
      ("codex_yolo", `Bool settings.codex_yolo);
      ( "branch_prefix",
        match settings.branch_prefix with
        | None -> `Null
        | Some branch_prefix -> `String branch_prefix );
      ( "agent_profile",
        match settings.agent_profile with
        | None -> `Null
        | Some agent_profile -> `String agent_profile );
      ( "terminal",
        match settings.terminal with
        | None -> `Null
        | Some terminal -> `String (Terminal.backend_to_string terminal) );
      ( "rex_session",
        match settings.rex_session with
        | None -> `Null
        | Some rex_session -> `String rex_session );
      ( "rex_sessions",
        `Assoc (List.map (fun (profile, session) -> (profile, `String session)) settings.rex_sessions) ) ]

let set_harness ~home harness =
  State_store.with_lock ~home (fun () ->
      let ( let* ) = Result.bind in
      let* settings = load ~home in
      State_store.write_json_atomic ~path:(path ~home)
        (to_json { settings with harness = Some harness }))

let set_codex_yolo ~home codex_yolo =
  State_store.with_lock ~home (fun () ->
      let ( let* ) = Result.bind in
      let* settings = load ~home in
      State_store.write_json_atomic ~path:(path ~home)
        (to_json { settings with codex_yolo }))

let set_branch_prefix ~home branch_prefix =
  State_store.with_lock ~home (fun () ->
      let ( let* ) = Result.bind in
      let* settings = load ~home in
      State_store.write_json_atomic ~path:(path ~home)
        (to_json { settings with branch_prefix = Some branch_prefix }))

let set_agent_profile ~home agent_profile =
  State_store.with_lock ~home (fun () ->
      let ( let* ) = Result.bind in
      let* settings = load ~home in
      State_store.write_json_atomic ~path:(path ~home)
        (to_json { settings with agent_profile = Some agent_profile }))

let effective_harness ~getenv ~home override =
  match override with
  | Some harness -> Ok harness
  | None -> (
      match getenv "MONTY_HARNESS" with
      | Some value when String.trim value <> "" ->
          Harness.of_string value
          |> Result.map_error (fun (`Msg message) -> message)
      | _ ->
          load ~home
          |> Result.map (fun settings ->
                 Option.value ~default:Harness.Pi settings.harness))

let bool_of_string value =
  match String.lowercase_ascii value with
  | "true" | "yes" | "1" | "on" -> Ok true
  | "false" | "no" | "0" | "off" -> Ok false
  | _ -> Error (Printf.sprintf "expected true or false, got %S" value)

let effective_codex_yolo ~getenv ~home override =
  if override then Ok true
  else
    match getenv "MONTY_CODEX_YOLO" with
    | Some value when String.trim value <> "" -> bool_of_string value
    | _ -> load ~home |> Result.map (fun settings -> settings.codex_yolo)

let effective_branch_prefix ~getenv ~home override =
  match override with
  | Some branch_prefix -> Ok branch_prefix
  | None ->
      load ~home
      |> Result.map (fun settings ->
             match settings.branch_prefix with
             | Some branch_prefix -> branch_prefix
             | None -> (
                 match getenv "MONTY_BRANCH_PREFIX" with
                 | Some value when String.trim value <> "" -> value
                 | _ -> "monty"))

let set_terminal ~home terminal =
  State_store.with_lock ~home (fun () ->
      let ( let* ) = Result.bind in
      let* settings = load ~home in
      State_store.write_json_atomic ~path:(path ~home)
        (to_json { settings with terminal = Some terminal }))

(* --terminal, then a valid MONTY_TERMINAL, then the persisted setting, then
   Rex. An unrecognized environment value falls through, as it always has. *)
let effective_terminal ~getenv ~home override =
  match override with
  | Some terminal -> Ok terminal
  | None -> (
      match Option.map Terminal.backend_of_string (getenv "MONTY_TERMINAL") with
      | Some (Ok terminal) -> Ok terminal
      | _ ->
          load ~home
          |> Result.map (fun settings ->
                 Option.value ~default:Terminal.Rex settings.terminal))

(* The Rex session that tabs requested from outside Rex should land in, for
   the active Mantle profile (None means no profile). *)
let rex_session_for settings profile =
  match profile with
  | Some profile -> List.assoc_opt profile settings.rex_sessions
  | None -> settings.rex_session

(* An empty value clears the entry, which returns that profile's tabs to the
   session Monty owns for it. *)
let set_rex_session ~home ~profile value =
  let value = match String.trim value with "" -> None | value -> Some value in
  State_store.with_lock ~home (fun () ->
      let ( let* ) = Result.bind in
      let* settings = load ~home in
      let settings =
        match profile with
        | None -> { settings with rex_session = value }
        | Some profile ->
            let others = List.remove_assoc profile settings.rex_sessions in
            { settings with
              rex_sessions =
                (match value with None -> others | Some value -> others @ [ (profile, value) ]) }
      in
      State_store.write_json_atomic ~path:(path ~home) (to_json settings))

let effective_agent_profile ~home override =
  match override with
  | Some agent_profile -> Ok agent_profile
  | None ->
      load ~home
      |> Result.map (fun settings ->
             Option.value ~default:Agent_profile.default_id settings.agent_profile)

let render ?profile settings =
  let harness =
    settings.harness |> Option.value ~default:Harness.Pi |> Harness.to_string
  in
  let rex_session =
    match rex_session_for settings profile, profile with
    | Some session, Some profile -> Printf.sprintf "%s (profile %s)" session profile
    | Some session, None -> session
    | None, Some profile -> Printf.sprintf "(session labelled %s, profile %s)" profile profile
    | None, None -> "(monty-owned session)"
  in
  String.concat "\n"
    ([ "SETTING        VALUE";
      "-------------  -----";
      "harness       " ^ harness;
      "codex-yolo    " ^ if settings.codex_yolo then "true" else "false";
      "branch-prefix "
      ^ Option.value ~default:"monty" settings.branch_prefix;
      "agent-profile "
      ^ Option.value ~default:Agent_profile.default_id settings.agent_profile;
      "terminal      "
      ^ Terminal.backend_to_string
          (Option.value ~default:Terminal.Rex settings.terminal);
      "rex-session   " ^ rex_session ]
    @ List.map (fun (profile, session) -> "rex-session." ^ profile ^ " " ^ session) settings.rex_sessions)
  ^ "\n"

let set_context_enabled ~home context_enabled =
  State_store.with_lock ~home (fun () ->
      let ( let* ) = Result.bind in
      let* settings = load ~home in
      State_store.write_json_atomic ~path:(path ~home)
        (to_json { settings with context_enabled }))
