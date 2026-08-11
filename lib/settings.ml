type t = {
  harness : Harness.t option;
  codex_yolo : bool;
}

let empty = { harness = None; codex_yolo = false }
let path ~home = Filename.concat (Filename.concat home ".monty") "settings.json"

let parse json =
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
  let* codex_yolo =
    match member "codex_yolo" json with
    | `Null -> Ok false
    | `Bool value -> Ok value
    | _ -> Error "settings field \"codex_yolo\" must be a boolean"
  in
  Ok { harness; codex_yolo }

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
      ("codex_yolo", `Bool settings.codex_yolo) ]

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

let render settings =
  let harness =
    settings.harness |> Option.value ~default:Harness.Pi |> Harness.to_string
  in
  String.concat "\n"
    [ "SETTING    VALUE";
      "---------  -----";
      "harness    " ^ harness;
      "codex-yolo " ^ if settings.codex_yolo then "true" else "false" ]
  ^ "\n"
