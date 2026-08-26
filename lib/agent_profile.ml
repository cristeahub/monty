open Yojson.Safe

let schema = "monty:agent-profile:v1"
let snapshot_schema = "monty:agent-profile-snapshot:v1"
let default_id = "reviewed"
let snapshot_name = "agent-profile.json"

type review = {
  id : string;
  title : string;
  instructions : string;
}

type t = {
  id : string;
  description : string;
  interactive : string;
  implementation : string;
  reviews : review list;
  fix : string option;
}

let ( let* ) = Result.bind

let string_field json name =
  match Util.member name json with
  | `String value when String.trim value <> "" -> Ok value
  | _ -> Error (Printf.sprintf "agent profile field %S must be a non-empty string" name)

let optional_string_field json name =
  match Util.member name json with
  | `Null -> Ok None
  | `String value when String.trim value <> "" -> Ok (Some value)
  | _ -> Error (Printf.sprintf "agent profile field %S must be a non-empty string when present" name)

let safe_id label value = State_path.safe_component ~label value

let read_regular_file ~profile_dir name =
  let* name = safe_id "agent profile instruction filename" name in
  let path = Filename.concat profile_dir name in
  let* state = State_store.lstat path in
  match state with
  | Some { Unix.st_kind = Unix.S_REG; _ } -> (
      try
        let contents = Shell.read_file path in
        if String.trim contents = "" then
          Error (Printf.sprintf "agent profile instruction file is empty: %s" path)
        else Ok contents
      with Sys_error message -> Error message)
  | Some { Unix.st_kind = Unix.S_LNK; _ } ->
      Error (Printf.sprintf "unsafe agent profile instruction is a symlink: %s" path)
  | Some _ ->
      Error (Printf.sprintf "agent profile instruction is not a regular file: %s" path)
  | None -> Error (Printf.sprintf "agent profile instruction is missing: %s" path)

let parse_review ~instructions json =
  let* id = string_field json "id" in
  let* id = safe_id "agent profile reviewer id" id in
  let* title = string_field json "title" in
  let* value = string_field json "instructions" in
  let* instructions = instructions value in
  Ok { id; title; instructions }

let parse_reviews ~instructions json =
  match Util.member "reviews" json with
  | `List values ->
      let* reviews =
        List.fold_left
          (fun result json ->
            let* reviews = result in
            let* review = parse_review ~instructions json in
            Ok (review :: reviews))
          (Ok []) values
        |> Result.map List.rev
      in
      let ids =
        List.map (fun (review : review) -> review.id) reviews
      in
      if List.length ids <> List.length (List.sort_uniq String.compare ids) then
        Error "agent profile contains duplicate reviewer ids"
      else Ok reviews
  | _ -> Error "agent profile headless field \"reviews\" must be an array"

let parse ~expected_schema ~instructions json =
  let* parsed_schema = string_field json "schema" in
  if parsed_schema <> expected_schema then
    Error (Printf.sprintf "unsupported agent profile schema %S" parsed_schema)
  else
    let* id = string_field json "id" in
    let* id = safe_id "agent profile id" id in
    let* description = string_field json "description" in
    let* interactive_name = string_field json "interactive" in
    let* interactive = instructions interactive_name in
    let headless = Util.member "headless" json in
    let* implementation_name = string_field headless "implementation" in
    let* implementation = instructions implementation_name in
    let* reviews = parse_reviews ~instructions headless in
    let* fix_name = optional_string_field headless "fix" in
    let* fix =
      match fix_name with
      | None -> Ok None
      | Some name -> instructions name |> Result.map Option.some
    in
    if reviews = [] && fix <> None then
      Error "agent profile cannot define a fix stage without reviewers"
    else Ok { id; description; interactive; implementation; reviews; fix }

let load_directory path =
  let* state = State_store.lstat path in
  match state with
  | Some { Unix.st_kind = Unix.S_LNK; _ } ->
      Error (Printf.sprintf "unsafe agent profile directory is a symlink: %s" path)
  | Some { Unix.st_kind = Unix.S_DIR; _ } ->
      let metadata = Filename.concat path "profile.json" in
      let* metadata_state = State_store.lstat metadata in
      (match metadata_state with
      | Some { Unix.st_kind = Unix.S_REG; _ } -> (
          try
            let json = Yojson.Safe.from_file metadata in
            let* profile =
              parse ~expected_schema:schema
                ~instructions:(read_regular_file ~profile_dir:path) json
            in
            let directory_id = Filename.basename path in
            if String.equal profile.id directory_id then Ok profile
            else
              Error
                (Printf.sprintf
                   "agent profile id %S does not match its directory name %S"
                   profile.id directory_id)
          with
          | Sys_error message -> Error message
          | Yojson.Json_error message ->
              Error (Printf.sprintf "invalid JSON in %s: %s" metadata message))
      | Some { Unix.st_kind = Unix.S_LNK; _ } ->
          Error (Printf.sprintf "unsafe agent profile metadata is a symlink: %s" metadata)
      | Some _ ->
          Error (Printf.sprintf "agent profile metadata is not a regular file: %s" metadata)
      | None -> Error (Printf.sprintf "agent profile metadata is missing: %s" metadata))
  | Some _ -> Error (Printf.sprintf "agent profile path is not a directory: %s" path)
  | None -> Error (Printf.sprintf "agent profile directory is missing: %s" path)

let directory_entries path =
  try Ok (Sys.readdir path |> Array.to_list |> List.sort String.compare)
  with Sys_error message -> Error message

let discover_root root =
  let* state = State_store.lstat root in
  match state with
  | None -> Ok []
  | Some { Unix.st_kind = Unix.S_LNK; _ } ->
      Error (Printf.sprintf "unsafe agent profiles directory is a symlink: %s" root)
  | Some { Unix.st_kind = Unix.S_DIR; _ } ->
      let* names = directory_entries root in
      List.fold_left
        (fun result name ->
          let* profiles = result in
          let* _ = safe_id "agent profile directory name" name in
          let* profile = load_directory (Filename.concat root name) in
          Ok (profile :: profiles))
        (Ok []) names
      |> Result.map List.rev
  | Some _ -> Error (Printf.sprintf "agent profiles path is not a directory: %s" root)

let roots ~home =
  [ Filename.concat home "agent-profiles";
    Filename.concat (Filename.concat home ".monty") "agent-profiles" ]

let discover ~home =
  let* home = State_path.canonicalize home in
  let* profiles =
    List.fold_left
      (fun result root ->
        let* profiles = result in
        let* found = discover_root root in
        Ok (profiles @ found))
      (Ok []) (roots ~home)
  in
  let rec reject_duplicates seen = function
    | [] -> Ok profiles
    | profile :: rest ->
        if List.mem profile.id seen then
          Error (Printf.sprintf "duplicate agent profile id %S" profile.id)
        else reject_duplicates (profile.id :: seen) rest
  in
  reject_duplicates [] profiles

let find ~home id =
  let* id = safe_id "agent profile id" id in
  let* profiles = discover ~home in
  match List.find_opt (fun profile -> String.equal profile.id id) profiles with
  | Some profile -> Ok profile
  | None -> Error (Printf.sprintf "unknown agent profile %S" id)

let snapshot_path worker_dir = Filename.concat worker_dir snapshot_name

let review_json (review : review) =
  `Assoc
    [ ("id", `String review.id);
      ("title", `String review.title);
      ("instructions", `String review.instructions) ]

let to_snapshot_json profile =
  `Assoc
    [ ("schema", `String snapshot_schema);
      ("id", `String profile.id);
      ("description", `String profile.description);
      ("interactive", `String profile.interactive);
      ( "headless",
        `Assoc
          [ ("implementation", `String profile.implementation);
            ("reviews", `List (List.map review_json profile.reviews));
            ("fix", Option.fold ~none:`Null ~some:(fun value -> `String value) profile.fix) ] ) ]

let write_snapshot ~worker_dir profile =
  State_store.write_json_atomic ~path:(snapshot_path worker_dir)
    (to_snapshot_json profile)

let load_snapshot worker_dir =
  let path = snapshot_path worker_dir in
  let* state = State_store.lstat path in
  match state with
  | Some { Unix.st_kind = Unix.S_REG; _ } -> (
      try
        let json = Yojson.Safe.from_file path in
        parse ~expected_schema:snapshot_schema
          ~instructions:(fun value -> Ok value) json
      with
      | Sys_error message -> Error message
      | Yojson.Json_error message ->
          Error (Printf.sprintf "invalid JSON in %s: %s" path message))
  | Some { Unix.st_kind = Unix.S_LNK; _ } ->
      Error (Printf.sprintf "unsafe agent profile snapshot is a symlink: %s" path)
  | Some _ -> Error (Printf.sprintf "agent profile snapshot is not a regular file: %s" path)
  | None -> Error (Printf.sprintf "agent profile snapshot is missing: %s" path)

let load_pinned ~home ~worker_dir selected_id =
  match State_store.lstat (snapshot_path worker_dir) with
  | Error _ as error -> error
  | Ok (Some _) ->
      let* profile = load_snapshot worker_dir in
      (match selected_id with
      | None -> Ok profile
      | Some id when String.equal id profile.id -> Ok profile
      | Some id ->
          Error
            (Printf.sprintf
               "worker agent profile %S does not match pinned snapshot %S"
               id profile.id))
  | Ok None -> (
      match selected_id with
      | None -> find ~home default_id
      | Some id ->
          Error
            (Printf.sprintf
               "worker selects agent profile %S but its pinned snapshot is missing: %s"
               id (snapshot_path worker_dir)))

let stage_summary profile =
  let implementation = "implementation" in
  match (profile.reviews, profile.fix) with
  | [], None -> implementation
  | reviews, fix ->
      let reviews =
        Printf.sprintf "%d parallel review%s" (List.length reviews)
          (if List.length reviews = 1 then "" else "s")
      in
      String.concat " -> "
        ([ implementation; reviews ]
        @ (if fix = None then [] else [ "fix" ]))

let render profile = Yojson.Safe.pretty_to_string (to_snapshot_json profile) ^ "\n"
