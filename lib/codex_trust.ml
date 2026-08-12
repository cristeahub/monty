let ( let* ) = Result.bind

let error_of_unix action path err fn arg =
  Printf.sprintf "%s %s failed via %s(%s): %s" action path fn arg
    (Unix.error_message err)

let canonical_directory ~label path =
  let path = Shell.normalize (Shell.abs_path path) in
  try
    if Sys.file_exists path && Sys.is_directory path then Ok (Unix.realpath path)
    else Error (Printf.sprintf "%s is not an existing directory: %s" label path)
  with Unix.Unix_error (err, fn, arg) ->
    Error (error_of_unix ("inspect " ^ label) path err fn arg)

let toml_basic_string value =
  let buffer = Buffer.create (String.length value + 8) in
  Buffer.add_char buffer '"';
  String.iter
    (fun character ->
      match character with
      | '"' -> Buffer.add_string buffer "\\\""
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '\b' -> Buffer.add_string buffer "\\b"
      | '\t' -> Buffer.add_string buffer "\\t"
      | '\n' -> Buffer.add_string buffer "\\n"
      | '\012' -> Buffer.add_string buffer "\\f"
      | '\r' -> Buffer.add_string buffer "\\r"
      | character when Char.code character < 0x20 || Char.code character = 0x7f ->
          Buffer.add_string buffer (Printf.sprintf "\\u%04X" (Char.code character))
      | character -> Buffer.add_char buffer character)
    value;
  Buffer.add_char buffer '"';
  Buffer.contents buffer

let project_header path = "[projects." ^ toml_basic_string path ^ "]"
let trusted_assignment = "trust_level = \"trusted\""

let leading_whitespace line =
  let rec loop index =
    if index < String.length line && (line.[index] = ' ' || line.[index] = '\t')
    then loop (index + 1)
    else String.sub line 0 index
  in
  loop 0

let is_table_header line =
  let line = String.trim line in
  String.length line >= 2 && line.[0] = '['

let is_trust_assignment line =
  let line = String.trim line in
  if line = "" || line.[0] = '#' then false
  else
    match String.index_opt line '=' with
    | None -> false
    | Some index ->
        String.sub line 0 index |> String.trim |> String.equal "trust_level"

let rec find_index predicate index = function
  | [] -> None
  | value :: rest ->
      if predicate value then Some index else find_index predicate (index + 1) rest

let rec take_while predicate = function
  | value :: rest when predicate value -> value :: take_while predicate rest
  | _ -> []

let replace_at index replacement values =
  values
  |> List.mapi (fun current value ->
         if current = index then replacement else value)

let insert_at index value values =
  let rec loop current before = function
    | rest when current = index -> List.rev_append before (value :: rest)
    | [] -> List.rev (value :: before)
    | item :: rest -> loop (current + 1) (item :: before) rest
  in
  loop 0 [] values

let append_project contents header =
  let separator =
    if contents = "" then ""
    else if String.ends_with ~suffix:"\n\n" contents then ""
    else if String.ends_with ~suffix:"\n" contents then "\n"
    else "\n\n"
  in
  contents ^ separator ^ header ^ "\n" ^ trusted_assignment ^ "\n"

let updated_contents ~path contents =
  let header = project_header path in
  let lines = String.split_on_char '\n' contents in
  match find_index (fun line -> String.equal (String.trim line) header) 0 lines with
  | None -> Ok (append_project contents header)
  | Some header_index ->
      let section_lines =
        lines |> List.mapi (fun index line -> (index, line))
        |> List.filter (fun (index, _) -> index > header_index)
        |> take_while (fun (_, line) -> not (is_table_header line))
      in
      let assignments =
        section_lines
        |> List.filter (fun (_, line) -> is_trust_assignment line)
      in
      (match assignments with
      | [] ->
          Ok
            (insert_at (header_index + 1) trusted_assignment lines
            |> String.concat "\n")
      | [ (_, line) ] when String.equal (String.trim line) trusted_assignment ->
          Ok contents
      | [ (index, line) ] ->
          let replacement = leading_whitespace line ^ trusted_assignment in
          Ok (replace_at index replacement lines |> String.concat "\n")
      | _ ->
          Error
            (Printf.sprintf
               "Codex project section %s contains duplicate trust_level assignments"
               header))

let writable_config_path path =
  let existing =
    try Ok (Some (Unix.lstat path)) with
    | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> Ok None
    | Unix.Unix_error (err, fn, arg) ->
        Error (error_of_unix "inspect Codex config" path err fn arg)
  in
  let* existing = existing in
  match existing with
  | None -> Ok path
  | Some { Unix.st_kind = Unix.S_REG; _ } -> Ok path
  | Some { Unix.st_kind = Unix.S_LNK; _ } -> (
      try
        let target = Unix.realpath path in
        if (Unix.stat target).Unix.st_kind = Unix.S_REG then Ok target
        else
          Error
            (Printf.sprintf
               "Codex config symlink target is not a regular file: %s" target)
      with Unix.Unix_error (err, fn, arg) ->
        Error (error_of_unix "resolve Codex config symlink" path err fn arg))
  | Some _ -> Error (Printf.sprintf "Codex config is not a regular file: %s" path)

let ensure_file ~config_path ~path =
  let* path = canonical_directory ~label:"Codex project" path in
  let* config_path = writable_config_path config_path in
  let contents =
    if Sys.file_exists config_path then
      try Ok (Shell.read_file config_path)
      with Sys_error message ->
        Error (Printf.sprintf "read Codex config %s failed: %s" config_path message)
    else Ok ""
  in
  let* contents = contents in
  let* updated = updated_contents ~path contents in
  if String.equal contents updated then Ok ()
  else
    let permissions =
      try (Unix.stat config_path).Unix.st_perm land 0o777
      with Unix.Unix_error _ -> 0o600
    in
    State_store.write_file_atomic ~path:config_path ~perm:permissions updated

let configured_home ~getenv =
  match getenv "CODEX_HOME" with
  | Some path when String.trim path <> "" -> Ok path
  | _ -> (
      match getenv "HOME" with
      | Some path when String.trim path <> "" -> Ok (Filename.concat path ".codex")
      | _ -> Error "cannot trust the Codex project because neither CODEX_HOME nor HOME is set")

let ensure_with_getenv ~getenv ~home ~path =
  let* path = canonical_directory ~label:"Codex project" path in
  let* configured_home = configured_home ~getenv in
  State_store.with_lock ~home (fun () ->
      let* codex_home =
        let configured_home = Shell.normalize (Shell.abs_path configured_home) in
        if Sys.file_exists configured_home then
          canonical_directory ~label:"Codex home" configured_home
        else
          let* () =
            State_store.ensure_real_directory ~label:"Codex home" ~mode:0o700
              configured_home
          in
          canonical_directory ~label:"Codex home" configured_home
      in
      ensure_file ~config_path:(Filename.concat codex_home "config.toml") ~path)

let ensure ~home ~path = ensure_with_getenv ~getenv:Sys.getenv_opt ~home ~path
