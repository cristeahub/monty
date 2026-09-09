open Yojson.Safe
open Overview_types

let monty_dir home = Filename.concat home ".monty"
let projects_file home = Filename.concat (monty_dir home) "projects.json"
let projects_dir home = Filename.concat (monty_dir home) "projects"
let memory_file ~home id = Filename.concat (projects_dir home) (id ^ ".md")
let project_memory_file = memory_file

let member_string json name =
  match Util.member name json with
  | `String value when String.trim value <> "" -> Ok value
  | _ -> Error (Printf.sprintf "missing string field %S" name)

let optional_string json name =
  match Util.member name json with
  | `Null -> Ok None
  | `String value when String.trim value = "" -> Ok None
  | `String value -> Ok (Some value)
  | _ -> Error (Printf.sprintf "field %S must be a string when present" name)

let list_field json name =
  match Util.member name json with
  | `Null -> Ok []
  | `List values -> Ok values
  | _ -> Error (Printf.sprintf "field %S must be an array" name)

let read_json_file path =
  try Ok (Yojson.Safe.from_file path) with
  | Sys_error msg -> Error msg
  | Yojson.Json_error msg -> Error ("invalid JSON in " ^ path ^ ": " ^ msg)

let fold_results values ~init ~f =
  List.fold_left
    (fun acc value ->
      match acc with Error _ as err -> err | Ok acc -> f acc value)
    (Ok init) values

let parse_source json =
  let ( let* ) = Result.bind in
  let* kind = member_string json "kind" in
  match kind with
  | "github_issues" ->
      let* repo = member_string json "repo" in
      let* query = optional_string json "query" in
      Ok Overview_types.{ repo; query }
  | _ -> Error (Printf.sprintf "unknown project source kind %S" kind)

let json_of_source ({ repo; query } : github_source) =
  let fields =
    [ ("kind", `String "github_issues"); ("repo", `String repo) ]
    @ (match query with None -> [] | Some value -> [ ("query", `String value) ])
  in
  `Assoc fields

let group_name = State_path.safe_component ~label:"project group name"

let check_group groups = function
  | None -> Ok ()
  | Some name ->
      let ( let* ) = Result.bind in
      let* name = group_name name in
      if List.mem name groups then Ok ()
      else Error (Printf.sprintf
          "no Monty project group matching %S; use monty projects groups list or monty projects groups add %s"
          name (Shell.quote name))

let parse_raw_project ~groups json =
  let ( let* ) = Result.bind in
  let* persisted_id = optional_string json "id" in
  let* persisted_id =
    match persisted_id with
    | None -> Ok None
    | Some id ->
        State_path.safe_component ~label:"project id" id
        |> Result.map Option.some
  in
  let* repo = member_string json "repo" in
  let repo = Shell.normalize (Shell.abs_path repo) in
  let* sources_json = list_field json "sources" in
  let* sources = fold_results sources_json ~init:[] ~f:(fun acc json -> parse_source json |> Result.map (fun source -> source :: acc)) in
  let group = Util.member "group" json |> Util.to_string_option in
  let* () = check_group groups group in
  Ok { persisted_id; repo; sources = List.rev sources; group }

let json_of_raw_project (project : raw_project) =
  let fields =
    [ ("repo", `String project.repo);
      ("sources", `List (List.map json_of_source project.sources)) ]
    @ (match project.persisted_id with None -> [] | Some id -> [ ("id", `String id) ])
    @ (match project.group with None -> [] | Some name -> [ ("group", `String name) ])
  in
  `Assoc fields

let repo_basename repo =
  match Filename.basename repo with "" | "." | "/" -> "repo" | name -> name

let base_id (project : raw_project) = Slug.of_title (repo_basename project.repo)

let disambiguated_id (project : raw_project) =
  match project.sources with
  | { repo; _ } :: _ -> Slug.of_title repo
  | [] ->
      let parent = project.repo |> Filename.dirname |> Filename.basename |> Slug.of_title in
      let base = base_id project in
      if parent = "" || parent = base then base else parent ^ "-" ^ base

let unique_ids (projects : raw_project list) : (string * raw_project) list =
  let count_base base =
    projects |> List.filter (fun project -> String.equal (base_id project) base) |> List.length
  in
  let candidate project =
    match project.persisted_id with
    | Some id -> id
    | None ->
        let base = base_id project in
        if count_base base = 1 then base else disambiguated_id project
  in
  let rec unique seen id index =
    let value = if index = 0 then id else id ^ "-" ^ string_of_int (index + 1) in
    if List.exists (String.equal value) seen then unique seen id (index + 1)
    else value
  in
  let rec loop seen acc = function
    | [] -> List.rev acc
    | project :: rest ->
        let id =
          match project.persisted_id with
          | Some id when List.exists (String.equal id) seen ->
              invalid_arg (Printf.sprintf "duplicate persisted Monty project id %S" id)
          | Some id -> id
          | None -> unique seen (candidate project) 0
        in
        loop (id :: seen) ((id, project) :: acc) rest
  in
  loop [] [] projects

let with_ids (projects : raw_project list) : project list =
  unique_ids projects
  |> List.map (fun (id, (project : raw_project)) ->
         { id; repo = project.repo; sources = project.sources; group = project.group })

let duplicate_value values =
  values |> List.sort String.compare
  |> List.find_map (fun value ->
         if List.length (List.filter (String.equal value) values) > 1 then Some value
         else None)

let load_registry ~home =
  State_store.decode_json ~path:(projects_file home) (fun () ->
    let path = projects_file home in
    if not (Sys.file_exists path) then Ok ([], [])
    else
      let ( let* ) = Result.bind in
      let* json = read_json_file path in
      let* groups_json = list_field json "groups" in
      let* groups =
        fold_results groups_json ~init:[] ~f:(fun groups json ->
            let* name = group_name (Util.to_string json) in
            if List.mem name groups then
              Error (Printf.sprintf "duplicate project group %S in %s" name path)
            else Ok (name :: groups))
        |> Result.map List.rev
      in
      let* projects_json = list_field json "projects" in
      let* projects =
        fold_results projects_json ~init:[] ~f:(fun acc json ->
            parse_raw_project ~groups json |> Result.map (fun project -> project :: acc))
        |> Result.map List.rev
      in
      let* () =
        match duplicate_value (List.map (fun (project : raw_project) -> project.repo) projects) with
        | Some repo -> Error (Printf.sprintf "duplicate project repo %S in %s" repo path)
        | None -> Ok ()
      in
      let* () =
        try ignore (unique_ids projects); Ok ()
        with Invalid_argument msg -> Error msg
      in
      Ok (groups, projects))

let save_registry_unlocked ~home groups (projects : raw_project list) =
  let path = projects_file home in
  State_store.write_json_atomic ~path
    (`Assoc
      [ ("groups", `List (List.map (fun name -> `String name) groups));
        ("projects", `List (List.map json_of_raw_project projects)) ])

let load_projects ~home =
  load_registry ~home |> Result.map (fun (_, projects) -> with_ids projects)

let list_projects ~home ?group () =
  let ( let* ) = Result.bind in
  let* groups, projects = load_registry ~home in
  let* () = check_group groups group in
  Ok (with_ids projects |> List.filter (fun (project : project) -> group = None || project.group = group))

let list_groups ~home = load_registry ~home |> Result.map fst

let update_registry ~home change =
  State_store.with_lock ~home (fun () ->
      let ( let* ) = Result.bind in
      let* groups, projects = load_registry ~home in
      let* groups, projects = change groups projects in
      save_registry_unlocked ~home groups projects)

let add_group ~home name =
  update_registry ~home (fun groups projects ->
      let ( let* ) = Result.bind in
      let* name = group_name name in
      if List.mem name groups then
        Error (Printf.sprintf "project group already exists: %s" name)
      else Ok (groups @ [ name ], projects))

let delete_group ~home name =
  update_registry ~home (fun groups projects ->
      let ( let* ) = Result.bind in
      let* () = check_group groups (Some name) in
      let groups = List.filter (fun group -> group <> name) groups in
      let projects =
        List.map (fun (project : raw_project) ->
            if project.group = Some name then { project with group = None }
            else project) projects
      in
      Ok (groups, projects))

let source_label ({ repo; query } : github_source) =
  match query with
  | None -> "github:" ^ repo
  | Some query -> "github:" ^ repo ^ " search:" ^ query

let sources_label sources =
  match sources with [] -> "local" | sources -> sources |> List.map source_label |> String.concat ", "

let compare_project (left : project) (right : project) = String.compare left.id right.id

let resolve_project (projects : project list) needle =
  let needle_slug = Slug.of_title needle in
  let matches =
    projects
    |> List.filter (fun (project : project) ->
           String.equal needle project.id
           || String.equal needle_slug project.id
           || String.equal needle project.repo
           || String.equal needle_slug
                (base_id { persisted_id = None; repo = project.repo;
                           sources = project.sources; group = project.group }))
  in
  match matches with
  | [ project ] -> Ok project
  | [] -> Error (Printf.sprintf "no Monty project matching %S" needle)
  | many ->
      let labels =
        many
        |> List.map (fun (project : project) -> "- " ^ project.id ^ " " ^ project.repo)
        |> String.concat "\n"
      in
      Error (Printf.sprintf "multiple Monty projects match %S:\n%s" needle labels)

let set_project_group ~home ~project group =
  update_registry ~home (fun groups projects ->
      let ( let* ) = Result.bind in
      let* selected = resolve_project (with_ids projects) project in
      let* () = check_group groups group in
      let projects =
        List.map (fun (raw : raw_project) ->
            if String.equal raw.repo selected.repo then { raw with group }
            else raw) projects
      in
      Ok (groups, projects))

let project_memory_template (project : project) =
  String.concat "\n"
    [ "# " ^ project.id;
      "";
      "Repo: " ^ project.repo;
      "Sources: " ^ sources_label project.sources;
      "";
      "## What this project is";
      "";
      "TODO";
      "";
      "## How to work on it";
      "";
      "TODO";
      "";
      "## Current direction";
      "";
      "TODO";
      "" ]

let ensure_project_memory ~home (project : project) =
  let path = project_memory_file ~home project.id in
  if Sys.file_exists path then Ok ()
  else (
    Shell.ensure_dir (Filename.dirname path);
    Shell.write_file path (project_memory_template project);
    Ok ())

let project_id_changes old_projects new_projects =
  new_projects
  |> List.filter_map (fun (project : project) ->
         match
           List.find_opt
             (fun (old : project) -> String.equal old.repo project.repo)
             old_projects
         with
         | Some old when not (String.equal old.id project.id) -> Some (old.id, project.id)
         | _ -> None)

let migrate_project_memory ~home old_projects new_projects =
  let changes = project_id_changes old_projects new_projects in
  let migrate (old_id, new_id) =
    let old_path = project_memory_file ~home old_id in
    let new_path = project_memory_file ~home new_id in
    if Sys.file_exists old_path && not (Sys.file_exists new_path) then
      try
        Shell.ensure_dir (Filename.dirname new_path);
        Unix.rename old_path new_path;
        Ok ()
      with Unix.Unix_error (err, fn, arg) ->
        Error
          (Printf.sprintf "failed to migrate project memory via %s(%s): %s" fn arg
             (Unix.error_message err))
    else Ok ()
  in
  fold_results changes ~init:() ~f:(fun () change -> migrate change)

let add_project ~home ~repo ?github ?query () =
  let repo = Shell.normalize (Shell.abs_path repo) in
  if not (Sys.file_exists repo && Sys.is_directory repo) then
    Error (Printf.sprintf "repo is not an existing directory: %s" repo)
  else
    State_store.with_lock ~home (fun () ->
        let ( let* ) = Result.bind in
        let* groups, projects = load_registry ~home in
        if List.exists (fun (project : raw_project) -> String.equal project.repo repo) projects then
          Error (Printf.sprintf "project already exists for repo: %s" repo)
        else
          let* old_projects =
            try Ok (with_ids projects) with Invalid_argument msg -> Error msg
          in
          let stabilized =
            List.map2
              (fun raw (project : project) -> { raw with persisted_id = Some project.id })
              projects old_projects
          in
          let sources =
            match github with
            | None -> []
            | Some repo -> [ Overview_types.{ repo; query } ]
          in
          let candidates = stabilized @ [ { persisted_id = None; repo; sources; group = None } ] in
          let* assigned =
            try Ok (with_ids candidates) with Invalid_argument msg -> Error msg
          in
          let persisted =
            List.map2
              (fun raw (project : project) -> { raw with persisted_id = Some project.id })
              candidates assigned
          in
          let* () = save_registry_unlocked ~home groups persisted in
          let* projects = load_projects ~home in
          let* () = migrate_project_memory ~home old_projects projects in
          let* project = resolve_project projects repo in
          let* () = ensure_project_memory ~home project in
          Ok project)
