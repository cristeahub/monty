(* Monty's agent integration, built the way Rex builds its own: a stub skill
   that defers to the installed CLI, a SessionStart hook that tells an agent
   which Monty session it is in, and lifecycle commands (status, setup, remove,
   show). Every installed file carries a Monty-managed marker, so later runs
   update, and only, what Monty installed. *)

let ( let* ) = Result.bind

(* ---- Where each harness keeps its skills and hooks ---------------------- *)

let harnesses = [ Harness.Claude; Harness.Codex; Harness.Pi ]

let display_name = function
  | Harness.Claude -> "Claude Code"
  | Harness.Codex -> "Codex"
  | Harness.Pi -> "Pi"

let agent_directory ?(getenv = Sys.getenv_opt) harness =
  let variable = Harness.account_variable harness in
  match getenv variable with
  | Some path when String.trim path <> "" -> Ok (Shell.abs_path path)
  | _ -> (
      match Harness.default_account_directory ~getenv harness with
      | Some directory -> Ok directory
      | None -> Error ("set " ^ variable ^ " or HOME to install the Monty skill"))

(* As in Rex, an agent counts as present when its account directory exists or
   its program is on PATH. *)
let detected ?getenv harness =
  (match agent_directory ?getenv harness with
   | Ok directory -> Sys.file_exists directory && Sys.is_directory directory
   | Error _ -> false)
  || Process.command_exists (Harness.to_string harness) <> None

(* ---- Managed files ------------------------------------------------------ *)

(* The skill's first frontmatter line and the manual's first line mark a file
   as Monty's; anything else there is someone else's file and is left alone. *)
let skill_marker = "---\n# Managed by Monty."
let manual_marker = "<!-- Managed by Monty. -->"

let skill_files =
  [ ("SKILL.md", skill_marker, String.trim Embedded_skill.text ^ "\n");
    ("references/workflows.md", manual_marker, manual_marker ^ "\n" ^ Embedded_manual.text) ]

let skill_root directory = Filename.concat directory "skills"
let skill_directory directory = Filename.concat (skill_root directory) "monty"
let lock_path directory = Filename.concat (skill_root directory) ".monty.lock"

(* The guide an agent reads: the embedded manual, exactly what the installed
   references/workflows.md holds. *)
let guide = Embedded_manual.text

let hook_file harness directory =
  match harness with
  | Harness.Claude -> Some (Filename.concat directory "settings.json")
  | Harness.Codex -> Some (Filename.concat directory "hooks.json")
  | Harness.Pi -> None

(* The hook runs the monty on PATH, so upgrading Monty updates what the agent is
   told without another setup. It never fails the session. *)
let hook_signature = "monty __internal session-start"

let hook_command harness =
  Printf.sprintf "/bin/sh -c '%s --harness %s 2>/dev/null || true'" hook_signature
    (Harness.to_string harness)

let hook_entry harness =
  `Assoc
    [ ( "hooks",
        `List
          [ `Assoc
              [ ("type", `String "command");
                ("command", `String (hook_command harness));
                ("timeout", `Int 5) ] ] ) ]

(* ---- States ------------------------------------------------------------- *)

type state = Current | Outdated | Missing | Foreign | Unreadable of string | Unsupported

let state_to_string = function
  | Current -> "current"
  | Outdated -> "outdated"
  | Missing -> "missing"
  | Foreign -> "foreign"
  | Unreadable _ -> "unreadable"
  | Unsupported -> "unsupported"

let regular_file path =
  match State_store.lstat path with
  | Error message -> Error message
  | Ok None -> Ok None
  | Ok (Some { Unix.st_kind = Unix.S_REG; st_size; _ }) when st_size <= 1_048_576 -> (
      match Shell.read_file path with
      | contents -> Ok (Some contents)
      | exception Sys_error message -> Error message)
  | Ok (Some { Unix.st_kind = Unix.S_REG; _ }) -> Error ("file is larger than 1 MiB: " ^ path)
  | Ok (Some { Unix.st_kind = Unix.S_LNK; _ }) -> Error ("unsafe symlink: " ^ path)
  | Ok (Some _) -> Error ("not a regular file: " ^ path)

let file_state path marker contents =
  match regular_file path with
  | Error message -> Unreadable message
  | Ok None -> Missing
  | Ok (Some current) when current = contents -> Current
  | Ok (Some current) when String.starts_with ~prefix:marker current -> Outdated
  | Ok (Some _) -> Foreign

let directory_state path =
  match State_store.lstat path with
  | Error message -> Some (Unreadable message)
  | Ok None -> Some Missing
  | Ok (Some { Unix.st_kind = Unix.S_DIR; _ }) -> None
  | Ok (Some { Unix.st_kind = Unix.S_LNK; _ }) -> Some (Unreadable ("unsafe symlink: " ^ path))
  | Ok (Some _) -> Some (Unreadable ("not a directory: " ^ path))

(* One state for the skill directory: an unreadable or foreign file wins, a
   missing SKILL.md means missing, any other missing or stale file means
   outdated. *)
let skill_state directory =
  let target = skill_directory directory in
  match directory_state target with
  | Some state -> state
  | None ->
      let states =
        List.map
          (fun (name, marker, contents) -> (name, file_state (Filename.concat target name) marker contents))
          skill_files
      in
      let find predicate = List.find_opt (fun (_, state) -> predicate state) states in
      if Option.is_some (find (function Unreadable _ -> true | _ -> false)) then
        snd (Option.get (find (function Unreadable _ -> true | _ -> false)))
      else if Option.is_some (find (( = ) Foreign)) then Foreign
      else if List.assoc "SKILL.md" states = Missing then Missing
      else if List.for_all (fun (_, state) -> state = Current) states then Current
      else Outdated

(* ---- Hook files --------------------------------------------------------- *)

let our_hook = function
  | `Assoc fields -> (
      match List.assoc_opt "command" fields with
      | Some (`String command) -> Home.contains_substring command hook_signature
      | _ -> false)
  | _ -> false

let group_hooks = function
  | `Assoc fields -> ( match List.assoc_opt "hooks" fields with Some (`List hooks) -> hooks | _ -> [])
  | _ -> []

let contains_ours group = List.exists our_hook (group_hooks group)

(* Removes Monty's hook from every SessionStart group and drops groups that held
   nothing else; other people's hooks keep their places. *)
let strip groups =
  List.filter_map
    (fun group ->
      if not (contains_ours group) then Some group
      else
        match group with
        | `Assoc fields -> (
            match List.filter (fun hook -> not (our_hook hook)) (group_hooks group) with
            | [] -> None
            | hooks -> Some (`Assoc (List.map (fun (key, value) -> if key = "hooks" then (key, `List hooks) else (key, value)) fields)))
        | _ -> Some group)
    groups

(* Replaces a key in place, or appends it, keeping the file's own order. *)
let set_field key value = function
  | `Assoc fields when List.mem_assoc key fields ->
      `Assoc (List.map (fun (k, v) -> if k = key then (k, value) else (k, v)) fields)
  | `Assoc fields -> `Assoc (fields @ [ (key, value) ])
  | _ -> `Assoc [ (key, value) ]

let remove_field key = function `Assoc fields -> `Assoc (List.remove_assoc key fields) | json -> json

let render_json json = Yojson.Safe.pretty_to_string ~std:true json ^ "\n"

type hook_file = { json : Yojson.Safe.t; hooks : Yojson.Safe.t; groups : Yojson.Safe.t list }

let read_hook_file path =
  match regular_file path with
  | Error message -> Error message
  | Ok None -> Ok { json = `Assoc []; hooks = `Assoc []; groups = [] }
  | Ok (Some text) -> (
      match Yojson.Safe.from_string text with
      | exception Yojson.Json_error message -> Error (Printf.sprintf "invalid JSON in %s: %s" path message)
      | `Assoc _ as json -> (
          match Yojson.Safe.Util.member "hooks" json with
          | `Null -> Ok { json; hooks = `Assoc []; groups = [] }
          | `Assoc _ as hooks -> (
              match Yojson.Safe.Util.member "SessionStart" hooks with
              | `Null -> Ok { json; hooks; groups = [] }
              | `List groups -> Ok { json; hooks; groups }
              | _ -> Error (Printf.sprintf "%s: hooks.SessionStart is not an array" path))
          | _ -> Error (Printf.sprintf "%s: hooks is not an object" path))
      | _ -> Error (Printf.sprintf "%s: not a JSON object" path))

let hook_state harness directory =
  match hook_file harness directory with
  | None -> Unsupported
  | Some path -> (
      match State_store.lstat path with
      | Ok None -> Missing
      | _ -> (
          match read_hook_file path with
          | Error message -> Unreadable message
          | Ok file ->
              if not (List.exists contains_ours file.groups) then Missing
              else if file.groups = strip file.groups @ [ hook_entry harness ] then Current
              else Outdated))

let installed_hook_text harness file =
  render_json (set_field "hooks" (set_field "SessionStart" (`List (strip file.groups @ [ hook_entry harness ])) file.hooks) file.json)

let removed_hook_text file =
  let hooks =
    match strip file.groups with
    | [] -> remove_field "SessionStart" file.hooks
    | groups -> set_field "SessionStart" (`List groups) file.hooks
  in
  render_json (match hooks with `Assoc [] -> remove_field "hooks" file.json | hooks -> set_field "hooks" hooks file.json)

(* ---- Targets: every account directory Monty may launch into -------------- *)

type target = {
  label : string;
  harness : Harness.t;
  directory : string;
  detected : bool;
  profile : string option;  (* a configured service profile, else the harness's own account *)
}

let account_key = Harness.account_field

(* The profiles the service launches into: the saved configuration when one was
   selected, else the file Monty generates. Unreadable configuration yields no
   profiles rather than failing a read-only status. *)
let profile_targets ~home =
  let state = Filename.concat (Filename.concat home ".monty") "client-service.json" in
  let saved =
    match State_store.read_json ~path:state with
    | Ok (Some json) -> (
        match Yojson.Safe.Util.member "config_path" json with `String path when path <> "" -> Some path | _ -> None)
    | _ -> None
  in
  let path = match saved with Some path -> path | None -> Filename.concat (Filename.concat home ".monty") "service-profiles.json" in
  match State_store.read_json ~path with
  | Ok (Some json) ->
      (match Yojson.Safe.Util.member "profiles" json with `List profiles -> profiles | _ -> [])
      |> List.filter_map (fun profile ->
             let member name = match Yojson.Safe.Util.member name profile with `String value -> Some value | _ -> None in
             match (member "id", Option.map Harness.of_string (member "harness")) with
             | Some id, Some (Ok harness) ->
                 Option.map
                   (fun directory ->
                     { label = Printf.sprintf "%s (service profile %s)" (display_name harness) id; harness;
                       directory = Shell.normalize (Shell.abs_path directory);
                       detected = Sys.file_exists directory && Sys.is_directory directory; profile = Some id })
                   (member (account_key harness))
             | _ -> None)
  | _ -> []

let same_directory left right =
  let canonical path = match State_path.canonicalize path with Ok path -> path | Error _ -> path in
  canonical left = canonical right

let targets ?getenv ~home ~harnesses:selected () =
  let accounts =
    List.filter_map
      (fun harness ->
        match agent_directory ?getenv harness with
        | Ok directory ->
            Some
              { label = display_name harness; harness; directory = Shell.normalize directory;
                detected = detected ?getenv harness; profile = None }
        | Error _ -> None)
      selected
  in
  let profiles =
    profile_targets ~home
    |> List.filter (fun target ->
           List.mem target.harness selected
           && not (List.exists (fun account -> same_directory account.directory target.directory) accounts))
  in
  accounts @ profiles

(* ---- Install, inspect and remove one account directory ------------------ *)

type change = Created | Updated | Unchanged | Removed | Conflict | Kept

let change_to_string = function
  | Created -> "created"
  | Updated -> "updated"
  | Unchanged -> "unchanged"
  | Removed -> "removed"
  | Conflict -> "conflict"
  | Kept -> "kept"

type item = { kind : string; path : string option; state : state; change : change option }

(* Writes are serialized under the home lock and a lock in the skills root,
   because several Monty homes may install into the same account. *)
let with_account_lock ~home ~directory f =
  State_store.protect_result ~action:"update Monty skill" ~path:directory (fun () ->
      let* directory = State_path.canonicalize directory in
      State_store.with_lock ~home (fun () ->
          let* () = State_store.ensure_real_directory ~label:"harness skills" ~mode:0o700 (skill_root directory) in
          State_store.with_file_lock ~path:(lock_path directory) (fun () -> f directory)))

let write_skill directory =
  let target = skill_directory directory in
  let* () = State_store.ensure_real_directory ~label:"Monty skill" ~mode:0o700 target in
  let* () =
    State_store.ensure_real_directory ~label:"Monty skill references" ~mode:0o700 (Filename.concat target "references")
  in
  List.fold_left
    (fun result (name, marker, contents) ->
      let* () = result in
      let path = Filename.concat target name in
      match file_state path marker contents with
      | Current -> Ok ()
      | Missing | Outdated | Foreign -> State_store.write_file_atomic ~path ~perm:0o600 contents
      | Unreadable message -> Error ("refusing unsafe Monty skill file: " ^ message)
      | Unsupported -> Ok ())
    (Ok ()) skill_files

let write_hook harness directory =
  match hook_file harness directory with
  | None -> Ok ()
  | Some path ->
      let* file = read_hook_file path in
      let text = installed_hook_text harness file in
      if Sys.file_exists path && Shell.read_file path = text then Ok ()
      else State_store.write_file_atomic ~path ~perm:0o600 text

(* Plans one account directory's changes from its current states. *)
let plan ~force ~action harness directory =
  let skill = skill_state directory and hook = hook_state harness directory in
  let skill_change =
    match (action, skill) with
    | `Setup, Current -> Unchanged
    | `Setup, Missing -> Created
    | `Setup, Outdated -> Updated
    | `Setup, Foreign -> if force then Updated else Conflict
    | `Setup, (Unreadable _ | Unsupported) -> Conflict
    | `Remove, (Current | Outdated) -> Removed
    | `Remove, Missing -> Unchanged
    | `Remove, Foreign -> Kept
    | `Remove, (Unreadable _ | Unsupported) -> Conflict
  in
  let hook_change =
    match (action, hook) with
    | _, Unsupported -> Unchanged
    | `Setup, Current -> Unchanged
    | `Setup, Missing -> Created
    | `Setup, (Outdated | Foreign) -> Updated
    | `Setup, Unreadable _ -> Conflict
    | `Remove, (Current | Outdated | Foreign) -> Removed
    | `Remove, Missing -> Unchanged
    | `Remove, Unreadable _ -> Conflict
  in
  [ { kind = "skill"; path = Some (skill_directory directory); state = skill; change = Some skill_change };
    { kind = "hook"; path = hook_file harness directory; state = hook; change = Some hook_change } ]

let inspect harness directory =
  List.map (fun item -> { item with change = None }) (plan ~force:false ~action:`Setup harness directory)

let remove_skill directory =
  let target = skill_directory directory in
  let* () =
    List.fold_left
      (fun result (name, _, _) ->
        let* () = result in
        let path = Filename.concat target name in
        match State_store.lstat path with
        | Ok (Some { Unix.st_kind = Unix.S_REG; _ }) -> (try Ok (Unix.unlink path) with Unix.Unix_error (err, _, _) -> Error (Unix.error_message err))
        | Ok _ -> Ok ()
        | Error message -> Error message)
      (Ok ()) skill_files
  in
  let rmdir path = try Unix.rmdir path with Unix.Unix_error _ -> () in
  rmdir (Filename.concat target "references");
  rmdir target;
  Ok ()

let remove_hook harness directory =
  match hook_file harness directory with
  | None -> Ok ()
  | Some path -> (
      match State_store.lstat path with
      | Ok None -> Ok ()
      | _ ->
          let* file = read_hook_file path in
          let text = removed_hook_text file in
          if Shell.read_file path = text then Ok () else State_store.write_file_atomic ~path ~perm:0o600 text)

(* Applies a plan. A conflict is reported, never written over, and leaves
   nothing else undone: every other item of the directory is still applied. *)
let apply ~home ~action harness directory items =
  if List.for_all (fun item -> item.change = Some Unchanged || item.change = Some Kept) items then Ok ()
  else
    with_account_lock ~home ~directory (fun directory ->
        List.fold_left
          (fun result item ->
            let* () = result in
            match (item.kind, item.change, action) with
            | _, (Some Unchanged | Some Kept | Some Conflict | None), _ -> Ok ()
            | "skill", _, `Setup -> write_skill directory
            | "skill", _, `Remove -> remove_skill directory
            | "hook", _, `Setup -> write_hook harness directory
            | "hook", _, `Remove -> remove_hook harness directory
            | _ -> Ok ())
          (Ok ()) items)

(* ---- Launch-time installation ------------------------------------------- *)

(* Installs or refreshes the skill and hook in one account directory before a
   harness starts there. A foreign or unreadable file is an error: Monty never
   overwrites what it did not install. *)
let ensure_at ~home ~harness ~directory =
  let items = plan ~force:false ~action:`Setup harness directory in
  let* () =
    List.fold_left
      (fun result item ->
        let* () = result in
        match (item.change, item.state) with
        | Some Conflict, Unreadable message -> Error ("refusing unsafe Monty " ^ item.kind ^ " file: " ^ message)
        | Some Conflict, _ ->
            Error ("Monty will not overwrite an unmanaged " ^ item.kind ^ " file: " ^ Option.value ~default:directory item.path)
        | _ -> Ok ())
      (Ok ()) items
  in
  apply ~home ~action:`Setup harness directory items

let ensure ~home ~harness =
  let* directory = agent_directory harness in
  ensure_at ~home ~harness ~directory

(* ---- Reporting ---------------------------------------------------------- *)

let abbreviate path =
  match Sys.getenv_opt "HOME" with
  | Some home when String.trim home <> "" && String.starts_with ~prefix:(home ^ "/") path ->
      "~" ^ String.sub path (String.length home) (String.length path - String.length home)
  | _ -> path

let item_json item =
  `Assoc
    ([ ("kind", `String item.kind) ]
    @ (match item.path with Some path -> [ ("path", `String path) ] | None -> [])
    @ [ ("state", `String (state_to_string item.state)) ]
    @ (match item.state with Unreadable message -> [ ("error", `String message) ] | _ -> [])
    @ match item.change with Some change -> [ ("change", `String (change_to_string change)) ] | None -> [])

let target_json target items =
  `Assoc
    [ ("id", `String (Harness.to_string target.harness));
      ("label", `String target.label);
      ("profile", match target.profile with Some id -> `String id | None -> `Null);
      ("directory", `String target.directory);
      ("detected", `Bool target.detected);
      ("items", `List (List.map item_json items)) ]

let render_target ~show_change target items =
  let header = Printf.sprintf "%s (%s)" target.label (if target.detected then "found" else "not found") in
  let rows =
    List.map
      (fun item ->
        let value =
          match (show_change, item.change, item.state) with
          | _, _, Unsupported -> "unsupported"
          | true, Some change, _ -> change_to_string change
          | _ -> state_to_string item.state
        in
        let path = match item.path with Some path -> "  " ^ abbreviate path | None -> "" in
        let detail = match item.state with Unreadable message -> "  (" ^ message ^ ")" | _ -> "" in
        String.trim (Printf.sprintf "%-5s  %-11s%s%s" item.kind value path detail) |> ( ^ ) "  ")
      items
  in
  String.concat "\n" (header :: rows)

let notes ~action targets_items =
  let changed harness =
    List.exists
      (fun (target, items) ->
        target.harness = harness
        && List.exists (fun item -> item.kind = "hook" && (item.change = Some Created || item.change = Some Updated || item.change = Some Removed)) items)
      targets_items
  in
  (if action = `Setup && changed Harness.Codex then [ "Approve the hook in Codex with /hooks; Codex skips it until then." ] else [])
  @ (if action = `Setup && changed Harness.Claude then [ "Claude Code runs hooks once you trust the folder a session starts in." ] else [])
  @ (if action = `Remove && changed Harness.Codex then [ "Codex may ask you to approve again the hooks that followed Monty's in hooks.json." ] else [])
  @
  if List.exists (fun (_, items) -> List.exists (fun item -> item.change = Some Conflict) items) targets_items then
    [ (if action = `Setup then "A conflict is a skill Monty did not install or a file Monty cannot read; pass --force to replace a foreign skill."
       else "A conflict is a file Monty cannot read safely; repair it and retry.") ]
  else []

(* ---- Commands ----------------------------------------------------------- *)

let print_json json = print_string (Yojson.Safe.pretty_to_string ~std:true json ^ "\n")

let no_agent = "No agent found. Name one with --harness, or set HOME, CLAUDE_CONFIG_DIR, CODEX_HOME or PI_CODING_AGENT_DIR."

let run_status ~getenv ~home ~harnesses:selected ~json =
  let targets = targets ~getenv ~home ~harnesses:selected () in
  let reported = List.map (fun target -> (target, inspect target.harness target.directory)) targets in
  if json then print_json (`Assoc [ ("action", `String "status"); ("targets", `List (List.map (fun (t, items) -> target_json t items) reported)) ])
  else if reported = [] then print_string (no_agent ^ "\n")
  else (
    print_string (String.concat "\n\n" (List.map (fun (t, items) -> render_target ~show_change:false t items) reported));
    print_string "\n\nRun \"monty skill setup\" to install or update the skill.\n");
  if reported = [] then 1 else 0

let run_change ~action ~getenv ~home ~harnesses:selected ~explicit ~dry_run ~force ~json =
  let targets =
    targets ~getenv ~home ~harnesses:selected ()
    |> List.filter (fun target -> explicit || target.detected)
  in
  let planned = List.map (fun target -> (target, plan ~force ~action target.harness target.directory)) targets in
  let failures =
    if dry_run then []
    else
      List.filter_map
        (fun (target, items) ->
          match apply ~home ~action target.harness target.directory items with
          | Ok () -> None
          | Error message -> Some (Printf.sprintf "%s: %s" target.label message))
        planned
  in
  let notes = notes ~action planned @ failures in
  let conflicts = List.exists (fun (_, items) -> List.exists (fun item -> item.change = Some Conflict) items) planned in
  if json then
    print_json
      (`Assoc
        [ ("action", `String (match action with `Setup -> "setup" | `Remove -> "remove"));
          ("dry_run", `Bool dry_run);
          ("targets", `List (List.map (fun (t, items) -> target_json t items) planned));
          ("notes", `List (List.map (fun note -> `String note) notes)) ])
  else (
    if planned = [] then print_string (no_agent ^ "\n")
    else print_string (String.concat "\n\n" (List.map (fun (t, items) -> render_target ~show_change:true t items) planned) ^ "\n");
    if notes <> [] then print_string ("\n" ^ String.concat "\n" notes ^ "\n");
    if (not dry_run) && failures = [] && (not conflicts) && planned <> [] then
      print_string
        (if action = `Setup then "\nStart a new agent session inside Monty to use it.\n"
         else "\nMonty's skill and hooks were removed.\n"));
  if failures <> [] || conflicts || planned = [] then 1 else 0

let command ~home_arg ~getenv =
  let open Cmdliner in
  let harness_conv = Arg.conv (Harness.of_string, fun ppf value -> Fmt.pf ppf "%s" (Harness.to_string value)) in
  let harness_arg =
    Arg.(value & opt_all harness_conv [] & info [ "harness" ] ~docv:"HARNESS"
           ~doc:"Use only this harness (claude, codex or pi), whether or not it was found; repeatable.")
  in
  let json_arg = Arg.(value & flag & info [ "json" ] ~doc:"Print the result as JSON.") in
  let dry_run_arg = Arg.(value & flag & info [ "dry-run" ] ~doc:"Report what would change without changing anything.") in
  let force_arg = Arg.(value & flag & info [ "force" ] ~doc:"Replace a skill named monty that Monty did not install.") in
  let selection = function [] -> (harnesses, false) | selected -> (selected, true) in
  let status =
    Cmd.v (Cmd.info "status" ~doc:"Report whether Monty's skill and hook are installed in each agent and service profile.")
      Term.(const (fun home selected json ->
              let harnesses, _ = selection selected in
              run_status ~getenv ~home ~harnesses ~json) $ home_arg $ harness_arg $ json_arg)
  in
  let setup =
    Cmd.v (Cmd.info "setup" ~doc:"Install or update Monty's skill and SessionStart hook in the agents found on this machine, or in the agents named with --harness.")
      Term.(const (fun home selected dry_run force json ->
              let harnesses, explicit = selection selected in
              run_change ~action:`Setup ~getenv ~home ~harnesses ~explicit ~dry_run ~force ~json)
            $ home_arg $ harness_arg $ dry_run_arg $ force_arg $ json_arg)
  in
  let remove =
    Cmd.v (Cmd.info "remove" ~doc:"Remove Monty's skill and hook from agents; other skills, hooks and settings stay as they were.")
      Term.(const (fun home selected dry_run json ->
              let harnesses, explicit = selection selected in
              run_change ~action:`Remove ~getenv ~home ~harnesses ~explicit ~dry_run ~force:false ~json)
            $ home_arg $ harness_arg $ dry_run_arg $ json_arg)
  in
  let show =
    Cmd.v (Cmd.info "show" ~doc:"Print the guide Monty's skill gives agents.")
      Term.(const (fun () -> print_string guide; 0) $ const ())
  in
  Cmd.group
    (Cmd.info "skill"
       ~doc:"Set up agents such as Claude Code, Codex and Pi to use Monty."
       ~man:
         [ `S Manpage.s_description;
           `P "Monty installs two things into each agent. A skill teaches the agent the monty command; it reads its guide from the installed monty, so upgrading Monty updates what agents are told. A hook, in agents that support hooks, tells the agent at the start of each session whether it is a Monty worker or the head butler, and what is pending.";
           `P "Every Monty launch installs both into the account it launches into, so setup is only needed for sessions started outside Monty. Run \"monty skill status\" to see what is installed." ])
    [ status; setup; remove; show ]
