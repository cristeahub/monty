(* Stages: named, user-written instructions for what a worker does after its
   implementation is finished, such as committing and opening a draft pull
   request. A stage is one Markdown file, `<id>.md`. The user starts a stage by
   naming it in the worker's session; the agent then reads it with
   `monty stage show ID` and carries it out itself. Monty only stores and
   presents stages: it never runs one and keeps no state about them. *)

let ( let* ) = Result.bind

type t = {
  id : string;
  title : string;
  path : string;
  text : string;
}

(* Stages shipped with the control room, then the user's own. A stage in a
   later root replaces the one with the same id from an earlier root, so a
   private `.monty/stages/ship.md` overrides the shipped `ship`. *)
let roots ~home =
  [ Filename.concat home "stages";
    Filename.concat (Filename.concat home ".monty") "stages" ]

(* The first line that says something, without its Markdown heading marks. *)
let title_of text =
  String.split_on_char '\n' text
  |> List.find_map (fun line ->
         let line = String.trim line in
         let length = String.length line in
         let rec skip index =
           if index < length && (line.[index] = '#' || line.[index] = ' ') then skip (index + 1)
           else index
         in
         let start = skip 0 in
         if start >= length then None else Some (String.sub line start (length - start)))

let load ~id path =
  let* state = State_store.lstat path in
  match state with
  | Some { Unix.st_kind = Unix.S_REG; _ } -> (
      match Shell.read_file path with
      | exception Sys_error message -> Error message
      | text -> (
          match title_of text with
          | Some title -> Ok { id; title; path; text }
          | None -> Error (Printf.sprintf "stage file is empty: %s" path)))
  | Some { Unix.st_kind = Unix.S_LNK; _ } ->
      Error (Printf.sprintf "unsafe stage file is a symlink: %s" path)
  | _ -> Error (Printf.sprintf "stage is not a regular file: %s" path)

let discover_root root =
  let* state = State_store.lstat root in
  match state with
  | None -> Ok []
  | Some { Unix.st_kind = Unix.S_LNK; _ } ->
      Error (Printf.sprintf "unsafe stages directory is a symlink: %s" root)
  | Some { Unix.st_kind = Unix.S_DIR; _ } ->
      let* names =
        try Ok (Sys.readdir root |> Array.to_list |> List.sort String.compare)
        with Sys_error message -> Error message
      in
      (* Only `<id>.md` files are stages; anything else in the directory is ignored. *)
      Json_fields.fold_results names ~init:[] ~f:(fun stages name ->
          if not (Filename.check_suffix name ".md") then Ok stages
          else
            let* id = State_path.safe_component ~label:"stage id" (Filename.chop_suffix name ".md") in
            let* stage = load ~id (Filename.concat root name) in
            Ok (stage :: stages))
  | Some _ -> Error (Printf.sprintf "stages path is not a directory: %s" root)

let discover ~home =
  let* home = State_path.canonicalize home in
  let* stages =
    Json_fields.fold_results (roots ~home) ~init:[] ~f:(fun stages root ->
        let* found = discover_root root in
        let replaced (stage : t) = List.exists (fun (later : t) -> String.equal later.id stage.id) found in
        Ok (List.filter (fun stage -> not (replaced stage)) stages @ found))
  in
  Ok (List.sort (fun (left : t) right -> String.compare left.id right.id) stages)

let find ~home id =
  let* id = State_path.safe_component ~label:"stage id" id in
  let* stages = discover ~home in
  match List.find_opt (fun (stage : t) -> String.equal stage.id id) stages with
  | Some stage -> Ok stage
  | None ->
      Error
        (Printf.sprintf "unknown stage %S; available stages: %s" id
           (match stages with
           | [] -> "<none>"
           | stages -> String.concat ", " (List.map (fun (stage : t) -> stage.id) stages)))

(* What the agent reads before carrying a stage out. The preamble is the same
   for every stage so that no stage file has to restate what naming it means. *)
let render (stage : t) =
  String.concat "\n"
    [ Printf.sprintf "Stage `%s` (%s)" stage.id stage.path;
      "";
      "Carry this stage out only because the user asked for it in this session. Their request approves \
       exactly the actions the stage describes, for this task only, and nothing beyond them. Do the \
       work yourself, run the commands, and report the result.";
      "";
      String.trim stage.text;
      "" ]

(* The stages as one phrase for a session's opening context. *)
let names stages =
  String.concat ", "
    (List.map (fun (stage : t) -> Printf.sprintf "`%s` (%s)" stage.id stage.title) stages)
