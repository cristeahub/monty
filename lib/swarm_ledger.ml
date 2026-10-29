(* The swarm ledger: `swarm/ledger.json` in a driver's worker directory. It is
   the one durable record of every child the driver spawned, the landing check
   and the limits. Every write goes through the one-home lock and atomic
   replacement; nothing here calls git, wt, the check command or a harness. *)

open Yojson.Safe

let ( let* ) = Result.bind

let schema = "monty:swarm-ledger:v1"

type state = Running | Finished | Landed | Retired | Failed

type child = {
  name : string;
  id : string;
  branch : string;
  brief : string;
  spawned_at : string;
  resumed_at : string option;
  state : state;
  landed_commit : string option;
  last_check : string option;
  error : string option;
}

type t = {
  driver : string;
  check : string option;
  max_workers : int;
  max_spawns : int;
  worker_profile : string;
  children : child list;
}

let state_to_string = function
  | Running -> "running"
  | Finished -> "finished"
  | Landed -> "landed"
  | Retired -> "retired"
  | Failed -> "failed"

let state_of_string = function
  | "running" -> Ok Running
  | "finished" -> Ok Finished
  | "landed" -> Ok Landed
  | "retired" -> Ok Retired
  | "failed" -> Ok Failed
  | value -> Error (Printf.sprintf "unknown swarm child state %S" value)

let directory worker_dir = Filename.concat worker_dir "swarm"
let path worker_dir = Filename.concat (directory worker_dir) "ledger.json"
let briefs_dir worker_dir = Filename.concat (directory worker_dir) "briefs"
let brief_path worker_dir name = Filename.concat (briefs_dir worker_dir) (name ^ ".md")

let check_artifacts_dir worker_dir name =
  Filename.concat (Filename.concat (Filename.concat worker_dir "artifacts") "swarm") name

let child_json child =
  let optional name = function None -> [] | Some value -> [ (name, `String value) ] in
  `Assoc
    ([ ("name", `String child.name);
       ("id", `String child.id);
       ("branch", `String child.branch);
       ("brief", `String child.brief);
       ("spawned_at", `String child.spawned_at);
       ("state", `String (state_to_string child.state)) ]
    @ optional "resumed_at" child.resumed_at
    @ optional "landed_commit" child.landed_commit
    @ optional "last_check" child.last_check
    @ optional "error" child.error)

let to_json ledger =
  `Assoc
    [ ("schema", `String schema);
      ("driver", `String ledger.driver);
      ("check", Option.fold ~none:`Null ~some:(fun value -> `String value) ledger.check);
      ("max_workers", `Int ledger.max_workers);
      ("max_spawns", `Int ledger.max_spawns);
      ("worker_profile", `String ledger.worker_profile);
      ("updated_at", `String (Worker_memory.now_utc ()));
      ("children", `List (List.map child_json ledger.children)) ]

let required json name =
  match Util.member name json with
  | `String value when String.trim value <> "" -> Ok value
  | _ -> Error (Printf.sprintf "swarm ledger missing string field %S" name)

let optional json name =
  match Util.member name json with
  | `Null -> Ok None
  | `String value -> Ok (Some value)
  | _ -> Error (Printf.sprintf "swarm ledger field %S must be a string when present" name)

let positive json name =
  match Util.member name json with
  | `Int value when value > 0 -> Ok value
  | _ -> Error (Printf.sprintf "swarm ledger field %S must be a positive integer" name)

let child_of_json json =
  let* name = required json "name" in
  let* name = State_path.safe_component ~label:"swarm child name" name in
  let* id = required json "id" in
  let* id = State_path.safe_component ~label:"swarm child id" id in
  let* branch = required json "branch" in
  let* brief = required json "brief" in
  let* spawned_at = required json "spawned_at" in
  let* resumed_at = optional json "resumed_at" in
  let* state = required json "state" in
  let* state = state_of_string state in
  let* landed_commit = optional json "landed_commit" in
  let* last_check = optional json "last_check" in
  let* error = optional json "error" in
  Ok { name; id; branch; brief; spawned_at; resumed_at; state; landed_commit; last_check; error }

(* When the current run started: the resume if there was one, else the spawn. *)
let run_started_at child = Option.value ~default:child.spawned_at child.resumed_at

let of_json json =
  let* parsed_schema = required json "schema" in
  if parsed_schema <> schema then
    Error (Printf.sprintf "unsupported swarm ledger schema %S" parsed_schema)
  else
    let* driver = required json "driver" in
    let* check = optional json "check" in
    let* max_workers = positive json "max_workers" in
    let* max_spawns = positive json "max_spawns" in
    let* worker_profile = required json "worker_profile" in
    let* children =
      match Util.member "children" json with
      | `List values ->
          Json_fields.fold_results values ~init:[] ~f:(fun acc value ->
              child_of_json value |> Result.map (fun child -> child :: acc))
          |> Result.map List.rev
      | _ -> Error "swarm ledger field \"children\" must be an array"
    in
    Ok { driver; check; max_workers; max_spawns; worker_profile; children }

(* Reads the ledger without the lock; callers that mutate reload under it. *)
let load_opt ~worker_dir =
  let path = path worker_dir in
  let* state = State_store.lstat path in
  match state with
  | None -> Ok None
  | Some { Unix.st_kind = Unix.S_LNK; _ } ->
      Error (Printf.sprintf "unsafe swarm ledger is a symlink: %s" path)
  | Some { Unix.st_kind = Unix.S_REG; _ } ->
      let* json = State_store.read_json ~path in
      (match json with
      | None -> Ok None
      | Some json ->
          State_store.decode_json ~path (fun () -> of_json json)
          |> Result.map Option.some)
  | Some _ -> Error (Printf.sprintf "swarm ledger is not a regular file: %s" path)

let save_unlocked ~worker_dir ledger =
  let* () =
    State_store.ensure_real_directory ~label:"swarm ledger directory" ~mode:0o700
      (directory worker_dir)
  in
  State_store.write_json_atomic ~path:(path worker_dir) (to_json ledger)

(* Reload, transform and write the ledger under the one-home lock. The
   transform must not call external programs. *)
let update ~home ~worker_dir transform =
  State_store.with_lock ~home (fun () ->
      let* current = load_opt ~worker_dir in
      let* updated = transform current in
      let* () = save_unlocked ~worker_dir updated in
      Ok updated)

let find_child ledger name =
  List.find_opt (fun child -> String.equal child.name name) ledger.children

let replace_child ledger child =
  { ledger with
    children =
      List.map (fun current -> if String.equal current.name child.name then child else current)
        ledger.children }

let active ledger = List.filter (fun child -> child.state <> Retired) ledger.children
let running ledger = List.filter (fun child -> child.state = Running) ledger.children
