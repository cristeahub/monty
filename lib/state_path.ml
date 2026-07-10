type location = Active | Archived

type t = {
  home : string;
  run_id : string;
  id : string;
  location : location;
  run_dir : string;
  worker_dir : string;
  job_file : string;
}

let ( let* ) = Result.bind

let errorf fmt = Printf.ksprintf (fun message -> Error message) fmt

let path_exists path =
  try
    ignore (Unix.lstat path);
    true
  with _ -> false

let rec canonicalize path =
  let path = Shell.normalize (Shell.abs_path path) in
  try Ok (Unix.realpath path) with
  | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
      let parent = Filename.dirname path in
      if String.equal parent path then Ok path
      else
        let* parent = canonicalize parent in
        Ok (Filename.concat parent (Filename.basename path) |> Shell.normalize)
  | Unix.Unix_error (err, fn, arg) ->
      errorf "cannot canonicalize %s via %s(%s): %s" path fn arg
        (Unix.error_message err)
  | Sys_error msg -> errorf "cannot canonicalize %s: %s" path msg
  | Invalid_argument msg -> errorf "cannot canonicalize %s: %s" path msg

let is_contained ~root path =
  String.equal root path
  ||
  let prefix = if String.ends_with ~suffix:"/" root then root else root ^ "/" in
  String.length path > String.length prefix
  && String.sub path 0 (String.length prefix) = prefix

let split_relative ~root path =
  if String.equal root path then Some []
  else
    let prefix = if String.ends_with ~suffix:"/" root then root else root ^ "/" in
    if
      String.length path > String.length prefix
      && String.sub path 0 (String.length prefix) = prefix
    then
      Some
        (String.sub path (String.length prefix)
           (String.length path - String.length prefix)
        |> String.split_on_char '/')
    else None

let safe_component ~label value =
  let trimmed = String.trim value in
  let safe_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' -> true
    | _ -> false
  in
  if not (String.equal value trimmed) then
    errorf "%s must not contain leading or trailing whitespace, got %S" label value
  else if value = "" then errorf "%s must not be empty" label
  else if value = "." || value = ".." then
    errorf "%s must be a safe single path component, got %S" label value
  else if not (String.for_all safe_char value) then
    errorf
      "%s must be a safe single path component containing only letters, digits, '.', '-' or '_', got %S"
      label value
  else Ok value

let location_dir = function Active -> "workers" | Archived -> "archive"
let runs_root home = Filename.concat home ".monty/runs" |> Shell.normalize

(* Mutable state is valid only when its lexical path is also its physical path.
   The one permitted alias is MONTY_HOME itself, which callers resolve before
   constructing the hierarchy below it. *)
let require_exact_physical_path ~label path =
  let path = Shell.normalize (Shell.abs_path path) in
  let* physical = canonicalize path in
  if String.equal path physical then Ok physical
  else
    errorf
      "unsafe %s uses a symlink alias: %s resolves to %s; repair the state layout explicitly"
      label path physical

let make ~home ~run_id ~id ~location =
  let* run_id = safe_component ~label:"run id" run_id in
  let* id = safe_component ~label:"worker id" id in
  let* home = canonicalize home in
  let run_dir = Filename.concat (runs_root home) run_id |> Shell.normalize in
  let worker_dir =
    Filename.concat (Filename.concat run_dir (location_dir location)) id
    |> Shell.normalize
  in
  let* _runs = require_exact_physical_path ~label:".monty/runs hierarchy" (runs_root home) in
  let* worker_dir = require_exact_physical_path ~label:"Monty worker path" worker_dir in
  let job_file = Filename.concat worker_dir "job.json" in
  Ok { home; run_id; id; location; run_dir; worker_dir; job_file }

let active ~home ~run_id ~id = make ~home ~run_id ~id ~location:Active
let archived ~home ~run_id ~id = make ~home ~run_id ~id ~location:Archived

let path_under_resolved_home ~home path =
  let raw_home = Shell.normalize (Shell.abs_path home) in
  let raw_path = Shell.normalize (Shell.abs_path path) in
  let* physical_home = canonicalize raw_home in
  match split_relative ~root:raw_home raw_path with
  | Some parts -> Ok (List.fold_left Filename.concat physical_home parts |> Shell.normalize)
  | None -> Ok raw_path

let of_job_file ~home path =
  let* home_path = path_under_resolved_home ~home path in
  let* home = canonicalize home in
  let runs = runs_root home in
  let* _runs = require_exact_physical_path ~label:".monty/runs hierarchy" runs in
  let* path = require_exact_physical_path ~label:"job.json path" home_path in
  match split_relative ~root:runs path with
  | Some [ run_id; kind; id; "job.json" ] ->
      let* location =
        match kind with
        | "workers" -> Ok Active
        | "archive" -> Ok Archived
        | _ ->
            errorf
              "job.json is outside the canonical workers/archive layout: %s; repair the legacy record explicitly"
              path
      in
      let* expected = make ~home ~run_id ~id ~location in
      if String.equal expected.job_file path then Ok expected
      else
        errorf
          "job.json physical path does not match its canonical Monty state path: %s; expected %s"
          path expected.job_file
  | _ ->
      errorf
        "job.json is outside the canonical layout %s/<run>/{workers,archive}/<id>/job.json: %s; repair the legacy record explicitly"
        runs path

let of_worker_dir ~home ~id path =
  let* path = path_under_resolved_home ~home path in
  let* home = canonicalize home in
  let runs = runs_root home in
  let* _runs = require_exact_physical_path ~label:".monty/runs hierarchy" runs in
  let* path = require_exact_physical_path ~label:"worker directory" path in
  match split_relative ~root:runs path with
  | Some [ run_id; "workers"; physical_id ] ->
      let* id = safe_component ~label:"worker id" id in
      if not (String.equal physical_id id) then
        errorf "worker path id %S does not match worker id %S" physical_id id
      else active ~home ~run_id ~id
  | _ ->
      errorf
        "worker directory must use the canonical layout %s/<run>/workers/<id>, got %s"
        runs path

let validate_persisted_path ~home ~label ~expected = function
  | None -> Ok ()
  | Some actual ->
      let* actual = path_under_resolved_home ~home actual in
      let* actual = require_exact_physical_path ~label:("persisted " ^ label) actual in
      let* expected = require_exact_physical_path ~label:("expected " ^ label) expected in
      if String.equal actual expected then Ok ()
      else
        errorf
          "unsafe legacy job metadata: persisted %s %s does not match physical canonical path %s; repair the record explicitly"
          label actual expected

let ensure_contained_for_mutation state =
  let* home = canonicalize state.home in
  let runs = runs_root home in
  let* runs = require_exact_physical_path ~label:".monty/runs hierarchy" runs in
  let* worker = require_exact_physical_path ~label:"Monty worker path" state.worker_dir in
  if is_contained ~root:runs worker then Ok ()
  else errorf "unsafe Monty state path escapes %s: %s" runs state.worker_dir
