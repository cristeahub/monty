type workspace = {
  repo : string;
  branch : string option;
}

(* Swarm configuration from the manifest: the landing check and per-job limits
   of a driver. It is configuration, not runtime state. *)
type swarm_config = {
  check : string option;
  max_workers : int option;
  worker_profile : string option;
}

type t = {
  id : string option;
  title : string;
  repo : string;
  branch : string option;
  workspaces : workspace list;
  context : string;
  worker_dir : string option;
  prompt : string option;
  task_key : string option;
  agent_profile : string option;
  swarm : swarm_config option;
  swarm_parent : string option;
  swarm_name : string option;
}

let workspace ?branch repo = { repo; branch }

(* The manifest and job.json share one `swarm` object shape. *)
let swarm_config_of_json json =
  let ( let* ) = Result.bind in
  let open Yojson.Safe.Util in
  match member "swarm" json with
  | `Null -> Ok None
  | `Assoc _ as swarm ->
      let text name =
        match member name swarm with
        | `Null -> Ok None
        | `String value when String.trim value <> "" -> Ok (Some value)
        | _ -> Error (Printf.sprintf "swarm field %S must be a non-empty string when present" name)
      in
      let* check = text "check" in
      let* worker_profile = text "worker_profile" in
      let* worker_profile =
        match worker_profile with
        | None -> Ok None
        | Some value ->
            State_path.safe_component ~label:"swarm worker_profile" value
            |> Result.map Option.some
      in
      let* max_workers =
        match member "max_workers" swarm with
        | `Null -> Ok None
        | `Int value when value > 0 -> Ok (Some value)
        | _ -> Error "swarm field \"max_workers\" must be a positive integer when present"
      in
      Ok (Some { check; max_workers; worker_profile })
  | _ -> Error "field \"swarm\" must be an object when present"

let swarm_config_json config =
  `Assoc
    ((match config.check with Some value -> [ ("check", `String value) ] | None -> [])
    @ (match config.max_workers with Some value -> [ ("max_workers", `Int value) ] | None -> [])
    @ match config.worker_profile with
      | Some value -> [ ("worker_profile", `String value) ]
      | None -> [])

(* The driver id and child name of a swarm child; None for every other job. *)
let swarm_child job =
  match (job.swarm_parent, job.swarm_name) with
  | Some parent, Some name -> Some (parent, name)
  | _ -> None

let normalize_workspaces ~repo ~branch = function
  | [] -> [ { repo; branch } ]
  | workspaces -> workspaces

let make ?id ?branch ?(workspaces = []) ?worker_dir ?prompt ?task_key
    ?agent_profile ?swarm ?swarm_parent ?swarm_name ~title ~repo ~context () =
  let workspaces = normalize_workspaces ~repo ~branch workspaces in
  let first = List.hd workspaces in
  {
    id;
    title;
    repo = first.repo;
    branch = first.branch;
    workspaces;
    context;
    worker_dir;
    prompt;
    task_key;
    agent_profile;
    swarm;
    swarm_parent;
    swarm_name;
  }

let make_with_workspaces ?id ?worker_dir ?prompt ?task_key ?agent_profile ?swarm
    ~title ~(workspaces : workspace list) ~context () =
  match workspaces with
  | [] -> invalid_arg "a Monty job needs at least one workspace"
  | first :: _ ->
      make ?id ?worker_dir ?prompt ?task_key ?agent_profile ?swarm ~title
        ~repo:first.repo ?branch:first.branch ~workspaces ~context ()

let with_workspaces job (workspaces : workspace list) =
  match workspaces with
  | [] -> invalid_arg "a Monty job needs at least one workspace"
  | first :: _ ->
      { job with repo = first.repo; branch = first.branch; workspaces }

let workspaces_with_default_branches ?(prefix = "monty") ?index job =
  let default = Slug.branch ~prefix ?index job.title in
  job.workspaces
  |> List.map (fun (workspace : workspace) ->
         match workspace.branch with
         | Some _ -> workspace
         | None -> { workspace with branch = Some default })

let branch_leaf branch =
  branch |> String.split_on_char '/' |> List.rev
  |> List.find_opt (fun part -> String.trim part <> "")
  |> Option.value ~default:branch

let id_or_default ~branch job =
  match job.id with
  | Some id -> id
  | _ -> (
      match job.branch with
      | Some branch when String.trim branch <> "" -> branch_leaf branch |> Slug.of_title
      | _ -> branch_leaf branch |> Slug.of_title)

let default_prompt = "Start this task. Read the Monty instructions and task context first."

let prompt job = match job.prompt with Some prompt -> prompt | None -> default_prompt
