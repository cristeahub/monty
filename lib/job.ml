type workspace = {
  repo : string;
  branch : string option;
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
}

let workspace ?branch repo = { repo; branch }

let normalize_workspaces ~repo ~branch = function
  | [] -> [ { repo; branch } ]
  | workspaces -> workspaces

let make ?id ?branch ?(workspaces = []) ?worker_dir ?prompt ?task_key ~title
    ~repo ~context () =
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
  }

let make_with_workspaces ?id ?worker_dir ?prompt ?task_key ~title
    ~(workspaces : workspace list)
    ~context () =
  match workspaces with
  | [] -> invalid_arg "a Monty job needs at least one workspace"
  | first :: _ ->
      make ?id ?worker_dir ?prompt ?task_key ~title ~repo:first.repo
        ?branch:first.branch ~workspaces ~context ()

let with_workspaces job (workspaces : workspace list) =
  match workspaces with
  | [] -> invalid_arg "a Monty job needs at least one workspace"
  | first :: _ ->
      { job with repo = first.repo; branch = first.branch; workspaces }

let map_workspaces f job = with_workspaces job (List.map f job.workspaces)

let branch_or_default ?(prefix = "monty") ?index job =
  match job.branch with
  | Some branch -> branch
  | None -> Slug.branch ~prefix ?index job.title

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
