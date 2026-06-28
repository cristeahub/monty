type t = {
  id : string option;
  title : string;
  repo : string;
  branch : string option;
  context : string;
  worker_dir : string option;
  prompt : string option;
}

let make ?id ?branch ?worker_dir ?prompt ~title ~repo ~context () =
  { id; title; repo; branch; context; worker_dir; prompt }

let branch_or_default ?(prefix = "monty") ?index job =
  match job.branch with
  | Some branch -> branch
  | None -> Slug.branch ~prefix ?index job.title

let branch_leaf branch =
  branch |> String.split_on_char '/' |> List.rev
  |> List.find_opt (fun part -> String.trim part <> "")
  |> Option.value ~default:branch

let id_or_default ~branch job =
  match job.id with
  | Some id when String.trim id <> "" -> Slug.of_title id
  | _ -> (
      match job.branch with
      | Some branch when String.trim branch <> "" -> branch_leaf branch |> Slug.of_title
      | _ -> branch_leaf branch |> Slug.of_title)

let default_prompt = "Start this task. Read the Monty instructions and task context first."

let prompt job = match job.prompt with Some prompt -> prompt | None -> default_prompt
