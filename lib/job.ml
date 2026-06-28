type t = {
  title : string;
  repo : string;
  branch : string option;
  context : string;
  prompt : string option;
}

let make ?branch ?prompt ~title ~repo ~context () =
  { title; repo; branch; context; prompt }

let branch_or_default ?index job =
  match job.branch with Some branch -> branch | None -> Slug.branch ?index job.title

let default_prompt = "Start this task. Read the context first."

let prompt job = match job.prompt with Some prompt -> prompt | None -> default_prompt
