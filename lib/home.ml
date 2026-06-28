let contains_substring text needle =
  let text_len = String.length text in
  let needle_len = String.length needle in
  if needle_len = 0 then true
  else
    let rec loop i =
      if i + needle_len > text_len then false
      else if String.sub text i needle_len = needle then true
      else loop (i + 1)
    in
    loop 0

let looks_like_monty_root dir =
  let dune_project = Filename.concat dir "dune-project" in
  Sys.file_exists dune_project
  &&
  try
    let text = Shell.read_file dune_project in
    contains_substring text "(name monty)"
  with _ -> false

let rec find_up dir =
  if looks_like_monty_root dir then Some dir
  else
    let parent = Filename.dirname dir in
    if parent = dir then None else find_up parent

let executable_home_candidate () =
  let executable = Shell.abs_path Sys.executable_name |> Shell.normalize in
  let executable_dir = Filename.dirname executable in
  let candidates =
    [ Filename.dirname executable_dir;
      Filename.dirname (Filename.dirname executable_dir);
      Filename.dirname (Filename.dirname (Filename.dirname executable_dir)) ]
  in
  List.find_opt looks_like_monty_root candidates

let default () =
  match Sys.getenv_opt "MONTY_HOME" with
  | Some path when String.trim path <> "" -> Shell.normalize (Shell.abs_path path)
  | _ -> (
      match find_up (Sys.getcwd ()) with
      | Some root -> Shell.normalize root
      | None -> (
          match executable_home_candidate () with
          | Some root -> Shell.normalize root
          | None -> Sys.getcwd () |> Shell.normalize))

let runtime_script_dir ?home () =
  let home = match home with Some home -> home | None -> default () in
  Filename.concat home ".monty/runtime/scripts"
