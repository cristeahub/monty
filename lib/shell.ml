let quote s =
  let b = Buffer.create (String.length s + 8) in
  Buffer.add_char b '\'';
  String.iter
    (function
      | '\'' -> Buffer.add_string b "'\\''"
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '\'';
  Buffer.contents b

let applescript_string s =
  let b = Buffer.create (String.length s + 8) in
  Buffer.add_char b '"';
  String.iter
    (function
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let is_abs path = Filename.is_relative path |> not

let abs_path ?base path =
  if is_abs path then path
  else
    let base = match base with Some base -> base | None -> Sys.getcwd () in
    Filename.concat base path

let normalize path =
  match Fpath.of_string path with
  | Ok p -> Fpath.to_string (Fpath.normalize p)
  | Error _ -> path

let ensure_dir path =
  let rec loop path =
    if path = "" || path = Filename.dirname path then ()
    else if Sys.file_exists path then
      if Sys.is_directory path then ()
      else invalid_arg (Printf.sprintf "%s exists and is not a directory" path)
    else (
      loop (Filename.dirname path);
      Unix.mkdir path 0o755)
  in
  loop path

let write_file path contents =
  ensure_dir (Filename.dirname path);
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents)

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let chmod_executable path = Unix.chmod path 0o755

let last_non_empty_line text =
  text |> String.split_on_char '\n' |> List.rev
  |> List.find_opt (fun line -> String.trim line <> "")
  |> Option.map String.trim
