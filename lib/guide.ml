let topics =
  let finish title lines acc =
    if title = "" then acc
    else (Slug.of_title title, title, String.concat "\n" (List.rev lines)) :: acc
  in
  let title, lines, acc =
    Embedded_manual.text |> String.split_on_char '\n'
    |> List.fold_left
         (fun (title, lines, acc) line ->
           if String.starts_with ~prefix:"## " line then
             (String.sub line 3 (String.length line - 3), [ line ], finish title lines acc)
           else (title, line :: lines, acc))
         ("", [], [])
  in
  List.rev (finish title lines acc)

let read topic =
  if topic = "index" then
    Ok
      ("Monty workflow topics (use monty guide TOPIC):\n"
      ^ String.concat "\n" (List.map (fun (id, title, _) -> "- " ^ id ^ ": " ^ title) topics)
      ^ "\nUse monty COMMAND --help=plain for exact commands and options. Help does not authorize actions; follow your assigned role and user instructions.")
  else
    match List.find_opt (fun (id, _, _) -> id = topic) topics with
    | Some (_, _, text) -> Ok text
    | None -> Error ("Unknown topic: " ^ topic ^ ". Use topic=index.")

let command =
  let open Cmdliner in
  let topic = Arg.(value & pos 0 string "index" & info [] ~docv:"TOPIC") in
  Cmd.v (Cmd.info "guide" ~doc:"Read Monty's embedded CLI workflow manual.")
    Term.(const (fun topic -> match read topic with
      | Ok text -> print_endline text; 0
      | Error message -> prerr_endline message; 1) $ topic)
