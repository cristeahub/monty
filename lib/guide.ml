(* `monty guide`: the embedded workflow manual by topic, the text the installed
   skill defers to. Each "## " heading of docs/monty-workflows.md is a topic. *)

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

let index_footer =
  "Use monty COMMAND --help=plain for exact commands and options. Help does not authorize actions; follow your assigned role and user instructions."

let read topic =
  if topic = "index" then
    Ok
      ("Monty workflow topics (use monty guide TOPIC):\n"
      ^ String.concat "\n" (List.map (fun (id, title, _) -> "- " ^ id ^ ": " ^ title) topics)
      ^ "\n" ^ index_footer)
  else if topic = "all" then Ok Embedded_manual.text
  else
    match List.find_opt (fun (id, _, _) -> id = topic) topics with
    | Some (_, _, text) -> Ok text
    | None -> Error ("Unknown topic: " ^ topic ^ ". Use topic=index.")

let json topic =
  let entry (id, title, text) = `Assoc [ ("id", `String id); ("title", `String title); ("text", `String text) ] in
  if topic = "index" then
    Ok (`Assoc [ ("topics", `List (List.map (fun (id, title, _) -> `Assoc [ ("id", `String id); ("title", `String title) ]) topics)) ])
  else if topic = "all" then Ok (`Assoc [ ("topics", `List (List.map entry topics)) ])
  else
    match List.find_opt (fun (id, _, _) -> id = topic) topics with
    | Some topic -> Ok (entry topic)
    | None -> Error ("Unknown topic: " ^ topic ^ ". Use topic=index.")

let command =
  let open Cmdliner in
  let topic = Arg.(value & pos 0 string "index" & info [] ~docv:"TOPIC" ~doc:"A topic id from the index, or all for the whole manual.") in
  let json_arg = Arg.(value & flag & info [ "json" ] ~doc:"Print the index, a topic or the whole manual as JSON.") in
  Cmd.v (Cmd.info "guide" ~doc:"Read Monty's embedded CLI workflow manual.")
    Term.(const (fun topic as_json ->
      let result = if as_json then Result.map (fun value -> Yojson.Safe.pretty_to_string ~std:true value) (json topic) else read topic in
      match result with
      | Ok text -> print_endline text; 0
      | Error message -> prerr_endline message; 1) $ topic $ json_arg)
