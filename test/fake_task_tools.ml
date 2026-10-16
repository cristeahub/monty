open Test_support

let git args =
  let result = run "git" args in
  if result.code <> 0 then (
    prerr_string result.stderr; prerr_string result.stdout; exit result.code);
  String.trim result.stdout

let worktrees () =
  let text = git [ "worktree"; "list"; "--porcelain" ] in
  let finish fields entries =
    match List.assoc_opt "branch" fields, List.assoc_opt "worktree" fields with
    | Some branch, Some path when String.starts_with ~prefix:"refs/heads/" branch ->
        (String.sub branch 11 (String.length branch - 11), path) :: entries
    | _ -> entries
  in
  let rec parse fields entries = function
    | [] -> List.rev (finish fields entries)
    | "" :: rest -> parse [] (finish fields entries) rest
    | line :: rest ->
        (match String.index_opt line ' ' with
        | None -> parse fields entries rest
        | Some at -> parse ((String.sub line 0 at, String.sub line (at + 1) (String.length line - at - 1)) :: fields) entries rest)
  in
  parse [] [] (String.split_on_char '\n' text)

let wt = function
  | [ "list" ] ->
      Printf.printf "%s:\n" (Filename.basename (Sys.getcwd ()));
      List.iter (fun (branch, path) -> Printf.printf "  %s -> %s\n" branch path) (worktrees ())
  | [ "b"; branch ] ->
      let entries = worktrees () in
      let path = match List.assoc_opt branch entries with
        | Some path -> path
        | None ->
            let repo = Sys.getcwd () in
            let path = Filename.dirname repo / ("worktree-" ^ Filename.basename repo ^ "-" ^ String.map (function '/' -> '-' | c -> c) branch) in
            ignore (git [ "worktree"; "add"; "-b"; branch; path ]); path
      in
      print_endline path
  | [ "db"; branch ] ->
      Option.iter (fun path -> ignore (git [ "worktree"; "remove"; path ])) (List.assoc_opt branch (worktrees ()));
      ignore (git [ "branch"; "-D"; branch ])
  | args -> failwith ("unsupported fake wt arguments: " ^ String.concat " " args)

let tool home name args =
  let path = home / "tools.log" in
  let channel = open_out_gen [ Open_creat; Open_append; Open_text ] 0o600 path in
  Fun.protect ~finally:(fun () -> close_out channel) (fun () ->
      output_string channel (name ^ " " ^ Yojson.Safe.to_string (`List (List.map string args)) ^ "\n"));
  if name = "osascript" && Sys.file_exists (home / "fail-terminal") then exit 7

let () = match Array.to_list Sys.argv with
  | _ :: "wt" :: args -> wt args
  | _ :: "tool" :: home :: name :: args -> tool home name args
  | _ -> failwith "expected fixture wt or tool mode"
