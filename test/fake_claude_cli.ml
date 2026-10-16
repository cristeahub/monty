open Monty.Json_fields
let string value = `String value
let obj fields = `Assoc fields
let args = Array.to_list Sys.argv |> List.tl
let rec option name = function
  | key::value::_ when key=name -> value
  | _::rest -> option name rest
  | [] -> failwith ("missing fixture argument " ^ name)
let () =
  if List.mem "--version" args then (print_endline "2.1.296";exit 0);
  if List.mem "--print" args then (
    ignore(In_channel.input_all stdin);
    print_endline(Yojson.Safe.to_string(obj["type",string "result";"subtype",string "success";
      "is_error",`Bool false;"session_id",string "12345678-1234-4234-8234-123456789abc";
      "permission_denials",`List[];"result",string "complete Claude phase"]));exit 0);
  let native=option (if List.mem "--session-id" args then "--session-id" else "--resume") args in
  let rex=Array.to_list(Unix.environment ()) |> List.filter_map(fun binding->
    let boundary=String.index binding '=' in
    let key=String.sub binding 0 boundary in
    if String.starts_with ~prefix:"REX_" key then
      Some(key,string(String.sub binding (boundary+1) (String.length binding-boundary-1))) else None) in
  Yojson.Safe.to_file (Filename.concat(Sys.getenv "CLAUDE_CONFIG_DIR") "terminal.json")
    (obj["pid",`Int(Unix.getpid ());"ppid",`Int(Unix.getppid ());"native",string native;
      "tty",`Bool(Unix.isatty Unix.stdin);"args",`List(List.map string args);"rex",`Assoc rex]);
  print_endline("NATIVE CLAUDE " ^ native);flush stdout;
  let rec read () = match input_line stdin with
    | line -> let line=String.trim line in print_endline("RECEIVED " ^ line);flush stdout;
        if line<>"/exit" then read ()
    | exception End_of_file -> () in
  read ()
