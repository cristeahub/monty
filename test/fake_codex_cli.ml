open Monty.Json_fields
let args=Array.to_list Sys.argv |> List.tl
let () =
  let config=Yojson.Safe.from_file(Filename.concat(Sys.getenv "CODEX_HOME") "fixture.json") in
  if List.filteri(fun index _->index<3) args=["app-server";"daemon";"start"] then (
    print_endline(Yojson.Safe.to_string(`Assoc["socketPath",field "socket" config;"appServerVersion",`String "0.160.0"])))
  else (
    match field "terminal_log" config with
    | `Null -> ()
    | `String path ->
        let output=open_out_gen [Open_wronly;Open_creat;Open_append;Open_binary] 0o600 path in
        Fun.protect ~finally:(fun ()->close_out output) (fun ()->
          output_string output (Yojson.Safe.to_string(`Assoc["native",`String(List.hd(List.rev args));
            "cwd",`String(Sys.getcwd ());"worker",Option.fold ~none:`Null ~some:(fun value->`String value)(Sys.getenv_opt "MONTY_JOB_ID");
            "args",`List(List.map(fun value->`String value)args)]) ^ "\n"))
    | _ -> failwith "invalid fake Codex terminal log")
