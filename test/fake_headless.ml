open Monty.Json_fields
let obj fields = `Assoc fields
let string value = `String value
let args = Array.to_list Sys.argv |> List.tl
let rec option name = function
  | key::value::_ when key=name -> value
  | _::rest -> option name rest
  | [] -> failwith("missing fixture argument " ^ name)
let rec ancestors pid =
  if pid<=1 then [] else
  let command=Printf.sprintf "ps -o ppid= -p %d" pid in
  let channel=Unix.open_process_in command in
  let text=In_channel.input_all channel |> String.trim in
  ignore(Unix.close_process_in channel);
  pid :: (if text="" then [] else ancestors(int_of_string text))
let append path value =
  let out=open_out_gen [Open_wronly;Open_creat;Open_append;Open_binary] 0o600 path in
  Fun.protect ~finally:(fun ()->close_out out) (fun ()->output_string out (Yojson.Safe.to_string value ^ "\n"))
let () =
  if List.mem "--version" args then (print_endline "2.1.296";exit 0);
  ignore(In_channel.input_all stdin);
  let root=Sys.getenv "MONTY_HOME" and worker=Sys.getenv "MONTY_JOB_ID" in
  let path name=Filename.concat root name in
  append (path "phases.jsonl") (obj["worker",string worker;"pid",`Int(Unix.getpid ());
    "parents",`List(List.map(fun pid->`Int pid)(ancestors(Unix.getppid ())));"args",`List(List.map string args);
    "yolo",Option.fold ~none:`Null ~some:string(Sys.getenv_opt "MONTY_CODEX_YOLO");
    "private_nonce_cleared",`Bool(Option.value ~default:""(Sys.getenv_opt "MONTY_SERVER_NONCE")="");
    "scoped_auth",`Bool(Sys.getenv_opt "OPENAI_API_KEY"=Some "scoped-test-key")]);
  if worker="native-failure" && not(Sys.file_exists(path "allow-native-failure")) then exit 7;
  if worker="cancelled" && not(Sys.file_exists(path "allow-cancelled")) then (
    Monty.Shell.write_file (path "cancelled-started") "";Unix.sleepf 60.);
  if List.mem worker ["wait-a";"wait-b"] && not(Sys.file_exists(path("ready-" ^ worker))) then (
    Monty.Shell.write_file (path("ready-" ^ worker)) "";
    let deadline=Unix.gettimeofday ()+.10. in
    while not(List.for_all(fun name->Sys.file_exists(path("ready-" ^ name)))["wait-a";"wait-b"]) do
      if Unix.gettimeofday ()>deadline then failwith "wait batch dispatched sequentially";
      Unix.sleepf 0.02
    done);
  if List.mem "--print" args then
    print_endline(Yojson.Safe.to_string(obj["type",string "result";"subtype",string "success";
      "is_error",`Bool false;"session_id",string(Monty.Claude_rpc.uuid ());"permission_denials",`List[];
      "result",string "Validated scoped server phase."]))
  else (
    assert(List.mem "exec" args);
    Monty.Shell.write_file (option "--output-last-message" args) "Validated scoped server phase.\n";
    print_endline(Yojson.Safe.to_string(obj["type",string "turn.completed";"usage",obj["input_tokens",`Int 1;"output_tokens",`Int 1]])))
