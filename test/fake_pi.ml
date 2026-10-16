open Monty.Json_fields

let json = Yojson.Safe.to_string
let string value = `String value
let obj fields = `Assoc fields
let argv = Array.to_list Sys.argv |> List.tl
let rec option name = function
  | key :: value :: _ when key = name -> Some value
  | _ :: rest -> option name rest
  | [] -> None
let opt name default = Option.value ~default (option name argv)
let write path text = Monty.Shell.ensure_dir (Filename.dirname path); Monty.Shell.write_file path text
let append_line path value =
  let out = open_out_gen [Open_wronly; Open_creat; Open_append; Open_binary] 0o600 path in
  Fun.protect ~finally:(fun () -> close_out out) (fun () -> output_string out (json value ^ "\n"))
let emit value = print_endline (json value); flush stdout
let env name = Sys.getenv_opt name

let run () =
  if List.mem "--version" argv then (print_endline "1.0.2"; exit 0);
  let home = Sys.getenv "PI_CODING_AGENT_DIR" in
  let path, header, initial = match option "--session" argv with
    | Some path ->
        let records = Monty.Shell.read_file path |> String.split_on_char '\n'
          |> List.filter ((<>) "") |> List.map Yojson.Safe.from_string in
        path, List.hd records, List.tl records
    | None ->
        let native = opt "--session-id" "" in
        Filename.concat (opt "--session-dir" "") (native ^ ".jsonl"),
        obj ["type",string "session";"version",`Int 3;"id",string native;"cwd",string (Sys.getcwd ())], [] in
  let entries = ref initial and busy = ref false and queued = ref [] and dialog = ref None in
  let model = ref (obj ["provider",string (opt "--provider" "fixture");"id",string (opt "--model" "first")]) in
  let reported = if option "--session" argv <> None && env "MONTY_FIXTURE_REBIND" <> None
    then Filename.concat (Filename.dirname path) "redirected.jsonl" else path in
  let log value = append_line (Filename.concat home "calls.jsonl") value in
  log (obj ["start",field "id" header;"pid",`Int (Unix.getpid ());"argv",`List(List.map string argv);
    "cwd",string(Sys.getcwd ());"environment",`Assoc(Array.to_list(Unix.environment ()) |> List.filter_map(fun binding ->
      let p=String.index binding '=' in let key=String.sub binding 0 p in
      if String.starts_with ~prefix:"MONTY_" key then Some(key,string(String.sub binding (p+1) (String.length binding-p-1))) else None))]);
  let respond command data = emit(obj ["id",field "id" command;"type",string "response";
    "command",field "type" command;"success",`Bool true;"data",data]) in
  let append kind fields =
    let parent = match List.rev !entries with [] -> `Null | row::_ -> field "id" row in
    let entry=obj(["type",string kind;"id",string("entry-" ^ string_of_int(List.length !entries+1));"parentId",parent] @ fields) in
    entries:= !entries @ [entry];
    Monty.Shell.ensure_dir (Filename.dirname path);
    if not(Sys.file_exists path) then write path (json header ^ "\n");
    append_line path entry;entry in
  let finish text =
    let message=obj ["role",string "assistant";"content",`List[obj["type",string "text";"text",string text]];
      "provider",field "provider" !model;"model",field "id" !model] in
    ignore(append "message" ["message",message]);
    emit(obj["type",string "message_end";"message",message]);busy:=false;
    emit(obj["type",string "agent_end";"messages",`List[]]);emit(obj["type",string "agent_settled"]) in
  let process command =
    log(obj["session",field "id" header;"command",command]);
    match s "type" command with
    | "get_state" -> respond command (obj["sessionId",field "id" header;"sessionFile",string reported;
        "model",!model;"isStreaming",`Bool !busy;"isCompacting",`Bool false;"pendingMessageCount",`Int(List.length !queued)])
    | "get_entries" ->
        if env "MONTY_FIXTURE_UNFLUSHED"<>None then (
          let parent=match List.rev !entries with []->`Null|row::_->field "id" row in
          entries:= !entries @ [obj["type",string "custom";"id",string("setup-" ^ string_of_int(List.length !entries+1));"parentId",parent;"data",string "unflushed setup"]]);
        respond command (obj["entries",`List !entries;"leafId",(match List.rev !entries with []->`Null|row::_->field "id" row)])
    | "steer" | "follow_up" -> queued:= !queued @ [field "message" command];respond command (obj["disposition",string "queued"])
    | "prompt" ->
        busy:=true;emit(obj["type",string "agent_start"]);
        let text=s "message" command in
        ignore(append "message" ["message",obj["role",string "user";"content",`List[obj["type",string "text";"text",string text]]]]);
        if text="lost response" then exit 7
        else if text="malformed stream" then (print_endline "{malformed";output_string stdout (String.make 262144 'x');flush stdout)
        else if List.mem text ["dialog";"expiring dialog"] then (
          dialog:=Some command;emit(obj["type",string "extension_ui_request";"id",string "native-dialog";
            "method",string "confirm";"title",string "Confirm fixture action";"timeout",`Int(if text="expiring dialog" then 100 else 10000)]))
        else (
          respond command (obj["disposition",string "started"]);
          if text="stream" then (
            emit(obj["type",string "message_start";"message",obj["role",string "assistant";"content",`List[]]]);
            emit(obj["type",string "message_update";"assistantMessageEvent",obj["type",string "text_start";"contentIndex",`Int 0]]);
            List.iter(fun delta->Unix.sleepf 0.6;emit(obj["type",string "message_update";"assistantMessageEvent",obj["type",string "text_delta";"contentIndex",`Int 0;"delta",string delta]])) ["Live Pi ";"answer"];
            Unix.sleepf 1.5;emit(obj["type",string "message_update";"assistantMessageEvent",obj["type",string "text_end";"contentIndex",`Int 0;"content",string "Live Pi answer"]]);finish "Live Pi answer")
          else if text<>"hold" then finish "Pi fixture answer")
    | "extension_ui_response" ->
        assert(field "id" command=string "native-dialog");
        respond (Option.get !dialog) (obj["disposition",string "started"]);dialog:=None;finish "Pi fixture answer"
    | "clear_queue" -> respond command (obj["steering",`List !queued;"followUp",`List[]]);queued:=[]
    | "abort" ->
        busy:=false;Option.iter(fun pending->respond pending(obj["disposition",string "handled"])) !dialog;
        dialog:=None;emit(obj["type",string "agent_settled"]);respond command(obj[])
    | "set_model" -> model:=obj["provider",field "provider" command;"id",field "modelId" command];
        ignore(append "model_change" ["provider",field "provider" !model;"modelId",field "id" !model]);respond command !model
    | kind -> emit(obj["id",field "id" command;"type",string "response";"command",string kind;"success",`Bool false;"error",string "unsupported fixture command"]) in
  (try while true do process (Yojson.Safe.from_string (input_line stdin)) done with End_of_file -> ());
  Option.iter(fun gate->log(obj["stopping",field "id" header]);while Sys.file_exists gate do Unix.sleepf 0.05 done) (env "MONTY_FIXTURE_STOP_GATE");
  log(obj["stopped",field "id" header;"pid",`Int(Unix.getpid ())])

let () = run ()
