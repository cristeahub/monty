open Lwt.Infix
open Json_fields
open Conversation_store

let value = function Ok value -> value | Error message -> fail 409 message
let bounded limit value = try bounded_string limit value with Invalid_argument _ -> fail 400 "missing or oversized task text"
let projects home = value (Project_storage.load_projects ~home)
let project_json (p:Overview_types.project) = obj ["id",string p.id;"repo",string p.repo;
  "group",Option.fold ~none:`Null ~some:string p.group]
let catalog home = `List (List.map project_json (projects home))
let task home key =
  if not (String.starts_with ~prefix:"local:" key) then fail 400 "a canonical local task key is required";
  match value (Task_storage.load_local_tasks ~home) |> List.find_opt (fun (t:Overview_types.local_task) -> "local:" ^ t.id=key) with
  | Some task -> task | None -> fail 404 "task not found"
let worker home key (task:Overview_types.local_task) =
  let workers=value (Job_store.load_all ~home) |> List.filter (fun (r:Job_store.record) -> r.job.task_key=Some key) in
  match workers with
  | [] when task.worker_id=None && task.worker_key=None -> None
  | [record] -> ignore(value (Reconciliation.validate_worker_task_link ~home record));Some record
  | _ -> fail 409 "task worker identity is missing or ambiguous; reconcile it before managing this task"
let detail home key =
  if String.starts_with ~prefix:"worker:" key then (
    let record=match value(Job_store.load_all ~home) |> List.find_opt (fun r->Reconciliation.diagnostic_task_key r=key) with
      | Some r->r|None->fail 404 "worker not found" in
    let t=Reconciliation.diagnostic_task (projects home) record in
    obj ["key",string key;"id",string t.display_id;"title",string t.title;"status",string t.status;
      "project",string t.project;"read_only",`Bool true;
      "workspaces",`List(List.map Task_storage.json_of_task_workspace t.workspaces)])
  else
  let task=task home key in
  let worker=worker home key task in
  let raw=Task_storage.json_of_local_task task in
  let project=value(Project_storage.resolve_project (projects home) task.project) in
  let record=Option.fold ~none:`Null ~some:(fun (r:Job_store.record) -> read r.path) worker in
  let revision=sha (Yojson.Safe.to_string (`List [raw;record;project_json project])) in
  puts ["key",string key;"revision",string revision;"project_repo",string project.repo;
    "worker",Option.fold ~none:`Null ~some:(fun (r:Job_store.record) -> obj ["id",string r.id;
      "status",string r.status;"directory",string r.worker_dir]) worker] raw
let current home request =
  let key=required "task" request in
  let detail=detail home key in
  if field "revision" request<>field "revision" detail then fail 409 "task changed; refresh and review it before retrying";
  key,detail
let run ~env args =
  Lwt_process.with_process_full ~env ("",Array.of_list (Unix.realpath Sys.executable_name :: args)) (fun p ->
    Lwt.both (Lwt_io.read p#stdout) (Lwt_io.read p#stderr) >>= fun (out,err) ->
    p#status >|= fun status ->
    let code=match status with Unix.WEXITED n->n|Unix.WSIGNALED n|Unix.WSTOPPED n->128+n in
    obj ["exit_code",`Int code;"stdout",string out;"stderr",string err])
let wt_command () = match Sys.getenv_opt "MONTY_WT_COMMAND" with Some s when String.trim s<>""->s|_->"wt"
let settings home = value(Settings.load ~home)
let role home key =
  let task=task home key in
  (match worker home key task with
  | None -> value(Agent_profile.find ~home (value(Settings.effective_agent_profile ~home None)))
  | Some r -> value(Agent_profile.load_pinned ~home ~worker_dir:r.worker_dir r.job.agent_profile))
  |> Agent_profile.to_snapshot_json
let profile profiles id = match List.find_opt (fun p->s "id" p=id) profiles with
  | Some p when List.mem (s "harness" p) ["codex";"pi";"claude"] -> p
  | _ -> fail 400 "select a configured Pi, Codex or Claude account"
let harness p = match Harness.of_string (s "harness" p) with Ok harness -> harness | Error _ -> fail 400 "select a configured Pi, Codex or Claude account"
let account p = let harness=harness p in harness,s (Harness.account_field harness) p
let environment make_env home p =
  let harness,directory=account p in
  make_env (["MONTY_HOME=" ^ home;"MONTY_CODEX_YOLO=false"] @ Harness.account_environment ~getenv:Sys.getenv_opt harness directory)
let options home p = ["--home";home;"--harness";s "harness" p;
  ("--" ^ s "harness" p ^ "-command");Shell.quote(s "command" p);
  "--wt-command";wt_command ();"--worktree";"always"]
let terminal home = Terminal.backend_to_string (value (Settings.effective_terminal ~getenv:Sys.getenv_opt ~home None))
let doctor ~env home p terminal = run ~env ("doctor" :: options home p @ ["--terminal";terminal])
let ready result = field "exit_code" result=`Int 0
let plan_dir home id = Filename.concat home (".monty/runs/web-" ^ component id)
let check_roots p workspaces = List.iter (fun (w:Overview_types.task_workspace) ->
  let path=try Unix.realpath w.repo with _->fail 409 "task workspace is unavailable" in
  if path<>w.repo || not(List.exists (fun root->within (str root) path) (xs "roots" p)) then
    fail 403 "task workspace is outside the selected account's configured roots") workspaces
let check_account p (record:Job_store.record) =
  let environment=match record.launch_script with
    | None -> None
    | Some path ->
        (match value(State_store.lstat (safe path)) with
        | Some {Unix.st_kind=Unix.S_REG;_} -> Launcher.script_environment (Shell.read_file path)
        | _ -> None) in
  let harness,expected=account p in
  let key=Harness.account_variable harness in
  match environment with
  | Some (_,bindings) when List.assoc_opt key bindings=Some(Some expected) -> ()
  (* A worker launched without the variable runs on the default login. *)
  | Some (_,bindings) when List.assoc_opt key bindings=Some None && Harness.uses_default_login ~getenv:Sys.getenv_opt harness expected -> ()
  | _ -> fail 409 "the selected account does not match this worker's saved launch environment; select its original account or inspect its CLI recovery"
let prepare ~home ~profiles ~make_env ~id request =
  let key,before=current home request in
  let p=profile profiles (required "profile" request) in
  let settings=settings home in
  let action=required "action" request in
  let task=task home key in
  let record=worker home key task in
  let projects=projects home in
  let workspaces=match Task_storage.planned_workspaces projects task with
    | [] -> [Overview_types.{repo=s "project_repo" before;
        branch=Slug.branch ~prefix:(value(Settings.effective_branch_prefix ~getenv:Sys.getenv_opt ~home None)) task.title}]
    | workspaces -> workspaces in
  check_roots p workspaces;
  let dir=plan_dir home id in
  let manifest=Filename.concat dir "jobs.json" in
  let context_path=Filename.concat dir "context.md" in
  let context,files,args=match action,record with
    | "launch",None ->
        if String.lowercase_ascii task.status<>"open" then fail 409 "only an open task can be launched";
        let context=bounded 65536 (field "context" request) in
        if String.trim context="" then fail 400 "a standalone task context is required";
        let context=String.concat "\n" ["# " ^ task.title;"";"Task: " ^ key;"";"## Workspaces";
          String.concat "\n" (List.map (fun (w:Overview_types.task_workspace)->"- " ^ w.repo ^ " · " ^ w.branch) workspaces);"";context;""] in
        let agent_profile=value(Settings.effective_agent_profile ~home None) in
        let json=obj ["jobs",`List [obj ["id",string ("web-" ^ task.id);"title",string task.title;"task_key",string key;
          "workspaces",`List(List.map Task_storage.json_of_task_workspace workspaces);
          "worker_dir",string(Filename.concat dir ("workers/web-" ^ task.id));"context",string context_path;"agent_profile",string agent_profile]]] in
        let manifest_text=Yojson.Safe.pretty_to_string json ^ "\n" in
        context,[context_path,context;manifest,manifest_text],["launch-many";"--manifest";manifest]
    | "resume",Some r ->
        check_account p r;
        "",[],["resume";key] @ (if String.lowercase_ascii task.status="done" then ["--archived"] else [])
    | "launch",Some _ -> fail 409 "this task already owns a worker; review a resume instead"
    | "resume",None -> fail 409 "this task has no worker to resume"
    | _ -> fail 400 "task preflight action must be launch or resume" in
  locked home (fun () ->
    ignore(current home request);
    get(State_store.ensure_real_directory ~label:"task plan" ~mode:0o700 dir);
    List.iter (fun (path,contents)->get(State_store.write_file_atomic ~path ~perm:0o600 contents)) files);
  let env=environment make_env home p in
  let role=role home key in
  let terminal=terminal home in
  doctor ~env home p terminal >>= fun health ->
  (if ready health then run ~env (args @ options home p @ ["--terminal";"dry-run"])
   else Lwt.return (obj ["exit_code",`Int 1;"stdout",string "";"stderr",string "Resolve doctor failures before preflight."])) >|= fun preview ->
  let plan=obj ["task",string key;"revision",field "revision" before;"action",string action;
    "profile",string(s "id" p);"profile_hash",string(sha(Yojson.Safe.to_string p));
    "settings",Settings.to_json settings;"role",role;"wt_command",string(wt_command ());"terminal",string terminal;
    "args",`List(List.map string args);"files",`List(List.map (fun (path,contents)->obj["path",string path;"sha256",string(sha contents)]) files)] in
  locked home (fun () -> get(State_store.write_json_atomic ~path:(Filename.concat dir "plan.json") plan));
  obj ["task",string key;"preflight",string id;"action",string action;"ready",`Bool(ready health && ready preview);
    "context",string context;"doctor",health;"preview",preview;"profile",string(s "id" p);"agent_profile",field "id" role;"terminal",string terminal;
    "workspaces",`List(List.map Task_storage.json_of_task_workspace workspaces)]
let apply ~home ~profiles ~make_env ~id request =
  if field "confirm" request<>`Bool true then fail 400 "review the preflight and confirm this task action";
  let preflight=required "preflight" request in
  let receipt=receipt home preflight in
  if s "status" receipt<>"finished" || field "ready" (field "result" receipt)<>`Bool true then fail 409 "a successful preflight is required";
  let dir=plan_dir home preflight in
  let plan=read (Filename.concat dir "plan.json") in
  let p=profile profiles (s "profile" plan) in
  let validate () =
    ignore(current home plan);
    (match worker home (s "task" plan) (task home (s "task" plan)) with Some r->check_account p r|None->());
    if field "settings" plan<>Settings.to_json(settings home) || s "wt_command" plan<>wt_command () || s "terminal" plan<>terminal home ||
       field "role" plan<>role home (s "task" plan) ||
       s "profile_hash" plan<>sha(Yojson.Safe.to_string p) then fail 409 "launch configuration changed; run preflight again";
    List.iter (fun file ->
      if sha(Shell.read_file (s "path" file))<>s "sha256" file then fail 409 "task context or manifest changed; run preflight again") (xs "files" plan);
    if Sys.file_exists (Filename.concat dir "requested.json") then fail 409 "this preflight was already submitted; inspect its receipt and worker before requesting a resume" in
  locked home validate;
  let env=environment make_env home p in
  let terminal=s "terminal" plan in
  doctor ~env home p terminal >>= fun health ->
  if not(ready health) then Lwt.return(obj["task",field "task" plan;"requested",`Bool false;"doctor",health]) else
  let args=List.map str (xs "args" plan) @ options home p in
  run ~env (args @ ["--terminal";"dry-run"]) >>= fun preview ->
  if not(ready preview) then Lwt.return(obj["task",field "task" plan;"requested",`Bool false;"preview",preview]) else (
    locked home (fun () -> validate ();
      get(State_store.write_json_atomic ~path:(Filename.concat dir "requested.json") (obj["command",string id])));
    run ~env (args @ ["--terminal";terminal]) >|= fun result ->
    obj ["task",field "task" plan;"requested",`Bool true;"action",field "action" plan;"terminal",string terminal;"process",result])
let execute ~home ~profiles ~make_env ~id request = match required "op" request with
  | "task_create" ->
      let title=String.trim(bounded 500 (field "title" request)) in
      if title="" then fail 400 "task title is required";
      let notes=match field "notes" request with `Null|`String ""->None|v->Some(bounded 65536 v) in
      let project=value(Project_storage.resolve_project (projects home) (required "project" request)) in
      let task=get(Task_storage.add_local_task ~home ~project:project.id ~title ?notes ()) in
      Lwt.return(put "key" (string("local:" ^ task.id)) (Task_storage.json_of_local_task task))
  | "task_preflight" -> prepare ~home ~profiles ~make_env ~id request
  | "task_apply" -> apply ~home ~profiles ~make_env ~id request
  | "task_complete" ->
      if field "confirm" request<>`Bool true then fail 400 "confirm task completion";
      let key,_=current home request in
      (match worker home key (task home key) with
      | None -> get(Task_storage.done_local_task ~home key);Lwt.return(obj["task",string key;"action",string "complete"])
      | Some _ ->
          let failures=Doctor.state_checks ~home ~wt_command:(wt_command ()) () |> List.filter (fun c->c.Doctor.level=Doctor.Fail) in
          if failures<>[] then fail 409 (String.concat "\n" (List.map (fun c->c.Doctor.message) failures));
          ignore(current home request);
          run ~env:(make_env ["MONTY_HOME=" ^ home]) ["done";key;"--home";home;"--wt-command";wt_command ()] >|= fun result ->
          obj ["task",string key;"action",string "complete";"process",result])
  | _ -> Lwt.fail(Failure(400,"unknown task command"))
