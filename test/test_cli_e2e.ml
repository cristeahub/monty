open Monty

let failf fmt = Printf.ksprintf failwith fmt

let string_contains text needle =
  let text_length = String.length text in
  let needle_length = String.length needle in
  let rec loop offset =
    needle_length = 0
    || (offset + needle_length <= text_length
       && (String.sub text offset needle_length = needle || loop (offset + 1)))
  in
  loop 0

let require_contains label text needle =
  if not (string_contains text needle) then
    failf "%s: expected %S to contain %S" label text needle

let require_line label text expected =
  if
    not
      (text |> String.split_on_char '\n'
      |> List.exists (fun line -> String.equal line expected))
  then failf "%s: expected an exact line %S in:\n%s" label expected text

let require_empty_log log =
  if Sys.file_exists log && String.trim (Shell.read_file log) <> "" then
    failf "unexpected fake external command log:\n%s" (Shell.read_file log)

let rec remove_tree path =
  try
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR ->
        Sys.readdir path
        |> Array.iter (fun name -> remove_tree (Filename.concat path name));
        Unix.rmdir path
    | _ -> Unix.unlink path
  with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> ()

let rec tree_has path predicate =
  match Unix.lstat path with
  | exception Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> false
  | { Unix.st_kind = Unix.S_DIR; _ } ->
      Sys.readdir path |> Array.exists (fun name -> tree_has (Filename.concat path name) predicate)
  | { Unix.st_kind = Unix.S_REG; _ } -> predicate path
  | _ -> false

let wait_for_path path =
  let rec loop remaining =
    if Sys.file_exists path then ()
    else if remaining = 0 then failf "timed out waiting for %s" path
    else (
      ignore (Unix.select [] [] [] 0.02);
      loop (remaining - 1))
  in
  loop 250

let executable =
  let test_dir = Shell.abs_path Sys.executable_name |> Filename.dirname in
  let candidate = Filename.concat test_dir "../bin/main.exe" |> Shell.normalize in
  if not (Sys.file_exists candidate) then failf "built Monty binary is missing: %s" candidate;
  Unix.realpath candidate

let test_servers : (string * string array) list ref = ref []
let env_value env name = Array.to_list env |> List.find_map(fun value ->
  let prefix=name ^ "=" in if String.starts_with ~prefix value then Some(String.sub value (String.length prefix)(String.length value-String.length prefix)) else None)
let prepare_test_server ~root ~env args =
  if List.exists(fun command->List.mem command ["run";"resume";"run-many"]) args && List.hd args="headless" then (
    let home=Option.value ~default:(Filename.concat root "home")(env_value env "MONTY_HOME") in
    let account harness variable fallback =
      let directory=Option.value ~default:(Filename.concat root fallback)(env_value env variable) in
      Shell.ensure_dir directory;
      let directory=Unix.realpath directory in
      let command=Option.value ~default:harness(env_value env ("MONTY_" ^ String.uppercase_ascii harness ^ "_COMMAND")) in
      let wrapper=Filename.concat root ("server-" ^ harness) in
      Shell.write_file wrapper ("#!/bin/sh\nexec " ^ command ^ " \"$@\"\n");Shell.chmod_executable wrapper;
      `Assoc["id",`String harness;"harness",`String harness;"command",`String((if String.starts_with ~prefix:(harness ^ " ") command || command=harness then Shell.quote(Filename.concat root ("fake-bin/" ^ harness)) ^ String.sub command (String.length harness)(String.length command-String.length harness) else command));
        (if harness="pi" then "pi_home" else "codex_home"),`String directory;"roots",`List[`String(Unix.realpath root)]] in
    let profiles=List.map(fun (h,v,f)->account h v f) ["pi","PI_CODING_AGENT_DIR","pi-agent";"codex","CODEX_HOME","codex-home"] in
    let config=Filename.concat root "server-profiles.json" in
    Yojson.Safe.to_file config (`Assoc["profiles",`List profiles]);
    let metadata=Filename.concat home ".monty/client-service.json" in
    if not(Sys.file_exists metadata) then (
      let log=Unix.openfile(Filename.concat root "test-service.log")[Unix.O_CREAT;Unix.O_APPEND;Unix.O_WRONLY] 0o600 in
      let child=Unix.create_process_env executable [|executable;"service";"start";"--home";home;"--config";config|] env Unix.stdin log log in
      Unix.close log;ignore(Unix.waitpid [] child);
      test_servers:=(home,env)::!test_servers))

let with_temp_root name f =
  let marker = Filename.temp_file ("monty-cli-" ^ name ^ "-") ".tmp" in
  Sys.remove marker;
  Unix.mkdir marker 0o700;
  Fun.protect ~finally:(fun () ->
    List.iter(fun (home,env) -> if String.starts_with ~prefix:(Unix.realpath marker) (Unix.realpath home) then (
      let null=Unix.openfile "/dev/null" [Unix.O_RDWR] 0 in
      let pid=Unix.create_process_env executable [|executable;"service";"stop";"--home";home|] env null null null in
      Unix.close null;ignore(Unix.waitpid [] pid))) !test_servers;
    test_servers:=List.filter(fun (home,_)->not(String.starts_with ~prefix:(Unix.realpath marker) (Unix.realpath home))) !test_servers;
    remove_tree marker) (fun () -> f marker)

let read_file path = if Sys.file_exists path then Shell.read_file path else ""

let reviewed_profile =
  Agent_profile.
    { id = "reviewed";
      description = "test reviewed profile";
      interactive = "Run /review.";
      implementation = "Implement and validate the task.";
      reviews =
        [ { id = "correctness";
            title = "Correctness review";
            instructions = "Review correctness." };
          { id = "quality";
            title = "Quality and tests review";
            instructions = "Review quality and tests." } ];
      fix = Some "Verify findings and fix valid issues.";
      fix_role = Fixer }

let solo_profile =
  Agent_profile.
    { id = "solo";
      description = "test solo profile";
      interactive = "Self-validate the task.";
      implementation = "Implement, self-validate, and report the task.";
      reviews = [];
      fix = None; fix_role = Fixer }

let install_profile home (profile : Agent_profile.t) =
  let directory = Filename.concat (Filename.concat home "agent-profiles") profile.id in
  Shell.ensure_dir directory;
  let instruction name contents =
    Shell.write_file (Filename.concat directory name) contents;
    `String name
  in
  let reviews =
    profile.reviews
    |> List.map (fun (review : Agent_profile.review) ->
           let name = review.id ^ ".md" in
           `Assoc
             [ ("id", `String review.id); ("title", `String review.title);
               ("instructions", instruction name review.instructions) ])
  in
  Yojson.Safe.to_file (Filename.concat directory "profile.json")
    (`Assoc
      [ ("schema", `String Agent_profile.schema); ("id", `String profile.id);
        ("description", `String profile.description);
        ("interactive", instruction "interactive.md" profile.interactive);
        ( "headless",
          `Assoc
            [ ("implementation",
               instruction "implementation.md" profile.implementation);
              ("reviews", `List reviews);
              ("fix_role", `String (Agent_profile.fix_role_name profile.fix_role));
              ( "fix",
                match profile.fix with
                | None -> `Null
                | Some fix -> instruction "fix.md" fix ) ] ) ])

let replace_env base overrides =
  let overridden key =
    List.exists (fun (name, _) -> String.equal key name) overrides
  in
  let base =
    base |> Array.to_list
    |> List.filter (fun entry ->
           match String.index_opt entry '=' with
           | None -> true
           | Some index -> not (overridden (String.sub entry 0 index)))
  in
  (base @ List.map (fun (name, value) -> name ^ "=" ^ value) overrides)
  |> Array.of_list

(* NO_COLOR keeps cmdliner diagnostics plain whatever TERM the test runner inherited,
   so assertions can match their text. *)
let env_with overrides = replace_env (Unix.environment () |> Array.to_list |> List.filter(fun value->not(String.starts_with ~prefix:"MONTY_" value)) |> Array.of_list) (("NO_COLOR", "1") :: overrides)

type child = {
  pid : int;
  stdout_path : string;
  stderr_path : string;
  argv : string array;
}

type result = {
  code : int;
  stdout : string;
  stderr : string;
  argv : string array;
}

let spawn ?cwd ?(program = executable) ?(stdin_path = "/dev/null") ?(new_session = false) ~root ~env index args =
  if program=executable && args<>[] then prepare_test_server ~root ~env args;
  let stdout_path = Filename.concat root (Printf.sprintf "stdout-%d" index) in
  let stderr_path = Filename.concat root (Printf.sprintf "stderr-%d" index) in
  let stdout_fd =
    Unix.openfile stdout_path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600
  in
  let stderr_fd =
    Unix.openfile stderr_path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600
  in
  let stdin_fd = Unix.openfile stdin_path [ Unix.O_RDONLY ] 0 in
  let argv = Array.of_list (program :: args) in
  let pid =
    if not new_session && Option.is_none cwd then Unix.create_process_env program argv env stdin_fd stdout_fd stderr_fd
    else match Unix.fork () with
      | 0 ->
          if new_session then ignore (Unix.setsid ());
          Option.iter Unix.chdir cwd;
          Unix.dup2 stdin_fd Unix.stdin;
          Unix.dup2 stdout_fd Unix.stdout;
          Unix.dup2 stderr_fd Unix.stderr;
          Unix.close stdin_fd; Unix.close stdout_fd; Unix.close stderr_fd;
          Unix.execve program argv env
      | pid -> pid
  in
  Unix.close stdin_fd;
  Unix.close stdout_fd;
  Unix.close stderr_fd;
  { pid; stdout_path; stderr_path; argv }

let await child =
  let _, status = Unix.waitpid [] child.pid in
  let code =
    match status with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED signal -> 128 + signal
    | Unix.WSTOPPED signal -> 128 + signal
  in
  {
    code;
    stdout = read_file child.stdout_path;
    stderr = read_file child.stderr_path;
    argv = child.argv;
  }

let run ?cwd ?program ~root ~env index args = spawn ?cwd ?program ~root ~env index args |> await

let command result = result.argv |> Array.to_list |> String.concat " "

let require_code expected result =
  if result.code <> expected then
    failf "command exited %d instead of %d: %s\nstdout:\n%s\nstderr:\n%s"
      result.code expected (command result) result.stdout result.stderr

let setup_environment ?(agent_profile = Some "reviewed") root =
  let home = Filename.concat root "home" in
  let fake_bin = Filename.concat root "fake-bin" in
  let log = Filename.concat root "fake-tools.log" in
  Shell.ensure_dir fake_bin;
  install_profile home reviewed_profile;
  install_profile home solo_profile;
  (* Existing scenarios exercise the explicit historical profile. *)
  Option.iter (fun _ ->
      Shell.ensure_dir (Filename.concat home ".monty");
      Yojson.Safe.to_file (Settings.path ~home)
        (Settings.to_json { Settings.empty with agent_profile })) agent_profile;
  List.iter
    (fun name ->
      let path = Filename.concat fake_bin name in
      Shell.write_file path
        (String.concat "\n"
           [ "#!/bin/sh";
             "printf '%s\\n' \"$0 $*\" >> " ^ Shell.quote log;
             "exit 97";
             "" ]);
      Shell.chmod_executable path)
    [ "wt"; "gh"; "pi"; "codex"; "osascript"; "sdef"; "ghostty" ];
  (* The default terminal is Rex; these scenarios pin Ghostty through the
     environment and keep a fake rex whose server check passes for fallbacks. *)
  let rex = Filename.concat fake_bin "rex" in
  Shell.write_file rex
    (String.concat "\n"
       [ "#!/bin/sh"; "printf '%s\\n' \"$0 $*\" >> " ^ Shell.quote log;
         "printf '%s\\n' '{\"endpoint\":\"unix:///fake/rex.sock\"}'"; "" ]);
  Shell.chmod_executable rex;
  (* An empty HOME keeps the operator's real account directories out of the
     service configuration Monty generates for a first launch. *)
  let user_home = Filename.concat root "user-home" in
  Shell.ensure_dir user_home;
  let env =
    env_with
      [ ("MONTY_HOME", home);
        ("HOME", user_home);
        ("MONTY_TERMINAL", "ghostty");
        ("PI_CODING_AGENT_DIR", Filename.concat root "pi-agent");
        ("MANTLE_CONTEXT", "");
        ("CODEX_HOME", Filename.concat root "codex-home");
        ("PATH", fake_bin ^ ":/usr/bin:/bin");
        ("MONTY_WT_COMMAND", "wt");
        ("MONTY_PI_COMMAND", "pi") ]
  in
  (home, log, env)

let add_project ~root ~home ~env repo =
  Shell.ensure_dir repo;
  let result =
    run ~root ~env 1 [ "projects"; "add"; "--home"; home; "--repo"; repo ]
  in
  require_code 0 result

let list_unique values =
  let table = Hashtbl.create (List.length values) in
  List.iter (fun value -> Hashtbl.replace table value ()) values;
  Hashtbl.length table

let test_concurrent_task_adds_keep_unique_tasks () =
  with_temp_root "concurrent" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      add_project ~root ~home ~env repo;
      let children =
        List.init 36 (fun index ->
            let number = index + 1 in
            spawn ~root ~env (number + 10)
              [ "task";
                "add";
                "--home";
                home;
                "--project";
                "repo";
                "--title";
                Printf.sprintf "Concurrent task %d" number ])
      in
      let results = List.map await children in
      List.iter (require_code 0) results;
      let path = Filename.concat home ".monty/tasks.local.json" in
      let tasks =
        match Yojson.Safe.from_file path |> Yojson.Safe.Util.member "tasks" with
        | `List tasks -> tasks
        | _ -> failwith "tasks.local.json did not contain a tasks array"
      in
      let strings field =
        List.map
          (fun json ->
            match Yojson.Safe.Util.member field json with
            | `String value -> value
            | _ -> failf "task missing string field %s" field)
          tasks
      in
      if List.length tasks <> 36 then failf "expected 36 tasks, got %d" (List.length tasks);
      if list_unique (strings "id") <> 36 then failwith "task IDs were not unique";
      if list_unique (strings "title") <> 36 then failwith "task titles were lost";
      if Sys.file_exists log && String.trim (Shell.read_file log) <> "" then
        failwith "task mutations unexpectedly invoked a fake external command")

let test_malformed_json_is_not_overwritten () =
  with_temp_root "malformed" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      add_project ~root ~home ~env repo;
      let path = Filename.concat home ".monty/tasks.local.json" in
      let malformed = "{not-json\n" in
      Shell.write_file path malformed;
      let result =
        run ~root ~env 2
          [ "task";
            "add";
            "--home";
            home;
            "--project";
            "repo";
            "--title";
            "Must fail" ]
      in
      if result.code = 0 then failwith "malformed JSON mutation unexpectedly succeeded";
      require_contains "malformed JSON diagnostic" result.stderr ("invalid JSON in " ^ path);
      if not (String.equal malformed (Shell.read_file path)) then
        failwith "malformed JSON was overwritten";
      require_empty_log log)

let write_manifest path jobs =
  Shell.ensure_dir (Filename.dirname path);
  Yojson.Safe.to_file path (`Assoc [ ("jobs", `List jobs) ])

let manifest_job ?id ?branch ?task_key ?agent_profile ~title ~repo ~context () =
  `Assoc
    ([ ("title", `String title); ("repo", `String repo);
       ("context", `String context) ]
    @ (match id with None -> [] | Some value -> [ ("id", `String value) ])
    @ (match branch with
      | None -> []
      | Some value -> [ ("branch", `String value) ])
    @
    (match task_key with
    | None -> []
    | Some value -> [ ("task_key", `String value) ])
    @
    match agent_profile with
    | None -> []
    | Some value -> [ ("agent_profile", `String value) ])

let count_lines_containing path needle =
  read_file path |> String.split_on_char '\n'
  |> List.filter (fun line -> string_contains line needle)
  |> List.length

let test_dry_run_rejects_unsafe_manifest_before_side_effects () =
  with_temp_root "unsafe-manifest" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "task.md" in
      let manifest = Filename.concat home ".monty/runs/run-1/jobs.json" in
      let escaped = Filename.concat root "escaped-worker" in
      Shell.ensure_dir repo;
      Shell.write_file context "# Task\n";
      write_manifest manifest
        [ `Assoc
            [ ("id", `String "safe-id");
              ("title", `String "Unsafe path");
              ("repo", `String repo);
              ("context", `String context);
              ("worker_dir", `String escaped) ] ];
      let result =
        run ~root ~env 3
          [ "launch-many";
            "--terminal";
            "dry-run";
            "--home";
            home;
            "--manifest";
            manifest ]
      in
      if result.code = 0 then failwith "unsafe worker path passed dry-run validation";
      if Sys.file_exists escaped then failwith "unsafe worker directory was created";
      require_empty_log log;
      let result =
        run ~root ~env 4
          [ "launch-many";
            "--terminal";
            "ghostty";
            "--home";
            home;
            "--manifest";
            manifest ]
      in
      if result.code = 0 then failwith "unsafe worker path passed real-launch validation";
      if Sys.file_exists escaped then failwith "unsafe real launch created its worker directory";
      require_empty_log log;
      write_manifest manifest
        [ `Assoc
            [ ("id", `String "../escape");
              ("title", `String "Unsafe id");
              ("repo", `String repo);
              ("context", `String context) ] ];
      let result =
        run ~root ~env 5
          [ "launch-many";
            "--terminal";
            "dry-run";
            "--home";
            home;
            "--manifest";
            manifest ]
      in
      if result.code = 0 then failwith "traversal worker id passed manifest validation";
      require_empty_log log)

let test_readme_worker_path_is_home_relative () =
  with_temp_root "readme-worker-path" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "task.md" in
      let manifest = Filename.concat home ".monty/runs/run-1/jobs.json" in
      Shell.ensure_dir repo;
      Shell.write_file context "# Task\n";
      add_project ~root ~home ~env repo;
      write_manifest manifest
        [ `Assoc
            [ ("id", `String "issue-123");
              ("title", `String "README worker path");
              ("repo", `String repo);
              ("context", `String context);
              ( "worker_dir",
                `String ".monty/runs/run-1/workers/issue-123" ) ] ];
      let result =
        run ~root ~env 10
          [ "launch-many";
            "--terminal";
            "dry-run";
            "--home";
            home;
            "--manifest";
            manifest ]
      in
      require_code 0 result;
      require_contains "README worker path" result.stdout
        (Filename.concat home ".monty/runs/run-1/workers/issue-123");
      require_empty_log log)

let test_readme_context_path_is_home_relative () =
  with_temp_root "readme-context-path" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let relative_context = ".monty/runs/run-1/task.md" in
      let context = Filename.concat home relative_context in
      let manifest = Filename.concat home ".monty/runs/run-1/jobs.json" in
      Shell.ensure_dir repo;
      Shell.write_file context "# Task\n";
      add_project ~root ~home ~env repo;
      write_manifest manifest
        [ `Assoc
            [ ("id", `String "issue-123");
              ("title", `String "README context path");
              ("repo", `String repo);
              ("context", `String relative_context);
              ( "worker_dir",
                `String ".monty/runs/run-1/workers/issue-123" ) ] ];
      let result =
        run ~root ~env 10
          [ "launch-many";
            "--terminal";
            "dry-run";
            "--home";
            home;
            "--manifest";
            manifest ]
      in
      require_code 0 result;
      require_contains "README context path" result.stdout context;
      require_empty_log log)

let test_cli_atomic_fault_preserves_previous_json () =
  with_temp_root "atomic-fault" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      add_project ~root ~home ~env repo;
      let first =
        run ~root ~env 20
          [ "task"; "add"; "--home"; home; "--project"; "repo"; "--title"; "First" ]
      in
      require_code 0 first;
      let path = Filename.concat home ".monty/tasks.local.json" in
      let previous = Shell.read_file path in
      let fault_env =
        replace_env env [ ("MONTY_FAULT_INJECT", "state-store-before-rename") ]
      in
      let result =
        run ~root ~env:fault_env 21
          [ "task"; "add"; "--home"; home; "--project"; "repo"; "--title"; "Second" ]
      in
      if result.code = 0 then failwith "fault-injected task mutation unexpectedly succeeded";
      require_contains "atomic fault" result.stderr "fault injected before atomic state rename";
      if Shell.read_file path <> previous then
        failwith "fault-injected CLI mutation changed the previous task JSON";
      let temporary_files =
        Sys.readdir (Filename.dirname path) |> Array.to_list
        |> List.filter (fun name -> string_contains name "monty-tmp")
      in
      if temporary_files <> [] then failwith "fault-injected CLI mutation left a temp file";
      require_empty_log log)

let test_concurrent_project_adds_keep_every_project () =
  with_temp_root "concurrent-projects" (fun root ->
      let home, log, env = setup_environment root in
      let repos =
        List.init 20 (fun index ->
            let repo = Filename.concat root (Printf.sprintf "repo-%02d" (index + 1)) in
            Shell.ensure_dir repo;
            repo)
      in
      let children =
        List.mapi
          (fun index repo ->
            spawn ~root ~env (100 + index)
              [ "projects"; "add"; "--home"; home; "--repo"; repo ])
          repos
      in
      List.map await children |> List.iter (require_code 0);
      let projects =
        Yojson.Safe.from_file (Filename.concat home ".monty/projects.json")
        |> Yojson.Safe.Util.member "projects"
      in
      (match projects with
      | `List values when List.length values = 20 -> ()
      | `List values -> failf "expected 20 projects, got %d" (List.length values)
      | _ -> failwith "projects.json did not contain a projects array");
      require_empty_log log)

let job_json ~id ~repo ~context ~worker_dir ~run_dir =
  `Assoc
    [ ("id", `String id);
      ("title", `String "Unsafe persisted worker");
      ("repo", `String repo);
      ("branch", `String "cto/unsafe-worker");
      ("context", `String context);
      ("worker_dir", `String worker_dir);
      ("run_dir", `String run_dir);
      ("worktree_mode", `String "never");
      ("status", `String "active") ]

let test_cli_rejects_unsafe_persisted_worker_before_external_commands () =
  with_temp_root "unsafe-persisted-worker" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      let run_dir = Filename.concat home ".monty/runs/run-1" in
      let worker_dir = Filename.concat run_dir "workers/worker-1" in
      let job_file = Filename.concat worker_dir "job.json" in
      Shell.ensure_dir repo;
      Shell.write_file context "# Context\n";
      let write id persisted_worker persisted_run =
        Shell.ensure_dir worker_dir;
        Yojson.Safe.to_file job_file
          (job_json ~id ~repo ~context ~worker_dir:persisted_worker
             ~run_dir:persisted_run)
      in
      let reject index label =
        let result =
          run ~root ~env index
            [ "done"; "worker-1"; "--home"; home; "--wt-command"; "wt" ]
        in
        if result.code = 0 then failf "%s unexpectedly passed CLI validation" label;
        require_empty_log log
      in
      write "different-id" worker_dir run_dir;
      reject 200 "persisted id mismatch";
      write "worker-1" (Filename.concat root "outside-worker") run_dir;
      reject 201 "persisted worker_dir mismatch";
      write "worker-1" worker_dir (Filename.concat root "outside-run");
      reject 202 "persisted run_dir mismatch";
      remove_tree worker_dir;
      let outside = Filename.concat root "outside-worker" in
      Shell.ensure_dir outside;
      Yojson.Safe.to_file (Filename.concat outside "job.json")
        (job_json ~id:"worker-1" ~repo ~context ~worker_dir:outside ~run_dir);
      Unix.symlink outside worker_dir;
      reject 203 "worker directory symlink escape")

let test_cli_rejects_state_parent_and_lock_symlinks () =
  with_temp_root "state-symlinks" (fun root ->
      let home, log, env = setup_environment ~agent_profile:None root in
      let repo = Filename.concat root "repo" in
      let outside_parent = Filename.concat root "outside-state" in
      Shell.ensure_dir home;
      Shell.ensure_dir repo;
      Shell.ensure_dir outside_parent;
      Unix.symlink outside_parent (Filename.concat home ".monty");
      let result =
        run ~root ~env 300 [ "projects"; "add"; "--home"; home; "--repo"; repo ]
      in
      if result.code = 0 then failwith "symlinked .monty parent unexpectedly succeeded";
      require_contains "state parent symlink" result.stderr "state parent is a symlink";
      if Sys.readdir outside_parent |> Array.length <> 0 then
        failwith "state-parent rejection mutated the symlink target";
      Unix.unlink (Filename.concat home ".monty");
      add_project ~root ~home ~env repo;
      let lock_path = Filename.concat home ".monty/state.lock" in
      Unix.unlink lock_path;
      let lock_target = Filename.concat root "outside-lock" in
      let sentinel = "outside lock must stay unchanged\n" in
      Shell.write_file lock_target sentinel;
      Unix.symlink lock_target lock_path;
      let result =
        run ~root ~env 301
          [ "task"; "add"; "--home"; home; "--project"; "repo"; "--title"; "Blocked" ]
      in
      if result.code = 0 then failwith "symlinked state lock unexpectedly succeeded";
      require_contains "state lock symlink" result.stderr "state lock is a symlink";
      if Shell.read_file lock_target <> sentinel then
        failwith "state-lock rejection changed the symlink target";
      require_empty_log log)

let test_external_terminal_request_runs_without_state_lock () =
  with_temp_root "external-without-lock" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      add_project ~root ~home ~env repo;
      Shell.write_file context "# Context\n";
      let osascript = Filename.concat root "fake-bin/osascript" in
      Shell.write_file osascript
        (String.concat "\n"
           [ "#!/bin/sh";
             "set -eu";
             "printf '%s\\n' \"$0 $*\" >> " ^ Shell.quote log;
             Shell.quote executable
             ^ " task add --home \"$MONTY_HOME\" --project repo --title 'Nested terminal task' >/dev/null";
             "" ]);
      Shell.chmod_executable osascript;
      let result =
        run ~root ~env 400
          [ "launch";
            "--terminal";
            "ghostty";
            "--worktree";
            "never";
            "--home";
            home;
            "--repo";
            repo;
            "--title";
            "Lock probe";
            "--context";
            context;
            "--branch";
            "cto/lock-probe" ]
      in
      require_code 0 result;
      require_contains "terminal fake invocation" (Shell.read_file log) "osascript";
      let tasks =
        Yojson.Safe.from_file (Filename.concat home ".monty/tasks.local.json")
        |> Yojson.Safe.Util.member "tasks"
      in
      match tasks with
      | `List [ _; _ ] -> ()
      | _ -> failwith "nested terminal task mutation did not complete outside the state lock")

let test_codex_launch_uses_command_local_trust () =
  with_temp_root "codex-trust" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      let codex_config = Filename.concat root "codex-home/config.toml" in
      add_project ~root ~home ~env repo;
      Shell.write_file context "# Codex trust context\n";
      let osascript = Filename.concat root "fake-bin/osascript" in
      Shell.write_file osascript
        (String.concat "\n"
           [ "#!/bin/sh";
             "printf '%s\\n' \"$0 $*\" >> " ^ Shell.quote log;
             "exit 0";
             "" ]);
      Shell.chmod_executable osascript;
      let common =
        [ "launch";
          "--home";
          home;
          "--repo";
          repo;
          "--title";
          "Codex trusted worker";
          "--context";
          context;
          "--branch";
          "cto/codex-trusted-worker";
          "--worktree";
          "never";
          "--harness";
          "codex" ]
      in
      let dry = run ~root ~env 410 (common @ [ "--terminal"; "dry-run" ]) in
      require_code 0 dry;
      if Sys.file_exists codex_config then
        failwith "Codex dry-run changed the Codex config";
      let launched =
        run ~root ~env 411 (common @ [ "--terminal"; "ghostty" ])
      in
      require_code 0 launched;
      if Sys.file_exists codex_config then
        failwith "Codex launch changed the Codex config";
      let job_file =
        Filename.concat home
          ".monty/runs/manual/workers/codex-trusted-worker/job.json"
      in
      let launch_script =
        match
          Yojson.Safe.from_file job_file
          |> Yojson.Safe.Util.member "launch_script"
        with
        | `String path -> path
        | _ -> failwith "Codex worker did not record its launch script"
      in
      require_contains "Codex uses service" (Shell.read_file launch_script)
        " terminal --home ";
      require_contains "Codex terminal requested" (Shell.read_file log) "osascript")

let local_task_status home =
  match
    Yojson.Safe.from_file (Filename.concat home ".monty/tasks.local.json")
    |> Yojson.Safe.Util.member "tasks"
  with
  | `List [ task ] -> (
      match Yojson.Safe.Util.member "status" task with
      | `String status -> status
      | _ -> failwith "local task status is missing")
  | _ -> failwith "expected exactly one local task"

let lifecycle_job_json ?(id = "worker-1") ?(title = "Lifecycle worker")
    ?(branch = "cto/lifecycle-worker") ?(worktree_mode = "never")
    ?(status = "active") ?last_known_worktree ?task_key ~repo ~context
    ~worker_dir ~run_dir () =
  `Assoc
    ([ ("id", `String id);
       ("title", `String title);
       ("repo", `String repo);
       ("branch", `String branch);
       ("context", `String context);
       ("worker_dir", `String worker_dir);
       ("run_dir", `String run_dir);
       ("worktree_mode", `String worktree_mode);
       ("status", `String status) ]
    @ (match last_known_worktree with
      | None -> []
      | Some value -> [ ("last_known_worktree", `String value) ])
    @ (match task_key with None -> [] | Some value -> [ ("task_key", `String value) ]))

let job_status path =
  match Yojson.Safe.from_file path |> Yojson.Safe.Util.member "status" with
  | `String status -> status
  | _ -> failwith "job status is missing"

let test_lifecycle_faults_recover_from_both_locations () =
  let completion_faults =
    [ "complete-after-intent"; "complete-after-cleanup"; "complete-after-move";
      "complete-after-normalize"; "complete-after-task";
      "complete-before-finalize" ]
  in
  let reopen_faults =
    [ "reopen-after-intent"; "reopen-after-move"; "reopen-after-normalize";
      "reopen-after-task"; "reopen-before-finalize" ]
  in
  List.iteri
    (fun scenario completion_fault ->
      with_temp_root ("lifecycle-" ^ string_of_int scenario) (fun root ->
          let home, _log, env = setup_environment root in
          let repo = Filename.concat root "repo" in
          let context = Filename.concat root "context.md" in
          let run_dir = Filename.concat home ".monty/runs/run-1" in
          let active_dir = Filename.concat run_dir "workers/worker-1" in
          let archive_dir = Filename.concat run_dir "archive/worker-1" in
          add_project ~root ~home ~env repo;
          Shell.write_file context "# Lifecycle\n";
          let added =
            run ~root ~env 500
              [ "task"; "add"; "--home"; home; "--project"; "repo";
                "--title"; "Lifecycle worker" ]
          in
          require_code 0 added;
          Shell.ensure_dir active_dir;
          Yojson.Safe.to_file (Filename.concat active_dir "job.json")
            (lifecycle_job_json ~task_key:"local:local-001" ~repo ~context
               ~worker_dir:active_dir ~run_dir ());
          let fault_env =
            replace_env env [ ("MONTY_FAULT_INJECT", completion_fault) ]
          in
          let interrupted =
            run ~root ~env:fault_env 501
              [ "done"; "worker-1"; "--home"; home; "--wt-command"; "wt" ]
          in
          if interrupted.code = 0 then
            failf "completion fault %s unexpectedly succeeded" completion_fault;
          require_contains "completion fault output" interrupted.stderr completion_fault;
          if scenario = 0 then (
            let doctor = run ~root ~env 502 [ "doctor"; "--home"; home ] in
            require_code 0 doctor;
            require_contains "doctor completion recovery" doctor.stdout
              "monty done 'worker-1' --home" );
          let recovered =
            run ~root ~env 503
              [ "done"; "worker-1"; "--home"; home; "--wt-command"; "wt" ]
          in
          require_code 0 recovered;
          if Sys.file_exists active_dir || not (Sys.file_exists archive_dir) then
            failwith "completion did not converge to the canonical archive";
          if job_status (Filename.concat archive_dir "job.json") <> "done" then
            failwith "completion did not finalize done";
          if local_task_status home <> "done" then
            failwith "completion finalized before closing its linked task";
          let reopen_fault =
            List.nth reopen_faults (scenario mod List.length reopen_faults)
          in
          let osascript = Filename.concat root "fake-bin/osascript" in
          Shell.write_file osascript "#!/bin/sh\nexit 0\n";
          Shell.chmod_executable osascript;
          let fault_env = replace_env env [ ("MONTY_FAULT_INJECT", reopen_fault) ] in
          let interrupted =
            run ~root ~env:fault_env 504
              [ "resume"; "--archived"; "worker-1"; "--home"; home;
                "--terminal"; "ghostty"; "--worktree"; "never" ]
          in
          if interrupted.code = 0 then
            failf "reopen fault %s unexpectedly succeeded" reopen_fault;
          require_contains "reopen fault output" interrupted.stderr reopen_fault;
          let recovered =
            run ~root ~env 505
              [ "resume"; "--archived"; "worker-1"; "--home"; home;
                "--terminal"; "ghostty"; "--worktree"; "never" ]
          in
          require_code 0 recovered;
          if not (Sys.file_exists active_dir) || Sys.file_exists archive_dir then
            failwith "reopening did not converge to the canonical active path";
          if
            job_status (Filename.concat active_dir "job.json")
            <> "launch-requested"
          then failwith "reopening did not persist the terminal request state";
          if local_task_status home <> "open" then
            failwith "reopening did not reopen its linked local task"))
    completion_faults

let init_git_repo path =
  Shell.ensure_dir path;
  let must_run command =
    match Process.run_quiet ~cwd:path command with
    | Ok () -> ()
    | Error msg -> failwith msg
  in
  must_run "git init -q";
  must_run "git config user.email monty@example.invalid";
  must_run "git config user.name Monty";
  Shell.write_file (Filename.concat path "tracked.txt") "tracked\n";
  must_run "git add tracked.txt";
  must_run "git commit -qm initial"

let install_create_wt ~root ~log =
  let worktrees = Filename.concat root "created-worktrees" in
  let wt = Filename.concat root "fake-bin/wt" in
  Shell.write_file wt
    (String.concat "\n"
       [ "#!/bin/sh";
         "set -eu";
         "printf '%s\\n' \"$0 $*\" >> " ^ Shell.quote log;
         "case \"$1\" in";
         "  b)";
         "    branch=$2";
         "    if [ \"${MONTY_TEST_WT_FAIL_BRANCH:-}\" = \"$branch\" ]; then exit 77; fi";
         "    safe=$(printf '%s' \"$branch\" | tr '/ ' '__')";
         "    worktree=" ^ Shell.quote worktrees ^ "/$safe";
         "    if [ ! -d \"$worktree\" ]; then";
         "      mkdir -p " ^ Shell.quote worktrees;
         "      git worktree add -q -b \"$branch\" \"$worktree\"";
         "    fi";
         "    printf '%s\\n' \"$worktree\"";
         "    ;;";
         "  list) printf '%s\\n' \"repo:\" ;;";
         "  *) exit 92 ;;";
         "esac";
         "" ]);
  Shell.chmod_executable wt

let install_repo_scoped_wt ~root ~log =
  let worktrees = Filename.concat root "multi-worktrees" in
  let state = Filename.concat root "multi-wt-state" in
  let wt = Filename.concat root "fake-bin/wt" in
  Shell.ensure_dir state;
  Shell.write_file wt
    (String.concat "\n"
       [ "#!/bin/sh";
         "set -eu";
         "printf '%s\\n' \"$0 $(pwd -P) $*\" >> " ^ Shell.quote log;
         "repo=$(pwd -P)";
         "repo_name=$(basename \"$repo\")";
         "case \"$1\" in";
         "  b)";
         "    branch=$2";
         "    if [ \"${MONTY_TEST_WT_FAIL_BRANCH:-}\" = \"$branch\" ]; then exit 77; fi";
         "    safe=$(printf '%s' \"$branch\" | tr '/ ' '__')";
         "    entry=" ^ Shell.quote state ^ "/${repo_name}_${safe}.entry";
         "    worktree=" ^ Shell.quote worktrees ^ "/${repo_name}_${safe}";
         "    if [ ! -f \"$entry\" ]; then";
         "      mkdir -p " ^ Shell.quote worktrees;
         "      git worktree add -q -b \"$branch\" \"$worktree\"";
         "      printf '%s\\n%s\\n' \"$branch\" \"$worktree\" > \"$entry\"";
         "    fi";
         "    sed -n '2p' \"$entry\"";
         "    ;;";
         "  list)";
         "    printf '%s:\\n' \"$repo_name\"";
         "    for entry in " ^ Shell.quote state ^ "/${repo_name}_*.entry; do";
         "      [ -f \"$entry\" ] || continue";
         "      branch=$(sed -n '1p' \"$entry\")";
         "      worktree=$(sed -n '2p' \"$entry\")";
         "      printf '  %s -> %s\\n' \"$branch\" \"$worktree\"";
         "    done";
         "    ;;";
         "  db)";
         "    branch=$2";
         "    safe=$(printf '%s' \"$branch\" | tr '/ ' '__')";
         "    entry=" ^ Shell.quote state ^ "/${repo_name}_${safe}.entry";
         "    if [ -f \"$entry\" ]; then";
         "      worktree=$(sed -n '2p' \"$entry\")";
         "      git worktree remove -f \"$worktree\"";
         "      git branch -D \"$branch\" >/dev/null";
         "      rm -f \"$entry\"";
         "    fi";
         "    ;;";
         "  *) exit 92 ;;";
         "esac";
         "" ]);
  Shell.chmod_executable wt

let install_pi_exec ~root ~log =
  let pi = Filename.concat root "fake-bin/pi" in
  Shell.write_file pi
    ("#!/bin/sh\nset -eu\nprintf '%s\n' \"$0 $*\" >> " ^ Shell.quote log ^ "\n" ^ {|prompt=$(cat)
case "$prompt" in
  *'Independently review Monty worker'*)
    case "$*" in *'--tools read,grep,find,ls'*) ;; *) exit 81 ;; esac
    ;;
  *'Finalize Monty worker'*)
    case "$prompt" in *'PI IMPLEMENTATION HANDOFF'*) ;; *) exit 82 ;; esac
    ;;
esac
printf '%s\n' '{"type":"message_end","message":{"role":"assistant","stopReason":"stop","content":[{"type":"text","text":"PI IMPLEMENTATION HANDOFF: affected checks passed."}]}}'
printf '%s\n' '{"type":"agent_end"}'
|});
  Shell.chmod_executable pi

let install_codex_exec ~root ~log =
  let codex = Filename.concat root "fake-bin/codex" in
  Shell.write_file codex
    (String.concat "\n"
       [ "#!/bin/sh";
         "set -eu";
         "printf '%s\\n' \"$0 $*\" >> " ^ Shell.quote log;
         "output=";
         "while [ \"$#\" -gt 0 ]; do";
         "  case \"$1\" in";
         "    --output-last-message|-o) output=$2; shift 2 ;;";
         "    --sandbox|-C|--cd|--add-dir|-c|--config|--color) shift 2 ;;";
         "    *) shift ;;";
         "  esac";
         "done";
         "cat >/dev/null";
         "if [ -z \"$output\" ]; then exit 86; fi";
         "phase=$(basename \"$output\" .md)";
         "touch \"$output.started\"";
         "if [ \"${MONTY_TEST_CODEX_FAIL_PHASE:-}\" = \"$phase\" ]; then exit 73; fi";
         "case \"$phase\" in";
         "  correctness)";
         "    peer=$(dirname \"$output\")/quality.md.started";
         "    count=0";
         "    while [ ! -f \"$peer\" ] && [ \"$count\" -lt 100 ]; do";
         "      count=$((count + 1))";
         "      sleep 0.01";
         "    done";
         "    if [ ! -f \"$peer\" ]; then exit 74; fi";
         "    ;;";
         "  final)";
         "    root=$(dirname \"$output\")";
         "    if [ -f \"$root/implementation.md\" ]; then";
         "      test -f \"$root/reviews/correctness.md\"";
         "      test -f \"$root/reviews/quality.md\"";
         "    fi";
         "    ;;";
         "esac";
         "printf 'fake Codex %s handoff\\n' \"$phase\" > \"$output\"";
         "printf '%s\\n' '{\"type\":\"turn.completed\"}'";
         "" ]);
  Shell.chmod_executable codex

let install_remove_only_wt ~root ~repo ~branch ~present =
  let log = Filename.concat root "remove-wt.log" in
  let wt = Filename.concat root "fake-bin/wt" in
  let list_lines =
    if present then
      [ "printf 'repo:\\n  %s -> %s\\n' " ^ Shell.quote branch ^ " "
        ^ Shell.quote repo ]
    else [ "printf 'repo:\\n'" ]
  in
  Shell.write_file wt
    (String.concat "\n"
       ([ "#!/bin/sh"; "set -eu";
          "printf '%s\\n' \"$*\" >> " ^ Shell.quote log;
          "case \"$1\" in";
          "  list)" ]
       @ List.map (fun line -> "    " ^ line) list_lines
       @ [ "    ;;"; "  db) exit 0 ;;"; "  b) exit 91 ;;";
           "  *) exit 92 ;;"; "esac"; "" ]));
  Shell.chmod_executable wt;
  log

let install_stateful_remove_wt ~root ~worktree ~branch =
  let log = Filename.concat root "stateful-remove-wt.log" in
  let marker = Filename.concat root "stateful-worktree-present" in
  let wt = Filename.concat root "fake-bin/wt" in
  Shell.write_file marker "present\n";
  Shell.write_file wt
    (String.concat "\n"
       [ "#!/bin/sh";
         "set -eu";
         "printf '%s\\n' \"$*\" >> " ^ Shell.quote log;
         "case \"$1\" in";
         "  list)";
         "    printf 'repo:\\n'";
         "    if [ -f " ^ Shell.quote marker ^ " ]; then";
         "      printf '  %s -> %s\\n' " ^ Shell.quote branch ^ " "
         ^ Shell.quote worktree;
         "    fi";
         "    ;;";
         "  db)";
         "    git -C " ^ Shell.quote worktree ^ " checkout -q --detach";
         "    git -C " ^ Shell.quote worktree ^ " branch -D " ^ Shell.quote branch ^ " >/dev/null";
         "    rm -f " ^ Shell.quote marker ^ " ;;";
         "  b) exit 91 ;;";
         "  *) exit 92 ;;";
         "esac";
         "" ]);
  Shell.chmod_executable wt;
  log

let install_partial_remove_wt ~root ~worktree ~branch =
  let log = Filename.concat root "partial-remove-wt.log" in
  let marker = Filename.concat root "partial-remove-first-call" in
  let wt = Filename.concat root "fake-bin/wt" in
  Shell.write_file marker "first\n";
  Shell.write_file wt
    (String.concat "\n"
       [ "#!/bin/sh";
         "set -eu";
         "printf '%s\n' \"$*\" >> " ^ Shell.quote log;
         "case \"$1\" in";
         "  list)";
         "    printf 'repo:\\n  %s -> %s\\n' " ^ Shell.quote branch ^ " "
         ^ Shell.quote worktree;
         "    ;;";
         "  db)";
         "    if [ -f " ^ Shell.quote marker ^ " ]; then";
         "      rm -f " ^ Shell.quote marker;
         "      rm -f " ^ Shell.quote (Filename.concat worktree ".git");
         "      exit 77";
         "    fi";
         "    exit 0";
         "    ;;";
         "  b) exit 91 ;;";
         "  *) exit 92 ;;";
         "esac";
         "" ]);
  Shell.chmod_executable wt;
  log

let logged_command calls command =
  calls |> String.split_on_char '\n'
  |> List.exists (fun line ->
         let line = String.trim line in
         String.equal line command || String.starts_with ~prefix:(command ^ " ") line)

let command_count calls command =
  calls |> String.split_on_char '\n'
  |> List.fold_left
       (fun count line ->
         let line = String.trim line in
         if String.equal line command
            || String.starts_with ~prefix:(command ^ " ") line
         then count + 1
         else count)
       0

let setup_direct_worker ~home ~repo ~context ~worktree_mode
    ?last_known_worktree () =
  if worktree_mode = "always" && last_known_worktree = Some repo then
    (match Process.run_quiet ~cwd:repo "git checkout -qB cto/force-lifecycle" with
     | Ok () -> () | Error message -> failwith message);
  let run_dir = Filename.concat home ".monty/runs/run-force" in
  let worker_dir = Filename.concat run_dir "workers/worker-force" in
  Shell.ensure_dir worker_dir;
  Shell.write_file context "# Force lifecycle\n";
  Yojson.Safe.to_file (Filename.concat worker_dir "job.json")
    (lifecycle_job_json ~id:"worker-force" ~title:"Force lifecycle"
       ~branch:"cto/force-lifecycle" ~worktree_mode ?last_known_worktree ~repo
       ~context ~worker_dir ~run_dir ());
  (run_dir, worker_dir)

let test_completion_persists_force_and_never_creates_worktree () =
  with_temp_root "force-false" (fun root ->
      let home, _tool_log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      init_git_repo repo;
      let _run_dir, worker_dir =
        setup_direct_worker ~home ~repo ~context ~worktree_mode:"always"
          ~last_known_worktree:repo ()
      in
      let wt_log =
        install_remove_only_wt ~root ~repo ~branch:"cto/force-lifecycle"
          ~present:true
      in
      let interrupted =
        run ~root
          ~env:(replace_env env [ ("MONTY_FAULT_INJECT", "complete-after-intent") ])
          600
          [ "done"; "worker-force"; "--home"; home; "--wt-command"; "wt" ]
      in
      if interrupted.code = 0 then failwith "false-force intent fault succeeded";
      Shell.write_file (Filename.concat repo "dirty.txt") "dirty\n";
      let blocked =
        run ~root ~env 601
          [ "done"; "worker-force"; "--force"; "--home"; home;
            "--wt-command"; "wt" ]
      in
      if blocked.code = 0 then failwith "retry upgraded persisted false force";
      require_contains "recorded false force" blocked.stderr "uncommitted or untracked";
      if not (Sys.file_exists worker_dir) then
        failwith "blocked false-force retry moved worker state";
      Sys.remove (Filename.concat repo "dirty.txt");
      require_code 0
        (run ~root ~env 602
           [ "done"; "worker-force"; "--home"; home; "--wt-command"; "wt" ]);
      if logged_command (read_file wt_log) "b" then
        failwith "completion called wt b");
  with_temp_root "force-true" (fun root ->
      let home, _tool_log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      init_git_repo repo;
      ignore
        (setup_direct_worker ~home ~repo ~context ~worktree_mode:"always"
           ~last_known_worktree:repo ());
      let wt_log =
        install_remove_only_wt ~root ~repo ~branch:"cto/force-lifecycle"
          ~present:true
      in
      Shell.write_file (Filename.concat repo "dirty.txt") "dirty\n";
      let interrupted =
        run ~root
          ~env:(replace_env env [ ("MONTY_FAULT_INJECT", "complete-after-intent") ])
          610
          [ "done"; "worker-force"; "--force"; "--home"; home;
            "--wt-command"; "wt" ]
      in
      if interrupted.code = 0 then failwith "true-force intent fault succeeded";
      require_code 0
        (run ~root ~env 611
           [ "done"; "worker-force"; "--home"; home; "--wt-command"; "wt" ]);
      if Sys.file_exists (Filename.concat repo "dirty.txt") then
        failwith "recorded true force did not survive retry";
      if logged_command (read_file wt_log) "b" then
        failwith "true-force retry called wt b");
  with_temp_root "missing-worktree" (fun root ->
      let home, _tool_log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      init_git_repo repo;
      ignore
        (setup_direct_worker ~home ~repo ~context ~worktree_mode:"always" ());
      let wt_log =
        install_remove_only_wt ~root ~repo ~branch:"cto/force-lifecycle"
          ~present:false
      in
      require_code 0
        (run ~root ~env 620
           [ "done"; "worker-force"; "--home"; home; "--wt-command"; "wt" ]);
      let calls = read_file wt_log in
      if logged_command calls "b" then failwith "missing worktree invoked wt b";
      if logged_command calls "db" then
        failwith "missing worktree and branch invoked wt db")

let test_completion_makes_ignored_artifacts_removable () =
  with_temp_root "read-only-worktree-artifacts" (fun root ->
      let home, _tool_log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      init_git_repo repo;
      ignore
        (setup_direct_worker ~home ~repo ~context ~worktree_mode:"always"
           ~last_known_worktree:repo ());
      Shell.write_file (Filename.concat repo ".git/info/exclude")
        "read-only-artifacts/\n";
      let artifact_dir = Filename.concat repo "read-only-artifacts/sealed" in
      let artifact = Filename.concat artifact_dir "artifact.json" in
      Shell.ensure_dir artifact_dir;
      Shell.write_file artifact "{}\n";
      Unix.chmod artifact 0o400;
      Unix.chmod artifact_dir 0o500;
      let wt_log =
        install_remove_only_wt ~root ~repo ~branch:"cto/force-lifecycle"
          ~present:true
      in
      require_code 0
        (run ~root ~env 625
           [ "done"; "worker-force"; "--home"; home; "--wt-command"; "wt" ]);
      if not (logged_command (read_file wt_log) "db") then
        failwith "read-only artifact completion did not invoke wt db";
      if (Unix.stat artifact_dir).st_perm land 0o200 = 0 then
        failwith "completion left the ignored artifact directory read-only";
      if (Unix.stat artifact).st_perm land 0o200 = 0 then
        failwith "completion left the ignored artifact file read-only")

let test_completion_recovers_after_partial_worktree_removal () =
  with_temp_root "partial-worktree-removal" (fun root ->
      let home, _tool_log, env = setup_environment root in
      (* TMPDIR can be inside the checkout; missing fixture metadata must not
         discover that unrelated parent repository. *)
      let env = replace_env env [ ("GIT_CEILING_DIRECTORIES", root) ] in
      let repo = Filename.concat root "repo" in
      let worktree = Filename.concat root "worktree" in
      let context = Filename.concat root "context.md" in
      init_git_repo repo;
      let add_worktree =
        Process.run_quiet ~cwd:repo
          (Printf.sprintf "git worktree add -q -b %s %s"
             (Shell.quote "cto/force-lifecycle") (Shell.quote worktree))
      in
      (match add_worktree with
      | Ok () -> ()
      | Error message -> failwith message);
      ignore
        (setup_direct_worker ~home ~repo ~context ~worktree_mode:"always"
           ~last_known_worktree:worktree ());
      let wt_log =
        install_partial_remove_wt ~root ~worktree
          ~branch:"cto/force-lifecycle"
      in
      let git_file = Filename.concat worktree ".git" in
      let git_metadata = read_file git_file in
      let interrupted =
        run ~root ~env 626
          [ "done"; "worker-force"; "--home"; home; "--wt-command"; "wt" ]
      in
      if interrupted.code = 0 then
        failwith "partial worktree removal unexpectedly completed";
      if Sys.file_exists (Filename.concat worktree ".git") then
        failwith "partial removal fixture retained Git metadata";
      let sentinel = Filename.concat worktree "possibly-unrelated-data" in
      Shell.write_file sentinel "keep this\n";
      Unix.chmod sentinel 0o400;
      let rejected = run ~root ~env 6261
          [ "done"; "worker-force"; "--home"; home; "--wt-command"; "wt" ] in
      require_code 1 rejected;
      require_contains "partial recovery requires identity" rejected.stderr
        "cannot verify repository identity";
      if (Unix.stat sentinel).st_perm <> 0o400 || read_file sentinel <> "keep this\n" then
        failwith "unverified residual directory was modified";
      if command_count (read_file wt_log) "db" <> 1 then
        failwith "unverified residual directory was sent to wt db";
      (* The fixture knows this is the original directory; restore its metadata
         explicitly before retrying the persisted completion transition. *)
      Unix.unlink sentinel;
      Shell.write_file git_file git_metadata;
      require_code 0
        (run ~root ~env 627
           [ "done"; "worker-force"; "--home"; home; "--wt-command"; "wt" ]);
      if command_count (read_file wt_log) "db" <> 2 then
        failwith "completion retry did not repeat the partial wt removal")

let test_collision_task_failure_and_resume_dry_run_are_safe () =
  with_temp_root "collision" (fun root ->
      let home, _tool_log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      init_git_repo repo;
      let run_dir, worker_dir =
        setup_direct_worker ~home ~repo ~context ~worktree_mode:"always"
          ~last_known_worktree:repo ()
      in
      let wt_log =
        install_remove_only_wt ~root ~repo ~branch:"cto/force-lifecycle"
          ~present:true
      in
      Shell.ensure_dir (Filename.concat run_dir "archive/worker-force");
      let result =
        run ~root ~env 630
          [ "done"; "worker-force"; "--home"; home; "--wt-command"; "wt" ]
      in
      if result.code = 0 then failwith "archive collision unexpectedly succeeded";
      if read_file wt_log <> "" then failwith "collision invoked wt before failing";
      if not (Sys.file_exists worker_dir) then failwith "collision moved active worker");
  with_temp_root "task-failure-dry-run" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      let run_dir = Filename.concat home ".monty/runs/run-1" in
      let active_dir = Filename.concat run_dir "workers/worker-1" in
      let archive_dir = Filename.concat run_dir "archive/worker-1" in
      add_project ~root ~home ~env repo;
      Shell.write_file context "# Lifecycle\n";
      require_code 0
        (run ~root ~env 640
           [ "task"; "add"; "--home"; home; "--project"; "repo";
             "--title"; "Lifecycle worker" ]);
      Shell.ensure_dir active_dir;
      Yojson.Safe.to_file (Filename.concat active_dir "job.json")
        (lifecycle_job_json ~task_key:"local:local-001" ~repo ~context
           ~worker_dir:active_dir ~run_dir ());
      let interrupted =
        run ~root
          ~env:(replace_env env [ ("MONTY_FAULT_INJECT", "complete-after-normalize") ])
          641 [ "done"; "worker-1"; "--home"; home ]
      in
      if interrupted.code = 0 then failwith "task failure setup did not interrupt";
      let tasks_path = Filename.concat home ".monty/tasks.local.json" in
      let saved_tasks = Shell.read_file tasks_path in
      Sys.remove tasks_path;
      let failed = run ~root ~env 642 [ "done"; "worker-1"; "--home"; home ] in
      if failed.code = 0 then failwith "missing linked task finalized completion";
      if job_status (Filename.concat archive_dir "job.json") <> "completing" then
        failwith "task failure did not leave completion incomplete";
      Shell.write_file tasks_path saved_tasks;
      require_code 0 (run ~root ~env 643 [ "done"; "worker-1"; "--home"; home ]);
      let before_job = Shell.read_file (Filename.concat archive_dir "job.json") in
      let before_tasks = Shell.read_file tasks_path in
      let dry =
        run ~root ~env 644
          [ "resume"; "--archived"; "worker-1"; "--home"; home;
            "--terminal"; "dry-run" ]
      in
      require_code 0 dry;
      if string_contains dry.stdout "rehydrate worktree" then
        failwith "archived dry-run ignored persisted never worktree mode";
      require_empty_log log;
      if Sys.file_exists active_dir then failwith "dry-run resume moved archived state";
      if Shell.read_file (Filename.concat archive_dir "job.json") <> before_job
         || Shell.read_file tasks_path <> before_tasks
      then failwith "dry-run archived resume mutated durable state")

let test_cleanup_stale_wt_doctor_and_transition_guards () =
  with_temp_root "cleanup-phase" (fun root ->
      let home, _tool_log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      init_git_repo repo;
      ignore
        (setup_direct_worker ~home ~repo ~context ~worktree_mode:"always"
           ~last_known_worktree:repo ());
      let wt_log =
        install_stateful_remove_wt ~root ~worktree:repo
          ~branch:"cto/force-lifecycle"
      in
      let interrupted =
        run ~root
          ~env:(replace_env env [ ("MONTY_FAULT_INJECT", "complete-after-cleanup") ])
          700
          [ "done"; "worker-force"; "--home"; home; "--wt-command"; "wt" ]
      in
      if interrupted.code = 0 then failwith "cleanup-phase fault unexpectedly succeeded";
      if command_count (read_file wt_log) "db" <> 1 then
        failwith "cleanup-phase fault did not execute one wt db";
      require_code 0
        (run ~root ~env 701
           [ "done"; "worker-force"; "--home"; home; "--wt-command"; "wt" ]);
      if command_count (read_file wt_log) "db" <> 1 then
        failwith "cleanup retry repeated already completed wt db");
  with_temp_root "stale-wt-entry" (fun root ->
      let home, _tool_log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let stale = Filename.concat root "deleted-worktree" in
      let context = Filename.concat root "context.md" in
      init_git_repo repo;
      ignore (Process.run_quiet ~cwd:repo "git branch cto/force-lifecycle");
      ignore
        (setup_direct_worker ~home ~repo ~context ~worktree_mode:"always"
           ~last_known_worktree:stale ());
      let wt_log =
        install_remove_only_wt ~root ~repo:stale
          ~branch:"cto/force-lifecycle" ~present:true
      in
      require_code 0
        (run ~root ~env 710
           [ "done"; "worker-force"; "--home"; home; "--wt-command"; "wt" ]);
      if not (logged_command (read_file wt_log) "db") then
        failwith "stale matching wt entry was not removed";
      if logged_command (read_file wt_log) "b" then
        failwith "stale matching wt entry invoked wt b");
  with_temp_root "doctor-and-guards" (fun root ->
      let home, _tool_log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      let run_dir = Filename.concat home ".monty/runs/run-1" in
      let active_dir = Filename.concat run_dir "workers/worker-1" in
      let archive_dir = Filename.concat run_dir "archive/worker-1" in
      add_project ~root ~home ~env repo;
      Shell.write_file context "# Lifecycle\n";
      require_code 0
        (run ~root ~env 720
           [ "task"; "add"; "--home"; home; "--project"; "repo";
             "--title"; "Lifecycle worker" ]);
      Shell.ensure_dir active_dir;
      Yojson.Safe.to_file (Filename.concat active_dir "job.json")
        (lifecycle_job_json ~task_key:"local:local-001" ~repo ~context
           ~worker_dir:active_dir ~run_dir ());
      let interrupted =
        run ~root
          ~env:(replace_env env [ ("MONTY_FAULT_INJECT", "complete-after-intent") ])
          721 [ "done"; "worker-1"; "--home"; home; "--wt-command"; "wt" ]
      in
      if interrupted.code = 0 then failwith "completion guard setup unexpectedly succeeded";
      let ordinary =
        run ~root ~env 722
          [ "resume"; "worker-1"; "--home"; home; "--terminal"; "dry-run" ]
      in
      if ordinary.code = 0 then failwith "ordinary resume accepted a completing worker";
      require_contains "completing resume guard" ordinary.stderr "is completing";
      let corrupt_dir = Filename.concat run_dir "workers/corrupt" in
      Shell.ensure_dir corrupt_dir;
      Shell.write_file (Filename.concat corrupt_dir "job.json") "{broken\n";
      let doctor =
        run ~root ~env 723
          [ "doctor"; "--home"; home; "--wt-command"; "custom-wt" ]
      in
      require_code 1 doctor;
      require_contains "doctor corrupt warning" doctor.stdout "invalid JSON";
      require_line "doctor completion command" doctor.stdout
        (Printf.sprintf
           "Recovery: monty done 'worker-1' --home %s --wt-command 'custom-wt'"
           (Shell.quote home));
      remove_tree corrupt_dir;
      require_code 0
        (run ~root ~env 724
           [ "done"; "worker-1"; "--home"; home; "--wt-command"; "wt" ]);
      let osascript = Filename.concat root "fake-bin/osascript" in
      Shell.write_file osascript "#!/bin/sh\nexit 0\n";
      Shell.chmod_executable osascript;
      let interrupted =
        run ~root
          ~env:(replace_env env [ ("MONTY_FAULT_INJECT", "reopen-after-move") ])
          725
          [ "resume"; "--archived"; "worker-1"; "--home"; home;
            "--terminal"; "ghostty"; "--worktree"; "never" ]
      in
      if interrupted.code = 0 then failwith "reopen guard setup unexpectedly succeeded";
      if not (Sys.file_exists active_dir) || Sys.file_exists archive_dir then
        failwith "reopen-after-move did not leave the worker at its active path";
      let ordinary =
        run ~root ~env 726
          [ "resume"; "worker-1"; "--home"; home; "--terminal"; "dry-run" ]
      in
      if ordinary.code = 0 then failwith "ordinary resume accepted a reopening worker";
      require_contains "reopening resume guard" ordinary.stderr "is reopening";
      Shell.ensure_dir corrupt_dir;
      Shell.write_file (Filename.concat corrupt_dir "job.json") "{broken-again\n";
      let doctor =
        run ~root ~env 727
          [ "doctor"; "--home"; home; "--wt-command"; "custom-wt" ]
      in
      require_code 1 doctor;
      require_contains "doctor reopen corrupt warning" doctor.stdout "invalid JSON";
      require_line "doctor reopen command" doctor.stdout
        (Printf.sprintf
           "Recovery: monty resume --archived 'worker-1' --home %s --terminal ghostty --worktree 'never' --wt-command 'custom-wt'"
           (Shell.quote home));
      remove_tree corrupt_dir;
      require_code 0
        (run ~root ~env 728
           [ "resume"; "--archived"; "worker-1"; "--home"; home;
             "--terminal"; "ghostty"; "--worktree"; "never" ]))

let test_invalid_explicit_local_task_key_is_never_inferred () =
  with_temp_root "invalid-task-key" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      let run_dir = Filename.concat home ".monty/runs/run-1" in
      let worker_dir = Filename.concat run_dir "workers/worker-1" in
      add_project ~root ~home ~env repo;
      Shell.write_file context "# Task identity\n";
      require_code 0
        (run ~root ~env 740
           [ "task"; "add"; "--home"; home; "--project"; "repo";
             "--title"; "Exact task" ]);
      Shell.ensure_dir worker_dir;
      Yojson.Safe.to_file (Filename.concat worker_dir "job.json")
        (lifecycle_job_json ~task_key:"local:local-001-extra" ~repo ~context
           ~worker_dir ~run_dir ());
      let result = run ~root ~env 741 [ "done"; "worker-1"; "--home"; home ] in
      if result.code = 0 then failwith "malformed explicit local task key was inferred";
      require_contains "invalid local task key" result.stderr
        "invalid local task key";
      if local_task_status home <> "open" then
        failwith "malformed explicit task key closed a different task";
      if job_status (Filename.concat worker_dir "job.json") <> "active" then
        failwith "malformed explicit task key started completion";
      require_empty_log log)

let local_tasks_json home =
  match
    Yojson.Safe.from_file (Filename.concat home ".monty/tasks.local.json")
    |> Yojson.Safe.Util.member "tasks"
  with
  | `List tasks -> tasks
  | _ -> failwith "tasks.local.json did not contain a tasks array"

let json_string field json =
  match Yojson.Safe.Util.member field json with
  | `String value -> value
  | _ -> failf "JSON field %s is not a string" field

let write_worker ~home ~run_id ~id ~title ~repo ~context ?branch ?task_key () =
  let run_dir = Filename.concat home (".monty/runs/" ^ run_id) in
  let worker_dir = Filename.concat run_dir ("workers/" ^ id) in
  Shell.ensure_dir worker_dir;
  Yojson.Safe.to_file (Filename.concat worker_dir "job.json")
    (lifecycle_job_json ~id ~title
       ~branch:(Option.value ~default:("cto/" ^ id) branch) ?task_key ~repo
       ~context ~worker_dir ~run_dir ());
  Filename.concat worker_dir "job.json"

let test_task_list_numbers () =
  with_temp_root "task-list-numbers" (fun root ->
      let home, log, env = setup_environment root in
      let alpha = Filename.concat root "alpha" in
      let beta = Filename.concat root "beta" in
      let context = Filename.concat root "context.md" in
      add_project ~root ~home ~env alpha;
      add_project ~root ~home ~env beta;
      Shell.write_file context "# Numbered tasks\n";
      [ ("beta", "First"); ("alpha", "Second"); ("alpha", "Closed") ]
      |> List.iteri (fun index (project, title) ->
             require_code 0
               (run ~root ~env (7000 + index)
                  [ "task"; "add"; "--project"; project; "--title"; title ]);
             if index < 2 then
               require_code 0
                 (run ~root ~env (7020 + index)
                    [ "task"; "workspace"; "add";
                      Printf.sprintf "local-%03d" (index + 1);
                      "--repo"; Filename.concat root project;
                      "--branch"; Printf.sprintf "cto/worker-%d" (index + 1) ]));
      ignore
        (write_worker ~home ~run_id:"run-1" ~id:"worker-1" ~title:"First"
           ~repo:beta ~context ~task_key:"local:local-001" ());
      ignore
        (write_worker ~home ~run_id:"run-2" ~id:"worker-2" ~title:"Second"
           ~repo:alpha ~context ~task_key:"local:local-002" ());
      require_code 0 (run ~root ~env 7003 [ "task"; "done"; "local-003" ]);
      let inventory index args expected =
        let result = run ~root ~env index args in
        require_code 0 result;
        let rows =
          String.split_on_char '\n' (String.trim result.stdout)
          |> List.map (fun line ->
                 String.split_on_char ' ' line
                 |> List.filter (fun word -> word <> "") |> String.concat " ")
        in
        if rows <> "# ID PROJECT STATUS TITLE BRANCH" :: expected then
          failf "unexpected numbered inventory for %s:\n%s" (command result)
            result.stdout;
        result.stdout
      in
      let active =
        [ "1 worker-2 alpha open Second cto/worker-2";
          "2 worker-1 beta open First cto/worker-1" ]
      in
      let listed = inventory 7004 [ "list" ] active in
      let tasks = inventory 7005 [ "tasks"; "list" ] active in
      if listed <> tasks then failwith "numbered list entry points diverged";
      let task_bytes = read_file (Filename.concat home ".monty/tasks.local.json") in
      ignore (inventory 7006 [ "tasks"; "list"; "--no-sync"; "--project"; "beta" ]
                [ "1 worker-1 beta open First cto/worker-1" ]);
      ignore (inventory 7007 [ "list"; "--no-sync"; "--run"; "run-1" ]
                [ "1 worker-1 beta open First cto/worker-1" ]);
      ignore (inventory 7008 [ "list"; "--no-sync"; "--archived" ]
                [ "1 local:local-003 alpha done Closed" ]);
      let all =
        [ "1 worker-2 alpha open Second cto/worker-2";
          "2 local:local-003 alpha done Closed";
          "3 worker-1 beta open First cto/worker-1" ]
      in
      let listed_all = inventory 7009 [ "list"; "--no-sync"; "--all" ] all in
      let tasks_all = inventory 7010 [ "tasks"; "list"; "--no-sync"; "--all" ] all in
      if listed_all <> tasks_all then failwith "numbered all entry points diverged";
      ignore (inventory 7011 [ "list"; "--no-sync"; "--run"; "missing" ] []);
      let numeric = run ~root ~env 7012 [ "task"; "done"; "1" ] in
      if numeric.code = 0 then failwith "CLI accepted a row number as a task ID";
      require_contains "numeric selector rejected" numeric.stderr "no local Monty task";
      if read_file (Filename.concat home ".monty/tasks.local.json") <> task_bytes then
        failwith "numbered reads or rejected selector changed task state";
      require_empty_log log)

let test_reconciliation_replay_idempotence_and_legacy_repair () =
  with_temp_root "reconcile-replay" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      add_project ~root ~home ~env repo;
      Shell.write_file context "# Reconcile\n";
      require_code 0
        (run ~root ~env 800
           [ "task"; "add"; "--home"; home; "--project"; "repo";
             "--title"; "Same title" ]);
      let job_file =
        write_worker ~home ~run_id:"run-1" ~id:"worker-1"
          ~title:"Same title" ~repo ~context ()
      in
      let fault =
        run ~root
          ~env:(replace_env env [ ("MONTY_FAULT_INJECT", "sync-after-tasks") ])
          801 [ "tasks"; "sync"; "--home"; home ]
      in
      if fault.code = 0 then failwith "sync-after-tasks fault unexpectedly succeeded";
      require_contains "sync fault" fault.stderr "sync-after-tasks";
      if List.length (local_tasks_json home) <> 2 then
        failwith "sync fault did not commit exactly one stable worker task";
      if Yojson.Safe.Util.member "task_key" (Yojson.Safe.from_file job_file) <> `Null then
        failwith "sync fault linked the job before committing the task";
      let replay = run ~root ~env 802 [ "tasks"; "sync"; "--home"; home ] in
      require_code 0 replay;
      require_contains "sync replay counts" replay.stdout "0 created, 0 updated, 1 linked jobs";
      let linked = json_string "task_key" (Yojson.Safe.from_file job_file) in
      if linked <> "local:local-002" then
        failf "ordinary sync title-matched the wrong task: %s" linked;
      let tasks_path = Filename.concat home ".monty/tasks.local.json" in
      let task_bytes = Shell.read_file tasks_path in
      let job_bytes = Shell.read_file job_file in
      let task_mtime = (Unix.stat tasks_path).Unix.st_mtime in
      let job_mtime = (Unix.stat job_file).Unix.st_mtime in
      let satisfied = run ~root ~env 803 [ "tasks"; "sync"; "--home"; home ] in
      require_code 0 satisfied;
      require_contains "satisfied sync counts" satisfied.stdout
        "0 created, 0 updated, 0 linked jobs";
      if Shell.read_file tasks_path <> task_bytes || Shell.read_file job_file <> job_bytes then
        failwith "satisfied sync rewrote state bytes";
      if (Unix.stat tasks_path).Unix.st_mtime <> task_mtime
         || (Unix.stat job_file).Unix.st_mtime <> job_mtime
      then failwith "satisfied sync changed state mtimes";
      ignore
        (write_worker ~home ~run_id:"run-2" ~id:"legacy-worker"
           ~title:"Same title" ~repo ~context ());
      let repair =
        run ~root ~env 804
          [ "tasks"; "repair-worker"; "legacy-worker"; "--home"; home ]
      in
      if repair.code = 0 then failwith "ambiguous legacy repair unexpectedly succeeded";
      require_contains "legacy repair ambiguity" repair.stderr "ambiguous";
      require_empty_log log)

let test_reconciliation_diagnostics_no_sync_and_unknown_launch () =
  with_temp_root "reconcile-diagnostics" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "healthy-repo" in
      let unknown_repo = Filename.concat root "unknown-repo" in
      let context = Filename.concat root "context.md" in
      add_project ~root ~home ~env repo;
      Shell.ensure_dir unknown_repo;
      Shell.write_file context "# Diagnostics\n";
      let healthy_job =
        write_worker ~home ~run_id:"run-1" ~id:"healthy-worker"
          ~title:"Healthy worker" ~repo ~context ()
      in
      ignore
        (write_worker ~home ~run_id:"run-1" ~id:"unknown-worker"
           ~title:"Unknown worker" ~repo:unknown_repo ~context ());
      let corrupt_dir = Filename.concat home ".monty/runs/run-1/workers/corrupt" in
      Shell.ensure_dir corrupt_dir;
      Shell.write_file (Filename.concat corrupt_dir "job.json") "{corrupt\n";
      let sync = run ~root ~env 820 [ "tasks"; "sync"; "--home"; home ] in
      require_code 0 sync;
      require_contains "corrupt sync warning" sync.stderr "invalid JSON";
      require_contains "unknown project warning" sync.stderr
        ("monty projects add --repo " ^ Shell.quote unknown_repo);
      if json_string "task_key" (Yojson.Safe.from_file healthy_job) <> "local:local-001"
      then failwith "healthy worker was not reconciled beside bad records";
      let tasks_path = Filename.concat home ".monty/tasks.local.json" in
      let task_bytes = Shell.read_file tasks_path in
      let task_mtime = (Unix.stat tasks_path).Unix.st_mtime in
      let log_before = read_file log in
      let list = run ~root ~env 821 [ "list"; "--no-sync"; "--home"; home ] in
      let tasks =
        run ~root ~env 822 [ "tasks"; "list"; "--no-sync"; "--home"; home ]
      in
      require_code 0 list;
      require_code 0 tasks;
      if list.stdout <> tasks.stdout then
        failwith "list and tasks list diverged under --no-sync";
      require_contains "healthy inventory" list.stdout "healthy-worker";
      require_contains "unknown inventory" list.stdout "unknown-worker";
      require_contains "no-sync corrupt warning" list.stderr "invalid JSON";
      require_contains "no-sync registration warning" list.stderr
        ("monty projects add --repo " ^ Shell.quote unknown_repo);
      require_contains "no-sync registration home" list.stderr
        ("--home " ^ Shell.quote home);
      let run_filtered =
        run ~root ~env 8221
          [ "list"; "--no-sync"; "--run"; "run-1"; "--home"; home ]
      in
      require_code 0 run_filtered;
      require_contains "run-filtered unknown diagnostic" run_filtered.stdout
        "unknown-worker";
      if Shell.read_file tasks_path <> task_bytes
         || (Unix.stat tasks_path).Unix.st_mtime <> task_mtime
      then failwith "--no-sync changed local task state";
      if read_file log <> log_before then
        failwith "--no-sync invoked an external command";
      let launch =
        run ~root ~env 823
          [ "launch"; "--terminal"; "ghostty"; "--home"; home;
            "--repo"; unknown_repo; "--title"; "Unknown launch";
            "--context"; context; "--branch"; "cto/unknown-launch" ]
      in
      if launch.code = 0 then failwith "unknown-project launch unexpectedly succeeded";
      require_contains "unknown launch registration" launch.stderr
        ("monty projects add --repo " ^ Shell.quote unknown_repo);
      require_contains "unknown launch registration home" launch.stderr
        ("--home " ^ Shell.quote home);
      if read_file log <> log_before then
        failwith "unknown-project launch invoked wt or terminal";
      let tasks_after = local_tasks_json home in
      if List.length tasks_after <> 1 then
        failwith "unknown-project launch created local task state")

let test_discovery_preserves_strict_and_tolerant_policies () =
  with_temp_root "discovery-policies" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      add_project ~root ~home ~env repo;
      Shell.write_file context "# Discovery\n";
      let healthy =
        write_worker ~home ~run_id:"mixed" ~id:"z-healthy"
          ~title:"Healthy worker" ~repo ~context ()
      in
      let healthy_bytes = Shell.read_file healthy in
      let run_dir = Filename.concat (Unix.realpath home) ".monty/runs/mixed" in
      let malformed = Filename.concat run_dir "workers/a-malformed/job.json" in
      Shell.write_file malformed "{broken\n";
      let outside = Filename.concat root "outside" in
      Shell.ensure_dir outside;
      let worker_link = Filename.concat run_dir "workers/b-link" in
      let archive_link = Filename.concat run_dir "archive/a-archive-link" in
      Shell.ensure_dir (Filename.dirname archive_link);
      Unix.symlink outside worker_link;
      Unix.symlink outside archive_link;
      let strict index expected_path =
        let result =
          run ~root ~env index
            [ "resume"; "z-healthy"; "--terminal"; "dry-run"; "--home"; home ]
        in
        require_code 1 result;
        let expected =
          "monty: unsafe symlink in Monty job state: " ^ expected_path
          ^ "; job discovery will not traverse it, so replace or remove the symlink before retrying\n"
        in
        if result.stderr <> expected then
          failf "strict discovery changed its first diagnostic: %S" result.stderr
      in
      strict 830 worker_link;
      let list = run ~root ~env 831 [ "list"; "--all"; "--no-sync"; "--home"; home ] in
      let tasks =
        run ~root ~env 832 [ "tasks"; "list"; "--all"; "--no-sync"; "--home"; home ]
      in
      require_code 0 list;
      require_code 0 tasks;
      if list.stdout <> tasks.stdout || list.stderr <> tasks.stderr then
        failwith "inventory views disagree about mixed worker state";
      require_contains "healthy record survives discovery" list.stdout "z-healthy";
      require_contains "malformed discovery warning" list.stderr ("invalid JSON in " ^ malformed);
      List.iter
        (fun path -> require_contains "symlink discovery warning" list.stderr
            (path ^ "; job discovery skipped it"))
        [ worker_link; archive_link ];
      let warnings =
        String.split_on_char '\n' list.stderr
        |> List.filter (String.starts_with ~prefix:"monty: warning: ")
      in
      if List.length warnings <> 3 || warnings <> List.sort_uniq String.compare warnings then
        failwith "discovery warnings are not sorted and unique";
      Unix.unlink worker_link;
      strict 833 archive_link;
      Unix.unlink archive_link;
      let malformed_result =
        run ~root ~env 834
          [ "resume"; "z-healthy"; "--terminal"; "dry-run"; "--home"; home ]
      in
      require_code 1 malformed_result;
      require_contains "strict parsing after traversal" malformed_result.stderr
        ("invalid JSON in " ^ malformed);
      if Shell.read_file healthy <> healthy_bytes
         || Sys.file_exists (Filename.concat home ".monty/tasks.local.json")
      then failwith "read-only discovery changed task or worker state";
      require_empty_log log)

let test_inventory_filters_preserve_diagnostics () =
  with_temp_root "inventory-filters" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      add_project ~root ~home ~env repo;
      let task id project title status =
        `Assoc [ ("id", `String id); ("project", `String project);
                 ("title", `String title); ("status", `String status) ]
      in
      let path = Filename.concat home ".monty/tasks.local.json" in
      Yojson.Safe.to_file path
        (`Assoc [ ("tasks", `List
            [ task "local-001" "repo" "Open task" "open";
              task "local-002" "repo" "Done task" "done";
              task "local-003" "missing" "Done orphan" "done" ]) ]);
      let before = Shell.read_file path in
      let inventory index args =
        let result = run ~root ~env index (args @ [ "--no-sync"; "--home"; home ]) in
        require_code 0 result;
        result
      in
      let list = inventory 840 [ "list" ] in
      let tasks = inventory 841 [ "tasks"; "list" ] in
      if list.stdout <> tasks.stdout || list.stderr <> tasks.stderr then
        failwith "open inventories diverged";
      require_contains "open task visible" list.stdout "Open task";
      if string_contains list.stdout "Done" then failwith "done tasks leaked into open inventory";
      require_contains "list retains orphan diagnostic" list.stderr "local:local-003";
      let all = inventory 842 [ "list"; "--all" ] in
      let all_tasks = inventory 843 [ "tasks"; "list"; "--all" ] in
      if all.stdout <> all_tasks.stdout || all.stderr <> all_tasks.stderr then
        failwith "all inventories diverged";
      require_contains "done orphan visible" all.stdout "Done orphan";
      let archived = inventory 844 [ "list"; "--archived" ] in
      require_contains "done task archived" archived.stdout "Done task";
      if string_contains archived.stdout "Open task" then failwith "open task appeared in archived inventory";
      let project = inventory 845 [ "tasks"; "list"; "--all"; "--project"; "repo" ] in
      require_contains "project done task" project.stdout "Done task";
      if string_contains project.stdout "Done orphan" || project.stderr <> "" then
        failwith "project filter retained an unrelated orphan";
      if Shell.read_file path <> before then failwith "inventory rewrote task state";
      require_empty_log log)

let install_fake_gh ~root ~log ~state ~title =
  let gh = Filename.concat root "fake-bin/gh" in
  let json =
    Yojson.Safe.to_string
      (`List
        [ `Assoc
            [ ("number", `Int 7); ("title", `String title);
              ("state", `String state);
              ("url", `String "https://example.invalid/issues/7") ] ])
  in
  Shell.write_file gh
    (String.concat "\n"
       [ "#!/bin/sh"; "set -eu";
         "printf '%s\\n' \"$0 $*\" >> " ^ Shell.quote log;
         "printf '%s\\n' " ^ Shell.quote json;
         "" ]);
  Shell.chmod_executable gh

let test_external_import_local_ownership_and_stable_projects () =
  with_temp_root "external-import" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "external-repo" in
      Shell.ensure_dir repo;
      require_code 0
        (run ~root ~env 840
           [ "projects"; "add"; "--home"; home; "--repo"; repo;
             "--github"; "owner/repo" ]);
      install_fake_gh ~root ~log ~state:"OPEN" ~title:"Imported title";
      require_code 0 (run ~root ~env 841 [ "tasks"; "sync"; "--home"; home ]);
      let imported = List.hd (local_tasks_json home) in
      if json_string "external_key" imported <> "github:owner/repo#7"
         || json_string "external_url" imported <> "https://example.invalid/issues/7"
         || json_string "status" imported <> "open"
      then failwith "external issue metadata was not imported locally";
      require_code 0 (run ~root ~env 842 [ "task"; "done"; "local-001"; "--home"; home ]);
      install_fake_gh ~root ~log ~state:"CLOSED" ~title:"Updated metadata";
      require_code 0 (run ~root ~env 843 [ "tasks"; "sync"; "--home"; home ]);
      let imported = List.hd (local_tasks_json home) in
      if json_string "status" imported <> "done" then
        failwith "remote issue state overwrote local done status";
      if json_string "title" imported <> "Updated metadata" then
        failwith "external metadata did not refresh";
      let log_before = read_file log in
      require_code 0
        (run ~root ~env 844
           [ "tasks"; "list"; "--no-sync"; "--all"; "--home"; home ]);
      if read_file log <> log_before then failwith "--no-sync invoked gh");
  with_temp_root "stable-project-ids" (fun root ->
      let home, _log, env = setup_environment root in
      let first = Filename.concat root "one/shared" in
      let second = Filename.concat root "two/shared" in
      Shell.ensure_dir first;
      Shell.ensure_dir second;
      require_code 0
        (run ~root ~env 850 [ "projects"; "add"; "--home"; home; "--repo"; first ]);
      require_code 0
        (run ~root ~env 851
           [ "task"; "add"; "--home"; home; "--project"; "shared";
             "--title"; "Stable project task" ]);
      require_code 0
        (run ~root ~env 852 [ "projects"; "add"; "--home"; home; "--repo"; second ]);
      let projects =
        Yojson.Safe.from_file (Filename.concat home ".monty/projects.json")
        |> Yojson.Safe.Util.member "projects"
      in
      let ids =
        match projects with
        | `List values -> List.map (json_string "id") values
        | _ -> failwith "projects.json missing projects"
      in
      if not (List.mem "shared" ids) || List.length (List.sort_uniq String.compare ids) <> 2
      then failwith "same-basename projects did not retain stable unique IDs";
      let task = List.hd (local_tasks_json home) in
      if json_string "project" task <> "shared" then
        failwith "same-basename project addition orphaned an existing task")

let test_duplicate_task_claims_warn_deterministically () =
  with_temp_root "duplicate-task-claims" (fun root ->
      let home, _log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      add_project ~root ~home ~env repo;
      Shell.write_file context "# Duplicate claims\n";
      require_code 0
        (run ~root ~env 870
           [ "task"; "add"; "--home"; home; "--project"; "repo";
             "--title"; "Claimed task" ]);
      ignore
        (write_worker ~home ~run_id:"run-z" ~id:"z-worker" ~title:"Z worker"
           ~repo ~context ~task_key:"local:local-001" ());
      ignore
        (write_worker ~home ~run_id:"run-a" ~id:"a-worker" ~title:"A worker"
           ~repo ~context ~task_key:"local:local-001" ());
      let sync = run ~root ~env 871 [ "tasks"; "sync"; "--home"; home ] in
      require_code 0 sync;
      require_contains "duplicate task warning" sync.stderr
        "multiple workers claim task local:local-001: a-worker, z-worker";
      let list = run ~root ~env 872 [ "list"; "--no-sync"; "--home"; home ] in
      require_code 0 list;
      require_contains "read-only duplicate warning" list.stderr
        "multiple workers claim task local:local-001";
      require_contains "deterministic duplicate display" list.stdout "a-worker";
      if string_contains list.stdout "z-worker" then
        failwith "duplicate task display did not choose deterministic worker");
  with_temp_root "duplicate-same-id-claims" (fun root ->
      let home, _log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      add_project ~root ~home ~env repo;
      Shell.write_file context "# Duplicate same ID claims\n";
      require_code 0
        (run ~root ~env 873
           [ "task"; "add"; "--home"; home; "--project"; "repo";
             "--title"; "Claimed task" ]);
      ignore
        (write_worker ~home ~run_id:"run-one" ~id:"same-worker"
           ~title:"Same worker one" ~repo ~context ~branch:"cto/one"
           ~task_key:"local:local-001" ());
      ignore
        (write_worker ~home ~run_id:"run-two" ~id:"same-worker"
           ~title:"Same worker two" ~repo ~context ~branch:"cto/two"
           ~task_key:"local:local-001" ());
      let sync = run ~root ~env 874 [ "tasks"; "sync"; "--home"; home ] in
      require_code 0 sync;
      require_contains "same-ID duplicate task warning" sync.stderr
        "multiple workers claim task local:local-001: same-worker, same-worker");
  with_temp_root "mixed-lifecycle-claims" (fun root ->
      let home, _log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      add_project ~root ~home ~env repo;
      Shell.write_file context "# Mixed lifecycle claims\n";
      require_code 0
        (run ~root ~env 875
           [ "task"; "add"; "--home"; home; "--project"; "repo";
             "--title"; "Mixed claimed task" ]);
      ignore
        (write_worker ~home ~run_id:"run-active" ~id:"z-worker"
           ~title:"Active winner" ~repo ~context
           ~task_key:"local:local-001" ());
      let archived_run = Filename.concat home ".monty/runs/run-archived" in
      let archived_dir = Filename.concat archived_run "archive/a-worker" in
      Shell.ensure_dir archived_dir;
      lifecycle_job_json ~id:"a-worker" ~title:"Archived claimant"
        ~branch:"cto/a-worker" ~status:"done" ~task_key:"local:local-001"
        ~repo ~context ~worker_dir:archived_dir ~run_dir:archived_run ()
      |> fun json -> Yojson.Safe.to_file (Filename.concat archived_dir "job.json") json;
      let sync = run ~root ~env 876 [ "tasks"; "sync"; "--home"; home ] in
      require_code 0 sync;
      require_contains "mixed lifecycle warning winner" sync.stderr
        "using z-worker for display";
      let list = run ~root ~env 877 [ "list"; "--no-sync"; "--home"; home ] in
      require_code 0 list;
      require_contains "mixed lifecycle display winner" list.stdout "z-worker";
      if string_contains list.stdout "a-worker" then
        failwith "archived claimant won display over active claimant")

let replace_assoc_field name value = function
  | `Assoc fields ->
      `Assoc ((name, value) :: List.remove_assoc name fields)
  | _ -> failwith "expected a JSON object"

let test_codex_session_resume_modes () =
  with_temp_root "codex-session-resume" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      let id = "codex-session-worker" in
      add_project ~root ~home ~env repo;
      Shell.write_file context "# Exact Codex session resume\n";
      let osascript = Filename.concat root "fake-bin/osascript" in
      Shell.write_file osascript
        (String.concat "\n"
           [ "#!/bin/sh";
             "printf 'codex-session-osascript\\n' >> " ^ Shell.quote log;
             "exit 0";
             "" ]);
      Shell.chmod_executable osascript;
      let codex_options terminal =
        [ "--home"; home; "--terminal"; terminal; "--harness"; "codex";
          "--codex-command"; "codex --fixed value" ]
      in
      let launched =
        run ~root ~env 878
          ([ "launch"; "--repo"; repo; "--title"; "Codex session worker";
             "--context"; context; "--branch"; "cto/codex-session-worker";
             "--worktree"; "never" ]
          @ codex_options "ghostty")
      in
      require_code 0 launched;
      let worker_dir =
        Filename.concat home (".monty/runs/manual/workers/" ^ id)
      in
      let job_file = Filename.concat worker_dir "job.json" in
      let sidecar = Filename.concat worker_dir "codex-session-id" in
      if Sys.file_exists sidecar then
        failwith "initial Codex launch wrote a session id before the native session starts";
      let launch_script =
        Yojson.Safe.from_file job_file |> json_string "launch_script"
      in
      let initial_script = Shell.read_file launch_script in
      require_contains "initial Codex service launch" initial_script " terminal --home ";
      require_contains "initial Codex service fresh mode" initial_script "--mode fresh";
      require_contains "initial Codex prompt" initial_script
        "Read the files below before acting";
      let capture session_id =
        match Native_session.write ~home ~harness:Harness.Codex ~worker_dir session_id with
        | Ok () -> () | Error message -> failwith message in
      let exact_one = "-thread/name with spaces" in
      capture exact_one;
      if Shell.read_file sidecar <> exact_one then failwith "native capture changed the session id";
      let exact_dry =
        run ~root ~env 881
          ([ "resume"; id ] @ codex_options "dry-run")
      in
      require_code 0 exact_dry;
      require_contains "exact Codex dry-run mode" exact_dry.stdout
        ("[dry-run] Codex session: exact " ^ exact_one);
      require_contains "exact Codex resume selector" exact_dry.stdout
        (" --session " ^ Shell.quote exact_one);
      if string_contains exact_dry.stdout "--last" then
        failwith "exact Codex resume used --last";
      if string_contains exact_dry.stdout "Read the files below before acting" then
        failwith "exact Codex resume resent the original worker prompt";
      let exact_resume =
        run ~root ~env 882
          ([ "resume"; id ] @ codex_options "ghostty")
      in
      require_code 0 exact_resume;
      require_contains "exact Codex launch script" (Shell.read_file launch_script)
        (" --session " ^ Shell.quote exact_one);
      let failed_fresh =
        run ~root
          ~env:
            (replace_env env
               [ ("MONTY_FAULT_INJECT", "launch-before-request-state") ])
          883
          ([ "resume"; "--fresh"; id ] @ codex_options "ghostty")
      in
      if failed_fresh.code = 0 then
        failwith "fault-injected fresh Codex resume unexpectedly succeeded";
      require_contains "fresh Codex retry preserves intent" failed_fresh.stdout
        " resume --fresh ";
      let fresh_resume =
        run ~root ~env 884
          ([ "resume"; "--fresh"; id ] @ codex_options "ghostty")
      in
      require_code 0 fresh_resume;
      if Shell.read_file sidecar <> exact_one then
        failwith "fresh resume erased the old Codex session before the native session starts";
      require_contains "fresh Codex resume prompt" (Shell.read_file launch_script)
        "Read the files below before acting";
      Sys.remove sidecar;
      let picker_resume =
        run ~root ~env 885
          ([ "resume"; id ] @ codex_options "ghostty")
      in
      require_code 0 picker_resume;
      let picker_script = Shell.read_file launch_script in
      require_contains "Codex native picker" picker_script " --mode picker";
      if string_contains picker_script "--last"
         || string_contains picker_script "Read the files below before acting"
      then failwith "Codex picker used --last or resent the original prompt";
      let exact_two = "thread's-second" in
      capture exact_two;
      require_code 0
        (run ~root ~env 887
           ([ "resume"; id ] @ codex_options "ghostty"));
      require_contains "picker capture became exact resume"
        (Shell.read_file launch_script)
        (" --session " ^ Shell.quote exact_two);
      Sys.remove sidecar;
      require_code 0
        (run ~root ~env 888
           ([ "resume"; id ] @ codex_options "ghostty"));
      let missing_sidecar_script = Shell.read_file launch_script in
      require_contains "missing sidecar rewrote exact script to picker"
        missing_sidecar_script " --mode picker";
      if string_contains missing_sidecar_script exact_two then
        failwith "missing sidecar retained the old exact Codex selector";
      capture exact_two;
      require_code 0
        (run ~root ~env 890
           ([ "resume"; id ] @ codex_options "ghostty"));
      Shell.write_file sidecar "invalid\n";
      let invalid_sidecar =
        run ~root ~env 891
          ([ "resume"; id ] @ codex_options "dry-run")
      in
      if invalid_sidecar.code = 0 then
        failwith "resume accepted a control character in the Codex sidecar";
      require_contains "invalid Codex sidecar" invalid_sidecar.stderr
        "control characters";
      let fresh_dry =
        run ~root ~env 892
          ([ "open"; "--fresh"; id ] @ codex_options "dry-run")
      in
      require_code 0 fresh_dry;
      require_contains "open alias fresh mode" fresh_dry.stdout
        "[dry-run] Codex session: fresh";
      if Shell.read_file sidecar <> "invalid\n" then
        failwith "fresh dry-run mutated the invalid old sidecar";
      require_code 0
        (run ~root ~env 893
           ([ "open"; "--fresh"; id ] @ codex_options "ghostty"));
      if Shell.read_file sidecar <> "invalid\n" then
        failwith "fresh resume mutated the invalid old sidecar";
      capture exact_two;
      let job_json = Yojson.Safe.from_file job_file in
      [ "codex_session_id"; "backend"; "session_id" ]
      |> List.iter (fun field ->
             if Yojson.Safe.Util.member field job_json <> `Null then
               failf "forbidden runtime field %s entered job.json" field);
      require_code 0
        (run ~root ~env 895 [ "done"; id; "--home"; home ]);
      let archived_dir =
        Filename.concat home (".monty/runs/manual/archive/" ^ id)
      in
      let archived_sidecar = Filename.concat archived_dir "codex-session-id" in
      if Shell.read_file archived_sidecar <> exact_two then
        failwith "archiving did not preserve the Codex session sidecar";
      let archived_dry =
        run ~root ~env 896
          ([ "resume"; "--archived"; id ] @ codex_options "dry-run")
      in
      require_code 0 archived_dry;
      require_contains "archived exact Codex resume" archived_dry.stdout
        ("[dry-run] Codex session: exact " ^ exact_two);
      if not (Sys.file_exists archived_dir) || Sys.file_exists worker_dir then
        failwith "archived resume dry-run moved durable worker memory")

let test_invalid_links_duplicate_ids_and_ambiguous_workers () =
  with_temp_root "invalid-links" (fun root ->
      let home, _log, env = setup_environment root in
      let repo_a = Filename.concat root "project-a" in
      let repo_b = Filename.concat root "project-b" in
      let unknown_repo = Filename.concat root "unknown" in
      let context = Filename.concat root "context.md" in
      Shell.ensure_dir repo_a;
      Shell.ensure_dir repo_b;
      Shell.ensure_dir unknown_repo;
      Shell.write_file context "# Invalid links\n";
      require_code 0
        (run ~root ~env 880
           [ "projects"; "add"; "--home"; home; "--repo"; repo_a ]);
      require_code 0
        (run ~root ~env 881
           [ "projects"; "add"; "--home"; home; "--repo"; repo_b ]);
      require_code 0
        (run ~root ~env 882
           [ "task"; "add"; "--home"; home; "--project"; "project-b";
             "--title"; "Project B task" ]);
      ignore
        (write_worker ~home ~run_id:"run-cross" ~id:"cross-worker"
           ~title:"Cross-project worker" ~repo:repo_a ~context
           ~task_key:"local:local-001" ());
      let multi_worker =
        write_worker ~home ~run_id:"run-multi" ~id:"multi-worker"
          ~title:"Multi-project worker" ~repo:repo_a ~context
          ~task_key:"local:local-001" ()
      in
      Yojson.Safe.from_file multi_worker
      |> replace_assoc_field "workspaces"
           (`List
             [ `Assoc
                 [ ("repo", `String repo_a);
                   ("branch", `String "cto/multi-worker") ];
               `Assoc
                 [ ("repo", `String repo_b);
                   ("branch", `String "cto/multi-worker-b") ] ])
      |> Yojson.Safe.to_file multi_worker;
      let archive_dir = Filename.concat home ".monty/runs/run-unknown/archive/unknown-worker" in
      let run_dir = Filename.concat home ".monty/runs/run-unknown" in
      Shell.ensure_dir archive_dir;
      lifecycle_job_json ~id:"unknown-worker" ~title:"Archived unknown worker"
        ~branch:"cto/unknown-worker" ~task_key:"local:local-001"
        ~repo:unknown_repo ~context ~worker_dir:archive_dir ~run_dir ()
      |> replace_assoc_field "status" (`String "done")
      |> fun json -> Yojson.Safe.to_file (Filename.concat archive_dir "job.json") json;
      let filtered =
        run ~root ~env 883
          [ "tasks"; "list"; "--no-sync"; "--project"; "project-a";
            "--home"; home ]
      in
      require_code 0 filtered;
      require_contains "cross-project diagnostic id" filtered.stdout "cross-worker";
      require_contains "cross-project diagnostic title" filtered.stdout
        "Cross-project worker";
      require_contains "cross-project warning" filtered.stderr
        "links task local:local-001 from project project-b";
      if string_contains filtered.stderr "worker multi-worker" then
        failwith "valid multi-project task link emitted a cross-project warning";
      if string_contains filtered.stderr "using cross-worker for display" then
        failwith "invalid task claimant was described as the display winner";
      let archived =
        run ~root ~env 884
          [ "list"; "--no-sync"; "--archived"; "--home"; home ]
      in
      require_code 0 archived;
      require_contains "unknown archived diagnostic id" archived.stdout "unknown-worker";
      require_contains "unknown archived diagnostic title" archived.stdout
        "Archived unknown worker";
      require_contains "unknown archived registration" archived.stderr
        ("monty projects add --repo " ^ Shell.quote unknown_repo));
  with_temp_root "duplicate-task-ids" (fun root ->
      let home, _log, env = setup_environment root in
      let repo_a = Filename.concat root "project-a" in
      let repo_b = Filename.concat root "project-b" in
      Shell.ensure_dir repo_a;
      Shell.ensure_dir repo_b;
      require_code 0
        (run ~root ~env 890
           [ "projects"; "add"; "--home"; home; "--repo"; repo_a ]);
      require_code 0
        (run ~root ~env 891
           [ "projects"; "add"; "--home"; home; "--repo"; repo_b ]);
      let tasks_path = Filename.concat home ".monty/tasks.local.json" in
      let duplicate =
        `Assoc
          [ ( "tasks",
              `List
                [ `Assoc
                    [ ("id", `String "local-001");
                      ("project", `String "project-a");
                      ("title", `String "First");
                      ("status", `String "open") ];
                  `Assoc
                    [ ("id", `String "local-001");
                      ("project", `String "project-b");
                      ("title", `String "Second");
                      ("status", `String "open") ] ] ) ]
      in
      Yojson.Safe.to_file tasks_path duplicate;
      let previous = Shell.read_file tasks_path in
      let result = run ~root ~env 892 [ "task"; "done"; "local-001"; "--home"; home ] in
      if result.code = 0 then failwith "duplicate local task IDs were mutated";
      require_contains "duplicate task id" result.stderr "duplicate local task id";
      if Shell.read_file tasks_path <> previous then
        failwith "duplicate task ID rejection overwrote task state");
  with_temp_root "ambiguous-worker-identity" (fun root ->
      let home, _log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      add_project ~root ~home ~env repo;
      Shell.write_file context "# Ambiguous identity\n";
      ignore
        (write_worker ~home ~run_id:"run-a" ~id:"same-worker"
           ~title:"Same worker A" ~repo ~context ());
      ignore
        (write_worker ~home ~run_id:"run-b" ~id:"same-worker"
           ~title:"Same worker B" ~repo ~context ());
      let sync = run ~root ~env 900 [ "tasks"; "sync"; "--home"; home ] in
      require_code 0 sync;
      require_contains "ambiguous worker warning" sync.stderr
        "remain unlinked";
      let tasks_path = Filename.concat home ".monty/tasks.local.json" in
      if Sys.file_exists tasks_path && local_tasks_json home <> [] then
        failwith "ambiguous worker identities created a shared task";
      let scan_job run_id =
        Yojson.Safe.from_file
          (Filename.concat home
             (".monty/runs/" ^ run_id ^ "/workers/same-worker/job.json"))
      in
      if Yojson.Safe.Util.member "task_key" (scan_job "run-a") <> `Null
         || Yojson.Safe.Util.member "task_key" (scan_job "run-b") <> `Null
      then failwith "ambiguous worker identities were automatically linked")

let test_launch_batch_preflight_is_all_or_nothing () =
  with_temp_root "launch-preflight" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      let missing = Filename.concat root "missing-context.md" in
      let manifest = Filename.concat home ".monty/runs/run-preflight/jobs.json" in
      let workers = Filename.concat (Filename.dirname manifest) "workers" in
      add_project ~root ~home ~env repo;
      Shell.write_file context "# Launch preflight\n";
      let launch ?(terminal = "ghostty") ?(harness = "pi")
          ?(pi_command = "pi --fixed value")
          ?(codex_command = "codex --fixed value") index =
        run ~root ~env index
          [ "launch-many"; "--manifest"; manifest; "--home"; home;
            "--terminal"; terminal; "--worktree"; "never"; "--harness";
            harness; "--pi-command"; pi_command; "--codex-command";
            codex_command ]
      in
      write_manifest manifest
        [ manifest_job ~id:"valid-first" ~branch:"cto/valid-first"
            ~title:"Valid first" ~repo ~context ();
          manifest_job ~id:"invalid-second" ~branch:"cto/invalid-second"
            ~title:"Invalid second" ~repo ~context:missing () ];
      let invalid = launch 1000 in
      if invalid.code = 0 then failwith "later invalid context passed batch preflight";
      require_contains "later invalid context" invalid.stderr "context is not an existing";
      if Sys.file_exists (Filename.concat home ".monty/tasks.local.json") then
        failwith "invalid batch created local tasks";
      if Sys.file_exists workers then failwith "invalid batch reserved worker memory";
      require_empty_log log;
      write_manifest manifest
        [ manifest_job ~id:"duplicate" ~branch:"cto/first"
            ~title:"Duplicate first" ~repo ~context ();
          manifest_job ~id:"duplicate" ~branch:"cto/second"
            ~title:"Duplicate second" ~repo ~context () ];
      let duplicate_id = launch 1001 in
      if duplicate_id.code = 0 then failwith "duplicate IDs passed batch preflight";
      require_contains "duplicate ID diagnostic" duplicate_id.stderr
        "duplicate launch worker id";
      require_contains "duplicate ID first entry" duplicate_id.stderr "job 1";
      require_contains "duplicate ID second entry" duplicate_id.stderr "job 2";
      write_manifest manifest
        [ manifest_job ~id:"branch-one" ~branch:"cto/shared"
            ~title:"Branch first" ~repo ~context ();
          manifest_job ~id:"branch-two" ~branch:"cto/shared"
            ~title:"Branch second" ~repo ~context () ];
      let duplicate_branch = launch 1002 in
      if duplicate_branch.code = 0 then
        failwith "duplicate repo+branch identity passed preflight";
      require_contains "duplicate repo branch" duplicate_branch.stderr
        "duplicate launch canonical repo+branch";
      let added =
        run ~root ~env 1003
          [ "task"; "add"; "--home"; home; "--project"; "repo";
            "--title"; "Reserved task" ]
      in
      require_code 0 added;
      let tasks_path = Filename.concat home ".monty/tasks.local.json" in
      let tasks_before = Shell.read_file tasks_path in
      write_manifest manifest
        [ manifest_job ~id:"task-one" ~branch:"cto/task-one"
            ~task_key:"local:local-001" ~title:"Task first" ~repo ~context ();
          manifest_job ~id:"task-two" ~branch:"cto/task-two"
            ~task_key:"local:local-001" ~title:"Task second" ~repo ~context () ];
      let duplicate_task = launch 1004 in
      if duplicate_task.code = 0 then failwith "duplicate task links passed preflight";
      require_contains "duplicate task link" duplicate_task.stderr
        "duplicate launch stable task link";
      require_contains "duplicate task first entry" duplicate_task.stderr "job 1";
      require_contains "duplicate task second entry" duplicate_task.stderr "job 2";
      if Shell.read_file tasks_path <> tasks_before then
        failwith "duplicate task preflight changed task state";
      write_manifest manifest
        [ manifest_job ~id:"dry-one" ~branch:"cto/dry-one" ~title:"Dry one"
            ~repo ~context ();
          manifest_job ~id:"dry-two" ~branch:"cto/dry-two" ~title:"Dry two"
            ~repo ~context () ];
      let dry = launch ~terminal:"dry-run" 1005 in
      require_code 0 dry;
      require_contains "dry-run first plan" dry.stdout "[dry-run] id: dry-one";
      require_contains "dry-run second plan" dry.stdout "[dry-run] id: dry-two";
      if Shell.read_file tasks_path <> tasks_before then
        failwith "valid dry-run created planned tasks";
      if Sys.file_exists workers then failwith "valid dry-run reserved worker memory";
      require_empty_log log;
      let codex_dry = launch ~terminal:"dry-run" ~harness:"codex" 10051 in
      require_code 0 codex_dry;
      require_contains "Codex dry-run harness" codex_dry.stdout
        "[dry-run] harness: codex";
      require_contains "Codex dry-run command" codex_dry.stdout "terminal --home";
      require_contains "Codex dry-run fixed args" codex_dry.stdout "--codex-command 'codex --fixed value'";
      if Shell.read_file tasks_path <> tasks_before then
        failwith "Codex dry-run created planned tasks";
      if Sys.file_exists workers then
        failwith "Codex dry-run reserved worker memory";
      require_empty_log log;
      let missing_dependency =
        launch ~pi_command:"monty-missing-pi --fixed value" 1006
      in
      if missing_dependency.code = 0 then
        failwith "missing fixed-argument executable passed preflight";
      require_contains "missing executable name" missing_dependency.stderr
        "monty-missing-pi";
      if Shell.read_file tasks_path <> tasks_before then
        failwith "dependency preflight changed task state";
      require_empty_log log)

let test_launch_batch_partial_failure_and_safe_retry () =
  with_temp_root "launch-partial" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      let manifest = Filename.concat home ".monty/runs/run-partial/jobs.json" in
      let osascript = Filename.concat root "fake-bin/osascript" in
      let counter = Filename.concat root "osascript-count" in
      add_project ~root ~home ~env repo;
      Shell.write_file context "# Partial launch\n";
      Shell.write_file osascript
        (String.concat "\n"
           [ "#!/bin/sh"; "set -eu";
             "count=0";
             "if [ -f " ^ Shell.quote counter ^ " ]; then count=$(cat "
             ^ Shell.quote counter ^ "); fi";
             "count=$((count + 1))";
             "printf '%s\\n' \"$count\" > " ^ Shell.quote counter;
             "printf 'osascript-request-%s\\n' \"$count\" >> " ^ Shell.quote log;
             "if [ \"$count\" -eq 2 ]; then exit 42; fi"; "exit 0"; "" ]);
      Shell.chmod_executable osascript;
      write_manifest manifest
        [ manifest_job ~id:"job-1" ~branch:"cto/job-1" ~title:"Job one" ~repo
            ~context ();
          manifest_job ~id:"job-2" ~branch:"cto/job-2" ~title:"Job two" ~repo
            ~context ();
          manifest_job ~id:"job-3" ~branch:"cto/job-3" ~title:"Job three" ~repo
            ~context () ];
      let args =
        [ "launch-many"; "--manifest"; manifest; "--home"; home;
          "--terminal"; "ghostty"; "--worktree"; "never"; "--pi-command";
          "pi --model fake" ]
      in
      let first = run ~root ~env 1100 args in
      if first.code = 0 then failwith "second terminal failure passed batch launch";
      require_contains "first requested result" first.stdout
        "job-1 (Job one): launch-requested";
      require_contains "second ambiguous result" first.stdout
        "job-2 (Job two): launch-requested";
      require_contains "third unattempted result" first.stdout
        "job-3 (Job three): unattempted/prepared";
      require_contains "requested recovery" first.stdout " resume 'job-1'";
      require_contains "safe batch retry" first.stdout
        ("launch-many --manifest '" ^ manifest ^ "'");
      require_contains "fixed pi retry" first.stdout
        "--pi-command 'pi --model fake'";
      let job_file id =
        Filename.concat home
          (".monty/runs/run-partial/workers/" ^ id ^ "/job.json")
      in
      if job_status (job_file "job-1") <> "launch-requested" then
        failwith "first job did not persist launch-requested";
      if job_status (job_file "job-2") <> "launch-requested" then
        failwith "second job did not preserve ambiguous launch-requested state";
      if job_status (job_file "job-3") <> "prepared" then
        failwith "third job did not remain prepared";
      if count_lines_containing log "osascript-request" <> 2 then
        failwith "partial launch did not stop after the second request";
      let memory =
        Filename.concat home ".monty/runs/run-partial/workers/job-1/memory.md"
      in
      Shell.write_file memory "preserve existing worker memory\n";
      let second = run ~root ~env 1101 args in
      require_code 0 second;
      if count_lines_containing log "osascript-request" <> 3 then
        failwith "batch retry duplicated a requested job or skipped the prepared job";
      if Shell.read_file memory <> "preserve existing worker memory\n" then
        failwith "batch retry overwrote existing worker memory";
      List.iter
        (fun id ->
          if job_status (job_file id) <> "launch-requested" then
            failf "retried job %s did not end launch-requested" id)
        [ "job-1"; "job-2"; "job-3" ])

let test_launch_request_fault_boundaries () =
  [ ("launch-before-request-state", "launch-failed", 0);
    ("launch-after-request-state", "launch-requested", 0);
    ("launch-after-terminal-request", "launch-requested", 1) ]
  |> List.iteri (fun scenario (checkpoint, expected_status, initial_requests) ->
         with_temp_root ("launch-fault-" ^ string_of_int scenario) (fun root ->
             let home, log, env = setup_environment root in
             let repo = Filename.concat root "repo" in
             let context = Filename.concat root "context.md" in
             let manifest =
               Filename.concat home ".monty/runs/run-fault/jobs.json"
             in
             let osascript = Filename.concat root "fake-bin/osascript" in
             add_project ~root ~home ~env repo;
             Shell.write_file context "# Request fault\n";
             Shell.write_file osascript
               (String.concat "\n"
                  [ "#!/bin/sh"; "printf 'fault-terminal-request\\n' >> "
                    ^ Shell.quote log; "exit 0"; "" ]);
             Shell.chmod_executable osascript;
             write_manifest manifest
               [ manifest_job ~id:"fault-job" ~branch:"cto/fault-job"
                   ~title:"Fault job" ~repo ~context () ];
             let args =
               [ "launch-many"; "--manifest"; manifest; "--home"; home;
                 "--terminal"; "ghostty"; "--worktree"; "never" ]
             in
             let fault_env =
               replace_env env [ ("MONTY_FAULT_INJECT", checkpoint) ]
             in
             let interrupted = run ~root ~env:fault_env (1200 + scenario) args in
             if interrupted.code = 0 then
               failf "request fault %s unexpectedly succeeded" checkpoint;
             require_contains "request fault output" interrupted.stdout checkpoint;
             let job_file =
               Filename.concat home
                 ".monty/runs/run-fault/workers/fault-job/job.json"
             in
             if job_status job_file <> expected_status then
               failf "request fault %s persisted %s instead of %s" checkpoint
                 (job_status job_file) expected_status;
             if count_lines_containing log "fault-terminal-request" <> initial_requests
             then failf "request fault %s made an unexpected terminal request" checkpoint;
             let replay = run ~root ~env (1210 + scenario) args in
             require_code 0 replay;
             let replay_requests =
               count_lines_containing log "fault-terminal-request"
             in
             let expected_replay_requests =
               if String.equal expected_status "launch-failed" then 1
               else initial_requests
             in
             if replay_requests <> expected_replay_requests then
               failf "batch replay at %s duplicated or omitted a terminal request" checkpoint;
             if String.equal expected_status "launch-requested" then (
               let resumed =
                 run ~root ~env (1220 + scenario)
                   [ "resume"; "fault-job"; "--home"; home; "--terminal";
                     "ghostty"; "--worktree"; "never" ]
               in
               require_code 0 resumed;
               if
                 count_lines_containing log "fault-terminal-request"
                 <> replay_requests + 1
               then failf "explicit resume at %s did not request exactly once" checkpoint)))

let test_reservation_failures_are_structured_and_replayable () =
  [ ("state-store-before-rename", false, false);
    ("reserve-after-stage", false, false);
    ("reserve-abrupt-after-stage", false, false);
    ("reserve-after-tasks", true, false);
    ("reserve-abrupt-after-tasks", true, false);
    ("reserve-after-install", true, true) ]
  |> List.iteri (fun scenario (checkpoint, task_persisted, worker_persisted) ->
         with_temp_root ("reserve-fault-" ^ string_of_int scenario) (fun root ->
             let home, log, env = setup_environment root in
             let repo = Filename.concat root "repo" in
             let context = Filename.concat root "context.md" in
             let manifest =
               Filename.concat home ".monty/runs/run-reserve/jobs.json"
             in
             let osascript = Filename.concat root "fake-bin/osascript" in
             add_project ~root ~home ~env repo;
             Shell.write_file context "# Reservation fault\n";
             Shell.write_file osascript "#!/bin/sh\nexit 0\n";
             Shell.chmod_executable osascript;
             write_manifest manifest
               [ manifest_job ~id:"reserve-worker"
                   ~branch:"cto/reserve-worker" ~title:"Reserve worker" ~repo
                   ~context () ];
             let args =
               [ "launch-many"; "--manifest"; manifest; "--home"; home;
                 "--terminal"; "ghostty"; "--worktree"; "never" ]
             in
             let fault_env =
               replace_env env [ ("MONTY_FAULT_INJECT", checkpoint) ]
             in
             let interrupted = run ~root ~env:fault_env (1330 + scenario) args in
             if interrupted.code = 0 then
               failf "reservation fault %s unexpectedly succeeded" checkpoint;
             require_contains "structured reservation result" interrupted.stdout
               "reserve-worker (Reserve worker): unattempted/prepared";
             require_contains "reservation retry command" interrupted.stdout
               ("launch-many --manifest '" ^ manifest ^ "'");
             let expected_fault =
               if String.equal checkpoint "state-store-before-rename" then
                 "fault injected before atomic state rename"
               else checkpoint
             in
             require_contains "reservation fault stderr" interrupted.stderr
               expected_fault;
             let tasks_path = Filename.concat home ".monty/tasks.local.json" in
             if Sys.file_exists tasks_path <> task_persisted then
               failf "reservation fault %s task persistence mismatch" checkpoint;
             let job_file =
               Filename.concat home
                 ".monty/runs/run-reserve/workers/reserve-worker/job.json"
             in
             if Sys.file_exists job_file <> worker_persisted then
               failf "reservation fault %s worker persistence mismatch" checkpoint;
             let abrupt = string_contains checkpoint "reserve-abrupt" in
             let staged_memory =
               Filename.concat home
                 ".monty/runs/run-reserve/.reservations/reserve-worker/memory.md"
             in
             if abrupt then (
               if not (Sys.file_exists staged_memory) then
                 failf "abrupt reservation fault %s did not leave durable staging"
                   checkpoint;
               Shell.write_file staged_memory "preserve staged memory\n");
             require_empty_log log;
             let replay = run ~root ~env (1340 + scenario) args in
             require_code 0 replay;
             if job_status job_file <> "launch-requested" then
               failf "reservation fault %s did not replay safely" checkpoint;
             if abrupt then
               let installed_memory =
                 Filename.concat home
                   ".monty/runs/run-reserve/workers/reserve-worker/memory.md"
               in
               if Shell.read_file installed_memory <> "preserve staged memory\n" then
                 failf "reservation replay %s overwrote staged memory" checkpoint))

let test_unsafe_staged_reservations_are_rejected () =
  [ "forged-run-dir"; "symlinked-instructions" ]
  |> List.iteri (fun scenario mutation ->
         with_temp_root ("unsafe-stage-" ^ string_of_int scenario) (fun root ->
             let home, _log, env = setup_environment root in
             let repo = Filename.concat root "repo" in
             let context = Filename.concat root "context.md" in
             let manifest =
               Filename.concat home ".monty/runs/run-stage/jobs.json"
             in
             add_project ~root ~home ~env repo;
             Shell.write_file context "# Unsafe staged reservation\n";
             write_manifest manifest
               [ manifest_job ~id:"stage-worker" ~branch:"cto/stage-worker"
                   ~title:"Stage worker" ~repo ~context () ];
             let args =
               [ "launch-many"; "--manifest"; manifest; "--home"; home;
                 "--terminal"; "ghostty"; "--worktree"; "never" ]
             in
             let abrupt_env =
               replace_env env
                 [ ("MONTY_FAULT_INJECT", "reserve-abrupt-after-stage") ]
             in
             let interrupted = run ~root ~env:abrupt_env (1350 + scenario) args in
             if interrupted.code = 0 then failwith "unsafe stage setup succeeded";
             let stage =
               Filename.concat home
                 ".monty/runs/run-stage/.reservations/stage-worker"
             in
             if String.equal mutation "forged-run-dir" then
               let path = Filename.concat stage "job.json" in
               let json = Yojson.Safe.from_file path in
               let forged =
                 match json with
                 | `Assoc fields ->
                     `Assoc
                       (("run_dir", `String (Filename.concat root "forged"))
                       :: List.remove_assoc "run_dir" fields)
                 | _ -> failwith "staged job was not an object"
               in
               Yojson.Safe.to_file path forged
             else
               let path = Filename.concat stage "MONTY.md" in
               let victim = Filename.concat root "victim.md" in
               Sys.remove path;
               Shell.write_file victim "victim\n";
               Unix.symlink victim path;
             let replay = run ~root ~env (1360 + scenario) args in
             if replay.code = 0 then
               failf "unsafe staged reservation %s was installed" mutation;
             require_contains "unsafe staging diagnostic" replay.stderr
               "staged reservation conflicts";
             let canonical =
               Filename.concat home
                 ".monty/runs/run-stage/workers/stage-worker/job.json"
             in
             if Sys.file_exists canonical then
               failf "unsafe staged reservation %s poisoned canonical state"
                 mutation))

let test_completion_accepts_every_open_launch_state () =
  [ "active"; "prepared"; "launch-failed"; "launch-requested" ]
  |> List.iteri (fun scenario status ->
         with_temp_root ("complete-open-" ^ string_of_int scenario) (fun root ->
             let home, log, env = setup_environment root in
             let repo = Filename.concat root "repo" in
             let context = Filename.concat root "context.md" in
             let run_dir = Filename.concat home ".monty/runs/run-open" in
             let worker_dir = Filename.concat run_dir "workers/open-worker" in
             let archive = Filename.concat run_dir "archive/open-worker/job.json" in
             Shell.ensure_dir repo;
             Shell.write_file context "# Complete open state\n";
             Shell.ensure_dir worker_dir;
             Yojson.Safe.to_file (Filename.concat worker_dir "job.json")
               (lifecycle_job_json ~id:"open-worker" ~title:"Open worker"
                  ~branch:"cto/open-worker" ~status ~repo ~context ~worker_dir
                  ~run_dir ());
             let completed =
               run ~root ~env (1300 + scenario)
                 [ "done"; "open-worker"; "--home"; home ]
             in
             require_code 0 completed;
             if job_status archive <> "done" then
               failf "completion did not archive launch state %s" status;
             require_empty_log log))

let test_launch_rejects_global_identity_conflicts_and_races () =
  with_temp_root "launch-global-conflicts" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      add_project ~root ~home ~env repo;
      Shell.write_file context "# Global launch identity\n";
      ignore
        (write_worker ~home ~run_id:"run-old" ~id:"shared-id"
           ~title:"Existing ID" ~branch:"cto/existing-id" ~repo ~context ());
      let id_manifest = Filename.concat home ".monty/runs/run-new/jobs.json" in
      write_manifest id_manifest
        [ manifest_job ~id:"shared-id" ~branch:"cto/new-id" ~title:"New ID"
            ~repo ~context () ];
      let id_conflict =
        run ~root ~env 1310
          [ "launch-many"; "--manifest"; id_manifest; "--home"; home;
            "--terminal"; "dry-run"; "--worktree"; "never" ]
      in
      if id_conflict.code = 0 then failwith "global duplicate worker ID launched";
      require_contains "global worker ID conflict" id_conflict.stderr
        "launch worker id identity";
      let added =
        run ~root ~env 1311
          [ "task"; "add"; "--home"; home; "--project"; "repo";
            "--title"; "Shared durable task" ]
      in
      require_code 0 added;
      ignore
        (write_worker ~home ~run_id:"run-task-owner" ~id:"task-owner"
           ~title:"Task owner" ~branch:"cto/task-owner"
           ~task_key:"local:local-001" ~repo ~context ());
      let tasks_path = Filename.concat home ".monty/tasks.local.json" in
      let tasks_before = Shell.read_file tasks_path in
      let task_manifest =
        Filename.concat home ".monty/runs/run-task-new/jobs.json"
      in
      write_manifest task_manifest
        [ manifest_job ~id:"task-new" ~branch:"cto/task-new"
            ~task_key:"local:local-001" ~title:"New task claimant" ~repo
            ~context () ];
      let task_conflict =
        run ~root ~env 1312
          [ "launch-many"; "--manifest"; task_manifest; "--home"; home;
            "--terminal"; "dry-run"; "--worktree"; "never" ]
      in
      if task_conflict.code = 0 then failwith "global duplicate task link launched";
      require_contains "global task conflict" task_conflict.stderr
        "launch stable task link identity";
      if Shell.read_file tasks_path <> tasks_before then
        failwith "global task conflict mutated the task registry";
      let osascript = Filename.concat root "fake-bin/osascript" in
      Shell.write_file osascript
        (String.concat "\n"
           [ "#!/bin/sh"; "printf 'race-terminal-request\\n' >> "
             ^ Shell.quote log; "exit 0"; "" ]);
      Shell.chmod_executable osascript;
      let race_manifest run_id id title =
        let path = Filename.concat home (".monty/runs/" ^ run_id ^ "/jobs.json") in
        write_manifest path
          [ manifest_job ~id ~branch:"cto/raced-branch" ~title ~repo ~context () ];
        path
      in
      let race_a = race_manifest "run-race-a" "race-a" "Race A" in
      let race_b = race_manifest "run-race-b" "race-b" "Race B" in
      let spawn_race index manifest =
        spawn ~root ~env index
          [ "launch-many"; "--manifest"; manifest; "--home"; home;
            "--terminal"; "ghostty"; "--worktree"; "never" ]
      in
      let results =
        [ spawn_race 1313 race_a; spawn_race 1314 race_b ] |> List.map await
      in
      let successes = List.filter (fun result -> result.code = 0) results in
      if List.length successes <> 1 then
        failf "concurrent duplicate identity launches had %d successes"
          (List.length successes);
      if count_lines_containing log "race-terminal-request" <> 1 then
        failwith "concurrent duplicate identity requested more than one terminal")

let test_codex_launch_preserves_mantle_environment () =
  with_temp_root "codex-mantle-environment" (fun root ->
      let home, log, base_env = setup_environment root in
      let names =
        [ "PI_CODING_AGENT_DIR"; "CODEX_HOME"; "CLAUDE_CONFIG_DIR"; "MANTLE_CONTEXT"; "_MANTLE_SELECTED_HOME";
          "_MANTLE_PREV_STATE"; "_MANTLE_PREV_HOME" ]
      in
      let clean_env =
        Array.to_list base_env
        |> List.filter (fun entry ->
               not (List.exists (fun name -> String.starts_with ~prefix:(name ^ "=") entry) names))
        |> Array.of_list
      in
      let caller bindings =
        replace_env clean_env (("OPENAI_API_KEY", "unrelated-secret-sentinel") :: bindings)
      in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      init_git_repo repo;
      add_project ~root ~home ~env:clean_env repo;
      Shell.write_file context "# Mantle environment\n";
      let osascript = Filename.concat root "fake-bin/osascript" in
      Shell.write_file osascript "#!/bin/sh\nexit 0\n";
      Shell.chmod_executable osascript;
      let observed = Filename.concat root "observed" in
      Shell.ensure_dir observed;
      let observe =
        names
        |> List.map (fun name ->
               Printf.sprintf "printf '%%s\\n%%s' \"${%s+x}\" \"${%s-}\" > %s"
                 name name (Shell.quote (Filename.concat observed name)))
        |> String.concat "\n"
      in
      let codex = Filename.concat root "fake-bin/codex" in
      let observe =
        observe ^ "\n" ^ Shell.quote executable
        ^ " projects list --all-groups --home " ^ Shell.quote home
        ^ " > " ^ Shell.quote (Filename.concat observed "projects") ^ "\n"
      in
      Shell.write_file codex
        ("#!/bin/sh\nset -eu\ntest \"${MONTY_CODEX_YOLO-}\" = false\n" ^ observe);
      Shell.chmod_executable codex;
      let injection_marker = Filename.concat root "SHOULD_NOT_EXIST" in
      let attack = "touch " ^ Shell.quote injection_marker in
      let special =
        Filename.concat root
          ("quote'\"\\$(" ^ attack ^ ")`" ^ attack ^ "`;\nexport CODEX_HOME=forged\n")
      in
      let profile name previous home_path =
        [ ("CODEX_HOME", home_path); ("MANTLE_CONTEXT", name);
          ("_MANTLE_SELECTED_HOME", home_path); ("_MANTLE_PREV_STATE", previous);
          ("_MANTLE_PREV_HOME", if previous = "unset" then "" else special) ]
      in
      let work = profile "work" "unset" (Filename.concat root "work") in
      let private_profile = profile "private" "export" (Filename.concat root "private") in
      let cases =
        [ ("work", work, Some "work");
          ("private", private_profile, Some "private");
          ("client-a", profile "client-a" "local" special, Some "client-a");
          ("off", [], None);
          ("custom-home", [ ("CODEX_HOME", special) ], None);
          ("pi-agent-home", [ ("PI_CODING_AGENT_DIR", special) ], None);
          ("empty", List.map (fun name -> (name, "")) names, None);
          ("mixed", [ ("CODEX_HOME", ""); ("_MANTLE_PREV_HOME", "") ], None) ]
      in
      let check_received bindings selected =
        List.iter (fun name ->
            let expected =
              match List.assoc_opt name bindings with
              | None -> "\n"
              | Some value -> "x\n" ^ value
            in
            if Shell.read_file (Filename.concat observed name) <> expected then
              failf "Codex received the wrong %s: expected %S, received %S" name expected
                (Shell.read_file (Filename.concat observed name))) names;
        let projects = Shell.read_file (Filename.concat observed "projects") in
        match selected with
        | Some profile ->
            require_line "child Monty profile" projects
              ("Profile: " ^ profile ^ " (Mantle) | Groups: all (--all-groups)")
        | None ->
            if string_contains projects "Profile:" then
              failwith "child Monty inherited a stale Mantle profile"
      in
      let options terminal =
        [ "--home"; home; "--harness"; "codex"; "--codex-command"; "codex";
          "--terminal"; terminal ]
      in
      let counter = ref 13200 in
      let cli bindings args =
        incr counter;
        run ~root ~env:(caller bindings) !counter args
      in
      let terminal_home = Filename.concat root "terminal-home" in
      Shell.ensure_dir terminal_home;
      let execute script bindings selected =
        (match List.assoc_opt "CODEX_HOME" bindings with
        | Some path -> require_contains "launch script preserves selected account" (Shell.read_file script)
            ("export CODEX_HOME=" ^ Shell.quote path ^ "\n")
        | None -> require_line "launch script preserves unset account" (Shell.read_file script) "unset CODEX_HOME");
        (* Isolate the Ghostty environment preamble from the service protocol,
           which has its own checkout-binary E2E test. The original authenticated
           launch script is never edited by this fixture. *)
        let fixture = script ^ ".environment-test" in
        let prefix = "exec env MONTY_CODEX_YOLO=false " ^ Shell.quote executable ^ " terminal" in
        Shell.write_file fixture (Shell.read_file script |> String.split_on_char '\n'
          |> List.map (fun line -> if String.starts_with ~prefix line then
            "exec env MONTY_CODEX_YOLO=false " ^ Shell.quote codex ^ String.sub line (String.length prefix) (String.length line-String.length prefix)
            else line) |> String.concat "\n");
        let script = fixture in
        List.iter (fun conflicting ->
            (* Exercise Ghostty's actual login-shell command with an isolated
               startup file, not the developer's shell configuration. *)
            let terminal_bindings =
              if conflicting then ("MONTY_CODEX_YOLO", "true") :: profile "stale" "export" "/stale/codex" else []
            in
            Shell.write_file (Filename.concat terminal_home ".zprofile")
              (terminal_bindings
              |> List.map (fun (name, value) -> "export " ^ name ^ "=" ^ Shell.quote value)
              |> String.concat "\n");
            let terminal_env =
              replace_env
                [| "HOME=" ^ terminal_home; "ZDOTDIR=" ^ terminal_home;
                   "PATH=/usr/bin:/bin" |] terminal_bindings
            in
            List.iter (fun name ->
                let path = Filename.concat observed name in
                if Sys.file_exists path then Sys.remove path) names;
            let program, args =
              if Sys.file_exists "/bin/zsh" then ("/bin/zsh", [ "-l"; script ])
              else ("/bin/sh", [ script ])
            in
            incr counter;
            let result=run ~program ~root ~env:terminal_env !counter args in
            require_code 0 result;
            check_received bindings selected) [ false; true ]
      in
      let script_for id =
        Filename.concat home (".monty/runs/manual/workers/" ^ id ^ "/job.json")
        |> Yojson.Safe.from_file |> json_string "launch_script"
      in
      List.iter (fun (label, bindings, selected) ->
          require_code 0
            (cli bindings
               ([ "launch"; "--repo"; repo; "--title"; label; "--context"; context;
                  "--branch"; "cto/" ^ label; "--worktree"; "never" ]
               @ options "ghostty"));
          let script = script_for label in
          execute script bindings selected;
          if string_contains (Shell.read_file script) "unrelated-secret-sentinel" then
            failwith "launch script captured an unrelated credential";
          (* Head-butler Codex requires the managed service; API E2E exercises
             its endpoint and profile checks. *)
          ()) cases;
      let script = script_for "work" in
      let resume terminal = [ "resume"; "work" ] @ options terminal in
      (* Repeatedly change/disable the profile of the same worker. Its historical
         values must authenticate, but the next execution must use today's values. *)
      List.iter (fun (_, bindings, selected) ->
          require_code 0 (cli bindings (resume "dry-run"));
          require_code 0 (cli bindings (resume "ghostty"));
          execute script bindings selected) cases;
      let sidecar = Filename.concat home ".monty/runs/manual/workers/work/codex-session-id" in
      Shell.write_file sidecar "exact-session";
      require_code 0 (cli work (resume "ghostty"));
      execute script work (Some "work");
      (* A newly captured id must not invalidate the previous exact template. *)
      Shell.write_file sidecar "successor-session";
      require_code 0 (cli private_profile (resume "ghostty"));
      execute script private_profile (Some "private");
      require_code 0 (cli [] (resume "ghostty" @ [ "--fresh" ]));
      execute script [] None;
      (* Retry a failed request after changing the profile. *)
      let retry_args =
        [ "launch"; "--repo"; repo; "--title"; "Retry profile"; "--context"; context;
          "--branch"; "cto/retry-profile"; "--worktree"; "never" ] @ options "ghostty"
      in
      incr counter;
      let failed =
        run ~root
          ~env:(replace_env (caller work) [ ("MONTY_FAULT_INJECT", "launch-before-request-state") ])
          !counter retry_args
      in
      require_code 1 failed;
      require_code 0 (cli private_profile retry_args);
      execute (script_for "retry-profile") private_profile (Some "private");
      (* A script missing the current environment or ownership markers is rejected. *)
      require_code 0 (cli [] (resume "ghostty"));
      let current = Shell.read_file script in
      List.iter (fun markerless ->
          let legacy =
            current |> String.split_on_char '\n'
            |> List.filter (fun line ->
                   not (List.exists (fun name -> line = "unset " ^ name) names)
                   && not (markerless &&
                           (line = "# monty-launch-script-v1"
                            || String.starts_with ~prefix:"export MONTY_JOB_FILE=" line)))
            |> String.concat "\n"
          in
          Shell.write_file script legacy;
          List.iter (fun terminal ->
              let rejected = cli work (resume terminal) in
              require_code 1 rejected;
              require_contains "incomplete script rejected" rejected.stderr "Monty-owned";
              if read_file script <> legacy then failwith "rejected script was overwritten")
            [ "dry-run"; "ghostty" ];
          Shell.write_file script current) [ false; true ];
      require_code 0 (cli work (resume "ghostty"));
      let valid = Shell.read_file script in
      let replace_line replacement =
        valid |> String.split_on_char '\n'
        |> List.map (fun line ->
               if String.starts_with ~prefix:"export CODEX_HOME=" line then replacement else line)
        |> String.concat "\n"
      in
      let forged =
        [ valid ^ attack ^ "\n";
          replace_line ("export CODEX_HOME=$(" ^ attack ^ ")");
          replace_line ("export CODEX_HOME='safe'; " ^ attack);
          replace_line "export CODEX_HOME='safe'\nexport OPENAI_API_KEY='forged'";
          replace_line "export CODEX_HOME='safe'\nunset CODEX_HOME";
          replace_line "unset CODEX_HOME\nunset MANTLE_CONTEXT";
          replace_line "# missing CODEX_HOME" ]
      in
      let tasks = Filename.concat home ".monty/tasks.local.json" in
      let tasks_before = Shell.read_file tasks in
      List.iter (fun contents ->
          Shell.write_file script contents;
          List.iter (fun terminal ->
              let rejected = cli [] (resume terminal) in
              require_code 1 rejected;
              require_contains "tampered environment rejected" rejected.stderr "Monty-owned";
              if Shell.read_file script <> contents || Shell.read_file tasks <> tasks_before then
                failwith "tampered script rejection mutated script or tasks") [ "dry-run"; "ghostty" ]) forged;
      if Sys.file_exists injection_marker then failwith "quoted environment executed shell code";
      require_empty_log log;
      (* Headless uses the configured server account, even when preparation
         happened under a different profile. Client account drift is rejected. *)
      install_create_wt ~root ~log;
      install_codex_exec ~root ~log;
      Shell.write_file codex ("#!/bin/sh\n" ^ observe ^ Shell.read_file codex);
      let manifest = Filename.concat home ".monty/runs/headless-profile/jobs.json" in
      write_manifest manifest
        [ manifest_job ~id:"headless-profile" ~branch:"cto/headless-profile"
            ~title:"Headless profile" ~repo ~context () ];
      let headless_options =
        [ "--home"; home; "--harness"; "codex"; "--codex-command"; "codex";
          "--agent-profile"; "solo" ]
      in
      require_code 0
        (cli work ([ "headless"; "prepare-many"; "--manifest"; manifest ] @ headless_options));
      require_code 0
        (cli private_profile ([ "headless"; "run"; "--wait"; "headless-profile" ] @ headless_options));
      let server_bindings=["CODEX_HOME",Unix.realpath(List.assoc "CODEX_HOME" private_profile)] in
      check_received server_bindings None;
      let rejected=cli [] ([ "headless"; "resume"; "--wait"; "headless-profile" ] @ headless_options) in
      require_code 1 rejected;
      require_contains "unconfigured account rejected" rejected.stderr "profile matching the native account directory";
      check_received server_bindings None)

let test_retry_uses_recorded_script_and_absolute_commands () =
  with_temp_root "launch-script-owner" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      let manifest = Filename.concat home ".monty/runs/run-script/jobs.json" in
      let osascript = Filename.concat root "fake-bin/osascript" in
      add_project ~root ~home ~env repo;
      Shell.write_file context "# Recorded script\n";
      Shell.write_file osascript "#!/bin/sh\nexit 0\n";
      Shell.chmod_executable osascript;
      write_manifest manifest
        [ manifest_job ~id:"script-worker" ~branch:"cto/script-worker"
            ~title:"Script worker" ~repo ~context () ];
      let args =
        [ "launch-many"; "--manifest"; manifest; "--home"; home;
          "--terminal"; "ghostty"; "--worktree"; "never"; "--pi-command";
          "/bin/sh -c true --fixed argument" ]
      in
      let fault_env =
        replace_env env [ ("MONTY_FAULT_INJECT", "launch-before-request-state") ]
      in
      let interrupted = run ~root ~env:fault_env 1320 args in
      if interrupted.code = 0 then failwith "script ownership setup fault succeeded";
      let job_file =
        Filename.concat home
          ".monty/runs/run-script/workers/script-worker/job.json"
      in
      let json = Yojson.Safe.from_file job_file in
      let recorded_script = json_string "launch_script" json in
      if not (Sys.file_exists recorded_script) then
        failwith "failed launch did not leave its owned script";
      let current_script = Shell.read_file recorded_script in
      let legacy_script =
        current_script |> String.split_on_char '\n'
        |> List.filter (fun line ->
               not (String.equal line "# monty-launch-script-v1"))
        |> String.concat "\n"
      in
      Shell.write_file recorded_script legacy_script;
      let rejected = run ~root ~env 13201 args in
      require_code 1 rejected;
      require_contains "markerless script rejected" rejected.stderr "Monty-owned";
      if read_file recorded_script <> legacy_script then failwith "rejected script was overwritten";
      Shell.write_file recorded_script current_script;
      let unrelated_dir = Filename.concat root "unrelated-scripts" in
      let unrelated =
        Filename.concat unrelated_dir "monty-script-worker-launch.sh"
      in
      Shell.ensure_dir unrelated_dir;
      Shell.write_file unrelated "do not overwrite\n";
      let retried =
        run ~root ~env 1321
          (args @ [ "--script-dir"; unrelated_dir ])
      in
      require_code 0 retried;
      if Shell.read_file unrelated <> "do not overwrite\n" then
        failwith "retry overwrote an unrelated script in the new script directory";
      let persisted =
        Yojson.Safe.from_file job_file |> json_string "launch_script"
      in
      if not (String.equal persisted recorded_script) then
        failwith "retry changed recorded script ownership";
      let resume_args backend =
        [ "resume"; "script-worker"; "--home"; home; "--terminal"; backend;
          "--pi-command"; "/bin/sh -c true --fixed argument" ]
      in
      let strange_path = Filename.concat root "quote'$(touch SHOULD_NOT_EXIST)" in
      let changed_path = Filename.concat root "fake-bin" ^ ":/usr/bin:/bin:" ^ strange_path in
      let changed_env = replace_env env [ ("PATH", changed_path) ] in
      require_code 0 (run ~root ~env:changed_env 13211 (resume_args "dry-run"));
      require_code 0 (run ~root ~env:changed_env 13212 (resume_args "ghostty"));
      require_contains "fresh PATH in rewritten script" (read_file recorded_script)
        ("export PATH=" ^ Shell.quote changed_path);
      require_code 0 (run ~root ~env 13213 (resume_args "dry-run"));
      require_code 0 (run ~root ~env 13214 (resume_args "ghostty"));
      let forged = read_file recorded_script ^ "\ntouch SHOULD_NOT_EXIST\n" in
      Shell.write_file recorded_script forged;
      List.iteri (fun index backend ->
          let rejected = run ~root ~env (13215 + index) (resume_args backend) in
          require_code 1 rejected;
          require_contains "forged script rejected" rejected.stderr "Monty-owned";
          if read_file recorded_script <> forged then failwith "forged script was overwritten")
        [ "dry-run"; "ghostty" ];
      require_empty_log log)

let test_ghostty_launch_script_matches_ensure_worktree_cli () =
  with_temp_root "ghostty-ensure-worktree-contract" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      let manifest =
        Filename.concat home ".monty/runs/run-ghostty-contract/jobs.json"
      in
      let osascript = Filename.concat root "fake-bin/osascript" in
      init_git_repo repo;
      add_project ~root ~home ~env repo;
      install_create_wt ~root ~log;
      Shell.write_file context "# Ghostty ensure-worktree contract\n";
      Shell.write_file osascript "#!/bin/sh\nexit 0\n";
      Shell.chmod_executable osascript;
      write_manifest manifest
        [ manifest_job ~id:"ghostty-contract"
            ~branch:"cto/ghostty-contract" ~title:"Ghostty contract" ~repo
            ~context () ];
      let launched =
        run ~root ~env 1322
          [ "launch-many"; "--manifest"; manifest; "--home"; home;
            "--terminal"; "ghostty"; "--worktree"; "always";
            "--pi-command"; "/bin/sh -c true"; "--wt-command"; "wt" ]
      in
      require_code 0 launched;
      let job_file =
        Filename.concat home
          ".monty/runs/run-ghostty-contract/workers/ghostty-contract/job.json"
      in
      let launch_script =
        Yojson.Safe.from_file job_file |> json_string "launch_script"
      in
      let script_contents = Shell.read_file launch_script in
      require_contains "Ghostty script raw ensure-worktree invocation"
        script_contents "ensure-worktree --repo";
      if string_contains script_contents "--branch 'cto/ghostty-contract' --home"
      then
        failf "Ghostty script passed unsupported --home to ensure-worktree:\n%s"
          script_contents;
      let server_env=replace_env env ["MONTY_PI_COMMAND","/bin/sh -c true"] in
      prepare_test_server ~root ~env:server_env ["headless";"run"];
      let result=run ~program:"/bin/sh" ~root ~env:server_env 1323 [launch_script] in
      require_code 0 result;
      match Native_session.read ~harness:Harness.Pi (Filename.dirname job_file) with
      | Error message->failwith message
      | Ok None->failwith "server did not capture the Ghostty worker's native session"
      | Ok(Some _)->())

let test_lifecycle_rejects_cross_project_and_owned_task_links () =
  with_temp_root "lifecycle-task-ownership" (fun root ->
      let home, log, env = setup_environment root in
      let repo_a = Filename.concat root "repo-a" in
      let repo_b = Filename.concat root "repo-b" in
      let context = Filename.concat root "context.md" in
      add_project ~root ~home ~env repo_a;
      add_project ~root ~home ~env repo_b;
      Shell.write_file context "# Task ownership\n";
      let added =
        run ~root ~env 1950
          [ "task"; "add"; "--home"; home; "--project"; "repo-b";
            "--title"; "Other project task" ]
      in
      require_code 0 added;
      let run_dir = Filename.concat home ".monty/runs/run-owner" in
      let worker_dir = Filename.concat run_dir "workers/owner-worker" in
      let job_file = Filename.concat worker_dir "job.json" in
      Shell.ensure_dir worker_dir;
      Yojson.Safe.to_file job_file
        (lifecycle_job_json ~id:"owner-worker" ~title:"Owner worker"
           ~branch:"cto/owner-worker" ~task_key:"local:local-001" ~repo:repo_a
           ~context ~worker_dir ~run_dir ());
      let tasks_path = Filename.concat home ".monty/tasks.local.json" in
      let tasks_before = Shell.read_file tasks_path in
      let job_before = Shell.read_file job_file in
      let cross_project =
        run ~root ~env 1951 [ "done"; "owner-worker"; "--home"; home ]
      in
      if cross_project.code = 0 then
        failwith "completion closed a cross-project task link";
      require_contains "cross-project lifecycle guard" cross_project.stderr
        "links task";
      if Shell.read_file tasks_path <> tasks_before
         || Shell.read_file job_file <> job_before
      then failwith "cross-project lifecycle guard mutated durable state";
      let tasks = Yojson.Safe.from_file tasks_path in
      let owned =
        match tasks with
        | `Assoc fields ->
            let values =
              match List.assoc "tasks" fields with
              | `List [ `Assoc task ] ->
                  `List
                    [ `Assoc
                        (("project", `String "repo-a")
                        :: ("worker_id", `String "different-worker")
                        :: List.remove_assoc "worker_id"
                             (List.remove_assoc "project" task)) ]
              | _ -> failwith "unexpected local task fixture"
            in
            `Assoc (("tasks", values) :: List.remove_assoc "tasks" fields)
        | _ -> failwith "unexpected task registry fixture"
      in
      Yojson.Safe.to_file tasks_path owned;
      let owned_before = Shell.read_file tasks_path in
      let wrong_owner =
        run ~root ~env 1952 [ "done"; "owner-worker"; "--home"; home ]
      in
      if wrong_owner.code = 0 then
        failwith "completion closed a task owned by another worker";
      require_contains "worker ownership lifecycle guard" wrong_owner.stderr
        "owned by worker";
      if Shell.read_file tasks_path <> owned_before
         || Shell.read_file job_file <> job_before
      then failwith "worker ownership lifecycle guard mutated durable state";
      require_empty_log log)

let test_live_resume_excludes_completion () =
  with_temp_root "launch-completion-race" (fun root ->
      let home, _log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      let run_dir = Filename.concat home ".monty/runs/run-race" in
      let worker_dir = Filename.concat run_dir "workers/race-worker" in
      let ready = Filename.concat root "resume-wt-ready" in
      let release = Filename.concat root "resume-wt-release" in
      let wt = Filename.concat root "fake-bin/wt" in
      init_git_repo repo;
      (match Process.run_quiet ~cwd:repo "git checkout -qb cto/race-worker" with
      | Ok () -> () | Error message -> failwith message);
      add_project ~root ~home ~env repo;
      Shell.write_file context "# Launch completion race\n";
      Shell.ensure_dir worker_dir;
      Yojson.Safe.to_file (Filename.concat worker_dir "job.json")
        (lifecycle_job_json ~id:"race-worker" ~title:"Race worker"
           ~branch:"cto/race-worker" ~worktree_mode:"always" ~repo ~context
           ~worker_dir ~run_dir ());
      Shell.write_file wt
        (String.concat "\n"
           [ "#!/bin/sh"; "set -eu"; "case \"$1\" in";
             "  b)"; "    : > " ^ Shell.quote ready;
             "    while [ ! -f " ^ Shell.quote release
             ^ " ]; do sleep 0.01; done";
             "    printf '%s\\n' " ^ Shell.quote repo; "    ;;";
             "  list) printf 'repo:\\n' ;;"; "  db) exit 0 ;;";
             "  *) exit 92 ;;"; "esac"; "" ]);
      Shell.chmod_executable wt;
      let osascript = Filename.concat root "fake-bin/osascript" in
      Shell.write_file osascript "#!/bin/sh\nexit 0\n";
      Shell.chmod_executable osascript;
      let child =
        spawn ~root ~env 1940
          [ "resume"; "race-worker"; "--home"; home; "--terminal";
            "ghostty"; "--wt-command"; "wt" ]
      in
      let rec wait_ready attempts =
        if Sys.file_exists ready then ()
        else if attempts = 0 then failwith "resume did not reach blocking wt call"
        else (
          Unix.sleepf 0.01;
          wait_ready (attempts - 1))
      in
      wait_ready 500;
      let done_env =
        replace_env env [ ("MONTY_FAULT_INJECT", "complete-after-intent") ]
      in
      let interrupted =
        run ~root ~env:done_env 1941
          [ "done"; "race-worker"; "--home"; home; "--wt-command"; "wt" ]
      in
      require_code 1 interrupted;
      require_contains "completion excludes live resume" interrupted.stderr "busy";
      Shell.write_file release "release\n";
      let resumed = await child in
      require_code 0 resumed;
      if job_status (Filename.concat worker_dir "job.json") <> "launch-requested" then
        failwith "completion changed the live resume state")

let test_materialization_excludes_completion () =
  List.iter
    (fun (headless, killed) ->
      let name =
        (if headless then "headless-prepare" else "fresh-launch")
        ^ (if killed then "-parent-loss" else "-completion")
      in
      with_temp_root name (fun root ->
          let home, log, env = setup_environment root in
          let repo = Filename.concat root "repo" in
          let context = Filename.concat root "context.md" in
          let run_dir = Filename.concat home ".monty/runs/materialize-lock" in
          let worker_dir = Filename.concat run_dir "workers/materialize-lock" in
          let job_file = Filename.concat worker_dir "job.json" in
          let task_file = Filename.concat home ".monty/tasks.local.json" in
          let manifest = Filename.concat run_dir "jobs.json" in
          let ready = Filename.concat root "materialize-ready" in
          let release = Filename.concat root "materialize-release" in
          init_git_repo repo;
          add_project ~root ~home ~env repo;
          Shell.write_file context "# Materialization ownership\n";
          install_repo_scoped_wt ~root ~log;
          let wt = Filename.concat root "fake-bin/wt" in
          let delegate = Filename.concat root "fake-bin/wt-delegate" in
          Unix.rename wt delegate;
          Shell.write_file wt
            (String.concat "\n"
               [ "#!/bin/sh"; "set -eu"; "if [ \"$1\" = b ]; then";
                 "  : > " ^ Shell.quote ready;
                 "  while [ ! -f " ^ Shell.quote release ^ " ]; do sleep 0.01; done";
                 "fi"; "exec " ^ Shell.quote delegate ^ " \"$@\""; "" ]);
          Shell.chmod_executable wt;
          let osascript = Filename.concat root "fake-bin/osascript" in
          Shell.write_file osascript
            (String.concat "\n"
               [ "#!/bin/sh"; "set -eu";
                 (* Terminal startup must be able to claim the lock after materialization. *)
                 Shell.quote executable ^ " task workspace ensure local-001 --home "
                 ^ Shell.quote home ^ " --wt-command wt >/dev/null"; "" ]);
          Shell.chmod_executable osascript;
          write_manifest manifest
            [ manifest_job ~id:"materialize-lock" ~title:"Materialization ownership"
                ~branch:"cto/materialize-lock" ~repo ~context () ];
          let args =
            (if headless then [ "headless"; "prepare-many" ]
             else [ "launch-many"; "--terminal"; "ghostty" ])
            @ [ "--manifest"; manifest; "--home"; home ]
          in
          let child = spawn ~root ~env ~new_session:true 1960 args in
          let awaited = ref false in
          Fun.protect
            ~finally:(fun () ->
              Shell.write_file release "release\n";
              if not !awaited then ignore (await child);
              try Unix.kill (-child.pid) Sys.sigkill with
              | Unix.Unix_error (Unix.ESRCH, _, _) -> ())
            (fun () ->
              wait_for_path ready;
              let before = (Shell.read_file job_file, Shell.read_file task_file) in
              if killed then (
                Unix.kill child.pid Sys.sigkill;
                awaited := true;
                let result = await child in
                if result.code = 0 then failwith "killed materialization parent succeeded");
              let done_args =
                [ "done"; "materialize-lock"; "--home"; home; "--wt-command"; "wt" ]
              in
              let blocked = run ~root ~env 1961 done_args in
              require_code 1 blocked;
              require_contains "completion excludes materialization" blocked.stderr "busy";
              if before <> (Shell.read_file job_file, Shell.read_file task_file) then
                failwith "blocked completion changed materializing worker or task";
              require_empty_log log;
              Shell.write_file release "release\n";
              if not killed then (
                awaited := true;
                require_code 0 (await child);
                let expected = if headless then "prepared" else "launch-requested" in
                if job_status job_file <> expected then
                  failwith "materialization changed its expected launch state");
              let rec complete remaining =
                let result = run ~root ~env 1962 done_args in
                if killed && result.code <> 0 && string_contains result.stderr "busy"
                   && remaining > 0 then (
                  Unix.sleepf 0.02;
                  complete (remaining - 1))
                else require_code 0 result
              in
              complete 250;
              if Sys.file_exists worker_dir then
                failwith "completion left the materialized worker active";
              let worktree = Filename.concat root "multi-worktrees/repo_cto_materialize-lock" in
              if Sys.file_exists worktree then
                failwith "completion left a worktree after materialization";
              let archived = Filename.concat run_dir "archive/materialize-lock/job.json" in
              if job_status archived <> "done" then
                failwith "completion did not archive materialized worker";
              if json_string "status" (List.hd (local_tasks_json home)) <> "done" then
                failwith "completion did not close the materialized task")))
    [ (false, false); (true, false); (false, true); (true, true) ]

let test_workspace_ensure_excludes_completion () =
  with_temp_root "workspace-ensure-completion" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      let run_dir = Filename.concat home ".monty/runs/ensure-lock" in
      let worker_dir = Filename.concat run_dir "workers/ensure-lock" in
      let job_file = Filename.concat worker_dir "job.json" in
      let task_file = Filename.concat home ".monty/tasks.local.json" in
      let ready = Filename.concat root "ensure-ready" in
      let release = Filename.concat root "ensure-release" in
      init_git_repo repo;
      add_project ~root ~home ~env repo;
      Shell.write_file context "# Workspace ownership\n";
      Shell.ensure_dir worker_dir;
      Yojson.Safe.to_file job_file
        (lifecycle_job_json ~id:"ensure-lock" ~title:"Workspace ownership"
           ~branch:"cto/ensure-lock" ~worktree_mode:"always" ~status:"prepared"
           ~repo ~context ~worker_dir ~run_dir ());
      require_code 0 (run ~root ~env 1949 [ "tasks"; "sync"; "--home"; home ]);
      install_repo_scoped_wt ~root ~log;
      let wt = Filename.concat root "fake-bin/wt" in
      let delegate = Filename.concat root "fake-bin/wt-delegate" in
      Unix.rename wt delegate;
      Shell.write_file wt
        (String.concat "\n"
           [ "#!/bin/sh"; "set -eu"; "if [ \"$1\" = b ]; then";
             "  : > " ^ Shell.quote ready;
             "  while [ ! -f " ^ Shell.quote release ^ " ]; do sleep 0.01; done";
             "fi"; "exec " ^ Shell.quote delegate ^ " \"$@\""; "" ]);
      Shell.chmod_executable wt;
      let ensure_args =
        [ "task"; "workspace"; "ensure"; "local-001"; "--home"; home;
          "--wt-command"; "wt" ]
      in
      let child = spawn ~root ~env 1950 ensure_args in
      let awaited = ref false in
      Fun.protect
        ~finally:(fun () ->
          Shell.write_file release "release\n";
          if not !awaited then ignore (await child))
        (fun () ->
          wait_for_path ready;
          let before = (Shell.read_file job_file, Shell.read_file task_file) in
          let done_args = [ "done"; "ensure-lock"; "--home"; home; "--wt-command"; "wt" ] in
          let blocked = run ~root ~env 1951 done_args in
          require_code 1 blocked;
          require_contains "completion excludes workspace materialization" blocked.stderr "busy";
          let duplicate = run ~root ~env 1952 ensure_args in
          require_code 1 duplicate;
          require_contains "duplicate workspace materialization" duplicate.stderr "busy";
          if before <> (Shell.read_file job_file, Shell.read_file task_file) then
            failwith "blocked lifecycle or ensure changed durable state";
          require_empty_log log;
          Shell.write_file release "release\n";
          awaited := true;
          let ensured = await child in
          require_code 0 ensured;
          let worktree = String.trim ensured.stdout in
          if not (Sys.file_exists worktree && Sys.is_directory worktree) then
            failwith "workspace ensure did not create its worktree";
          require_code 0 (run ~root ~env 1953 done_args);
          if Sys.file_exists worker_dir || Sys.file_exists worktree then
            failwith "completion left the worker or ensured worktree behind";
          let archived = Filename.concat run_dir "archive/ensure-lock/job.json" in
          if job_status archived <> "done" then
            failwith "completion did not archive the ensured worker";
          if json_string "status" (List.hd (local_tasks_json home)) <> "done" then
            failwith "completion did not close the ensured task"))

let test_worker_ensure_identity_and_delayed_script () =
  with_temp_root "worker-ensure-identity" (fun root ->
      let root = Unix.realpath root in
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      let run_dir = Filename.concat home ".monty/runs/worker-ensure" in
      let worker_dir = Filename.concat run_dir "workers/worker-ensure" in
      let job_file = Filename.concat worker_dir "job.json" in
      init_git_repo repo;
      add_project ~root ~home ~env repo;
      install_repo_scoped_wt ~root ~log;
      Shell.write_file context "# Worker-aware raw ensure\n";
      Shell.ensure_dir worker_dir;
      Yojson.Safe.to_file job_file
        (lifecycle_job_json ~id:"worker-ensure" ~title:"Worker-aware raw ensure"
           ~branch:"cto/worker-ensure" ~worktree_mode:"always" ~repo ~context
           ~worker_dir ~run_dir ());
      let args branch =
        [ "ensure-worktree"; "--repo"; repo; "--branch"; branch; "--wt-command"; "wt" ]
      in
      let identity =
        [ ("MONTY_WORKER_DIR", worker_dir); ("MONTY_JOB_ID", "worker-ensure");
          ("MONTY_JOB_FILE", job_file); ("MONTY_RUN_DIR", run_dir);
          ("MONTY_HOME", Filename.concat root "unrelated-home") ]
      in
      let reject bindings branch =
        let result = run ~root ~env:(replace_env env bindings) 1954 (args branch) in
        require_code 1 result;
        require_empty_log log
      in
      List.iter
        (fun (key, value) -> reject [ (key, value) ] "cto/worker-ensure")
        (List.filter (fun (key, _) -> key <> "MONTY_RUN_DIR" && key <> "MONTY_HOME") identity);
      List.iter
        (fun forged -> reject (forged :: List.remove_assoc (fst forged) identity) "cto/worker-ensure")
        [ ("MONTY_JOB_ID", "another-worker");
          ("MONTY_JOB_FILE", Filename.concat root "unrelated-job.json");
          ("MONTY_RUN_DIR", Filename.concat home ".monty/runs/another-run") ];
      reject identity "cto/not-owned";
      let ensured = run ~root ~env:(replace_env env identity) 1955 (args "cto/worker-ensure") in
      require_code 0 ensured;
      let worktree = String.trim ensured.stdout in
      if not (Sys.file_exists worktree && Sys.is_directory worktree) then
        failwith "complete legacy worker identity did not materialize its workspace";
      let alias_home = Filename.concat root "home-alias" in
      Unix.symlink home alias_home;
      let alias_run = Filename.concat alias_home ".monty/runs/worker-ensure" in
      let alias_worker = Filename.concat alias_run "workers/worker-ensure" in
      let alias_identity =
        [ ("MONTY_WORKER_DIR", alias_worker); ("MONTY_JOB_ID", "worker-ensure");
          ("MONTY_JOB_FILE", Filename.concat alias_worker "job.json");
          ("MONTY_RUN_DIR", alias_run);
          ("MONTY_HOME", Filename.concat root "unrelated-home") ]
      in
      let aliased = run ~root ~env:(replace_env env alias_identity) 19551 (args "cto/worker-ensure") in
      require_code 0 aliased;
      require_line "home alias resolves canonical worker identity" aliased.stdout worktree;
      if json_string "worker_dir" (Yojson.Safe.from_file job_file) <> worker_dir then
        failwith "home alias rewrote canonical worker metadata";
      let task_free = run ~root ~env 1956 (args "cto/task-free") in
      require_code 0 task_free;
      if String.trim task_free.stdout = worktree then
        failwith "task-free ensure ignored its requested branch";
      let osascript = Filename.concat root "fake-bin/osascript" in
      Shell.write_file osascript
        (String.concat "\n"
           [ "#!/bin/sh"; "set -eu";
             Shell.quote executable ^ " ensure-worktree --repo " ^ Shell.quote repo
             ^ " --branch cto/worker-ensure --wt-command wt >/dev/null"; "" ]);
      Shell.chmod_executable osascript;
      let resume_env = replace_env env identity in
      require_code 0
        (run ~root ~env:resume_env 1957
           [ "resume"; "worker-ensure"; "--home"; home; "--terminal"; "ghostty" ]));
  with_temp_root "delayed-worker-script" (fun root ->
      let root = Unix.realpath root in
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      init_git_repo repo;
      add_project ~root ~home ~env repo;
      install_repo_scoped_wt ~root ~log;
      Shell.write_file context "# Delayed singleton script\n";
      let osascript = Filename.concat root "fake-bin/osascript" in
      Shell.write_file osascript "#!/bin/sh\nexit 0\n";
      Shell.chmod_executable osascript;
      require_code 0
        (run ~root ~env 1958
           [ "launch"; "--repo"; repo; "--title"; "Delayed singleton";
             "--context"; context; "--branch"; "cto/delayed-singleton";
             "--home"; home; "--terminal"; "ghostty" ]);
      let job_file = Filename.concat home ".monty/runs/manual/workers/delayed-singleton/job.json" in
      let job = Yojson.Safe.from_file job_file in
      let script = json_string "launch_script" job in
      let worktree = json_string "last_known_worktree" job in
      require_code 0
        (run ~root ~env 1959 [ "done"; "delayed-singleton"; "--home"; home ]);
      let before = Shell.read_file log in
      let delayed =
        run ~root ~env ~program:"/bin/sh" 1963 [ script ]
      in
      if delayed.code = 0 then failwith "completed worker's delayed script succeeded";
      if Sys.file_exists worktree || Shell.read_file log <> before then
        failwith "completed worker's delayed script invoked wt or recreated its worktree");
  with_temp_root "never-worker-ensure" (fun root ->
      let root = Unix.realpath root in
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      let run_dir = Filename.concat home ".monty/runs/never-ensure" in
      let worker_dir = Filename.concat run_dir "workers/never-ensure" in
      let job_file = Filename.concat worker_dir "job.json" in
      init_git_repo repo;
      add_project ~root ~home ~env repo;
      Shell.write_file context "# Preserve never mode\n";
      Shell.ensure_dir worker_dir;
      Yojson.Safe.to_file job_file
        (lifecycle_job_json ~id:"never-ensure" ~title:"Never ensure"
           ~branch:"cto/never-ensure" ~worktree_mode:"never" ~repo ~context
           ~worker_dir ~run_dir ());
      require_code 0 (run ~root ~env 1964 [ "tasks"; "sync"; "--home"; home ]);
      let raw_env = replace_env env
          [ ("MONTY_WORKER_DIR", worker_dir); ("MONTY_JOB_ID", "never-ensure");
            ("MONTY_JOB_FILE", job_file) ] in
      let raw = run ~root ~env:raw_env 1965
          [ "ensure-worktree"; "--repo"; repo; "--branch"; "cto/never-ensure";
            "--wt-command"; "wt" ] in
      require_code 0 raw;
      require_line "never raw ensure retains source repository" raw.stdout repo;
      let task = run ~root ~env 1966
          [ "task"; "workspace"; "ensure"; "local-001"; "--home"; home ] in
      require_code 0 task;
      require_line "never task ensure retains source repository" task.stdout repo;
      require_empty_log log;
      require_code 0 (run ~root ~env 1967 [ "done"; "never-ensure"; "--home"; home ]);
      if not (Sys.file_exists (Filename.concat repo "tracked.txt")) then
        failwith "never-mode completion removed source repository";
      require_empty_log log)

let test_forged_launch_script_and_resume_mode_are_safe () =
  with_temp_root "forged-launch-script" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      let run_dir = Filename.concat home ".monty/runs/run-forged" in
      let worker_dir = Filename.concat run_dir "workers/forged" in
      let manifest = Filename.concat run_dir "jobs.json" in
      let victim_dir = Filename.concat root "outside-scripts" in
      let victim = Filename.concat victim_dir "monty-forged-launch.sh" in
      add_project ~root ~home ~env repo;
      Shell.write_file context "# Forged script\n";
      require_code 0
        (run ~root ~env 1943
           [ "task"; "add"; "--home"; home; "--project"; "repo";
             "--title"; "Forged worker" ]);
      Shell.ensure_dir worker_dir;
      Shell.ensure_dir victim_dir;
      let forged_contents =
        String.concat "\n"
          [ "#!/bin/sh"; "set -eu";
            "export MONTY_WORKER_DIR=" ^ Shell.quote (Unix.realpath worker_dir);
            "export MONTY_JOB_ID='forged'";
            "export MONTY_JOB_BRANCH='cto/forged'";
            "export MONTY_JOB_REPO=" ^ Shell.quote repo; "exec true"; "" ]
      in
      Shell.write_file victim forged_contents;
      lifecycle_job_json ~id:"forged" ~title:"Forged worker"
        ~branch:"cto/forged" ~status:"launch-failed"
        ~task_key:"local:local-001" ~repo ~context ~worker_dir ~run_dir ()
      |> replace_assoc_field "launch_script" (`String victim)
      |> fun json -> Yojson.Safe.to_file (Filename.concat worker_dir "job.json") json;
      write_manifest manifest
        [ manifest_job ~id:"forged" ~branch:"cto/forged"
            ~task_key:"local:local-001" ~title:"Forged worker" ~repo ~context () ];
      let tasks_before =
        Shell.read_file (Filename.concat home ".monty/tasks.local.json")
      in
      let result =
        run ~root ~env 1944
          [ "launch-many"; "--manifest"; manifest; "--home"; home;
            "--terminal"; "ghostty"; "--worktree"; "never" ]
      in
      if result.code = 0 then failwith "forged launch script metadata was trusted";
      require_contains "forged script ownership" result.stderr
        "does not match the complete Monty-owned script";
      if Shell.read_file victim <> forged_contents then
        failwith "forged launch script target was overwritten";
      if
        Shell.read_file (Filename.concat home ".monty/tasks.local.json")
        <> tasks_before
      then failwith "forged script preflight mutated task state";
      require_empty_log log);
  with_temp_root "persisted-resume-mode" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      let run_dir = Filename.concat home ".monty/runs/run-mode" in
      let worker_dir = Filename.concat run_dir "workers/mode-worker" in
      add_project ~root ~home ~env repo;
      Shell.write_file context "# Persisted resume mode\n";
      Shell.ensure_dir worker_dir;
      Yojson.Safe.to_file (Filename.concat worker_dir "job.json")
        (lifecycle_job_json ~id:"mode-worker" ~title:"Mode worker"
           ~branch:"cto/mode-worker" ~worktree_mode:"never" ~repo ~context
           ~worker_dir ~run_dir ());
      let osascript = Filename.concat root "fake-bin/osascript" in
      Shell.write_file osascript
        (String.concat "\n"
           [ "#!/bin/sh"; "printf 'resume-osascript\\n' >> " ^ Shell.quote log;
             "exit 0"; "" ]);
      Shell.chmod_executable osascript;
      let resumed =
        run ~root ~env 1945
          [ "resume"; "mode-worker"; "--home"; home; "--terminal";
            "ghostty" ]
      in
      require_code 0 resumed;
      let tool_log = read_file log in
      if string_contains tool_log "/wt " then
        failwith "resume used CLI default always instead of persisted never mode";
      require_contains "persisted-mode terminal request" tool_log
        "resume-osascript";
      let job =
        Yojson.Safe.from_file (Filename.concat worker_dir "job.json")
      in
      if json_string "worktree_mode" job <> "never" then
        failwith "resume changed the persisted worktree mode";
      let script = json_string "launch_script" job |> Shell.read_file in
      require_contains "persisted mode launch script" script
        "export MONTY_WORKTREE_MODE='never'";
      if string_contains script "ensure-worktree" then
        failwith "never-mode resume generated worktree recreation commands");
  with_temp_root "dangling-resume-script" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      let run_dir = Filename.concat home ".monty/runs/run-dangling" in
      let worker_dir = Filename.concat run_dir "workers/dangling" in
      let script_dir = Filename.concat home ".monty/runtime/scripts" in
      let script = Filename.concat script_dir "monty-dangling-launch.sh" in
      let escaped_target = Filename.concat root "escaped-script-target" in
      add_project ~root ~home ~env repo;
      Shell.write_file context "# Dangling script\n";
      Shell.ensure_dir worker_dir;
      Shell.ensure_dir script_dir;
      Yojson.Safe.to_file (Filename.concat worker_dir "job.json")
        (lifecycle_job_json ~id:"dangling" ~title:"Dangling"
           ~branch:"cto/dangling" ~worktree_mode:"never" ~repo ~context
           ~worker_dir ~run_dir ());
      Unix.symlink escaped_target script;
      let resumed =
        run ~root ~env 1946
          [ "resume"; "dangling"; "--home"; home; "--terminal"; "ghostty" ]
      in
      if resumed.code = 0 then
        failwith "resume claimed a dangling launch-script symlink";
      require_contains "dangling script ownership" resumed.stderr
        "already exists without recorded ownership";
      if Sys.file_exists escaped_target then
        failwith "dangling launch-script symlink created its escaped target";
      (match (Unix.lstat script).Unix.st_kind with
      | Unix.S_LNK -> ()
      | _ -> failwith "dangling script entry was unexpectedly replaced");
      require_empty_log log);
  with_temp_root "launch-script-swap" (fun root ->
      let home, _log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      let wt = Filename.concat root "fake-bin/wt" in
      let osascript = Filename.concat root "fake-bin/osascript" in
      let ready = Filename.concat root "swap-wt-ready" in
      let release = Filename.concat root "swap-wt-release" in
      let escaped_target = Filename.concat root "swap-escaped-target" in
      init_git_repo repo;
      (match Process.run_quiet ~cwd:repo "git checkout -qb cto/swap" with
      | Ok () -> () | Error message -> failwith message);
      add_project ~root ~home ~env repo;
      Shell.write_file context "# Script swap\n";
      Shell.write_file wt
        (String.concat "\n"
           [ "#!/bin/sh"; "case \"$1\" in";
             "  b) printf '%s\\n' " ^ Shell.quote repo ^ " ;;";
             "  list) printf 'repo:\\n' ;;"; "  *) exit 0 ;;"; "esac"; "" ]);
      Shell.chmod_executable wt;
      Shell.write_file osascript "#!/bin/sh\nexit 0\n";
      Shell.chmod_executable osascript;
      let launch_args =
        [ "launch"; "--repo"; repo; "--title"; "Swap worker"; "--context";
          context; "--branch"; "cto/swap"; "--home"; home; "--terminal";
          "ghostty"; "--worktree"; "always"; "--wt-command"; "wt" ]
      in
      let fault_env =
        replace_env env [ ("MONTY_FAULT_INJECT", "launch-before-request-state") ]
      in
      let first = run ~root ~env:fault_env 1947 launch_args in
      if first.code = 0 then failwith "script swap setup fault succeeded";
      let job_file =
        Filename.concat home ".monty/runs/manual/workers/swap/job.json"
      in
      let owned_script =
        Yojson.Safe.from_file job_file |> json_string "launch_script"
      in
      if not (Sys.file_exists owned_script) then
        failwith "script swap setup did not create an owned script";
      Shell.write_file wt
        (String.concat "\n"
           [ "#!/bin/sh"; "set -eu"; "case \"$1\" in"; "  b)";
             "    : > " ^ Shell.quote ready;
             "    while [ ! -f " ^ Shell.quote release
             ^ " ]; do sleep 0.01; done";
             "    printf '%s\\n' " ^ Shell.quote repo; "    ;;";
             "  list) printf 'repo:\\n' ;;"; "  *) exit 0 ;;"; "esac"; "" ]);
      Shell.chmod_executable wt;
      let child = spawn ~root ~env 1948 launch_args in
      let rec wait_ready attempts =
        if Sys.file_exists ready then ()
        else if attempts = 0 then failwith "retry did not reach blocking wt call"
        else (
          Unix.sleepf 0.01;
          wait_ready (attempts - 1))
      in
      wait_ready 500;
      Sys.remove owned_script;
      Unix.symlink escaped_target owned_script;
      Shell.write_file release "release\n";
      let retried = await child in
      require_code 0 retried;
      if Sys.file_exists escaped_target then
        failwith "validation-to-write script swap created its escaped target";
      (match (Unix.lstat owned_script).Unix.st_kind with
      | Unix.S_REG -> ()
      | _ -> failwith "atomic script publish did not replace swapped symlink");
      require_contains "atomically republished script"
        (Shell.read_file owned_script) "# monty-launch-script-v1")

let test_multi_workspace_project_suffixes () =
  with_temp_root "project-suffixes" (fun root ->
      let home, log, env = setup_environment root in
      let cli args = run ~root ~env 2200 (args @ [ "--home"; home ]) in
      let backend = Filename.concat root "django-backend" in
      let apps = Filename.concat root "apps" in
      List.iter (fun repo -> init_git_repo repo; add_project ~root ~home ~env repo)
        [ backend; apps ];
      install_repo_scoped_wt ~root ~log;
      require_code 0 (cli [ "settings"; "set"; "branch-prefix"; "cto" ]);
      require_code 0 (cli [ "task"; "add"; "--project"; "django-backend";
                            "--title"; "Planned feature" ]);
      let add repo branch =
        let result = cli [ "task"; "workspace"; "add"; "local-001";
                           "--repo"; repo; "--branch"; branch ] in
        require_code 0 result;
        result
      in
      let single = add backend "custom/Planned.Feature" in
      require_line "single-workspace confirmation" single.stdout
        ("  " ^ backend ^ " | custom/Planned.Feature");
      if json_string "branch" (List.hd (local_tasks_json home)) <> "custom/Planned.Feature" then
        failwith "single-workspace add changed its explicit branch";
      let promoted = add apps "custom/Planned.Feature" in
      let expected_plan =
        [ "custom/Planned.Feature-django-backend"; "custom/Planned.Feature-apps" ]
      in
      List.iter2 (fun repo branch ->
          require_line "saved workspace confirmation" promoted.stdout
            ("  " ^ repo ^ " | " ^ branch)) [ backend; apps ] expected_plan;
      let branches json =
        Yojson.Safe.Util.(json |> member "workspaces" |> to_list)
        |> List.map (json_string "branch")
      in
      let planned = List.hd (local_tasks_json home) in
      if branches planned <> expected_plan
         || json_string "branch" planned <> List.hd expected_plan then
        failwith "promotion did not update both workspace branches and the alias";
      let context = Filename.concat root "context.md" in
      Shell.write_file context "# Project suffixes\n";
      let manifest = Filename.concat home ".monty/runs/suffixes/jobs.json" in
      let workspace repo branch =
        `Assoc ([ ("repo", `String repo) ]
                @ Option.fold ~none:[] ~some:(fun value -> [ ("branch", `String value) ]) branch)
      in
      let job id title workspaces extra =
        `Assoc ([ ("id", `String id); ("title", `String title);
                  ("context", `String context); ("workspaces", `List workspaces) ] @ extra)
      in
      let generated = job "generated" "Shared feature"
          [ workspace backend None; workspace apps None ] [] in
      let explicit = job "explicit" "Explicit feature"
          [ workspace backend (Some "other/Keep.Feature");
            workspace apps (Some "other/Keep.Feature-apps") ] [] in
      let planned_job = job "planned" "Planned feature"
          [ workspace backend (Some "custom/Planned.Feature");
            workspace apps (Some "custom/Planned.Feature-apps") ]
          [ ("task_key", `String "local:local-001") ] in
      let single = job "single" "Single feature" [ workspace apps None ] [] in
      let jobs = [ generated; explicit; planned_job; single ] in
      write_manifest manifest jobs;
      let expected =
        [ [ "cto/01-shared-feature-django-backend"; "cto/01-shared-feature-apps" ];
          [ "other/Keep.Feature-django-backend"; "other/Keep.Feature-apps" ];
          expected_plan; [ "cto/04-single-feature" ] ]
      in
      let rec snapshot path =
        match (Unix.lstat path).Unix.st_kind with
        | Unix.S_DIR ->
            (path, "directory") ::
            (Sys.readdir path |> Array.to_list |> List.sort String.compare
             |> List.concat_map (fun name -> snapshot (Filename.concat path name)))
        | Unix.S_REG -> [ (path, Shell.read_file path) ]
        | _ -> failf "unexpected state entry: %s" path
      in
      let dry_commands =
        [ [ "launch-many"; "--terminal"; "dry-run" ];
          [ "headless"; "prepare-many"; "--dry-run"; "--harness"; "codex" ] ]
      in
      let dry_jobs () =
        List.iter (fun command ->
            let before = snapshot home in
            let result = cli (command @ [ "--manifest"; manifest ]) in
            require_code 0 result;
            List.iter (List.iter (require_contains "dry-run project branch" result.stdout)) expected;
            if snapshot home <> before then failwith "suffix dry-run mutated Monty state";
            require_empty_log log) dry_commands
      in
      dry_jobs ();
      let without_id = match generated with
        | `Assoc fields -> `Assoc (List.remove_assoc "id" fields)
        | _ -> assert false
      in
      write_manifest manifest [ without_id ];
      List.iter (fun command ->
          let before = snapshot home in
          let result = cli (command @ [ "--manifest"; manifest ]) in
          require_code 0 result;
          List.iter (require_contains "single-job generated project branch" result.stdout)
            [ "cto/shared-feature-django-backend"; "cto/shared-feature-apps";
              "/workers/shared-feature" ];
          if snapshot home <> before then failwith "single-job dry-run mutated state";
          require_empty_log log) dry_commands;
      (* Both dry-run paths reject collisions after suffixing, including an
         explicit branch that already has the final suffix. *)
      let collision = job "collision" "Collision"
          [ workspace backend (Some "other/Keep.Feature-django-backend") ] [] in
      let invalid_batches =
        [ (jobs @ [ collision ], "canonical repo+branch");
          (jobs @ [ job "duplicate" "Duplicate repo"
                      [ workspace backend None; workspace (backend ^ "/.") None ] [] ],
           "duplicate workspace repo");
          (jobs @ [ job "unknown" "Unknown project"
                      [ workspace backend None; workspace root None ] [] ],
           "not registered") ]
      in
      List.iter (fun (jobs, error) ->
          write_manifest manifest jobs;
          List.iter (fun command ->
              let before = snapshot home in
              let result = cli (command @ [ "--manifest"; manifest ]) in
              if result.code = 0 then failwith "invalid suffix batch passed preflight";
              require_contains "suffix preflight rejection" result.stderr error;
              if snapshot home <> before then failwith "rejected suffix batch mutated state";
              require_empty_log log) dry_commands) invalid_batches;
      write_manifest manifest jobs;
      let prepare () =
        cli [ "headless"; "prepare-many"; "--harness"; "codex"; "--manifest"; manifest ]
      in
      let prepared = prepare () in
      require_code 0 prepared;
      let returned = Yojson.Safe.Util.(Yojson.Safe.from_string prepared.stdout |> member "jobs" |> to_list) in
      if List.map branches returned <> expected then
        failwith "preparation differed from dry-run branches";
      List.iter2 (fun returned expected_branches ->
          let job_file = Filename.concat (json_string "worker_dir" returned) "job.json" in
          let persisted = Yojson.Safe.from_file job_file in
          if branches persisted <> expected_branches then failwith "worker branches differ from preflight";
          let task_id = json_string "task_key" persisted |> Task_storage.normalize_local_id in
          let task = List.find (fun task -> json_string "id" task = task_id) (local_tasks_json home) in
          if branches task <> expected_branches
             || json_string "branch" task <> List.hd expected_branches then
            failwith "linked task branches differ from worker branches";
          Yojson.Safe.Util.(persisted |> member "workspaces" |> to_list)
          |> List.iter (fun workspace ->
                 require_contains "repo-scoped wt invocation" (read_file log)
                   (json_string "repo" workspace ^ " b " ^ json_string "branch" workspace);
                 if not (Sys.file_exists (json_string "worktree" workspace)) then
                   failwith "prepared workspace was not materialized")) returned expected;
      let retried = prepare () in
      require_code 0 retried;
      let retried = Yojson.Safe.Util.(Yojson.Safe.from_string retried.stdout |> member "jobs" |> to_list) in
      if List.map branches retried <> expected then failwith "retry appended a duplicate suffix";
      (* Existing unlaunched metadata is normalized only when launch reserves it. *)
      require_code 0 (cli [ "task"; "add"; "--project"; "apps"; "--title"; "Older plan" ]);
      let tasks = match Task_storage.load_local_tasks ~home with Ok tasks -> tasks | Error msg -> failwith msg in
      let older = List.hd (List.rev tasks) in
      let older = { older with Overview_types.workspaces =
          [ { repo = backend; branch = "cto/older" }; { repo = apps; branch = "cto/older" } ];
          branch = Some "cto/older" } in
      (match Task_storage.save_local_tasks_unlocked ~home
               (List.map (fun (task : Overview_types.local_task) -> if task.id = older.id then older else task) tasks) with
      | Ok () -> () | Error msg -> failwith msg);
      write_manifest manifest
        [ job "older" "Older plan"
            [ workspace backend (Some "cto/older"); workspace apps (Some "cto/older") ]
            [ ("task_key", `String ("local:" ^ older.id)) ] ];
      let before = snapshot home in
      require_code 0 (cli [ "launch-many"; "--terminal"; "dry-run"; "--manifest"; manifest ]);
      if snapshot home <> before then failwith "old plan dry-run wrote normalized metadata";
      require_code 0 (prepare ()))

let test_legacy_multi_workspace_reservations () =
  List.iter (fun checkpoint ->
      List.iter (fun explicit_link ->
          with_temp_root "legacy-workspace-reservation" (fun root ->
              let home, log, env = setup_environment root in
              let cli args = run ~root ~env 2205 (args @ [ "--home"; home ]) in
              let repos = List.map (Filename.concat root) [ "backend"; "apps" ] in
              List.iter (fun repo -> init_git_repo repo; add_project ~root ~home ~env repo) repos;
              install_repo_scoped_wt ~root ~log;
              let osascript = Filename.concat root "fake-bin/osascript" in
              Shell.write_file osascript "#!/bin/sh\nexit 0\n";
              Shell.chmod_executable osascript;
              let context = Filename.concat root "context.md" in
              Shell.write_file context "# Legacy reservation\n";
              let manifest = Filename.concat home ".monty/runs/legacy/jobs.json" in
              let branch = "old/shared-feature" in
              let workspaces = `List (List.map (fun repo ->
                  `Assoc [ ("repo", `String repo); ("branch", `String branch) ]) repos) in
              if explicit_link then
                require_code 0 (cli [ "task"; "add"; "--project"; "backend";
                                     "--title"; "Legacy reservation" ]);
              write_manifest manifest
                [ `Assoc ([ ("id", `String "legacy"); ("title", `String "Legacy reservation");
                            ("context", `String context); ("workspaces", workspaces) ]
                          @ if explicit_link then [ ("task_key", `String "local:local-001") ] else []) ];
              let options = Launcher.
                { backend = Terminal.Ghostty; target = Terminal.Tab;
                  harness = Harness.Codex; harness_command = "codex";
                  codex_yolo = false;
                  wt_command = Filename.concat root "fake-bin/wt"; worktree_mode = Always;
                  branch_prefix = "monty"; agent_profile = "reviewed"; fork = None;
                  home; script_dir = Home.runtime_script_dir ~home (); monty_command = executable }
              in
              let must = function Ok value -> value | Error msg -> failwith msg in
              let index, job = List.hd (must (Manifest.load ~home manifest)) in
              (* Reserve the pre-upgrade identity, bypassing new-job suffixing. *)
              let prepared = must (Launcher.prepare_identity options index job) in
              let old_fault = Option.value ~default:"" (Sys.getenv_opt "MONTY_FAULT_INJECT") in
              let interrupted = Fun.protect
                  ~finally:(fun () -> Unix.putenv "MONTY_FAULT_INJECT" old_fault)
                  (fun () ->
                    Unix.putenv "MONTY_FAULT_INJECT" checkpoint;
                    Launcher.reserve_batch options [ prepared ]) in
              (match interrupted with
              | Ok _ -> failf "legacy reservation fault %s unexpectedly succeeded" checkpoint
              | Error msg -> require_contains "legacy reservation fault" msg checkpoint);
              let task_path = Task_storage.local_tasks_file home in
              let task_before = Shell.read_file task_path in
              let task = List.hd (local_tasks_json home) in
              let task_id = json_string "id" task in
              if Yojson.Safe.Util.member "workspaces" task <> workspaces then
                failwith "legacy fixture did not reserve unsuffixed branches";
              let job_file = Filename.concat prepared.worker_dir "job.json" in
              if Sys.file_exists job_file then failwith "fault installed worker state";
              let staging = Filename.concat home ".monty/runs/legacy/.reservations/legacy" in
              let abrupt = checkpoint = "reserve-abrupt-after-tasks" in
              if Sys.file_exists staging <> abrupt then failwith "unexpected legacy staging state";
              if abrupt then Shell.write_file (Filename.concat staging "memory.md") "preserve legacy staging\n";
              let staged_job = read_file (Filename.concat staging "job.json") in
              List.iter (fun command ->
                  let result = cli (command @ [ "--manifest"; manifest ]) in
                  require_code 0 result;
                  require_contains "legacy reservation dry-run branch" result.stdout branch;
                  List.iter (fun suffix ->
                      if string_contains result.stdout (branch ^ suffix) then
                        failwith "legacy reservation dry-run renamed branches") [ "-backend"; "-apps" ];
                  if Shell.read_file task_path <> task_before || Sys.file_exists job_file
                     || read_file (Filename.concat staging "job.json") <> staged_job then
                    failwith "legacy reservation dry-run mutated state";
                  require_empty_log log)
                [ [ "launch-many"; "--terminal"; "dry-run" ];
                  [ "headless"; "prepare-many"; "--harness"; "codex"; "--dry-run" ] ];
              let command =
                if explicit_link then [ "headless"; "prepare-many"; "--harness"; "codex" ]
                else [ "launch-many"; "--terminal"; "ghostty" ] in
              require_code 0 (cli (command @ [ "--manifest"; manifest ]));
              let persisted = Yojson.Safe.from_file job_file in
              if (Yojson.Safe.Util.member "workspaces" persisted
                  |> Yojson.Safe.Util.to_list
                  |> List.map (json_string "branch")) <> [ branch; branch ] then
                failwith "legacy reservation retry renamed branches";
              if json_string "task_key" persisted <> "local:" ^ task_id
                 || Shell.read_file task_path <> task_before then
                failwith "legacy reservation retry replaced or duplicated its task";
              if abrupt && read_file (Filename.concat prepared.worker_dir "memory.md")
                           <> "preserve legacy staging\n" then
                failwith "legacy reservation retry overwrote staged memory";
              if Sys.file_exists staging then failwith "legacy retry left staging behind"))
        [ false; true ])
    [ "reserve-after-tasks"; "reserve-abrupt-after-tasks" ]

let test_legacy_multi_workspace_branches () =
  with_temp_root "legacy-project-branches" (fun root ->
      let home, log, env = setup_environment root in
      let cli args = run ~root ~env 2210 (args @ [ "--home"; home ]) in
      let backend = Filename.concat root "backend" in
      let apps = Filename.concat root "apps" in
      List.iter (fun repo -> init_git_repo repo; add_project ~root ~home ~env repo)
        [ backend; apps ];
      install_repo_scoped_wt ~root ~log;
      let osascript = Filename.concat root "fake-bin/osascript" in
      Shell.write_file osascript "#!/bin/sh\nexit 0\n";
      Shell.chmod_executable osascript;
      let context = Filename.concat root "context.md" in
      Shell.write_file context "# Legacy worker\n";
      let worker_dir = Filename.concat home ".monty/runs/legacy/workers/legacy" in
      let manifest = Filename.concat home ".monty/runs/legacy/jobs.json" in
      let branch = "old/shared-feature" in
      let job = Job.make_with_workspaces ~id:"legacy" ~title:"Legacy worker"
          ~worker_dir ~context
          ~workspaces:[ Job.workspace ~branch backend; Job.workspace ~branch apps ] () in
      (* Seed the on-disk format written before project suffixes existed. *)
      ignore (Worker_memory.ensure ~home ~job ~profile:reviewed_profile ~branch
                ~repo:backend ~context ~worktree_mode:"always" ~last_known_worktree:None);
      let job_file = Filename.concat worker_dir "job.json" in
      let script = Filename.concat (Home.runtime_script_dir ~home ()) "monty-legacy-launch.sh" in
      (match State_store.with_lock ~home (fun () ->
          Job_store.update_file_unlocked job_file
            [ Job_store.string "status" "prepared"; Job_store.string "launch_script" script ]) with
      | Ok () -> () | Error msg -> failwith msg);
      require_code 0 (cli [ "tasks"; "sync" ]);
      let task = List.hd (local_tasks_json home) in
      let task_id = json_string "id" task in
      let workspaces =
        `List (List.map (fun repo -> `Assoc [ ("repo", `String repo); ("branch", `String branch) ])
                 [ backend; apps ])
      in
      write_manifest manifest
        [ `Assoc [ ("id", `String "legacy"); ("title", `String "Legacy worker");
                   ("context", `String context); ("workspaces", workspaces);
                   ("task_key", `String ("local:" ^ task_id)) ] ];
      let assert_identity path =
        let job = Yojson.Safe.from_file path in
        let actual = Yojson.Safe.Util.(job |> member "workspaces" |> to_list)
            |> List.map (fun workspace -> (json_string "repo" workspace, json_string "branch" workspace)) in
        if actual <> [ (backend, branch); (apps, branch) ] || json_string "branch" job <> branch then
          failwith "legacy worker repo/branch identity changed";
        let task = List.find (fun task -> json_string "id" task = task_id) (local_tasks_json home) in
        if Yojson.Safe.Util.member "workspaces" task <> workspaces
           || json_string "branch" task <> branch then
          failwith "legacy linked task identity changed"
      in
      let unchanged command =
        let task_path = Filename.concat home ".monty/tasks.local.json" in
        let before = (Shell.read_file job_file, Shell.read_file task_path) in
        let result = cli command in
        require_code 0 result;
        require_contains "legacy dry-run branch" result.stdout branch;
        if before <> (Shell.read_file job_file, Shell.read_file task_path) then
          failwith "legacy dry-run rewrote persisted state";
        require_empty_log log
      in
      unchanged [ "launch-many"; "--terminal"; "dry-run"; "--manifest"; manifest ];
      unchanged [ "headless"; "prepare-many"; "--dry-run"; "--manifest"; manifest ];
      (* Early task links can lack the ownership fields; matching persisted
         workspace metadata must still survive resume and reservation. *)
      (match State_store.with_lock ~home (fun () ->
          Result.bind (Task_storage.load_local_tasks ~home) (fun tasks ->
              Task_storage.save_local_tasks_unlocked ~home
                (List.map (fun (task : Overview_types.local_task) ->
                     { task with worker_id = None; worker_key = None }) tasks))) with
      | Ok () -> () | Error msg -> failwith msg);
      unchanged [ "resume"; "legacy"; "--terminal"; "dry-run" ];
      require_code 0 (cli [ "headless"; "prepare-many"; "--manifest"; manifest ]);
      assert_identity job_file;
      require_code 0 (cli [ "task"; "workspace"; "ensure"; task_id ]);
      assert_identity job_file;
      install_pi_exec ~root ~log;
      require_code 0 (cli [ "headless"; "run"; "--wait"; "legacy"; "--harness"; "pi" ]);
      assert_identity job_file;
      require_code 0 (cli [ "headless"; "resume"; "--wait"; "legacy"; "--harness"; "pi" ]);
      require_code 0 (cli [ "resume"; "legacy"; "--terminal"; "ghostty" ]);
      require_code 0 (cli [ "tasks"; "sync" ]);
      assert_identity job_file;
      require_code 0 (cli [ "done"; "legacy" ]);
      let archive = Filename.concat home ".monty/runs/legacy/archive/legacy/job.json" in
      assert_identity archive;
      require_code 0 (cli [ "resume"; "--archived"; "legacy"; "--terminal"; "ghostty" ]);
      assert_identity job_file;
      let logs = read_file log in
      if string_contains logs (branch ^ "-backend") || string_contains logs (branch ^ "-apps") then
        failwith "legacy lifecycle asked wt for renamed branches")

let test_multi_workspace_sonnet_task_lifecycle () =
  with_temp_root "multi-workspace-sonnet" (fun root ->
      let home, log, env = setup_environment root in
      let backend = Filename.concat root "django-backend" in
      let admin = Filename.concat root "admin" in
      let context =
        Filename.concat home
          ".monty/runs/2026-08-13-sonnet-5-invoices/sonnet-5-invoices.md"
      in
      let manifest =
        Filename.concat home
          ".monty/runs/2026-08-13-sonnet-5-invoices/jobs.json"
      in
      let backend_stem = "cto/invoice-parser-sonnet-5" in
      let admin_stem = "cto/admin-invoice-sonnet-5" in
      let backend_branch = backend_stem ^ "-django-backend" in
      let admin_branch = admin_stem ^ "-admin" in
      init_git_repo backend;
      init_git_repo admin;
      add_project ~root ~home ~env backend;
      add_project ~root ~home ~env admin;
      install_repo_scoped_wt ~root ~log;
      let osascript = Filename.concat root "fake-bin/osascript" in
      Shell.write_file osascript
        (String.concat "\n"
           [ "#!/bin/sh";
             "set -eu";
             "printf '%s\\n' \"$0 $*\" >> " ^ Shell.quote log;
             "" ]);
      Shell.chmod_executable osascript;
      let titles =
        [ "Filler task 1"; "Filler task 2"; "Filler task 3";
          "Filler task 4";
          "Upgrade incoming invoice parsing to Claude Sonnet 5";
          "Historical admin task";
          "Expose Claude Sonnet 5 invoice reprocessing in admin" ]
      in
      List.iteri
        (fun index title ->
          let project = if index = 6 then "admin" else "django-backend" in
          require_code 0
            (run ~root ~env (2100 + index)
               [ "task"; "add"; "--home"; home; "--project"; project;
                 "--title"; title ]))
        titles;
      require_code 0
        (run ~root ~env 2108
           [ "task"; "done"; "local-006"; "--home"; home ]);
      require_code 0
        (run ~root ~env 2109
           [ "task"; "workspace"; "add"; "local-005"; "--repo";
             backend; "--branch"; backend_stem; "--home"; home ]);
      require_code 0
        (run ~root ~env 2110
           [ "task"; "workspace"; "add"; "local-007"; "--repo"; admin;
             "--branch"; admin_stem; "--home"; home ]);
      let before_merge =
        run ~root ~env 2111 [ "task"; "show"; "local-005"; "--home"; home ]
      in
      require_code 0 before_merge;
      require_contains "pre-launch absolute backend repo" before_merge.stdout
        backend;
      require_contains "pre-launch workspace state" before_merge.stdout
        "<not materialized>";
      if json_string "branch" (List.nth (local_tasks_json home) 4) <> backend_stem then
        failwith "single-workspace planning changed its branch";
      let merge =
        run ~root ~env 2112
          [ "task"; "merge"; "local-007"; "--into"; "local-005";
            "--home"; home ]
      in
      require_code 0 merge;
      require_contains "Sonnet task merge" merge.stdout
        "Merged local:local-007 into local:local-005";
      let tasks = local_tasks_json home in
      let task id =
        match
          List.find_opt
            (fun json -> String.equal (json_string "id" json) id)
            tasks
        with
        | Some task -> task
        | None -> failf "missing fixture task %s" id
      in
      let retained = task "local-005" in
      let merged = task "local-007" in
      if json_string "status" retained <> "open" then
        failwith "local-005 was not retained as the open Sonnet task";
      if json_string "status" merged <> "done" then
        failwith "local-007 was not closed by the explicit merge";
      require_contains "merged task provenance"
        Yojson.Safe.Util.(merged |> member "notes" |> to_string)
        "merged_into=local:local-005";
      let retained_workspaces =
        Yojson.Safe.Util.(retained |> member "workspaces" |> to_list)
      in
      if List.length retained_workspaces <> 2 then
        failwith "local-005 did not retain both repository workspaces";
      if List.map (json_string "branch") retained_workspaces
         <> [ backend_branch; admin_branch ]
         || json_string "branch" retained <> backend_branch then
        failwith "merge did not suffix both branches and the task branch alias";
      let open_inventory =
        run ~root ~env 2113 [ "list"; "--no-sync"; "--home"; home ]
      in
      require_code 0 open_inventory;
      require_contains "multi-project task inventory" open_inventory.stdout
        "django-backend,admin";
      require_contains "backend branch inventory" open_inventory.stdout
        ("django-backend=" ^ backend_branch);
      require_contains "admin branch inventory" open_inventory.stdout
        ("admin=" ^ admin_branch);
      let admin_inventory =
        run ~root ~env 21131
          [ "tasks"; "list"; "--project"; "admin"; "--no-sync"; "--home";
            home ]
      in
      require_code 0 admin_inventory;
      require_contains "secondary project filter" admin_inventory.stdout
        "1 local:local-005";
      let premature_ensure =
        run ~root ~env 21135
          [ "task"; "workspace"; "ensure"; "local-005"; "--repo"; admin;
            "--home"; home; "--wt-command"; "wt" ]
      in
      if premature_ensure.code = 0 then
        failwith "an unlaunched task materialized a workspace without worker state";
      if string_contains (read_file log) "/wt " then
        failwith "rejected pre-launch workspace ensure invoked wt";
      Shell.ensure_dir (Filename.dirname context);
      Shell.write_file context
        (String.concat "\n"
           [ "# Claude Sonnet 5 invoice rollout";
             "";
             "Implement the backend parser upgrade first, then expose it in admin.";
             "Both repositories belong to one Monty task and one lifecycle.";
             "" ]);
      let workspace repo branch =
        `Assoc [ ("repo", `String repo); ("branch", `String branch) ]
      in
      write_manifest manifest
        [ `Assoc
            [ ("id", `String "sonnet-5-invoices");
              ( "title",
                `String
                  "Upgrade invoice parsing and admin reprocessing to Claude Sonnet 5" );
              ( "workspaces",
                `List
                  [ workspace backend backend_stem;
                    workspace admin admin_stem ] );
              ("context", `String context);
              ("task_key", `String "local:local-005") ] ];
      let dry =
        run ~root ~env 2114
          [ "launch-many"; "--manifest"; manifest; "--terminal"; "dry-run";
            "--home"; home ]
      in
      require_code 0 dry;
      require_contains "dry-run backend absolute repo" dry.stdout backend;
      require_contains "dry-run admin absolute repo" dry.stdout admin;
      require_contains "dry-run second workspace" dry.stdout
        "[dry-run] workspace 2 branch";
      if string_contains (read_file log) "/wt " then
        failwith "multi-workspace dry-run materialized a worktree";
      let worker_dir =
        Filename.concat home
          ".monty/runs/2026-08-13-sonnet-5-invoices/workers/sonnet-5-invoices"
      in
      let job_file = Filename.concat worker_dir "job.json" in
      let failed_env =
        replace_env env [ ("MONTY_TEST_WT_FAIL_BRANCH", admin_branch) ]
      in
      let failed_launch =
        run ~root ~env:failed_env 2115
          [ "launch-many"; "--manifest"; manifest; "--terminal"; "ghostty";
            "--home"; home ]
      in
      if failed_launch.code = 0 then
        failwith "second-workspace materialization failure was ignored";
      if job_status job_file <> "launch-failed" then
        failwith "partial multi-workspace launch was not retryable";
      let partial_workspaces =
        Yojson.Safe.Util.(
          Yojson.Safe.from_file job_file |> member "workspaces" |> to_list)
      in
      if
        Yojson.Safe.Util.(List.nth partial_workspaces 0 |> member "worktree")
        = `Null
        || Yojson.Safe.Util.(List.nth partial_workspaces 1 |> member "worktree")
           <> `Null
      then
        failwith "partial launch did not persist only the completed workspace";
      let headless_prepared =
        run ~root ~env 21152
          [ "headless"; "prepare-many"; "--manifest"; manifest; "--home";
            home ]
      in
      require_code 0 headless_prepared;
      let headless_workspaces =
        Yojson.Safe.Util.(
          Yojson.Safe.from_string headless_prepared.stdout |> member "jobs"
          |> index 0 |> member "workspaces" |> to_list)
      in
      if List.length headless_workspaces <> 2 then
        failwith "headless preparation omitted a Sonnet task workspace";
      if job_status job_file <> "prepared" then
        failwith "headless multi-workspace retry did not leave the worker prepared";
      let launched =
        run ~root ~env 21155
          [ "launch-many"; "--manifest"; manifest; "--terminal"; "ghostty";
            "--home"; home ]
      in
      require_code 0 launched;
      let job = Yojson.Safe.from_file job_file in
      let materialized =
        Yojson.Safe.Util.(job |> member "workspaces" |> to_list)
      in
      if List.length materialized <> 2 then
        failwith "the Sonnet worker did not persist two workspaces";
      let materialized_paths =
        materialized
        |> List.map (fun workspace ->
               let path =
                 Yojson.Safe.Util.(workspace |> member "worktree" |> to_string)
               in
               if Filename.is_relative path then
                 failf "persisted worktree is not absolute: %s" path;
               if not (Sys.file_exists path && Sys.is_directory path) then
                 failf "persisted worktree does not exist: %s" path;
               path)
      in
      let backend_worktree = List.nth materialized_paths 0 in
      let admin_worktree = List.nth materialized_paths 1 in
      if String.equal backend_worktree admin_worktree then
        failwith "two repositories resolved to one worktree";
      let shown =
        run ~root ~env 2116 [ "task"; "show"; "local-005"; "--home"; home ]
      in
      require_code 0 shown;
      List.iter
        (require_contains "task show absolute workspace" shown.stdout)
        [ backend; admin; backend_worktree; admin_worktree ];
      require_contains "first workspace launch cwd" shown.stdout
        ("Launch cwd: " ^ backend_worktree);
      let present_rows =
        shown.stdout |> String.split_on_char '\n'
        |> List.filter (fun line -> string_contains line "\tpresent")
        |> List.length
      in
      if present_rows <> 2 then
        failf "task show did not report both materialized workspaces present:\n%s"
          shown.stdout;
      let script =
        Shell.read_file
          (Filename.concat (Home.runtime_script_dir ~home ())
             "monty-sonnet-5-invoices-launch.sh")
      in
      require_contains "first workspace environment" script "MONTY_WORKSPACE_1";
      require_contains "second workspace environment" script "MONTY_WORKSPACE_2";
      require_contains "worker-scoped workspace rehydration" script
        "task workspace ensure";
      require_contains "workspace rehydration uses the control-room home" script
        ("--home " ^ Shell.quote home);
      let late_workspace =
        run ~root ~env 21158
          [ "task"; "workspace"; "add"; "local-005"; "--repo"; backend;
            "--branch"; "cto/late-workspace"; "--home"; home ]
      in
      if late_workspace.code = 0 then
        failwith "linked task accepted a workspace membership change";
      require_contains "linked workspace immutability" late_workspace.stderr
        "immutable after launch";
      let ensured_admin =
        run ~root ~env 2116
          [ "task"; "workspace"; "ensure"; "local-005"; "--repo"; admin;
            "--home"; home; "--wt-command"; "wt" ]
      in
      require_code 0 ensured_admin;
      require_line "task workspace ensure absolute output" ensured_admin.stdout
        admin_worktree;
      let task_resume =
        run ~root ~env 21165
          [ "resume"; "local-005"; "--terminal"; "dry-run"; "--home";
            home ]
      in
      require_code 0 task_resume;
      require_contains "task-id resume second workspace" task_resume.stdout admin;
      Shell.write_file (Filename.concat admin_worktree "dirty-admin.txt") "dirty\n";
      let blocked =
        run ~root ~env 2117
          [ "done"; "local-005"; "--home"; home; "--wt-command";
            "wt" ]
      in
      if blocked.code = 0 then
        failwith "dirty secondary workspace did not block the shared lifecycle";
      require_contains "secondary dirty workspace diagnostic" blocked.stderr
        admin_worktree;
      if
        not
          (Sys.file_exists backend_worktree && Sys.file_exists admin_worktree
          && Sys.file_exists worker_dir)
      then failwith "multi-workspace preflight deleted part of a blocked task";
      let blocked_task =
        local_tasks_json home
        |> List.find (fun json -> json_string "id" json = "local-005")
      in
      if json_string "status" blocked_task <> "open" then
        failwith "blocked completion closed the shared local task";
      let completed =
        run ~root ~env 2118
          [ "done"; "local:local-005"; "--force"; "--home"; home;
            "--wt-command"; "wt" ]
      in
      require_code 0 completed;
      require_contains "backend completion output" completed.stdout backend;
      require_contains "admin completion output" completed.stdout admin;
      if Sys.file_exists backend_worktree || Sys.file_exists admin_worktree then
        failwith "completion did not remove every task worktree";
      let archive_job =
        Filename.concat home
          ".monty/runs/2026-08-13-sonnet-5-invoices/archive/sonnet-5-invoices/job.json"
      in
      if not (Sys.file_exists archive_job) || job_status archive_job <> "done" then
        failwith "the shared worker was not archived once";
      let final_task =
        local_tasks_json home
        |> List.find (fun json -> json_string "id" json = "local-005")
      in
      if json_string "status" final_task <> "done" then
        failwith "the shared local task did not close with its worker";
      let archived_show =
        run ~root ~env 2119 [ "task"; "show"; "local-005"; "--home"; home ]
      in
      require_code 0 archived_show;
      let missing_rows =
        archived_show.stdout |> String.split_on_char '\n'
        |> List.filter (fun line -> string_contains line "\tmissing")
        |> List.length
      in
      if missing_rows <> 2 then
        failwith "archived task did not report both removed worktrees as missing";
      let archived_resume =
        run ~root ~env 2120
          [ "resume"; "--archived"; "local:local-005"; "--terminal";
            "dry-run"; "--home"; home ]
      in
      require_code 0 archived_resume;
      require_contains "archived task-id resume" archived_resume.stdout admin;
      if count_lines_containing log "/wt " < 6 then
        failwith "the repo-scoped wt fake did not observe both workspace lifecycles")

let test_headless_prepare_run_and_resume () =
  with_temp_root "headless" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      let manifest = Filename.concat home ".monty/runs/run-headless/jobs.json" in
      init_git_repo repo;
      add_project ~root ~home ~env repo;
      install_create_wt ~root ~log;
      Shell.write_file context "# Headless task\n\nImplement the requested change.\n";
      write_manifest manifest
        [ manifest_job ~id:"headless-one" ~branch:"cto/headless-one"
            ~title:"Headless one" ~repo ~context ();
          manifest_job ~id:"headless-two" ~branch:"cto/headless-two"
            ~title:"Headless two" ~repo ~context () ];
      let dry =
        run ~root ~env 1950
          [ "headless"; "prepare-many"; "--manifest"; manifest; "--home";
            home; "--dry-run" ]
      in
      require_code 0 dry;
      let dry_json = Yojson.Safe.from_string dry.stdout in
      if
        Yojson.Safe.Util.(dry_json |> member "schema" |> to_string)
        <> Headless.prepare_schema
      then failwith "headless dry-run returned the wrong schema";
      if
        Yojson.Safe.Util.(dry_json |> member "jobs" |> to_list |> List.length)
        <> 2
      then failwith "headless dry-run did not plan both workers";
      if Sys.file_exists (Filename.concat home ".monty/tasks.local.json") then
        failwith "headless dry-run created tasks";
      require_empty_log log;
      let prepare_failure_env =
        replace_env env [ ("MONTY_TEST_WT_FAIL_BRANCH", "cto/headless-two") ]
      in
      let failed_prepare =
        run ~root ~env:prepare_failure_env 1951
          [ "headless"; "prepare-many"; "--manifest"; manifest; "--home";
            home ]
      in
      if failed_prepare.code = 0 then
        failwith "headless preparation ignored a failing worktree request";
      let staged_job_file id =
        Filename.concat home
          (".monty/runs/run-headless/workers/" ^ id ^ "/job.json")
      in
      List.iter
        (fun id ->
          if job_status (staged_job_file id) <> "prepared" then
            failf "failed headless preparation did not leave %s prepared" id)
        [ "headless-one"; "headless-two" ];
      let prepared =
        run ~root ~env 1957
          [ "headless"; "prepare-many"; "--manifest"; manifest; "--home";
            home ]
      in
      require_code 0 prepared;
      let prepared_json = Yojson.Safe.from_string prepared.stdout in
      let jobs = Yojson.Safe.Util.(prepared_json |> member "jobs" |> to_list) in
      if List.length jobs <> 2 then failwith "headless prepare did not return both workers";
      let worktrees =
        List.map
          (fun job ->
            if Yojson.Safe.Util.member "containerized" job <> `Null then
              failwith "headless preparation exposed a removed execution mode";
            let worktree = Yojson.Safe.Util.(job |> member "worktree" |> to_string) in
            if not (Sys.file_exists worktree && Sys.is_directory worktree) then
              failf "headless worktree is missing: %s" worktree;
            worktree)
          jobs
      in
      let job_file id =
        Filename.concat home
          (".monty/runs/run-headless/workers/" ^ id ^ "/job.json")
      in
      let require_no_terminal_script id =
        let json = Yojson.Safe.from_file (job_file id) in
        let script = Yojson.Safe.Util.(json |> member "launch_script" |> to_string) in
        if Sys.file_exists script then
          failf "headless worker %s unexpectedly wrote a terminal launch script" id
      in
      List.iter
        (fun id ->
          let path = job_file id in
          if job_status path <> "prepared" then
            failf "headless prepare did not leave %s prepared" id;
          let json = Yojson.Safe.from_file path in
          let worktree = Yojson.Safe.Util.(json |> member "last_known_worktree" |> to_string) in
          if not (List.mem worktree worktrees) then
            failf "headless prepare did not persist %s worktree" id;
          List.iter
            (fun forbidden ->
              if Yojson.Safe.Util.member forbidden json <> `Null then
                failf "headless state persisted forbidden field %s" forbidden)
            [ "worker_backend"; "subagent"; "run_id"; "async_dir";
              "containerized"; "container_worker" ];
          require_no_terminal_script id)
        [ "headless-one"; "headless-two" ];
      require_contains "headless wt invocation" (read_file log) "/wt b";
      let stale_manifest =
        match Manifest.load ~home manifest with
        | Ok jobs -> jobs
        | Error message -> failwith message
      in
      let stale_options =
        Launcher.
          { backend = Terminal.Dry_run;
            target = Terminal.Tab;
            harness = Harness.Pi;
            harness_command = "pi";
            codex_yolo = false;
            wt_command = Filename.concat root "fake-bin/wt";
            worktree_mode = Always;
            branch_prefix = "monty";
            agent_profile = "reviewed";
            fork = None;
            home;
            script_dir = Home.runtime_script_dir ~home ();
            monty_command = executable }
      in
      let stale_preflight =
        match Launcher.preflight_batch stale_options stale_manifest with
        | Ok prepared -> prepared
        | Error message -> failwith message
      in
      if string_contains (read_file log) "osascript"
         || string_contains (read_file log) "/pi "
      then failwith "headless prepare invoked Pi or Ghostty";
      install_pi_exec ~root ~log;
      let cli args = run ~root ~env 1952 (args @ [ "--home"; home ]) in
      let premature_resume = cli [ "headless"; "resume"; "--wait"; "headless-two" ] in
      require_code 1 premature_resume;
      require_contains "headless successor guard" premature_resume.stderr "launch-requested";
      let begun = cli [ "headless"; "run"; "--wait"; "headless-one" ] in
      require_code 0 begun;
      let result = Yojson.Safe.from_string begun.stdout in
      if json_string "harness" result <> "pi" then failwith "Pi run used the wrong harness";
      let first_attempt = json_string "attempt_id" result in
      let artifact_dir = json_string "artifact_dir" result in
      let descriptor = Yojson.Safe.from_file (Filename.concat artifact_dir "attempt.json") in
      if json_string "schema" descriptor <> "monty:headless-attempt:v2" then
        failwith "Pi run did not use a process attempt";
      List.iter (fun path ->
          if not (Sys.file_exists (Filename.concat artifact_dir path)) then
            failwith ("missing Pi run artifact " ^ path))
        [ "implementation.md"; "reviews/correctness.md"; "reviews/quality.md"; "final.md" ];
      if job_status (job_file "headless-one") <> "launch-requested"
         || job_status (job_file "headless-two") <> "prepared" then
        failwith "headless run claimed the wrong workers";
      (match Launcher.reserve_batch ~reject_requested:true stale_options stale_preflight with
       | Ok _ -> failwith "headless reservation accepted a worker claimed after stale preflight"
       | Error message -> require_contains "headless post-lock claim guard" message "became launch-requested");
      let duplicate = cli [ "headless"; "run"; "--wait"; "headless-one" ] in
      require_code 1 duplicate;
      require_contains "headless replay recovery" duplicate.stderr "headless resume";
      let resumed = cli [ "headless"; "resume"; "--wait"; "headless-one" ] in
      require_code 0 resumed;
      let second_attempt = json_string "attempt_id" (Yojson.Safe.from_string resumed.stdout) in
      if first_attempt = second_attempt then failwith "resume reused its predecessor attempt";
      let pending = cli [ "handoff"; "pending"; "--format"; "json" ] in
      require_code 0 pending;
      let pending_count output = Yojson.Safe.Util.(Yojson.Safe.from_string output
          |> member "pending" |> to_list |> List.length) in
      if pending_count pending.stdout <> 2 then failwith "Pi runs did not publish two durable notices";
      (* Discovery no longer interprets retired callback artifacts. *)
      let retired = Filename.concat (Filename.dirname (job_file "headless-one"))
          "artifacts/headless/retired-callback" in
      Shell.ensure_dir retired;
      Yojson.Safe.to_file (Filename.concat retired "attempt.json")
        (`Assoc [ "schema", `String "monty:headless-attempt:v1";
          "attempt_id", `String "retired-callback"; "worker_id", `String "headless-one";
          "source", `String "headless-pi" ]);
      Shell.write_file (Filename.concat retired "final.md") "Retired callback artifact\n";
      let rediscovered = cli [ "handoff"; "pending"; "--format"; "json" ] in
      require_code 0 rediscovered;
      if pending_count rediscovered.stdout <> 2 then failwith "pending revived a retired callback attempt";
      List.iter require_no_terminal_script [ "headless-one"; "headless-two" ];
      let tasks = local_tasks_json home in
      if List.length tasks <> 2 || List.exists (fun task -> json_string "status" task <> "open") tasks then
        failwith "headless execution changed task lifecycle";
      if string_contains (read_file log) "osascript" then failwith "headless run opened Ghostty")

let test_agent_profiles_selection_snapshot_and_solo () =
  with_temp_root "agent-profiles" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      let mixed_manifest =
        Filename.concat home ".monty/runs/run-profiles/jobs.json"
      in
      let solo_manifest =
        Filename.concat home ".monty/runs/run-solo-codex/jobs.json"
      in
      init_git_repo repo;
      add_project ~root ~home ~env repo;
      install_create_wt ~root ~log;
      Shell.write_file context "# Agent profile task\n";
      let listed =
        run ~root ~env 1970 [ "agent-profiles"; "list"; "--home"; home ]
      in
      require_code 0 listed;
      require_contains "profile list reviewed" listed.stdout "reviewed";
      require_contains "profile list solo" listed.stdout "solo";
      let shown =
        run ~root ~env 1971
          [ "agent-profiles"; "show"; "solo"; "--home"; home ]
      in
      require_code 0 shown;
      require_contains "profile show schema" shown.stdout
        Agent_profile.snapshot_schema;
      let selected =
        run ~root ~env 1972
          [ "settings"; "set"; "agent-profile"; "solo"; "--home"; home ]
      in
      require_code 0 selected;
      write_manifest mixed_manifest
        [ manifest_job ~id:"profile-reviewed" ~branch:"cto/profile-reviewed"
            ~title:"Profile reviewed" ~repo ~context ();
          manifest_job ~id:"profile-solo" ~branch:"cto/profile-solo"
            ~agent_profile:"solo" ~title:"Profile solo" ~repo ~context ();
          manifest_job ~id:"profile-legacy" ~branch:"cto/profile-legacy"
            ~agent_profile:"reviewed" ~title:"Profile legacy" ~repo ~context () ];
      let dry =
        run ~root ~env 1973
          [ "headless"; "prepare-many"; "--dry-run"; "--manifest";
            mixed_manifest; "--agent-profile"; "reviewed"; "--home"; home ]
      in
      require_code 0 dry;
      let jobs =
        Yojson.Safe.Util.(
          Yojson.Safe.from_string dry.stdout |> member "jobs" |> to_list)
      in
      let profile id =
        jobs
        |> List.find (fun json ->
               Yojson.Safe.Util.(json |> member "id" |> to_string) = id)
        |> fun json ->
        Yojson.Safe.Util.(json |> member "agent_profile" |> to_string)
      in
      if profile "profile-reviewed" <> "reviewed" then
        failwith "CLI agent profile override did not beat the persisted setting";
      if profile "profile-solo" <> "solo" then
        failwith "manifest agent profile did not beat the CLI override";
      let prepared =
        run ~root ~env 1974
          [ "headless"; "prepare-many"; "--manifest"; mixed_manifest;
            "--agent-profile"; "reviewed"; "--home"; home ]
      in
      require_code 0 prepared;
      let worker_dir id =
        Filename.concat home (".monty/runs/run-profiles/workers/" ^ id)
      in
      List.iter
        (fun (id, expected) ->
          let directory = worker_dir id in
          let job = Yojson.Safe.from_file (Filename.concat directory "job.json") in
          if Yojson.Safe.Util.(job |> member "agent_profile" |> to_string) <> expected
          then failf "worker %s did not persist profile %s" id expected;
          let snapshot = Agent_profile.load_snapshot directory in
          match snapshot with
          | Ok profile when profile.id = expected -> ()
          | Ok profile ->
              failf "worker %s pinned profile %s instead of %s" id profile.id expected
          | Error message -> failwith message)
        [ ("profile-reviewed", "reviewed"); ("profile-solo", "solo") ];
      require_contains "reviewed interactive profile"
        (read_file (Filename.concat (worker_dir "profile-reviewed") "MONTY.md"))
        reviewed_profile.interactive;
      require_contains "solo interactive profile"
        (read_file (Filename.concat (worker_dir "profile-solo") "MONTY.md"))
        solo_profile.interactive;
      let legacy_dir = worker_dir "profile-legacy" in
      let legacy_job_path = Filename.concat legacy_dir "job.json" in
      let legacy_job = Yojson.Safe.from_file legacy_job_path in
      let legacy_job =
        match legacy_job with
        | `Assoc fields ->
            `Assoc (List.remove_assoc "agent_profile" fields)
        | _ -> failwith "legacy job fixture was not an object"
      in
      Yojson.Safe.to_file legacy_job_path legacy_job;
      Unix.unlink (Agent_profile.snapshot_path legacy_dir);
      let set_solo =
        run ~root ~env 1975
          [ "settings"; "set"; "agent-profile"; "solo"; "--home"; home ]
      in
      require_code 0 set_solo;
      install_pi_exec ~root ~log;
      let run_pi id expected_phases =
        let before = count_lines_containing log "/pi " in
        let result = run ~root ~env 1976
            [ "headless"; "run"; "--wait"; id; "--home"; home ] in
        require_code 0 result;
        if count_lines_containing log "/pi " - before <> expected_phases then
          failwith ("wrong pinned phase count for " ^ id);
        Yojson.Safe.from_string result.stdout
      in
      let reviewed = run_pi "profile-reviewed" 4 in
      if json_string "agent_profile" reviewed <> "reviewed" then
        failwith "setting changed a pinned reviewed profile";
      let legacy = run_pi "profile-legacy" 4 in
      if json_string "agent_profile" legacy <> "reviewed" then
        failwith "worker without profile metadata did not use reviewed";
      let set_reviewed =
        run ~root ~env 1977
          [ "settings"; "set"; "agent-profile"; "reviewed"; "--home"; home ]
      in
      require_code 0 set_reviewed;
      let solo = run_pi "profile-solo" 1 in
      if json_string "agent_profile" solo <> "solo" then
        failwith "setting changed a pinned solo profile";
      let artifacts = json_string "artifact_dir" solo in
      if not (Sys.file_exists (Filename.concat artifacts "final.md"))
         || Sys.file_exists (Filename.concat artifacts "reviews") then
        failwith "solo Pi run produced the wrong artifacts";
      install_codex_exec ~root ~log;
      let set_codex =
        run ~root ~env 1979
          [ "settings"; "set"; "harness"; "codex"; "--home"; home ]
      in
      require_code 0 set_codex;
      let set_solo =
        run ~root ~env 1980
          [ "settings"; "set"; "agent-profile"; "solo"; "--home"; home ]
      in
      require_code 0 set_solo;
      write_manifest solo_manifest
        [ manifest_job ~id:"solo-codex" ~branch:"cto/solo-codex"
            ~title:"Solo Codex" ~repo ~context () ];
      let prepared_solo =
        run ~root ~env 1981
          [ "headless"; "prepare-many"; "--manifest"; solo_manifest;
            "--home"; home ]
      in
      require_code 0 prepared_solo;
      let before = count_lines_containing log "/codex exec" in
      let ran =
        run ~root ~env 1982
          [ "headless"; "run"; "--wait"; "solo-codex"; "--home"; home ]
      in
      require_code 0 ran;
      if count_lines_containing log "/codex exec" - before <> 1 then
        failwith "solo Codex profile did not run exactly one phase";
      let result = Yojson.Safe.from_string ran.stdout in
      if Yojson.Safe.Util.(result |> member "agent_profile" |> to_string) <> "solo"
      then failwith "solo Codex result omitted its pinned profile";
      let artifacts =
        Yojson.Safe.Util.(result |> member "artifact_dir" |> to_string)
      in
      if not (Sys.file_exists (Filename.concat artifacts "final.md")) then
        failwith "solo Codex run omitted its final handoff";
      if Sys.file_exists (Filename.concat artifacts "reviews") then
        failwith "solo Codex run created reviewer artifacts";
      let handoff =
        Yojson.Safe.Util.(result |> member "handoff" |> to_string)
        |> Yojson.Safe.from_file
      in
      if Yojson.Safe.Util.(handoff |> member "review" |> member "summary") <> `Null
      then failwith "solo Codex handoff claimed reviews occurred")

let test_simplified_default_and_headless_writers () =
  with_temp_root "simplified" (fun root ->
      let home, log, env = setup_environment ~agent_profile:None root in
      let bundled_home =
        Option.value ~default:(Sys.getcwd ()) (Sys.getenv_opt "DUNE_SOURCEROOT") in
      let simplified = match Agent_profile.find ~home:bundled_home "simplified" with
        | Ok profile -> profile | Error message -> failwith message in
      install_profile home simplified;
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      let manifest = Filename.concat home ".monty/runs/simplified/jobs.json" in
      let worker_dir id = Filename.concat home (".monty/runs/simplified/workers/" ^ id) in
      let job_file id = Filename.concat (worker_dir id) "job.json" in
      let cli ?(env = env) args = run ~root ~env 19830 (args @ [ "--home"; home ]) in
      let ok args = let result = cli args in require_code 0 result; result.stdout in
      init_git_repo repo;
      add_project ~root ~home ~env repo;
      install_repo_scoped_wt ~root ~log;
      Shell.write_file context "# Simplified feature requirements\nPreserve required behavior.\n";
      if String.trim (ok [ "settings"; "get"; "agent-profile" ]) <> "simplified" then
        failwith "fresh setting did not use simplified";
      require_contains "default profile discovery" (ok [ "agent-profiles"; "list" ])
        "implementation -> simplification";
      require_contains "default profile show" (ok [ "agent-profiles"; "show" ])
        "can you radically simplify this?";
      write_manifest manifest
        (List.map (fun id -> manifest_job ~id ~branch:("cto/" ^ id)
           ~title:id ~repo ~context ()) [ "simple-pi"; "simple-codex"; "simple-fail" ]);
      require_contains "interactive launch default"
        (ok [ "launch-many"; "--manifest"; manifest; "--terminal"; "dry-run" ])
        "agent profile: simplified";
      let dry = ok [ "headless"; "prepare-many"; "--manifest"; manifest; "--dry-run" ] in
      require_contains "default launch preview" dry "\"agent_profile\": \"simplified\"";
      if Sys.file_exists (worker_dir "simple-pi") then failwith "dry-run created a worker";
      ignore (ok [ "headless"; "prepare-many"; "--manifest"; manifest ]);
      List.iter (fun id ->
          let instructions = read_file (Filename.concat (worker_dir id) "MONTY.md") in
          require_contains "interactive separate agent" instructions "one separate simplifier";
          require_contains "interactive editing access" instructions "with write access";
          require_contains "interactive supports all harnesses" instructions "interactive Pi, Codex or Claude";
          require_contains "read-only envelope scoped to reviewers" instructions
            "not implementers, simplifiers, or fixers")
        [ "simple-pi"; "simple-codex" ];
      (* Changing the default and role files must not change any prepared worker. *)
      ignore (ok [ "settings"; "set"; "agent-profile"; "solo" ]);
      install_profile home { simplified with interactive = "CHANGED PROFILE";
        fix_role = Agent_profile.Fixer; fix = Some "CHANGED FINAL WRITER" };
      ignore (ok [ "headless"; "prepare-many"; "--manifest"; manifest ]);
      install_pi_exec ~root ~log;
      let before_pi = count_lines_containing log "/pi " in
      let finished = ok [ "headless"; "run"; "--wait"; "simple-pi" ] |> Yojson.Safe.from_string in
      if count_lines_containing log "/pi " - before_pi <> 2 then
        failwith "Pi default did not run two sequential writers";
      let pi_artifacts = json_string "artifact_dir" finished in
      let simplifier_prompt = read_file (Filename.concat pi_artifacts "prompts/simplifier.md") in
      require_contains "Pi final role" simplifier_prompt "can you radically simplify this?";
      require_contains "Pi implementation handoff" simplifier_prompt "PI IMPLEMENTATION HANDOFF";
      if string_contains simplifier_prompt "CHANGED FINAL WRITER" then
        failwith "Pi run ignored its pinned simplifier";
      let assert_handoff result =
        let path = Yojson.Safe.Util.(result |> member "handoff" |> to_string) in
        let handoff = Yojson.Safe.from_file path in
        if Yojson.Safe.Util.(handoff |> member "review" |> member "summary") <> `Null then
          failwith "simplified handoff claimed independent reviews";
        require_contains "simplifier validation reporting" (read_file path) "final simplifier handoff"
      in
      assert_handoff finished;
      let codex = Filename.concat root "fake-bin/codex" in
      Shell.write_file codex ({|#!/bin/sh
set -eu
output= worktree= sandbox=
while [ "$#" -gt 0 ]; do
  case "$1" in
    --output-last-message) output=$2; shift 2 ;;
    -C) worktree=$2; shift 2 ;;
    --sandbox) sandbox=$2; shift 2 ;;
    *) shift ;;
  esac
done
test "$sandbox" = workspace-write
prompt=$(cat)
case "$output" in
  */implementation.md)
    phase=implementation
    printf 'implemented\n' > "$worktree/feature.txt"
    printf 'IMPLEMENTATION HANDOFF TOKEN\n' > "$output"
    ;;
  */final.md)
    phase=simplifier
    test "$(cat "$worktree/feature.txt")" = implemented
    case "$prompt" in *'IMPLEMENTATION HANDOFF TOKEN'*) ;; *) exit 81 ;; esac
    case "$prompt" in *'can you radically simplify this?'*) ;; *) exit 82 ;; esac
    case "$prompt" in *'Simplified feature requirements'*) ;; *) exit 83 ;; esac
    case "$prompt" in *'CHANGED FINAL WRITER'*) exit 84 ;; esac
    if [ "${MONTY_TEST_SIMPLIFIER_FAIL:-}" = 1 ]; then exit 73; fi
    printf 'simplified\n' > "$worktree/feature.txt"
    printf 'SIMPLIFIER FINAL HANDOFF: removed duplication; affected checks passed.\n' > "$output"
    ;;
  *) exit 85 ;;
esac
printf '%s %s\n' "$phase" "$worktree" >> |} ^ Shell.quote log ^ "\n");
      Shell.chmod_executable codex;
      ignore (ok [ "settings"; "set"; "harness"; "codex" ]);
      let result = ok [ "headless"; "run"; "--wait"; "simple-codex" ] |> Yojson.Safe.from_string in
      assert_handoff result;
      let artifacts = Yojson.Safe.Util.(result |> member "artifact_dir" |> to_string) in
      let worktree = Yojson.Safe.Util.(Yojson.Safe.from_file (job_file "simple-codex")
        |> member "workspaces" |> index 0 |> member "worktree" |> to_string) in
      require_contains "sequential writers in same worktree" (read_file log)
        ("implementation " ^ worktree ^ "\nsimplifier " ^ worktree ^ "\n");
      if count_lines_containing log "implementation " <> 1 ||
         count_lines_containing log "simplifier " <> 1 then
        failwith "simplified Codex did not run exactly two phases";
      if read_file (Filename.concat worktree "feature.txt") <> "simplified\n" then
        failwith "simplifier did not edit implementation";
      if Sys.file_exists (Filename.concat artifacts "reviews") then
        failwith "simplified run created reviewer artifacts";
      List.iter (fun path ->
          if not (Sys.file_exists (Filename.concat artifacts path)) then
            failwith ("missing simplifier artifact " ^ path))
        [ "prompts/simplifier.md"; "events/simplifier.jsonl"; "logs/simplifier.log" ];
      require_contains "final handoff captured in durable memory"
        (read_file (Filename.concat (worker_dir "simple-codex") "memory.md"))
        "SIMPLIFIER FINAL HANDOFF";
      let failed_env = replace_env env [ "MONTY_TEST_SIMPLIFIER_FAIL", "1" ] in
      let failed = cli ~env:failed_env [ "headless"; "run"; "--wait"; "simple-fail" ] in
      require_code 1 failed;
      require_contains "failure role" failed.stderr "simplifier phase failed";
      if job_status (job_file "simple-fail") <> "launch-requested" then
        failwith "simplifier failure did not retain claimed worker";
      let failure_dir = Filename.concat home ".monty/handoffs/simplified/simple-fail" in
      let failure_file = Sys.readdir failure_dir |> Array.to_list
        |> List.find (fun name -> Filename.check_suffix name ".json") in
      let failure = Yojson.Safe.from_file (Filename.concat failure_dir failure_file) in
      if Yojson.Safe.Util.(failure |> member "last_phase" |> to_string) <> "simplifier" then
        failwith "durable failure did not name the simplifier";
      let replay = cli [ "headless"; "run"; "--wait"; "simple-fail" ] in
      require_code 1 replay;
      require_contains "failure requires explicit resume" replay.stderr "headless resume";
      let resumed = ok [ "headless"; "resume"; "--wait"; "simple-fail" ] |> Yojson.Safe.from_string in
      assert_handoff resumed;
      List.iter (fun id ->
          if job_status (job_file id) <> "launch-requested" then
            failwith "successful simplification closed a worker")
        [ "simple-pi"; "simple-codex"; "simple-fail" ];
      List.iter (fun task ->
          if Yojson.Safe.Util.(task |> member "status" |> to_string) <> "open" then
            failwith "successful simplification closed a task") (local_tasks_json home);
      if not (Sys.file_exists worktree) then failwith "simplification removed worktree";
      (* Exercise archive/reopen only in this isolated fixture. *)
      ignore (ok [ "done"; "simple-pi" ]);
      let archive = Filename.concat home ".monty/runs/simplified/archive/simple-pi" in
      let assert_pinned directory = match Agent_profile.load_snapshot directory with
        | Ok profile when profile = simplified -> ()
        | _ -> failwith "archive/reopen changed the pinned simplifier" in
      assert_pinned archive;
      let osascript = Filename.concat root "fake-bin/osascript" in
      Shell.write_file osascript "#!/bin/sh\nexit 0\n";
      Shell.chmod_executable osascript;
      ignore (ok [ "resume"; "--archived"; "simple-pi"; "--harness"; "pi" ]);
      assert_pinned (worker_dir "simple-pi");
      let script = Yojson.Safe.Util.(Yojson.Safe.from_file (job_file "simple-pi")
        |> member "launch_script" |> to_string) in
      require_contains "resume routes skill installation through server" (read_file script)
        (Shell.quote executable ^ " terminal --home "))

let test_standalone_background_harnesses () =
  with_temp_root "background-harnesses" (fun root ->
      let home, log, env = setup_environment root in
      let bundled = Option.value ~default:(Sys.getcwd ()) (Sys.getenv_opt "DUNE_SOURCEROOT") in
      let simplified = match Agent_profile.find ~home:bundled "simplified" with
        | Ok profile -> profile | Error message -> failwith message in
      install_profile home simplified;
      let env = replace_env env
          [ "MONTY_JOB_ID", "parent-must-not-leak";
            "MONTY_WORKSPACE_3", "parent-workspace-must-not-leak";
            "MONTY_PI_COMMAND", "pi --test-argument 'two words'";
            "MONTY_CODEX_COMMAND", "codex --test-argument 'two words'" ] in
      let repos = List.map (Filename.concat root) [ "backend"; "frontend" ] in
      List.iter (fun repo -> init_git_repo repo; add_project ~root ~home ~env repo) repos;
      install_repo_scoped_wt ~root ~log;
      let context = Filename.concat root "context.md" in
      Shell.write_file context "# Background feature requirements\n";
      let manifest = Filename.concat home ".monty/runs/background/jobs.json" in
      let worker_dir id = Filename.concat home (".monty/runs/background/workers/" ^ id) in
      let job_file id = Filename.concat (worker_dir id) "job.json" in
      let jobs = [ "pi-simple", "simplified"; "pi-review", "reviewed";
                   "pi-solo", "solo"; "codex-solo", "solo";
                   "pi-error", "solo"; "missing", "solo" ] in
      let job (id, profile) = `Assoc
          [ "id", `String id; "title", `String id; "context", `String context;
            "agent_profile", `String profile;
            "workspaces", `List (List.map (fun repo -> `Assoc
                [ "repo", `String repo; "branch", `String ("cto/" ^ id) ]) repos) ] in
      write_manifest manifest (List.map job jobs);
      let release = Filename.concat root "release" in
      let fake =
        "#!/bin/sh\nset -eu\nroot=" ^ Shell.quote root ^ "\n" ^ {|output=
tools=
args=$*
while [ "$#" -gt 0 ]; do
  case "$1" in
    --output-last-message) output=$2; shift 2 ;;
    --tools) tools=$2; shift 2 ;;
    --test-argument) test "$2" = 'two words'; shift 2 ;;
    -C) cd "$2"; shift 2 ;;
    *) shift ;;
  esac
done
prompt=$(cat)
case "$prompt" in *'Background feature requirements'*) ;; *) exit 81 ;; esac
test "$MONTY_JOB_ID" != parent-must-not-leak
test "$MONTY_JOB_WORKTREE" = "$(pwd -P)"
test -f "$MONTY_JOB_FILE"
test -d "$MONTY_WORKSPACE_2"
test -z "${MONTY_WORKSPACE_3:-}"
case "$prompt" in
  *'Independently review Monty worker'*)
    case "$prompt" in *'Review correctness.'*) phase=correctness ;; *) phase=quality ;; esac
    test "$tools" = read,grep,find,ls
    case "$prompt" in *'HANDOFF-correctness'*|*'HANDOFF-quality'*) exit 82 ;; esac
    touch "$root/$MONTY_JOB_ID-$phase"
    count=0
    while [ ! -f "$root/$MONTY_JOB_ID-correctness" ] || [ ! -f "$root/$MONTY_JOB_ID-quality" ]; do
      count=$((count + 1)); [ "$count" -lt 500 ] || exit 83
      sleep 0.01
    done
    ;;
  *'Finalize Monty worker'*)
    phase=final
    case "$prompt" in *'HANDOFF-implementation'*) ;; *) exit 84 ;; esac
    if [ "$MONTY_JOB_ID" = pi-review ]; then
      case "$prompt" in *'HANDOFF-correctness'*'HANDOFF-quality'*) ;; *) exit 85 ;; esac
    fi
    test -f "$MONTY_WORKSPACE_2/feature.txt"
    printf 'simplified\n' > "$MONTY_WORKSPACE_2/feature.txt"
    ;;
  *)
    phase=implementation
    touch "$root/$MONTY_JOB_ID-started"
    if [ "${MONTY_TEST_BLOCK_RUN:-}" = yes ]; then
      count=0
      while [ ! -f "$root/release" ]; do
        count=$((count + 1)); [ "$count" -lt 1000 ] || exit 86
        sleep 0.01
      done
    fi
    printf 'implemented\n' > "$MONTY_WORKSPACE_2/feature.txt"
    ;;
esac
printf '%s %s %s\n' "$MONTY_JOB_ID" "$phase" "$args" >> "$root/phases.log"
if [ -n "$output" ]; then
  test -f "$CODEX_HOME/skills/monty/SKILL.md"
  printf 'HANDOFF-%s\n' "$phase" > "$output"
  printf '%s\n' '{"type":"turn.completed"}'
else
  test -f "$PI_CODING_AGENT_DIR/skills/monty/SKILL.md"
  test -f "$PI_CODING_AGENT_DIR/skills/monty/references/workflows.md"
  case "$args" in *'--print --mode json --no-session --tools '*) ;; *) exit 87 ;; esac
  if [ "$phase" != correctness ] && [ "$phase" != quality ]; then
    test "$tools" = read,bash,edit,write,grep,find,ls
  fi
  reason=stop
  if [ "${MONTY_TEST_FAIL_PI:-}" = yes ]; then reason=error; fi
  printf '{"type":"message_end","message":{"role":"assistant","stopReason":"%s","content":[{"type":"text","text":"HANDOFF-%s"}]}}\n' "$reason" "$phase"
  printf '%s\n' '{"type":"agent_end","messages":[]}'
fi
|}
      in
      List.iter (fun harness ->
          let path = Filename.concat root ("fake-bin/" ^ harness) in
          Shell.write_file path fake; Shell.chmod_executable path) [ "pi"; "codex" ];
      let cli ?(env = env) args = run ~root ~env 19840 (args @ [ "--home"; home ]) in
      let ok ?env args = let result = cli ?env args in require_code 0 result;
        Yojson.Safe.from_string result.stdout in
      ignore (ok [ "headless"; "prepare-many"; "--manifest"; manifest; "--harness"; "pi" ]);
      let missing_before = read_file (job_file "missing") in
      require_code 1 (cli [ "headless"; "run"; "missing"; "--harness"; "pi";
                           "--pi-command"; "missing-pi-executable" ]);
      require_code 1 (cli [ "headless"; "run"; "missing"; "--harness"; "pi";
                           "--fork"; "parent-session" ]);
      if read_file (job_file "missing") <> missing_before
         || Sys.file_exists (Filename.concat (worker_dir "missing") "artifacts/headless") then
        failwith "missing Pi executable mutated the prepared worker";
      let wait_until label predicate =
        let rec loop remaining =
          if predicate () then ()
          else if remaining = 0 then failwith ("timed out: " ^ label)
          else (Unix.sleepf 0.02; loop (remaining - 1)) in
        loop 750
      in
      let started = ref [] in
      let track json =
        if json_string "schema" json <> Headless.started_schema
           || json_string "run_status" json <> "started" then
          failwith "background launch did not return a startup receipt";
        started := json :: !started;
        json
      in
      let finished json =
        let path=Filename.concat home (".monty/conversation-commands/" ^ json_string "command" json ^ ".json") in
        if not(Sys.file_exists path) then false else
        List.mem (json_string "status" (Yojson.Safe.from_file path)) ["finished";"failed";"outcome_unknown"]
      in
      Fun.protect ~finally:(fun () ->
          Shell.write_file release "release\n";
          List.iter (fun json -> wait_until "background cleanup" (fun () -> finished json)) !started)
        (fun () ->
          List.iteri (fun index (harness, id) ->
              let blocked = replace_env env [ "MONTY_TEST_BLOCK_RUN", "yes" ] in
              let program = Filename.concat root "launch" in
              Shell.write_file program ("#!/bin/sh\ncd " ^ Shell.quote root
                ^ "\nexec " ^ Shell.quote executable ^ " \"$@\"\n");
              Shell.chmod_executable program;
              let caller = spawn ~program ~new_session:true ~root ~env:blocked (19841 + index)
                  [ "headless"; "run"; id; "--harness"; harness; "--home"; "home";
                    "--" ^ harness ^ "-command";
                    "'./fake-bin/" ^ harness ^ "' --test-argument 'two words'";
                    "--wt-command"; "./fake-bin/wt" ] in
              let result = await caller in
              require_code 0 result;
              let receipt = track (Yojson.Safe.from_string result.stdout) in
              wait_for_path (Filename.concat root (id ^ "-started"));
              if Sys.file_exists (json_string "handoff" receipt) then
                failwith "background command waited for the run result";
              (* Simulate closing the caller's terminal/session. *)
              (try Unix.kill (-caller.pid) Sys.sighup with
               | Unix.Unix_error (Unix.ESRCH, _, _) -> ());
              let busy = cli [ "headless"; "resume"; id; "--harness"; harness ] in
              require_code 1 busy;
              require_contains "detached worker lock" busy.stderr "busy";
              let busy_done = cli [ "done"; id; "--force" ] in
              require_code 1 busy_done;
              require_contains "detached completion guard" busy_done.stderr "busy")
            [ "pi", "pi-simple"; "codex", "codex-solo" ];
          (* A mixed batch fails full preflight before claiming any member. *)
          write_manifest manifest (List.map job [ "pi-solo", "solo"; "pi-simple", "simplified" ]);
          require_code 1 (cli [ "headless"; "run-many"; "--manifest"; manifest; "--harness"; "pi" ]);
          if job_status (job_file "pi-solo") <> "prepared" then failwith "batch preflight partially claimed workers";
          write_manifest manifest (List.map job [ "pi-solo", "solo"; "pi-review", "reviewed" ]);
          let batch = ok [ "headless"; "run-many"; "--manifest"; manifest; "--harness"; "pi" ] in
          Yojson.Safe.Util.(batch |> member "results" |> to_list)
          |> List.iter (fun result -> ignore (track (Yojson.Safe.Util.member "result" result)));
          let error_env = replace_env env [ "MONTY_TEST_FAIL_PI", "yes" ] in
          let error = track (ok ~env:error_env [ "headless"; "run"; "pi-error"; "--harness"; "pi" ]) in
          Shell.write_file release "release\n";
          List.iter (fun json -> wait_until "durable run result" (fun () -> finished json)) !started;
          List.iter (fun receipt ->
              let handoff = Yojson.Safe.from_file (json_string "handoff" receipt) in
              let id = json_string "worker_id" receipt in
              let expected = if id = "pi-error" then "failed" else "ready-for-review" in
              if json_string "outcome" handoff <> expected then failwith "wrong background outcome";
              if job_status (job_file id) <> "launch-requested" then failwith "background run closed worker";
              let state = Yojson.Safe.from_file (job_file id) in
              List.iter (fun key -> if Yojson.Safe.Util.member key state <> `Null then
                    failwith ("runtime state persisted in job.json: " ^ key))
                [ "pid"; "backend"; "runtime_status"; "codex_session_id"; "pi_run_id" ];
              if Sys.file_exists (Filename.concat (worker_dir id) "codex-session-id") then
                failwith "headless run claimed an interactive session") !started;
          require_contains "JSON exit-zero errors become failures"
            (read_file (json_string "handoff" error)) "stopReason";
          ignore (ok [ "headless"; "resume"; "pi-error"; "--harness"; "pi"; "--wait" ]);
          require_code 1 (cli [ "headless"; "run"; "pi-error"; "--harness"; "pi" ]);
          let phase_log = Filename.concat root "phases.log" in
          List.iter (fun (id, count) ->
              if count_lines_containing phase_log (id ^ " ") <> count then
                failwith ("wrong profile phase count for " ^ id))
            [ "pi-simple", 2; "pi-review", 4; "pi-solo", 1; "codex-solo", 1 ];
          let pending = ok [ "handoff"; "pending"; "--format"; "json" ] in
          if Yojson.Safe.Util.(pending |> member "pending" |> to_list |> List.length) <> 6 then
            failwith "standalone handoffs were not durably discoverable";
          List.iter (fun task -> if json_string "status" task <> "open" then
              failwith "headless run completed a local task") (local_tasks_json home);
          if string_contains (read_file log) "osascript" then failwith "background run opened Ghostty"))

let test_codex_headless_uses_effective_settings_without_ghostty () =
  with_temp_root "headless-codex" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      let manifest_one =
        Filename.concat home ".monty/runs/run-codex-one/jobs.json"
      in
      let manifest_many =
        Filename.concat home ".monty/runs/run-codex-many/jobs.json"
      in
      let manifest_failure =
        Filename.concat home ".monty/runs/run-codex-failure/jobs.json"
      in
      init_git_repo repo;
      add_project ~root ~home ~env repo;
      install_create_wt ~root ~log;
      install_codex_exec ~root ~log;
      Shell.write_file context
        "# Codex headless task\n\nImplement and validate the requested change.\n";
      let set_harness =
        run ~root ~env 1960
          [ "settings"; "set"; "harness"; "codex"; "--home"; home ]
      in
      require_code 0 set_harness;
      write_manifest manifest_one
        [ manifest_job ~id:"codex-one" ~branch:"cto/codex-one"
            ~title:"Codex one" ~repo ~context () ];
      let dry =
        run ~root ~env 1961
          [ "headless"; "prepare-many"; "--manifest"; manifest_one;
            "--home"; home; "--dry-run" ]
      in
      require_code 0 dry;
      let dry_json = Yojson.Safe.from_string dry.stdout in
      if Yojson.Safe.Util.(dry_json |> member "harness" |> to_string) <> "codex"
      then failwith "headless dry-run ignored the persisted Codex harness";
      if Yojson.Safe.Util.(dry_json |> member "codex_yolo" |> to_bool) then
        failwith "headless dry-run unexpectedly enabled Codex YOLO";
      let prepared_one =
        run ~root ~env 1962
          [ "headless"; "prepare-many"; "--manifest"; manifest_one;
            "--home"; home ]
      in
      require_code 0 prepared_one;
      if count_lines_containing log "/codex exec" <> 0 then
        failwith "headless preparation executed Codex";
      let job_file run id =
        Filename.concat home
          (Printf.sprintf ".monty/runs/%s/workers/%s/job.json" run id)
      in
      let first_run =
        run ~root ~env 1964
          [ "headless"; "run"; "--wait"; "codex-one"; "--home"; home ]
      in
      require_code 0 first_run;
      let first_json = Yojson.Safe.from_string first_run.stdout in
      if
        Yojson.Safe.Util.(first_json |> member "schema" |> to_string)
        <> Headless.codex_run_schema
      then failwith "Codex headless run returned the wrong schema";
      let first_artifacts =
        Yojson.Safe.Util.(first_json |> member "artifact_dir" |> to_string)
      in
      let first_handoff =
        Yojson.Safe.Util.(first_json |> member "handoff" |> to_string)
      in
      if not (Sys.file_exists first_handoff) then
        failwith "successful Codex run returned before its durable handoff existed";
      if
        Yojson.Safe.Util.(
          Yojson.Safe.from_file first_handoff |> member "outcome" |> to_string)
        <> "ready-for-review"
      then failwith "successful Codex run wrote the wrong handoff outcome";
      List.iter
        (fun relative ->
          let path = Filename.concat first_artifacts relative in
          if not (Sys.file_exists path) then
            failf "Codex headless run did not create %s" path)
        [ "implementation.md"; "reviews/correctness.md";
          "reviews/quality.md"; "final.md";
          "events/implementation.jsonl"; "logs/implementation.log" ];
      let implementation_prompt =
        Shell.read_file
          (Filename.concat first_artifacts "prompts/implementation.md")
      in
      require_contains "Codex prompt context" implementation_prompt
        "Implement and validate the requested change.";
      require_contains "Codex prompt instructions" implementation_prompt
        "Monty instructions";
      require_contains "Codex direct memory handling" implementation_prompt
        "Monty will append the captured handoff";
      if string_contains implementation_prompt
           "Append important implementation discoveries"
      then failwith "Codex direct prompt asked the agent to write worker memory";
      let memory =
        Shell.read_file
          (Filename.concat
             (Filename.dirname (job_file "run-codex-one" "codex-one"))
             "memory.md")
      in
      require_contains "Codex implementation memory" memory
        "fake Codex implementation handoff";
      require_contains "Codex final memory" memory "fake Codex final handoff";
      if count_lines_containing log "/codex exec" <> 4 then
        failwith "single Codex headless chain did not run four Codex phases";
      if count_lines_containing log "trust_level" <> 4 then
        failwith "Codex headless phases did not use command-local trust";
      if count_lines_containing log "--sandbox workspace-write" <> 2 then
        failwith "Codex writers did not use workspace-write";
      if count_lines_containing log "--sandbox read-only" <> 2 then
        failwith "Codex reviewers did not use read-only";
      if count_lines_containing log "--add-dir" <> 0 then
        failwith "Codex headless phases received write access outside the worktree";
      if
        count_lines_containing log
          "--dangerously-bypass-approvals-and-sandbox"
        <> 0
      then failwith "Codex YOLO was used while disabled";
      if job_status (job_file "run-codex-one" "codex-one") <> "launch-requested"
      then failwith "Codex run did not preserve the launch-requested lifecycle";
      let first_job_json =
        Yojson.Safe.from_file (job_file "run-codex-one" "codex-one")
      in
      List.iter
        (fun forbidden ->
          if Yojson.Safe.Util.member forbidden first_job_json <> `Null then
            failf "run handoff persisted forbidden job.json field %s" forbidden)
        [ "backend"; "pi_run_id"; "codex_session_id"; "runtime_status";
          "handoff"; "pending_result" ];
      let ordinary_resume =
        run ~root ~env 19641
          [ "resume"; "codex-one"; "--terminal"; "dry-run"; "--home";
            home ]
      in
      require_code 0 ordinary_resume;
      let osascript = Filename.concat root "fake-bin/osascript" in
      Shell.write_file osascript "#!/bin/sh\nexit 0\n";
      Shell.chmod_executable osascript;
      let interactive_resume =
        run ~root ~env 19642
          [ "resume"; "codex-one"; "--terminal"; "ghostty"; "--harness";
            "pi"; "--home"; home ]
      in
      require_code 0 interactive_resume;
      let recorded_script =
        Yojson.Safe.Util.(
          Yojson.Safe.from_file (job_file "run-codex-one" "codex-one")
          |> member "launch_script" |> to_string)
      in
      if not (Sys.file_exists recorded_script) then
        failwith "interactive resume did not write its recorded Pi launch script";
      let duplicate_run =
        run ~root ~env 1965
          [ "headless"; "run"; "--wait"; "codex-one"; "--home"; home ]
      in
      if duplicate_run.code = 0 then
        failwith "Codex headless run replayed a launch-requested worker";
      require_contains "Codex replay guard" duplicate_run.stderr
        "headless resume";
      let enable_yolo =
        run ~root ~env 1966
          [ "settings"; "set"; "codex-yolo"; "true"; "--home"; home ]
      in
      require_code 0 enable_yolo;
      write_manifest manifest_many
        [ manifest_job ~id:"codex-two" ~branch:"cto/codex-two"
            ~title:"Codex two" ~repo ~context ();
          manifest_job ~id:"codex-three" ~branch:"cto/codex-three"
            ~title:"Codex three" ~repo ~context () ];
      let prepared_many =
        run ~root ~env 1967
          [ "headless"; "prepare-many"; "--manifest"; manifest_many;
            "--home"; home ]
      in
      require_code 0 prepared_many;
      let many =
        run ~root ~env 1968
          [ "headless"; "run-many"; "--wait"; "--manifest"; manifest_many;
            "--home"; home ]
      in
      require_code 0 many;
      let many_json = Yojson.Safe.from_string many.stdout in
      if
        Yojson.Safe.Util.(many_json |> member "schema" |> to_string)
        <> Headless.codex_run_many_schema
      then failwith "Codex run-many returned the wrong schema";
      if not Yojson.Safe.Util.(many_json |> member "success" |> to_bool) then
        failwith "Codex run-many did not report success";
      if
        Yojson.Safe.Util.(many_json |> member "results" |> to_list |> List.length)
        <> 2
      then failwith "Codex run-many did not return both workers";
      List.iter
        (fun id ->
          if job_status (job_file "run-codex-many" id) <> "launch-requested"
          then failf "Codex run-many did not claim %s" id)
        [ "codex-two"; "codex-three" ];
      let successor =
        run ~root ~env 1969
          [ "headless"; "resume"; "--wait"; "codex-one"; "--home"; home ]
      in
      require_code 0 successor;
      if
        Yojson.Safe.Util.(
          Yojson.Safe.from_string successor.stdout |> member "harness" |> to_string)
        <> "codex"
      then failwith "headless resume ignored the effective Codex harness";
      let codex_one_handoffs =
        Filename.concat home ".monty/handoffs/run-codex-one/codex-one"
        |> Sys.readdir |> Array.to_list
        |> List.filter (String.ends_with ~suffix:".json")
      in
      if List.length codex_one_handoffs <> 2 then
        failwith "explicit successor run did not publish a distinct durable handoff";
      write_manifest manifest_failure
        [ manifest_job ~id:"codex-failure" ~branch:"cto/codex-failure"
            ~title:"Codex failure" ~repo ~context () ];
      let prepared_failure =
        run ~root ~env 1970
          [ "headless"; "prepare-many"; "--manifest"; manifest_failure;
            "--home"; home ]
      in
      require_code 0 prepared_failure;
      let failure_env =
        replace_env env [ ("MONTY_TEST_CODEX_FAIL_PHASE", "implementation") ]
      in
      let failed =
        run ~root ~env:failure_env 1971
          [ "headless"; "run"; "--wait"; "codex-failure"; "--home"; home ]
      in
      if failed.code = 0 then failwith "failing Codex phase reported success";
      require_contains "Codex phase failure" failed.stderr
        "Codex implementation phase failed";
      if
        job_status (job_file "run-codex-failure" "codex-failure")
        <> "launch-requested"
      then failwith "failed Codex dispatch did not remain launch-requested";
      let failure_handoff_dir =
        Filename.concat home
          ".monty/handoffs/run-codex-failure/codex-failure"
      in
      let failure_handoffs =
        Sys.readdir failure_handoff_dir |> Array.to_list
        |> List.filter (String.ends_with ~suffix:".json")
      in
      if List.length failure_handoffs <> 1 then
        failwith "failed Codex run did not write exactly one durable handoff";
      let failure_handoff =
        Yojson.Safe.from_file
          (Filename.concat failure_handoff_dir (List.hd failure_handoffs))
      in
      if
        Yojson.Safe.Util.(failure_handoff |> member "outcome" |> to_string)
        <> "failed"
        || Yojson.Safe.Util.(failure_handoff |> member "last_phase" |> to_string)
           <> "implementation"
        || Yojson.Safe.Util.(failure_handoff |> member "error") = `Null
      then failwith "failed Codex handoff omitted outcome, phase, or error";
      let recovered =
        run ~root ~env 1972
          [ "headless"; "resume"; "--wait"; "codex-failure"; "--home"; home ]
      in
      require_code 0 recovered;
      if count_lines_containing log "/codex exec" <> 21 then
        failwith "Codex headless execution ran an unexpected number of phases";
      if
        count_lines_containing log
          "--dangerously-bypass-approvals-and-sandbox"
        <> 9
      then failwith "Codex headless execution did not honor persisted YOLO";
      if count_lines_containing log "--sandbox read-only" <> 10 then
        failwith "Codex YOLO changed reviewer permissions";
      if string_contains (read_file log) "osascript"
         || string_contains (read_file log) "/ghostty "
      then failwith "Codex headless execution opened Ghostty";
      let tasks = local_tasks_json home in
      if List.length tasks <> 4 then
        failf "expected four Codex headless tasks, got %d" (List.length tasks);
      if
        List.exists
          (fun task ->
            Yojson.Safe.Util.(task |> member "status" |> to_string) <> "open")
          tasks
      then failwith "Codex headless execution closed a task automatically")

let test_run_handoff_publish_delivery_follow_up_and_lifecycle_race () =
  with_temp_root "run-handoff" (fun root ->
      let home, _log, env = setup_environment root in
      let repo_one = Filename.concat root "repo-one" in
      let repo_two = Filename.concat root "repo-two" in
      let context = Filename.concat root "context.md" in
      init_git_repo repo_one;
      init_git_repo repo_two;
      add_project ~root ~home ~env repo_one;
      add_project ~root ~home ~env repo_two;
      Shell.write_file context "# Shared handoff task\n";
      Shell.write_file (Filename.concat repo_one "tracked.txt") "changed one\n";
      Shell.write_file (Filename.concat repo_one "untracked.txt") "new\n";
      Shell.write_file (Filename.concat repo_two "tracked.txt") "changed two\n";
      let create_worker ~run_id ~id =
        let run_dir = Filename.concat home (".monty/runs/" ^ run_id) in
        let worker_dir = Filename.concat run_dir ("workers/" ^ id) in
        Shell.ensure_dir worker_dir;
        Shell.write_file (Filename.concat worker_dir "memory.md") "# Durable memory\n";
        Shell.write_file (Filename.concat worker_dir "MONTY.md") "# Instructions\n";
        let workspace repo branch =
          `Assoc
            [ ("repo", `String repo);
              ("branch", `String branch);
              ("worktree", `String repo) ]
        in
        Yojson.Safe.to_file (Filename.concat worker_dir "job.json")
          (`Assoc
            [ ("id", `String id);
              ("title", `String "Reusable run handoff");
              ("repo", `String repo_one);
              ("branch", `String "cto/handoff-one");
              ("context", `String context);
              ("worker_dir", `String worker_dir);
              ("run_dir", `String run_dir);
              ("worktree_mode", `String "never");
              ("status", `String "active");
              ("last_known_worktree", `String repo_one);
              ( "workspaces",
                `List
                  [ workspace repo_one "cto/handoff-one";
                    workspace repo_two "cto/handoff-two" ] ) ]);
        (run_dir, worker_dir)
      in
      let _run_dir, worker_dir =
        create_worker ~run_id:"run-handoff" ~id:"handoff-worker"
      in
      let published =
        run ~root ~env 2200
          [ "handoff"; "publish"; "handoff-worker"; "--home"; home;
            "--run-id"; "interactive-1"; "--summary";
            "Added one canonical contract for interactive and headless runs.";
            "--check"; "dune runtest --force: passed"; "--fixed";
            "Rejected forged inbox paths"; "--risk";
            "Draft PR still requires approval"; "--artifact"; "memory.md" ]
      in
      require_code 0 published;
      require_contains "styled handoff" published.stdout "### 🛎 Monty run finished";
      require_contains "styled task" published.stdout "Reusable run handoff";
      if string_contains published.stdout "Notice:" then
        failwith "interactive publication printed a head-butler notice id";
      if job_status (Filename.concat worker_dir "job.json") <> "active" then
        failwith "interactive publication changed the task lifecycle";
      let canonical_dir =
        Filename.concat home
          ".monty/handoffs/run-handoff/handoff-worker"
      in
      let canonical = Filename.concat canonical_dir "interactive-1.json" in
      if not (Sys.file_exists canonical) then
        failwith "interactive publication did not write a canonical handoff";
      let handoff = Yojson.Safe.from_file canonical in
      if
        Yojson.Safe.Util.(handoff |> member "schema" |> to_string)
        <> Run_handoff.schema
      then failwith "canonical handoff used the wrong schema";
      if
        Yojson.Safe.Util.(handoff |> member "workspaces" |> to_list |> List.length)
        <> 2
      then failwith "multi-workspace handoff omitted a workspace";
      let changes = Yojson.Safe.Util.(handoff |> member "changes" |> to_list) in
      if List.length changes <> 2 then
        failwith "multi-workspace handoff omitted Git change statistics";
      let changed_files =
        changes
        |> List.map (fun change ->
               Yojson.Safe.Util.(change |> member "files" |> to_list)
               |> List.map Yojson.Safe.Util.to_string)
        |> List.flatten
      in
      List.iter
        (fun path ->
          if not (List.mem path changed_files) then
            failf "handoff Git statistics omitted %s" path)
        [ "tracked.txt"; "untracked.txt" ];
      let notice_id =
        Run_handoff.notice_id
          {
            worker_run_id = "run-handoff";
            worker_id = "handoff-worker";
            handoff_id = "interactive-1";
          }
      in
      let pending_markdown =
        run ~root ~env 2201
          [ "handoff"; "pending"; "--format"; "json"; "--home"; home ]
      in
      require_code 0 pending_markdown;
      if
        Yojson.Safe.Util.(
          Yojson.Safe.from_string pending_markdown.stdout |> member "pending"
          |> to_list)
        <> []
      then failwith "interactive publication was queued for the head butler";
      let direct_notice =
        Run_handoff.notice_path home notice_id |> Yojson.Safe.from_file
      in
      if Yojson.Safe.Util.(direct_notice |> member "acknowledged_at") = `Null then
        failwith "interactive delivery receipt was not suppressed at publication";
      let pending_plain =
        run ~root ~env 2202
          [ "handoff"; "show"; "handoff-worker"; "--run-id";
            "interactive-1"; "--format"; "plain"; "--home"; home ]
      in
      require_code 0 pending_plain;
      require_contains "plain rendering" pending_plain.stdout "Monty run finished";
      if string_contains pending_plain.stdout "###" then
        failwith "plain handoff rendering contained Markdown headings";
      let duplicate =
        run ~root ~env 2203
          [ "handoff"; "publish"; "handoff-worker"; "--home"; home;
            "--run-id"; "interactive-1"; "--summary";
            "This duplicate must not replace the canonical run." ]
      in
      require_code 0 duplicate;
      require_contains "idempotent publication retained canonical summary"
        duplicate.stdout "Added one canonical contract";
      let canonical_files =
        Sys.readdir canonical_dir |> Array.to_list
        |> List.filter (String.ends_with ~suffix:".json")
      in
      if canonical_files <> [ "interactive-1.json" ] then
        failwith "duplicate delivery created a second canonical run";
      let follow_up =
        run ~root ~env 2204
          [ "handoff"; "follow-up"; "handoff-worker"; "--home"; home;
            "--question"; "Which review finding was fixed?" ]
      in
      require_code 0 follow_up;
      let follow_json = Yojson.Safe.from_string follow_up.stdout in
      if
        Yojson.Safe.Util.(follow_json |> member "schema" |> to_string)
        <> Run_handoff.follow_up_schema
        || not Yojson.Safe.Util.(follow_json |> member "read_only" |> to_bool)
      then failwith "follow-up did not use the versioned read-only contract";
      if
        Yojson.Safe.Util.(follow_json |> member "workspaces" |> to_list |> List.length)
        <> 2
      then failwith "follow-up omitted a current workspace";
      let artifacts = Filename.concat worker_dir "artifacts" in
      Shell.ensure_dir artifacts;
      Unix.symlink "/etc" (Filename.concat artifacts "external");
      let artifact_publish =
        run ~root ~env 22041
          [ "handoff"; "publish"; "handoff-worker"; "--home"; home;
            "--run-id"; "artifact-symlink"; "--summary";
            "Recorded worker-relative supporting evidence."; "--artifact";
            "artifacts/external/passwd" ]
      in
      require_code 0 artifact_publish;
      let rejected_artifact_symlink =
        run ~root ~env 22042
          [ "handoff"; "follow-up"; "handoff-worker"; "--home"; home;
            "--run-id"; "artifact-symlink"; "--question";
            "Inspect the supporting evidence." ]
      in
      if rejected_artifact_symlink.code = 0 then
        failwith "follow-up traversed an intermediate artifact symlink";
      require_contains "intermediate artifact symlink rejection"
        rejected_artifact_symlink.stderr "hierarchy contains a symlink";
      Unix.unlink (Filename.concat artifacts "external");
      let no_pending =
        run ~root ~env 2207
          [ "handoff"; "pending"; "--format"; "json"; "--home"; home ]
      in
      require_code 0 no_pending;
      if
        Yojson.Safe.Util.(
          Yojson.Safe.from_string no_pending.stdout |> member "pending" |> to_list)
        <> []
      then failwith "a direct interactive handoff entered the pending inbox";
      let fault_cases =
        [ ("fault-canonical", "run-handoff-after-canonical", false);
          ("fault-rendered", "run-handoff-after-rendered", true) ]
      in
      List.iteri
        (fun offset (handoff_id, checkpoint, expect_rendered) ->
          let interrupted =
            run ~root
              ~env:(replace_env env [ ("MONTY_FAULT_INJECT", checkpoint) ])
              (22071 + offset)
              [ "handoff"; "publish"; "handoff-worker"; "--home"; home;
                "--run-id"; handoff_id; "--summary";
                "Exercise publication crash reconciliation." ]
          in
          if interrupted.code = 0 then
            failf "publication checkpoint %s unexpectedly succeeded" checkpoint;
          require_contains "publication checkpoint" interrupted.stderr checkpoint;
          let reference : Run_handoff.reference =
            {
              worker_run_id = "run-handoff";
              worker_id = "handoff-worker";
              handoff_id;
            }
          in
          let json_path = Run_handoff.handoff_path home reference in
          let markdown_path = Run_handoff.rendered_path home reference in
          let notice = Run_handoff.notice_id reference in
          let inbox_path = Run_handoff.notice_path home notice in
          if not (Sys.file_exists json_path) then
            failwith "publication checkpoint lost the canonical handoff";
          if Sys.file_exists markdown_path <> expect_rendered then
            failwith "publication checkpoint stopped at the wrong boundary";
          if Sys.file_exists inbox_path then
            failwith "publication checkpoint unexpectedly wrote the inbox notice";
          let repaired =
            run ~root ~env (22073 + offset)
              [ "handoff"; "pending"; "--format"; "json"; "--home"; home ]
          in
          require_code 0 repaired;
          if not (Sys.file_exists markdown_path && Sys.file_exists inbox_path) then
            failwith "pending did not reconcile an interrupted publication";
          if
            Yojson.Safe.Util.(
              Yojson.Safe.from_string repaired.stdout |> member "pending"
              |> to_list)
            <> []
          then
            failwith
              "crash reconciliation turned an interactive handoff into a head-butler notification";
          let receipt = Yojson.Safe.from_file inbox_path in
          if Yojson.Safe.Util.(receipt |> member "acknowledged_at") = `Null then
            failwith "recovered interactive receipt was not suppressed")
        fault_cases;
      let resumed =
        run ~root ~env 2208
          [ "resume"; "handoff-worker"; "--terminal"; "dry-run";
            "--worktree"; "never"; "--home"; home ]
      in
      require_code 0 resumed;
      if job_status (Filename.concat worker_dir "job.json") <> "active" then
        failwith "resume after a finished run changed task status";
      let race_run_dir, race_worker_dir =
        create_worker ~run_id:"run-race" ~id:"race-worker"
      in
      let publish_child =
        spawn ~root ~env 2209
          [ "handoff"; "publish"; "race-worker"; "--home"; home;
            "--run-id"; "race-1"; "--summary";
            "Published while completion moved worker memory." ]
      in
      let done_child =
        spawn ~root ~env 2210
          [ "done"; "race-worker"; "--home"; home; "--wt-command"; "wt" ]
      in
      let publish_result = await publish_child in
      let done_result = await done_child in
      require_code 0 publish_result;
      require_code 0 done_result;
      if Sys.file_exists race_worker_dir then
        failwith "lifecycle race left the worker at its active path";
      let race_archive =
        Filename.concat race_run_dir "archive/race-worker/job.json"
      in
      if not (Sys.file_exists race_archive) || job_status race_archive <> "done" then
        failwith "lifecycle race corrupted completion state";
      let shown_after_archive =
        run ~root ~env 2211
          [ "handoff"; "show"; "race-worker"; "--format"; "json";
            "--home"; home ]
      in
      require_code 0 shown_after_archive;
      if
        Yojson.Safe.Util.(
          Yojson.Safe.from_string shown_after_archive.stdout |> member "id"
          |> to_string)
        <> "race-1"
      then failwith "archive/reopen-safe canonical handoff could not be recovered";
      let second =
        run ~root ~env 2212
          [ "handoff"; "publish"; "handoff-worker"; "--home"; home;
            "--run-id"; "interactive-2"; "--summary";
            "Created a second pending result for forgery validation." ]
      in
      require_code 0 second;
      let second_reference : Run_handoff.reference =
        {
          worker_run_id = "run-handoff";
          worker_id = "handoff-worker";
          handoff_id = "interactive-2";
        }
      in
      let forged_notice =
        Run_handoff.notice_path home (Run_handoff.notice_id second_reference)
      in
      let original_notice =
        match Yojson.Safe.from_file forged_notice with
        | `Assoc fields ->
            `Assoc
              (("acknowledged_at", `Null)
              :: List.remove_assoc "acknowledged_at" fields)
        | _ -> failwith "notice was not an object"
      in
      Yojson.Safe.to_file forged_notice original_notice;
      let second_canonical = Filename.concat canonical_dir "interactive-2.json" in
      let second_handoff_json = Yojson.Safe.from_file second_canonical in
      let copied_notice =
        Filename.concat (Filename.dirname forged_notice) "copied-notice.json"
      in
      Yojson.Safe.to_file copied_notice original_notice;
      let rejected_copied_notice =
        run ~root ~env 22121 [ "handoff"; "pending"; "--home"; home ]
      in
      if rejected_copied_notice.code = 0 then
        failwith "pending accepted a notice copied under a different filename";
      require_contains "notice filename authentication"
        rejected_copied_notice.stderr "filename does not match its canonical id";
      Unix.unlink copied_notice;
      let forged =
        match original_notice with
        | `Assoc fields ->
            let handoff =
              match List.assoc "handoff" fields with
              | `Assoc handoff_fields ->
                  `Assoc
                    (("path", `String "/tmp/forged-handoff.json")
                    :: List.remove_assoc "path" handoff_fields)
              | _ -> failwith "notice handoff was not an object"
            in
            `Assoc (("handoff", handoff) :: List.remove_assoc "handoff" fields)
        | _ -> failwith "notice was not an object"
      in
      Yojson.Safe.to_file forged_notice forged;
      let rejected_forgery =
        run ~root ~env 2213 [ "handoff"; "pending"; "--home"; home ]
      in
      if rejected_forgery.code = 0 then
        failwith "pending delivery accepted a forged canonical handoff path";
      require_contains "forged notice rejection" rejected_forgery.stderr
        "forged run-handoff notice path";
      Unix.unlink forged_notice;
      Unix.symlink canonical forged_notice;
      let rejected_symlink =
        run ~root ~env 2214 [ "handoff"; "pending"; "--home"; home ]
      in
      if rejected_symlink.code = 0 then
        failwith "pending delivery followed a symlinked notice";
      require_contains "symlinked notice rejection" rejected_symlink.stderr
        "symlink";
      Unix.unlink forged_notice;
      Yojson.Safe.to_file forged_notice original_notice;
      Unix.unlink second_canonical;
      let rejected_missing =
        run ~root ~env 2215 [ "handoff"; "pending"; "--home"; home ]
      in
      if rejected_missing.code = 0 then
        failwith "pending delivery accepted a missing canonical handoff";
      require_contains "missing handoff rejection" rejected_missing.stderr
        "run-handoff file is missing";
      Yojson.Safe.to_file second_canonical second_handoff_json;
      Shell.write_file second_canonical "{malformed\n";
      let rejected_malformed =
        run ~root ~env 2216 [ "handoff"; "pending"; "--home"; home ]
      in
      if rejected_malformed.code = 0 then
        failwith "pending delivery accepted a malformed canonical handoff";
      require_contains "malformed handoff rejection" rejected_malformed.stderr
        "invalid JSON";
      let forged_handoff =
        match second_handoff_json with
        | `Assoc fields ->
            let evidence =
              match List.assoc "evidence" fields with
              | `Assoc evidence_fields ->
                  `Assoc
                    (("rendered", `String "/tmp/forged-rendering.md")
                    :: List.remove_assoc "rendered" evidence_fields)
              | _ -> failwith "handoff evidence was not an object"
            in
            `Assoc (("evidence", evidence) :: List.remove_assoc "evidence" fields)
        | _ -> failwith "handoff was not an object"
      in
      Yojson.Safe.to_file second_canonical forged_handoff;
      let rejected_handoff_forgery =
        run ~root ~env 2217 [ "handoff"; "pending"; "--home"; home ]
      in
      if rejected_handoff_forgery.code = 0 then
        failwith "pending delivery accepted forged handoff evidence";
      require_contains "forged handoff rejection"
        rejected_handoff_forgery.stderr "forged rendered handoff path")

let test_settings_commands_and_effective_harness () =
  with_temp_root "settings" (fun root ->
      let home, _log, env = setup_environment root in
      let initial = run ~root ~env 1950 [ "settings"; "--home"; home ] in
      require_code 0 initial;
      require_contains "initial harness setting" initial.stdout "harness       pi";
      require_contains "initial Codex YOLO setting" initial.stdout
        "codex-yolo    false";
      require_contains "initial branch prefix setting" initial.stdout
        "branch-prefix monty";
      if string_contains initial.stdout "container-workers" then
        failwith "settings exposed a removed execution mode";
      let settings_path = Filename.concat home ".monty/settings.json" in
      let initial_settings = read_file settings_path in
      let removed_setting =
        run ~root ~env 19501
          [ "settings"; "set"; "container-workers"; "true"; "--home"; home ]
      in
      require_code 1 removed_setting;
      if read_file settings_path <> initial_settings then
        failwith "a removed setting mutated configuration";
      Shell.write_file settings_path {|{"codex_yolo":"invalid"}|};
      let invalid_doctor =
        run ~root ~env 19502
          [ "doctor"; "--home"; home; "--harness"; "pi";
            "--terminal"; "dry-run"; "--worktree"; "never" ]
      in
      require_code 1 invalid_doctor;
      require_contains "doctor still validates settings with CLI overrides"
        invalid_doctor.stderr "codex_yolo";
      Shell.write_file settings_path initial_settings;
      let set =
        run ~root ~env 1951
          [ "settings"; "set"; "harness"; "codex"; "--home"; home ]
      in
      require_code 0 set;
      require_contains "set harness result" set.stdout "harness = codex";
      if Yojson.Safe.Util.member "context_enabled"
           (Yojson.Safe.from_file settings_path) <> `Null then
        failwith "settings persisted the removed attention inbox option";
      let get =
        run ~root ~env 1952
          [ "settings"; "get"; "harness"; "--home"; home ]
      in
      require_code 0 get;
      require_contains "get harness result" get.stdout "codex";
      let set_yolo =
        run ~root ~env 19521
          [ "settings"; "set"; "codex-yolo"; "true"; "--home"; home ]
      in
      require_code 0 set_yolo;
      require_contains "set Codex YOLO result" set_yolo.stdout
        "codex-yolo = true";
      let get_yolo =
        run ~root ~env 19522
          [ "settings"; "get"; "codex-yolo"; "--home"; home ]
      in
      require_code 0 get_yolo;
      require_contains "get Codex YOLO result" get_yolo.stdout "true";
      let set_branch_prefix =
        run ~root ~env 19523
          [ "settings"; "set"; "branch-prefix"; "cto"; "--home"; home ]
      in
      require_code 0 set_branch_prefix;
      require_contains "set branch prefix result" set_branch_prefix.stdout
        "branch-prefix = cto";
      let get_branch_prefix =
        run ~root ~env 19524
          [ "settings"; "get"; "branch-prefix"; "--home"; home ]
      in
      require_code 0 get_branch_prefix;
      require_contains "get branch prefix result" get_branch_prefix.stdout "cto";
      if
        Yojson.Safe.Util.(
          Yojson.Safe.from_file settings_path |> member "harness" |> to_string)
        <> "codex"
      then failwith "settings command did not persist Codex harness";
      if
        not
          Yojson.Safe.Util.(
            Yojson.Safe.from_file settings_path |> member "codex_yolo"
            |> to_bool)
      then failwith "settings command did not persist Codex YOLO";
      if
        Yojson.Safe.Util.(
          Yojson.Safe.from_file settings_path |> member "branch_prefix"
          |> to_string)
        <> "cto"
      then failwith "settings command did not persist branch prefix";
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      let manifest = Filename.concat home ".monty/runs/settings/jobs.json" in
      add_project ~root ~home ~env repo;
      Shell.write_file context "# Settings harness task\n";
      write_manifest manifest
        [ manifest_job ~id:"settings-worker" ~branch:"cto/settings-worker"
            ~title:"Settings worker" ~repo ~context () ];
      let dry_run =
        run ~root ~env 19523
          [ "launch-many"; "--manifest"; manifest; "--home"; home;
            "--terminal"; "dry-run"; "--worktree"; "never" ]
      in
      require_code 0 dry_run;
      require_contains "persisted Codex harness command" dry_run.stdout
        "[dry-run] harness: codex";
      require_contains "persisted explicit unsafe request" dry_run.stdout
        "--codex-yolo";
      require_contains "persisted Codex service command" dry_run.stdout "terminal --home";
      let doctor =
        run ~root ~env 1953
          [ "doctor"; "--home"; home; "--terminal"; "dry-run";
            "--worktree"; "never" ]
      in
      require_code 0 doctor;
      require_contains "doctor uses persisted harness" doctor.stdout "codex";
      let override =
        run ~root ~env 1954
          [ "doctor"; "--home"; home; "--terminal"; "dry-run";
            "--worktree"; "never"; "--harness"; "pi" ]
      in
      require_code 0 override;
      require_contains "CLI harness override" override.stdout "pi")

let test_head_butler_continue_uses_native_sessions_without_monty_state () =
  with_temp_root "head-butler-continue" (fun root ->
      let home, log, env = setup_environment root in
      Shell.ensure_dir home;
      let picker =
        run ~root ~env 1955
          [ "continue"; "--home"; home; "--harness"; "pi" ]
      in
      require_code 1 picker;
      require_contains "managed Pi picker requires service" picker.stderr "Monty client service is not running";
      let last =
        run ~root ~env 1956
          [ "continue"; "--last"; "--home"; home; "--harness"; "pi" ]
      in
      require_code 1 last;
      require_contains "managed Pi last requires service" last.stderr "Monty client service is not running";
      let exact =
        run ~root ~env 1957
          [ "continue"; "design-notes"; "--home"; home; "--harness"; "pi" ]
      in
      require_code 1 exact;
      require_contains "managed Pi exact requires service" exact.stderr "Monty client service is not running";
      require_empty_log log;
      let codex =
        run ~root ~env 1958
          [ "continue"; "saved-chat"; "--home"; home; "--harness"; "codex";
            "--codex-command"; "codex"; "--codex-yolo" ]
      in
      require_code 1 codex;
      require_contains "managed continuation requires service" codex.stderr "Monty client service is not running";
      let conflict =
        run ~root ~env 1959
          [ "continue"; "saved-chat"; "--last"; "--home"; home ]
      in
      require_code 1 conflict;
      require_contains "continue selection conflict" conflict.stderr
        "either a session selector or --last";
      [ ".monty/tasks.local.json"; ".monty/runs"; ".monty/conversations" ]
      |> List.iter (fun relative ->
             if Sys.file_exists (Filename.concat home relative) then
               failwith
                 ("continuing a head-butler conversation created Monty state at "
                 ^ relative)))

let test_cli_parser_and_doctor_contracts () =
  with_temp_root "parser-doctor" (fun root ->
      let home, _log, env = setup_environment root in
      let open_result =
        run ~root ~env 1960
          [ "open"; "missing-worker"; "--home"; home; "--terminal"; "dry-run";
            "--worktree"; "never" ]
      in
      let resume_result =
        run ~root ~env 1961
          [ "resume"; "missing-worker"; "--home"; home; "--terminal";
            "dry-run"; "--worktree"; "never" ]
      in
      if open_result.code = 0 || resume_result.code = 0 then
        failwith "open/resume alias unexpectedly found a missing worker";
      require_contains "open alias dispatch" open_result.stderr "missing-worker";
      require_contains "resume dispatch" resume_result.stderr "missing-worker";
      let malformed = run ~root ~env 1962 [ "doctor"; "--not-a-real-flag" ] in
      require_code 124 malformed;
      require_contains "malformed flag diagnostic" malformed.stderr
        "--not-a-real-flag";
      List.iteri
        (fun index args ->
          let removed = run ~root ~env (19620 + index) args in
          require_code 124 removed;
          require_contains "removed command diagnostic" removed.stderr
            "unknown command")
        [ [ "container-image"; "register" ];
          [ "inspect"; "missing-worker" ];
          [ "stop"; "missing-worker" ];
          [ "context"; "on" ];
          [ "findings"; "--json" ] ];
      let invalid_defaults =
        replace_env env
          [ ("MONTY_TERMINAL", "not-a-backend");
            ("MONTY_TARGET", "not-a-target");
            ("MONTY_WORKTREE", "not-a-mode") ]
      in
      let fallback = run ~root ~env:invalid_defaults 1963 [ "doctor"; "--home"; home ] in
      require_code 0 fallback;
      require_contains "invalid enum fallback pass" fallback.stdout "PASS";
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      let run_dir = Filename.concat home ".monty/runs/run-doctor" in
      Shell.ensure_dir repo;
      Shell.write_file context "# Doctor states\n";
      [ ("prepared-worker", "prepared");
        ("failed-worker", "launch-failed");
        ("requested-worker", "launch-requested") ]
      |> List.iter (fun (id, status) ->
             let worker_dir = Filename.concat run_dir ("workers/" ^ id) in
             Shell.ensure_dir worker_dir;
             Yojson.Safe.to_file (Filename.concat worker_dir "job.json")
               (lifecycle_job_json ~id ~title:id ~status
                  ~branch:("cto/" ^ id) ~repo ~context ~worker_dir ~run_dir ()));
      let states =
        run ~root ~env 1964
          [ "doctor"; "--home"; home; "--terminal"; "dry-run"; "--worktree";
            "never" ]
      in
      require_code 0 states;
      require_contains "doctor literal pass" states.stdout "PASS";
      require_contains "doctor literal warn" states.stdout "WARN";
      require_contains "doctor prepared" states.stdout "prepared";
      require_contains "doctor launch failed" states.stdout "launch-failed";
      require_contains "doctor requested" states.stdout "launch-requested";
      require_line "doctor prepared recovery" states.stdout
        (Printf.sprintf "Recovery: monty resume 'prepared-worker' --home %s"
           (Shell.quote home));
      let corrupt_dir = Filename.concat run_dir "workers/corrupt-worker" in
      Shell.ensure_dir corrupt_dir;
      Shell.write_file (Filename.concat corrupt_dir "job.json") "{broken\n";
      let corrupt =
        run ~root ~env 1965
          [ "doctor"; "--home"; home; "--terminal"; "dry-run"; "--worktree";
            "never" ]
      in
      require_code 1 corrupt;
      require_contains "doctor corrupt fail level" corrupt.stdout "FAIL";
      require_contains "doctor corrupt state" corrupt.stdout "invalid JSON";
      remove_tree corrupt_dir;
      let duplicate_worker run_id =
        let duplicate_run = Filename.concat home (".monty/runs/" ^ run_id) in
        let duplicate_dir = Filename.concat duplicate_run "workers/duplicate" in
        Shell.ensure_dir duplicate_dir;
        Yojson.Safe.to_file (Filename.concat duplicate_dir "job.json")
          (lifecycle_job_json ~id:"duplicate" ~title:"Duplicate"
             ~branch:"cto/duplicate" ~repo ~context ~worker_dir:duplicate_dir
             ~run_dir:duplicate_run ())
      in
      duplicate_worker "doctor-duplicate-a";
      duplicate_worker "doctor-duplicate-b";
      let duplicate =
        run ~root ~env 1966
          [ "doctor"; "--home"; home; "--terminal"; "dry-run"; "--worktree";
            "never" ]
      in
      require_code 1 duplicate;
      require_contains "doctor duplicate identity fail" duplicate.stdout
        "duplicate worker id";
      remove_tree (Filename.concat home ".monty/runs/doctor-duplicate-a");
      remove_tree (Filename.concat home ".monty/runs/doctor-duplicate-b");
      let fail_env = replace_env env [ ("PATH", "/usr/bin:/bin") ] in
      let failed =
        run ~root ~env:fail_env 1967
          [ "doctor"; "--home"; home; "--terminal"; "dry-run"; "--worktree";
            "never"; "--pi-command"; "definitely-missing-pi" ]
      in
      require_code 1 failed;
      require_contains "doctor literal fail" failed.stdout "FAIL";
      require_contains "doctor exit diagnostic" failed.stderr
        "doctor found failing checks")

let test_json_shape_errors_are_actionable () =
  with_temp_root "json-shapes" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      let run_dir = Filename.concat home ".monty/runs/shapes" in
      let worker_dir = Filename.concat run_dir "workers/healthy" in
      add_project ~root ~home ~env repo;
      Shell.write_file context "# JSON shapes\n";
      Shell.ensure_dir worker_dir;
      Yojson.Safe.to_file (Filename.concat worker_dir "job.json")
        (lifecycle_job_json ~id:"healthy" ~title:"Healthy neighbor" ~branch:"cto/healthy"
           ~repo ~context ~worker_dir ~run_dir ());
      let corrupt = Filename.concat run_dir "workers/corrupt/job.json" in
      Shell.ensure_dir (Filename.dirname corrupt);
      List.iteri (fun index contents ->
          Shell.write_file corrupt contents;
          let listed = run ~root ~env (3100 + index)
              [ "list"; "--no-sync"; "--home"; home ] in
          require_code 0 listed;
          require_contains "healthy neighbor survives corrupt state" listed.stdout "Healthy neighbor";
          require_contains "corrupt path warning" listed.stderr corrupt;
          let doctor = run ~root ~env (3110 + index)
              [ "doctor"; "--home"; home; "--terminal"; "dry-run";
                "--worktree"; "never"; "--pi-command"; "/bin/sh -c true" ] in
          require_code 1 doctor;
          require_contains "corrupt state doctor" doctor.stdout "FAIL";
          require_contains "structural JSON diagnostic" doctor.stdout "invalid JSON structure";
          if read_file corrupt <> contents then failwith "corrupt worker was overwritten")
        [ "42"; "[]"; "null" ];
      remove_tree (Filename.dirname corrupt);
      let manifest = Filename.concat run_dir "jobs.json" in
      Shell.write_file manifest "{\"jobs\":[42]}";
      let rejected = run ~root ~env 3120
          [ "launch-many"; "--manifest"; manifest; "--home"; home;
            "--terminal"; "dry-run"; "--worktree"; "never" ] in
      require_code 1 rejected;
      require_contains "manifest shape error path" rejected.stderr manifest;
      List.iteri (fun index (name, contents, args) ->
          let path = Filename.concat home (".monty/" ^ name) in
          let previous = if Sys.file_exists path then Some (read_file path) else None in
          Shell.write_file path contents;
          let rejected = run ~root ~env (3130 + index) (args @ [ "--home"; home ]) in
          require_code 1 rejected;
          require_contains "registry shape error path" rejected.stderr path;
          if read_file path <> contents then failwith "malformed registry was overwritten";
          match previous with Some contents -> Shell.write_file path contents
          | None -> Unix.unlink path)
        [ ("tasks.local.json", "{\"tasks\":[42]}",
           [ "task"; "add"; "--project"; "repo"; "--title"; "Must not persist" ]);
          ("projects.json", "{\"projects\":[false]}",
           [ "projects"; "add"; "--repo"; repo ]);
          ("settings.json", "[]", [ "settings"; "set"; "harness"; "pi" ]) ];
      let profile = Filename.concat home "agent-profiles/solo/profile.json" in
      Shell.write_file profile "42";
      let rejected = run ~root ~env 3140
          [ "agent-profiles"; "show"; "solo"; "--home"; home ] in
      require_code 1 rejected;
      require_contains "profile shape diagnostic" rejected.stderr "invalid JSON structure";
      require_contains "profile shape source" rejected.stderr profile;
      let snapshot = Agent_profile.snapshot_path worker_dir in
      Shell.write_file snapshot "42";
      let rejected = run ~root ~env 3141
          [ "resume"; "healthy"; "--terminal"; "dry-run"; "--home"; home ] in
      require_code 1 rejected;
      require_contains "snapshot shape source" rejected.stderr snapshot;
      require_empty_log log)

let test_worktree_root_and_branch_are_verified () =
  with_temp_root "workspace-identity" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      let manifest = Filename.concat home ".monty/runs/identity/jobs.json" in
      init_git_repo repo;
      add_project ~root ~home ~env repo;
      install_repo_scoped_wt ~root ~log;
      Shell.write_file context "# Worktree identity\n";
      write_manifest manifest [ manifest_job ~id:"identity" ~branch:"cto/identity"
          ~agent_profile:"solo" ~title:"Identity" ~repo ~context () ];
      require_code 0 (run ~root ~env 3150
          [ "headless"; "prepare-many"; "--manifest"; manifest; "--home"; home ]);
      let job_file = Filename.concat home ".monty/runs/identity/workers/identity/job.json" in
      let worktree = Yojson.Safe.from_file job_file |> json_string "last_known_worktree" in
      let git command = match Process.run_quiet ~cwd:worktree command with
        | Ok () -> () | Error message -> failwith message in
      let rejected index =
        let before = read_file job_file in
        let ensure = run ~root ~env index
            [ "ensure-worktree"; "--repo"; repo; "--branch"; "cto/identity";
              "--wt-command"; "wt" ] in
        require_code 1 ensure;
        let done_result = run ~root ~env (index + 1)
            [ "done"; "identity"; "--force"; "--home"; home ] in
        require_code 1 done_result;
        if read_file job_file <> before then failf "wrong workspace changed lifecycle in case %d" index;
        if not (Sys.file_exists worktree) then failwith "wrong workspace was removed";
        if string_contains (read_file log) " db " then failwith "wt removal was attempted"
      in
      git "git checkout -qb wrong-branch";
      rejected 3151;
      git "git checkout -q --detach";
      rejected 3153;
      git "git checkout -q cto/identity";
      let nested = Filename.concat worktree "nested" in
      Shell.ensure_dir nested;
      let wt = Filename.concat root "fake-bin/wt" in
      let original = read_file wt in
      let original_job = read_file job_file in
      let rec replace_worktree = function
        | `String path when path = worktree -> `String nested
        | `Assoc fields -> `Assoc (List.map (fun (key, value) -> (key, replace_worktree value)) fields)
        | `List values -> `List (List.map replace_worktree values)
        | value -> value
      in
      Yojson.Safe.from_string original_job |> replace_worktree |> Yojson.Safe.to_file job_file;
      Shell.write_file wt (String.concat "\n"
          [ "#!/bin/sh"; "case \"$1\" in";
            "b) printf '%s\\n' " ^ Shell.quote nested ^ " ;;";
            "list) printf 'repo:\\n  cto/identity -> %s\\n' " ^ Shell.quote nested ^ " ;;";
            "*) exit 92 ;;"; "esac"; "" ]);
      rejected 3155;
      Shell.write_file job_file original_job;
      Shell.write_file wt original;
      Unix.rmdir nested;
      require_code 0 (run ~root ~env 3157
          [ "done"; "identity"; "--home"; home ]))

let test_live_codex_excludes_resume_and_completion () =
  with_temp_root "codex-worker-lock" (fun root ->
      let home, log, env = setup_environment root in
      let env = replace_env env [ ("MONTY_HARNESS", "codex") ] in
      let repo = Filename.concat root "repo" in
      let context = Filename.concat root "context.md" in
      let manifest = Filename.concat home ".monty/runs/lock/jobs.json" in
      init_git_repo repo;
      add_project ~root ~home ~env repo;
      install_repo_scoped_wt ~root ~log;
      install_codex_exec ~root ~log;
      Shell.write_file context "# Lock regression\n";
      write_manifest manifest
        (List.map (fun id -> manifest_job ~id ~branch:("cto/" ^ id)
             ~agent_profile:"solo" ~title:id ~repo ~context ())
           [ "blocked"; "independent" ]);
      require_code 0 (run ~root ~env 3000
          [ "headless"; "prepare-many"; "--manifest"; manifest; "--home"; home ]);
      let lock_dir = Filename.concat home ".monty/runs/lock/.worker-locks" in
      let lock_state () =
        if not (Sys.file_exists lock_dir) then []
        else Sys.readdir lock_dir |> Array.to_list |> List.sort String.compare
          |> List.map (fun name ->
                 let path = Filename.concat lock_dir name in
                 (name, (Unix.lstat path).st_ino, Shell.read_file path))
      in
      let locks_before = lock_state () in
      let missing = run ~root ~env 30001
          [ "headless"; "run"; "--wait"; "blocked"; "--home"; home;
            "--codex-command"; "definitely-missing-codex" ] in
      require_code 1 missing;
      require_contains "Codex dependency preflight" missing.stderr "definitely-missing-codex";
      if lock_state () <> locks_before then
        failwith "failed dependency preflight changed worker lock state";
      let worker = Filename.concat home ".monty/runs/lock/workers/blocked" in
      let job_file = Filename.concat worker "job.json" in
      let worktree = Yojson.Safe.from_file job_file |> json_string "last_known_worktree" in
      let started = Filename.concat root "codex-started" in
      let release = Filename.concat root "codex-release" in
      let finished = Filename.concat root "codex-finished" in
      let codex = Filename.concat root "fake-bin/codex" in
      let original = read_file codex in
      let block =
        String.concat "\n"
          [ "if [ \"$MONTY_TEST_BLOCK_CODEX\" = yes ]; then";
            "  trap " ^ Shell.quote ("touch " ^ Shell.quote finished) ^ " EXIT";
            "  echo $$ > " ^ Shell.quote started;
            "  count=0";
            "  while [ ! -f " ^ Shell.quote release ^ " ]; do";
            "    count=$((count + 1)); [ \"$count\" -lt 1000 ] || exit 89";
            "    sleep 0.01";
            "  done";
            "fi"; "" ]
      in
      Shell.write_file codex ("#!/bin/sh\n" ^ block ^ original);
      let blocked_env = replace_env env [ ("MONTY_TEST_BLOCK_CODEX", "yes") ] in
      let supervisor = spawn ~root ~env:blocked_env 3001
          [ "headless"; "run"; "--wait"; "blocked"; "--home"; home ] in
      let awaited = ref false in
      let wait_file path =
        let rec loop remaining =
          if Sys.file_exists path then ()
          else if remaining = 0 then failf "timed out waiting for %s" path
          else (Unix.sleepf 0.01; loop (remaining - 1))
        in loop 500
      in
      Fun.protect
        ~finally:(fun () ->
          Shell.write_file release "release\n";
          if not !awaited then ignore (await supervisor);
          if Sys.file_exists started then wait_file finished)
        (fun () ->
          wait_file started;
          let original_job = read_file job_file in
          let attempts = Sys.readdir (Filename.concat worker "artifacts/headless") in
          let rejected index args =
            let result = run ~root ~env index (args @ [ "--home"; home ]) in
            require_code 1 result;
            require_contains "worker busy" (result.stderr ^ result.stdout) "busy";
            if read_file job_file <> original_job then failwith "busy operation mutated job";
            if Sys.readdir (Filename.concat worker "artifacts/headless") <> attempts then
              failwith "busy operation created an attempt";
            if not (Sys.file_exists worktree) then failwith "busy worker was deleted"
          in
          rejected 3002 [ "headless"; "resume"; "--wait"; "blocked" ];
          rejected 30021 [ "headless"; "resume"; "--wait"; "blocked"; "--harness"; "pi" ];
          rejected 3003 [ "resume"; "blocked"; "--terminal"; "ghostty" ];
          rejected 3004 [ "done"; "blocked" ];
          rejected 3005 [ "done"; "blocked"; "--force" ];
          require_code 0 (run ~root ~env 3006
              [ "settings"; "set"; "branch-prefix"; "other"; "--home"; home ]);
          require_code 0 (run ~root ~env 3007
              [ "headless"; "run"; "--wait"; "independent"; "--home"; home ]);
          Unix.kill supervisor.pid Sys.sigkill;
          ignore (await supervisor);
          awaited := true;
          rejected 3008 [ "headless"; "resume"; "--wait"; "blocked" ];
          rejected 3009 [ "done"; "blocked"; "--force" ];
          Shell.write_file release "release\n";
          wait_file finished;
          let rec resume remaining =
            let result = run ~root ~env 3010
                [ "headless"; "resume"; "--wait"; "blocked"; "--home"; home ] in
            if result.code <> 0 && string_contains result.stderr "busy" && remaining > 0 then
              (Unix.sleepf 0.01; resume (remaining - 1))
            else require_code 0 result
          in
          resume 100;
          require_code 0 (run ~root ~env 3011
              [ "done"; "blocked"; "--home"; home ]);
          if Sys.file_exists worktree then failwith "finished worker worktree was retained";
          let archived = match Job_store.find ~home ~scope:Job_store.All "blocked" with
            | Ok record -> record | Error message -> failwith message in
          let archived_job = read_file archived.path in
          let task_file = Filename.concat home ".monty/tasks.local.json" in
          let archived_tasks = read_file task_file in
          let resume_args = [ "resume"; "--archived"; "blocked"; "--home"; home;
              "--terminal"; "ghostty"; "--harness"; "pi" ] in
          (match Worker_lock.with_lock ~home ~record:archived (fun () ->
               let rejected = run ~root ~env 3012 resume_args in
               require_code 1 rejected;
               require_contains "archive lock precedes reactivation" rejected.stderr "busy";
               if read_file archived.path <> archived_job
                  || read_file task_file <> archived_tasks
                  || Sys.file_exists worker then
                 failwith "busy archived resume mutated lifecycle state";
               Ok ()) with
          | Ok () -> () | Error message -> failwith message);
          let osascript = Filename.concat root "fake-bin/osascript" in
          Shell.write_file osascript "#!/bin/sh\nexit 0\n";
          Shell.chmod_executable osascript;
          require_code 0 (run ~root ~env 3013 resume_args);
          if not (Sys.file_exists worktree) || job_status job_file <> "launch-requested" then
            failwith "archived resume did not recover after the lock was released"))

let test_project_groups_crud_and_legacy_projects () =
  with_temp_root "project-groups" (fun root ->
      let home, log, env = setup_environment ~agent_profile:None root in
      let cli args = run ~root ~env 4000 ("projects" :: args) in
      let ok args = let result = cli args in require_code 0 result; result.stdout in
      ignore (ok [ "list" ]);
      if ok [ "groups"; "list" ] <> "GROUP\n" then failwith "new home has groups";
      require_code 1 (cli [ "list"; "--group"; "Work" ]);
      if Sys.file_exists (Filename.concat home ".monty") then
        failwith "read-only project/group listings created state";
      let repo = Filename.concat root "one/repo" in
      init_git_repo repo;
      let registry = Filename.concat home ".monty/projects.json" in
      Shell.ensure_dir (Filename.dirname registry);
      let legacy_project =
        `Assoc [ ("repo", `String repo);
                 ("sources", `List [ `Assoc [ ("kind", `String "github_issues");
                                              ("repo", `String "owner/repo");
                                              ("query", `String "is:open") ] ]) ]
      in
      Yojson.Safe.to_file registry (`Assoc [ ("projects", `List [ legacy_project ]) ]);
      let memory = Filename.concat home ".monty/projects/repo.md" in
      Shell.ensure_dir (Filename.dirname memory);
      Shell.write_file memory "# Important project memory\nKeep me.\n";
      require_code 0
        (run ~root ~env 4001 [ "task"; "add"; "--project"; "repo"; "--title"; "Keep task" ]);
      let run_dir = Filename.concat home ".monty/runs/groups" in
      let worker = Filename.concat run_dir "workers/keep" in
      let job_file = Filename.concat worker "job.json" in
      Shell.ensure_dir worker;
      Yojson.Safe.to_file job_file
        (job_json ~id:"keep" ~repo ~context:memory ~worker_dir:worker ~run_dir);
      let tasks_file = Filename.concat home ".monty/tasks.local.json" in
      let preserved = List.map (fun path -> (path, read_file path)) [ memory; tasks_file; job_file ] in
      let before = read_file registry in
      let stamp = Unix.stat registry in
      require_contains "legacy ungrouped listing" (ok [ "list" ]) "<ungrouped>";
      require_contains "legacy show" (ok [ "show"; "repo" ]) "Group: <ungrouped>";
      ignore (ok [ "groups"; "list" ]);
      if read_file registry <> before || (Unix.stat registry).Unix.st_mtime <> stamp.Unix.st_mtime then
        failwith "legacy listings rewrote the registry";
      List.iter (fun name -> ignore (ok [ "groups"; "add"; name ])) [ "Work"; "Private" ];
      if ok [ "groups"; "list" ] <> "GROUP\nPrivate\nWork\n" then
        failwith "empty groups were not persisted and sorted";
      let filtered_table group =
        ok [ "list"; "--group"; group ] |> String.split_on_char '\n' |> List.tl |> String.concat "\n"
      in
      let empty = filtered_table "Private" in
      if List.length (String.split_on_char '\n' (String.trim empty)) <> 1 then
        failwith "empty group listed projects";
      ignore (ok [ "set-group"; "repo"; "Work" ]);
      require_contains "assigned project filter" (ok [ "list"; "--group"; "Work" ]) repo;
      require_contains "assigned project show" (ok [ "show"; "repo" ]) "Group: Work";
      ignore (ok [ "set-group"; repo; "Private" ]);
      if filtered_table "Work" <> empty then failwith "reassignment left old membership";
      ignore (ok [ "set-group"; "repo" ]);
      if filtered_table "Private" <> empty then failwith "clear left membership";
      ignore (ok [ "set-group"; "repo"; "Work" ]);
      let second = Filename.concat root "two/repo" in
      init_git_repo second;
      ignore (ok [ "add"; "--repo"; second ]);
      require_contains "registration preserved group assignment"
        (ok [ "list"; "--group"; "Work" ]) repo;
      let members () = Yojson.Safe.Util.(Yojson.Safe.from_file registry |> member "projects" |> to_list) in
      let without_group = function `Assoc fields -> `Assoc (List.remove_assoc "group" fields) | _ -> assert false in
      let before_delete = List.map without_group (members ()) in
      let ids = List.map (json_string "id") (members ()) in
      if not (List.mem "repo" ids) || list_unique ids <> 2 then
        failwith "group edits or same-basename registration changed stable IDs";
      let first = List.find (fun p -> json_string "repo" p = repo) (members ()) in
      if Yojson.Safe.Util.member "sources" first <> Yojson.Safe.Util.member "sources" legacy_project then
        failwith "group mutations lost legacy sources";
      ignore (ok [ "set-group"; second; "Work" ]);
      ignore (ok [ "groups"; "delete"; "Work" ]);
      if List.map without_group (members ()) <> before_delete
         || List.exists (fun p -> Yojson.Safe.Util.member "group" p <> `Null) (members ()) then
        failwith "populated group deletion changed projects or left dangling memberships";
      require_code 1 (cli [ "list"; "--group"; "Work" ]);
      require_contains "all projects remain" (ok [ "list" ]) second;
      let stamp = Unix.stat registry in
      let before = read_file registry in
      ignore (ok [ "groups"; "list" ]);
      ignore (ok [ "list"; "--group"; "Private" ]);
      ignore (ok [ "show"; repo ]);
      if read_file registry <> before || (Unix.stat registry).Unix.st_ino <> stamp.Unix.st_ino then
        failwith "group-aware reads replaced registry";
      ignore (ok [ "groups"; "delete"; "Private" ]);
      if ok [ "groups"; "list" ] <> "GROUP\n" then failwith "empty group deletion failed";
      List.iter (fun (path, contents) ->
          if read_file path <> contents then failf "group commands changed %s" path) preserved;
      List.iter (fun repo ->
          match Process.run_quiet ~cwd:repo "git diff --exit-code HEAD" with
          | Ok () -> () | Error msg -> failwith msg) [ repo; second ];
      require_empty_log log)

let test_project_groups_invalid_input_and_atomic_failure () =
  with_temp_root "project-groups-errors" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      add_project ~root ~home ~env repo;
      let cli ?(env = env) args = run ~root ~env 4100 ("projects" :: args) in
      let ok args = require_code 0 (cli args) in
      ok [ "groups"; "add"; "Work" ];
      ok [ "groups"; "add"; "Private" ];
      ok [ "set-group"; "repo"; "Work" ];
      let registry = Filename.concat home ".monty/projects.json" in
      let previous = read_file registry in
      let reject args message =
        let result = cli args in
        require_code 1 result;
        require_contains "group validation error" result.stderr message;
        if read_file registry <> previous then failwith "invalid input changed registry"
      in
      List.iter (fun name ->
          reject [ "groups"; "add"; name ] "project group name";
          reject [ "groups"; "delete"; name ] "project group name";
          reject [ "set-group"; "repo"; name ] "project group name";
          reject [ "list"; "--group"; name ] "project group name")
        [ ""; " "; " Work"; "Work "; "."; ".."; "a/b"; "a\\b"; "a\nb"; "a\tb" ];
      reject [ "groups"; "add"; "Work" ] "already exists";
      reject [ "groups"; "delete"; "Missing" ] "monty projects groups list";
      reject [ "list"; "--group"; "Missing" ] "monty projects groups add";
      reject [ "list"; "--group"; "work" ] "no Monty project group";
      reject [ "set-group"; "repo"; "Missing" ] "no Monty project group";
      reject [ "set-group"; "missing-project"; "Work" ] "no Monty project";
      reject [ "set-group"; "missing-project" ] "no Monty project";
      List.iter (fun args ->
          let result = cli ~env:(replace_env env [ ("MONTY_FAULT_INJECT", "state-store-before-rename") ]) args in
          require_code 1 result;
          require_contains "group atomic fault" result.stderr "fault injected before atomic state rename";
          if read_file registry <> previous then failwith "atomic group failure partially wrote state";
          if tree_has home (fun path -> string_contains path "monty-tmp") then
            failwith "atomic group failure left a temporary file")
        [ [ "groups"; "add"; "New" ]; [ "groups"; "delete"; "Work" ];
          [ "set-group"; "repo"; "Private" ]; [ "set-group"; "repo" ] ];
      let json = Yojson.Safe.from_string previous in
      let projects = Yojson.Safe.Util.(json |> member "projects" |> to_list) in
      let first = List.hd projects in
      let fields = Yojson.Safe.Util.to_assoc first in
      List.iter (fun (groups, projects) ->
          Yojson.Safe.to_file registry (`Assoc [ ("groups", groups); ("projects", `List projects) ]);
          let malformed = read_file registry in
          List.iter (fun args ->
              require_code 1 (cli args);
              if read_file registry <> malformed then failwith "malformed group registry was overwritten")
            [ [ "groups"; "list" ]; [ "groups"; "add"; "New" ];
              [ "groups"; "delete"; "Work" ]; [ "set-group"; "repo" ]; [ "list" ] ])
        [ (`String "Work", projects);
          (`List [ `Bool true ], projects);
          (`List [ `String "Work"; `String "Work" ], projects);
          (`List [ `String "../bad" ], projects);
          (`List [], projects);
          (`List [ `String "Work" ], [ `Assoc (("group", `Int 1) :: List.remove_assoc "group" fields) ]);
          (`List [ `String "Work" ], [ `Assoc (("group", `String "") :: List.remove_assoc "group" fields) ]);
          (`List [ `String "Work" ], [ first; first ]);
          (`List [ `String "Work" ],
           [ first; `Assoc (("repo", `String (Filename.concat root "other")) :: List.remove_assoc "repo" fields) ]) ];
      Shell.write_file registry previous;
      let second = Filename.concat root "two/repo" in
      add_project ~root ~home ~env second;
      let previous = read_file registry in
      let ambiguous = cli [ "set-group"; "repo"; "Private" ] in
      require_code 1 ambiguous;
      require_contains "ambiguous project reference" ambiguous.stderr "multiple Monty projects";
      if read_file registry <> previous then failwith "ambiguous project assignment changed state";
      require_empty_log log)

let test_project_groups_concurrent_mutations () =
  with_temp_root "project-groups-concurrent" (fun root ->
      let home, log, env = setup_environment root in
      let cli args = run ~root ~env 4200 ("projects" :: args) in
      require_code 0 (cli [ "groups"; "add"; "Work" ]);
      let repos = List.init 8 (fun index ->
          let name = Printf.sprintf "repo-%d" index in
          add_project ~root ~home ~env (Filename.concat root name);
          name) in
      let children = List.mapi (fun index repo ->
          let added = Filename.concat root (Printf.sprintf "new-%d" index) in
          Shell.ensure_dir added;
          [ spawn ~root ~env (4210 + index * 3) [ "projects"; "set-group"; repo; "Work" ];
            spawn ~root ~env (4211 + index * 3) [ "projects"; "groups"; "add"; Printf.sprintf "Extra-%d" index ];
            spawn ~root ~env (4212 + index * 3) [ "projects"; "add"; "--repo"; added ] ]) repos
        |> List.concat in
      List.map await children |> List.iter (require_code 0);
      let registry = Filename.concat home ".monty/projects.json" in
      let read () =
        let json = Yojson.Safe.from_file registry in
        Yojson.Safe.Util.(json |> member "groups" |> to_list |> List.map to_string,
                          json |> member "projects" |> to_list)
      in
      let groups, projects = read () in
      if List.length groups <> 9 || List.length projects <> 16 then
        failwith "concurrent group/project additions lost records";
      List.iter (fun repo ->
          let project = List.find (fun p -> json_string "id" p = repo) projects in
          if json_string "group" project <> "Work" then failwith "concurrent mutation lost assignment") repos;
      let results = List.init 8 (fun index ->
          spawn ~root ~env (4300 + index) [ "projects"; "groups"; "add"; "Duplicate" ])
        |> List.map await in
      if List.length (List.filter (fun result -> result.code = 0) results) <> 1 then
        failwith "concurrent duplicate group creation did not have exactly one winner";
      List.iter (fun result -> if result.code <> 0 then (
          require_code 1 result;
          require_contains "concurrent duplicate" result.stderr "already exists")) results;
      let results =
        (spawn ~root ~env 4400 [ "projects"; "groups"; "delete"; "Work" ]
         :: List.mapi (fun index repo ->
             spawn ~root ~env (4401 + index) [ "projects"; "set-group"; repo; "Work" ]) repos)
        |> List.map await in
      require_code 0 (List.hd results);
      List.iter (fun result -> if result.code <> 0 then (
          require_code 1 result;
          require_contains "assignment after concurrent delete" result.stderr "no Monty project group")) (List.tl results);
      let groups, projects = read () in
      if List.mem "Work" groups || List.length groups <> 9 || List.length projects <> 16
         || List.exists (fun p -> Yojson.Safe.Util.member "group" p <> `Null) projects then
        failwith "concurrent deletion/assignment left dangling references or lost unrelated records";
      require_empty_log log)

let test_mantle_profile_filters_project_and_task_lists () =
  with_temp_root "mantle-lists" (fun root ->
      let home, log, env = setup_environment root in
      let cli ?(env = env) args = run ~root ~env 4500 args in
      let ok ?env args = let result = cli ?env args in require_code 0 result; result.stdout in
      let absent text value =
        if string_contains text value then failf "unexpected %S in:\n%s" value text
      in
      List.iter (fun name ->
          ignore (ok [ "projects"; "groups"; "add"; name ])) [ "work"; "private"; "client-a"; "Work" ];
      List.iter (fun (name, group) ->
          let repo = Filename.concat root name in
          Shell.ensure_dir repo;
          ignore (ok [ "projects"; "add"; "--repo"; repo; "--github"; "owner/" ^ name ]);
          Option.iter (fun group -> ignore (ok [ "projects"; "set-group"; name; group ])) group)
        [ ("work-repo", Some "work"); ("private-repo", Some "private"); ("local-repo", None) ];
      List.iter (fun (project, title) ->
          ignore (ok [ "task"; "add"; "--project"; project; "--title"; title ]))
        [ ("work-repo", "Work item"); ("private-repo", "Private item");
          ("local-repo", "Ungrouped item"); ("private-repo", "Shared item");
          ("work-repo", "Completed work item") ];
      ignore (ok [ "task"; "done"; "local-005" ]);
      List.iter (fun name ->
          ignore (ok [ "task"; "workspace"; "add"; "local-004"; "--repo";
                       Filename.concat root name; "--branch"; "cto/shared" ])) [ "private-repo"; "work-repo" ];
      let context = Filename.concat root "context.md" in
      Shell.write_file context "# Cross-group diagnostic\n";
      let job_file = write_worker ~home ~run_id:"mantle-run" ~id:"cross-group"
          ~title:"Cross-group diagnostic" ~repo:(Filename.concat root "private-repo")
          ~branch:"cto/shared" ~context () in
      Yojson.Safe.from_file job_file
      |> replace_assoc_field "workspaces" (`List (List.map (fun name ->
             `Assoc [ ("repo", `String (Filename.concat root name)); ("branch", `String "cto/shared") ])
             [ "private-repo"; "work-repo" ]))
      |> Yojson.Safe.to_file job_file;
      let preserved = List.map (fun path -> (path, read_file path, (Unix.stat path).Unix.st_mtime))
          [ Filename.concat home ".monty/projects.json"; Filename.concat home ".monty/tasks.local.json"; job_file ] in
      let profile name = replace_env env
          [ ("MANTLE_CONTEXT", name); ("_MANTLE_PREV_STATE", "unset");
            ("_MANTLE_SELECTED_HOME", Filename.concat root "codex-home") ] in
      let work = profile "work" in
      let projects = ok ~env:work [ "projects"; "list" ] in
      require_line "Mantle project scope" projects "Profile: work (Mantle) | Group: work";
      require_contains "work project" projects "work-repo";
      absent projects "private-repo";
      absent projects "local-repo";
      let private_projects = ok ~env:(profile "private") [ "projects"; "list" ] in
      require_line "changed Mantle profile" private_projects "Profile: private (Mantle) | Group: private";
      require_contains "private project" private_projects "private-repo";
      absent private_projects "work-repo";
      List.iter (fun command ->
          let args = command @ [ "--no-sync" ] in
          let listed = ok ~env:work args in
          require_line "task profile banner" listed "Profile: work (Mantle) | Group: work";
          List.iter (require_contains "work task selection" listed) [ "Work item"; "Shared item"; "Cross-group diagnostic" ];
          List.iter (absent listed) [ "Private item"; "Ungrouped item"; "Completed work item" ];
          require_contains "shared branches remain intact" listed "private-repo=cto/shared; work-repo=cto/shared";
          let rows = listed |> String.trim |> String.split_on_char '\n' |> List.tl |> List.tl in
          List.iteri (fun index row ->
              if not (String.starts_with ~prefix:(string_of_int (index + 1) ^ " ") row) then
                failwith "profile filtering did not renumber visible rows") rows;
          let overridden = ok ~env:work (args @ [ "--group"; "private" ]) in
          require_line "explicit group caption" overridden "Profile: work (Mantle) | Group: private (--group)";
          require_contains "explicit group override" overridden "Private item";
          absent overridden "Work item";
          let selected = ok ~env:work (args @ [ "--project"; "private-repo" ]) in
          require_line "explicit project caption" selected "Profile: work (Mantle) | Project: private-repo (--project)";
          require_contains "explicit project overrides profile" selected "Private item";
          absent selected "Work item";
          let all = ok ~env:work (args @ [ "--all-groups" ]) in
          require_line "all groups caption" all "Profile: work (Mantle) | Groups: all (--all-groups)";
          require_contains "all groups includes ungrouped tasks" all "Ungrouped item";
          let completed = ok ~env:work (args @ [ "--all" ]) in
          require_contains "--all retains done-task meaning" completed "Completed work item";
          absent completed "Private item") [ [ "list" ]; [ "tasks"; "list" ] ];
      let by_run = ok ~env:work [ "list"; "--no-sync"; "--run"; "mantle-run" ] in
      require_contains "run and profile intersect" by_run "Cross-group diagnostic";
      absent by_run "Work item";
      if ok ~env:work [ "list"; "--no-sync" ] <> ok ~env:work [ "tasks"; "list"; "--no-sync" ] then
        failwith "profile-aware task list aliases diverged";
      let archived = ok ~env:work [ "list"; "--no-sync"; "--archived" ] in
      require_contains "archived profile filter" archived "Completed work item";
      absent archived "Shared item";
      List.iter (fun command ->
          let args = if command = [ "projects"; "list" ] then command else command @ [ "--no-sync" ] in
          let empty = ok ~env:(profile "client-a") args in
          require_line "custom empty Mantle profile" empty "Profile: client-a (Mantle) | Group: client-a";
          if List.length (String.split_on_char '\n' (String.trim empty)) <> 2 then failwith "empty group listed rows";
          let capital = ok ~env:work (args @ [ "--group"; "Work" ]) in
          if List.length (String.split_on_char '\n' (String.trim capital)) <> 2 then failwith "group identity became case-insensitive";
          require_code 1 (cli ~env:work (command @ [ "--group"; "work"; "--all-groups" ]));
          let unknown = cli ~env:(profile "missing") command in
          require_code 1 unknown;
          require_contains "missing Mantle group is actionable" unknown.stderr "monty projects groups add 'missing'";
          ignore (ok ~env:(profile "missing") (args @ [ "--all-groups" ])))
        [ [ "projects"; "list" ]; [ "list" ]; [ "tasks"; "list" ] ];
      List.iter (fun overrides ->
          let inactive = replace_env work overrides in
          let listed = ok ~env:inactive [ "projects"; "list" ] in
          absent listed "Profile:";
          List.iter (require_contains "inactive profile includes every group" listed)
            [ "work-repo"; "private-repo"; "local-repo" ])
        [ [ ("MANTLE_CONTEXT", "") ]; [ ("MANTLE_CONTEXT", "bad\nname") ];
          [ ("MANTLE_CONTEXT", "off") ]; [ ("_MANTLE_PREV_STATE", "") ];
          [ ("_MANTLE_SELECTED_HOME", "") ]; [ ("CODEX_HOME", Filename.concat root "other-home") ] ];
      List.iter (fun (path, bytes, mtime) ->
          if read_file path <> bytes || (Unix.stat path).Unix.st_mtime <> mtime then
            failf "profile listing mutated %s" path) preserved;
      require_empty_log log)

let run_named name test =
  try
    test ();
    Fmt.pr "PASS %s\n" name
  with exn -> failwith (Printf.sprintf "%s: %s" name (Printexc.to_string exn))

let () =
  Unix.putenv "GIT_CONFIG_NOSYSTEM" "1";
  Unix.putenv "GIT_CONFIG_SYSTEM" "/dev/null";
  Unix.putenv "GIT_CONFIG_GLOBAL" "/dev/null";
  Unix.putenv "GIT_CONFIG_COUNT" "0";
  Unix.putenv "GIT_TEMPLATE_DIR" "/dev/null";
  [ ("cli_mantle_profile_filters_project_and_task_lists", test_mantle_profile_filters_project_and_task_lists);
    ("cli_project_groups_crud_and_legacy_projects", test_project_groups_crud_and_legacy_projects);
    ("cli_project_groups_invalid_input_and_atomic_failure", test_project_groups_invalid_input_and_atomic_failure);
    ("cli_project_groups_concurrent_mutations", test_project_groups_concurrent_mutations);
    ("cli_live_codex_excludes_resume_and_completion", test_live_codex_excludes_resume_and_completion);
    ("cli_json_shape_errors_are_actionable", test_json_shape_errors_are_actionable);
    ("cli_worktree_root_and_branch_are_verified", test_worktree_root_and_branch_are_verified);
    ("cli_concurrent_task_adds_keep_unique_tasks", test_concurrent_task_adds_keep_unique_tasks);
    ("cli_malformed_json_is_not_overwritten", test_malformed_json_is_not_overwritten);
    ( "cli_dry_run_rejects_unsafe_manifest_before_side_effects",
      test_dry_run_rejects_unsafe_manifest_before_side_effects );
    ("cli_readme_worker_path_is_home_relative", test_readme_worker_path_is_home_relative);
    ("cli_readme_context_path_is_home_relative", test_readme_context_path_is_home_relative);
    ("cli_atomic_fault_preserves_previous_json", test_cli_atomic_fault_preserves_previous_json);
    ("cli_concurrent_project_adds_keep_every_project", test_concurrent_project_adds_keep_every_project);
    ( "cli_rejects_unsafe_persisted_worker_before_external_commands",
      test_cli_rejects_unsafe_persisted_worker_before_external_commands );
    ("cli_rejects_state_parent_and_lock_symlinks", test_cli_rejects_state_parent_and_lock_symlinks);
    ( "cli_external_terminal_request_runs_without_state_lock",
      test_external_terminal_request_runs_without_state_lock );
    ( "cli_codex_launch_uses_command_local_trust",
      test_codex_launch_uses_command_local_trust );
    ( "cli_codex_session_resume_modes",
      test_codex_session_resume_modes );
    ( "cli_lifecycle_faults_recover_from_both_locations",
      test_lifecycle_faults_recover_from_both_locations );
    ( "cli_completion_persists_force_and_never_creates_worktree",
      test_completion_persists_force_and_never_creates_worktree );
    ( "cli_completion_makes_ignored_artifacts_removable",
      test_completion_makes_ignored_artifacts_removable );
    ( "cli_completion_recovers_after_partial_worktree_removal",
      test_completion_recovers_after_partial_worktree_removal );
    ( "cli_collision_task_failure_and_resume_dry_run_are_safe",
      test_collision_task_failure_and_resume_dry_run_are_safe );
    ( "cli_cleanup_stale_wt_doctor_and_transition_guards",
      test_cleanup_stale_wt_doctor_and_transition_guards );
    ( "cli_invalid_explicit_local_task_key_is_never_inferred",
      test_invalid_explicit_local_task_key_is_never_inferred );
    ( "cli_reconciliation_replay_idempotence_and_legacy_repair",
      test_reconciliation_replay_idempotence_and_legacy_repair );
    ("cli_task_list_numbers", test_task_list_numbers);
    ( "cli_reconciliation_diagnostics_no_sync_and_unknown_launch",
      test_reconciliation_diagnostics_no_sync_and_unknown_launch );
    ( "cli_discovery_preserves_strict_and_tolerant_policies",
      test_discovery_preserves_strict_and_tolerant_policies );
    ( "cli_inventory_filters_preserve_diagnostics",
      test_inventory_filters_preserve_diagnostics );
    ( "cli_external_import_local_ownership_and_stable_projects",
      test_external_import_local_ownership_and_stable_projects );
    ( "cli_duplicate_task_claims_warn_deterministically",
      test_duplicate_task_claims_warn_deterministically );
    ( "cli_invalid_links_duplicate_ids_and_ambiguous_workers",
      test_invalid_links_duplicate_ids_and_ambiguous_workers );
    ( "cli_launch_batch_preflight_is_all_or_nothing",
      test_launch_batch_preflight_is_all_or_nothing );
    ( "cli_launch_batch_partial_failure_and_safe_retry",
      test_launch_batch_partial_failure_and_safe_retry );
    ( "cli_launch_request_fault_boundaries",
      test_launch_request_fault_boundaries );
    ( "cli_reservation_failures_are_structured_and_replayable",
      test_reservation_failures_are_structured_and_replayable );
    ( "cli_unsafe_staged_reservations_are_rejected",
      test_unsafe_staged_reservations_are_rejected );
    ( "cli_completion_accepts_every_open_launch_state",
      test_completion_accepts_every_open_launch_state );
    ( "cli_launch_rejects_global_identity_conflicts_and_races",
      test_launch_rejects_global_identity_conflicts_and_races );
    ( "cli_retry_uses_recorded_script_and_absolute_commands",
      test_retry_uses_recorded_script_and_absolute_commands );
    ( "cli_codex_launch_preserves_mantle_environment",
      test_codex_launch_preserves_mantle_environment );
    ( "cli_ghostty_launch_script_matches_ensure_worktree_cli",
      test_ghostty_launch_script_matches_ensure_worktree_cli );
    ( "cli_lifecycle_rejects_cross_project_and_owned_task_links",
      test_lifecycle_rejects_cross_project_and_owned_task_links );
    ( "cli_live_resume_excludes_completion",
      test_live_resume_excludes_completion );
    ( "cli_materialization_excludes_completion",
      test_materialization_excludes_completion );
    ( "cli_workspace_ensure_excludes_completion",
      test_workspace_ensure_excludes_completion );
    ( "cli_worker_ensure_identity_and_delayed_script",
      test_worker_ensure_identity_and_delayed_script );
    ( "cli_forged_launch_script_and_resume_mode_are_safe",
      test_forged_launch_script_and_resume_mode_are_safe );
    ( "cli_multi_workspace_sonnet_task_lifecycle",
      test_multi_workspace_sonnet_task_lifecycle );
    ( "cli_multi_workspace_project_suffixes",
      test_multi_workspace_project_suffixes );
    ( "cli_legacy_multi_workspace_reservations",
      test_legacy_multi_workspace_reservations );
    ( "cli_legacy_multi_workspace_branches",
      test_legacy_multi_workspace_branches );
    ( "cli_headless_prepare_run_and_resume",
      test_headless_prepare_run_and_resume );
    ( "cli_simplified_default_and_headless_writers",
      test_simplified_default_and_headless_writers );
    ( "cli_agent_profiles_selection_snapshot_and_solo",
      test_agent_profiles_selection_snapshot_and_solo );
    ( "cli_codex_headless_uses_effective_settings_without_ghostty",
      test_codex_headless_uses_effective_settings_without_ghostty );
    ( "cli_standalone_background_harnesses", test_standalone_background_harnesses );
    ( "cli_run_handoff_publish_delivery_follow_up_and_lifecycle_race",
      test_run_handoff_publish_delivery_follow_up_and_lifecycle_race );
    ( "cli_settings_commands_and_effective_harness",
      test_settings_commands_and_effective_harness );
    ( "cli_head_butler_continue_uses_native_sessions_without_monty_state",
      test_head_butler_continue_uses_native_sessions_without_monty_state );
    ( "cli_parser_and_doctor_contracts", test_cli_parser_and_doctor_contracts ) ]
  |> List.iter (fun (name, test) -> run_named name test)
