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

let with_temp_root name f =
  let marker = Filename.temp_file ("monty-cli-" ^ name ^ "-") ".tmp" in
  Sys.remove marker;
  Unix.mkdir marker 0o700;
  Fun.protect ~finally:(fun () -> remove_tree marker) (fun () -> f marker)

let executable =
  let test_dir = Shell.abs_path Sys.executable_name |> Filename.dirname in
  let candidate = Filename.concat test_dir "../bin/main.exe" |> Shell.normalize in
  if not (Sys.file_exists candidate) then failf "built Monty binary is missing: %s" candidate;
  Unix.realpath candidate

let read_file path = if Sys.file_exists path then Shell.read_file path else ""

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

let env_with overrides = replace_env (Unix.environment ()) overrides

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

let spawn ~root ~env index args =
  let stdout_path = Filename.concat root (Printf.sprintf "stdout-%d" index) in
  let stderr_path = Filename.concat root (Printf.sprintf "stderr-%d" index) in
  let stdout_fd =
    Unix.openfile stdout_path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600
  in
  let stderr_fd =
    Unix.openfile stderr_path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600
  in
  let stdin_fd = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
  let argv = Array.of_list (executable :: args) in
  let pid = Unix.create_process_env executable argv env stdin_fd stdout_fd stderr_fd in
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

let run ~root ~env index args = spawn ~root ~env index args |> await

let command result = result.argv |> Array.to_list |> String.concat " "

let require_code expected result =
  if result.code <> expected then
    failf "command exited %d instead of %d: %s\nstdout:\n%s\nstderr:\n%s"
      result.code expected (command result) result.stdout result.stderr

let setup_environment root =
  let home = Filename.concat root "home" in
  let fake_bin = Filename.concat root "fake-bin" in
  let log = Filename.concat root "fake-tools.log" in
  Shell.ensure_dir fake_bin;
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
    [ "wt"; "gh"; "pi"; "osascript"; "sdef"; "ghostty" ];
  let env =
    env_with
      [ ("MONTY_HOME", home);
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

let manifest_job ?id ?branch ?task_key ~title ~repo ~context () =
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
    | Some value -> [ ("task_key", `String value) ]))

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
      let home, log, env = setup_environment root in
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
         "  db) rm -f " ^ Shell.quote marker ^ " ;;";
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
      let launch ?(terminal = "ghostty") ?(pi_command = "pi --fixed value") index =
        run ~root ~env index
          [ "launch-many"; "--manifest"; manifest; "--home"; home;
            "--terminal"; terminal; "--worktree"; "never"; "--pi-command";
            pi_command ]
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
          "/usr/bin/true --fixed argument" ]
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
      let legacy_script =
        Shell.read_file recorded_script |> String.split_on_char '\n'
        |> List.filter (fun line ->
               not (String.equal line "# monty-launch-script-v1"))
        |> String.concat "\n"
      in
      Shell.write_file recorded_script legacy_script;
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
      require_empty_log log)

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

let test_launch_state_race_preserves_completion_transition () =
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
      if interrupted.code = 0 then
        failwith "completion race setup did not stop after persisted intent";
      Shell.write_file release "release\n";
      let resumed = await child in
      if resumed.code = 0 then
        failwith "resume overwrote a concurrent completion transition";
      require_contains "launch transition compare-and-update" resumed.stdout
        "entered a complete transition";
      let record =
        match
          Job_store.parse_job_file ~home (Filename.concat worker_dir "job.json")
        with
        | Ok record -> record
        | Error message -> failwith ("race left unparsable state: " ^ message)
      in
      if not (String.equal record.Job_store.status "completing") then
        failwith "resume changed completing status during race";
      (match record.transition with
      | Some transition when transition.operation = Job_store.Complete -> ()
      | _ -> failwith "completion transition was lost during resume race");
      let recovered =
        run ~root ~env 1942
          [ "done"; "race-worker"; "--home"; home; "--wt-command"; "wt" ]
      in
      require_code 0 recovered)

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

let test_headless_prepare_begin_and_resume () =
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
      let set_worker_task_status worker status =
        let path = Filename.concat home ".monty/tasks.local.json" in
        let json = Yojson.Safe.from_file path in
        let tasks = Yojson.Safe.Util.(json |> member "tasks" |> to_list) in
        let tasks =
          List.map
            (fun task ->
              if json_string "worker_id" task = worker then
                replace_assoc_field "status" (`String status) task
              else task)
            tasks
        in
        Yojson.Safe.to_file path
          (replace_assoc_field "tasks" (`List tasks) json)
      in
      let wait_for_new_wt log_size =
        let rec wait attempts =
          if String.length (Shell.read_file log) > log_size then ()
          else if attempts = 0 then failwith "headless command did not reach wt"
          else (
            Unix.sleepf 0.01;
            wait (attempts - 1))
        in
        wait 300
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
            [ "worker_backend"; "subagent"; "run_id"; "async_dir" ];
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
            pi_command = "pi";
            wt_command = Filename.concat root "fake-bin/wt";
            worktree_mode = Always;
            branch_prefix = "monty";
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
      let premature_resume =
        run ~root ~env 1952
          [ "headless"; "resume"; "headless-two"; "--home"; home ]
      in
      if premature_resume.code = 0 then
        failwith "headless resume accepted a worker without a predecessor chain";
      require_contains "headless successor guard" premature_resume.stderr
        "launch-requested";
      let fault_env =
        replace_env env [ ("MONTY_FAULT_INJECT", "launch-before-request-state") ]
      in
      let faulted =
        run ~root ~env:fault_env 1953
          [ "headless"; "begin"; "headless-two"; "--home"; home ]
      in
      if faulted.code = 0 then failwith "headless begin ignored its injected fault";
      if job_status (job_file "headless-two") <> "prepared" then
        failwith "a pre-dispatch headless failure was not retryable as prepared";
      let state_lock = Filename.concat home ".monty/state.lock" in
      let state_fd = Unix.openfile state_lock [ Unix.O_CREAT; Unix.O_RDWR ] 0o600 in
      Unix.lockf state_fd Unix.F_LOCK 0;
      let log_size = String.length (Shell.read_file log) in
      let raced_begin =
        spawn ~root ~env 1958
          [ "headless"; "begin"; "headless-two"; "--home"; home ]
      in
      wait_for_new_wt log_size;
      set_worker_task_status "headless-two" "done";
      Unix.lockf state_fd Unix.F_ULOCK 0;
      Unix.close state_fd;
      let raced_begin = await raced_begin in
      if raced_begin.code = 0 then
        failwith "headless begin claimed a worker whose task closed under lock";
      require_contains "atomic headless begin task guard" raced_begin.stderr
        "not open";
      if job_status (job_file "headless-two") <> "prepared" then
        failwith "rejected headless begin changed prepared state";
      set_worker_task_status "headless-two" "open";
      let begun =
        run ~root ~env 1954
          [ "headless"; "begin"; "headless-one"; "--home"; home ]
      in
      require_code 0 begun;
      let dispatch = Yojson.Safe.from_string begun.stdout in
      if
        Yojson.Safe.Util.(dispatch |> member "schema" |> to_string)
        <> Headless.dispatch_schema
      then failwith "headless begin returned the wrong schema";
      let begun_worktree =
        Yojson.Safe.Util.(dispatch |> member "worker" |> member "worktree" |> to_string)
      in
      if not (List.mem begun_worktree worktrees) then
        failwith "headless begin returned an unexpected worktree";
      let harness_call = Yojson.Safe.Util.(dispatch |> member "harness_call") in
      if Yojson.Safe.Util.(harness_call |> member "tool" |> to_string) <> "subagent"
      then failwith "headless begin did not target the harness subagent tool";
      let chain =
        Yojson.Safe.Util.(harness_call |> member "arguments" |> member "chain" |> to_list)
      in
      let reviewers =
        Yojson.Safe.Util.(List.nth chain 1 |> member "parallel" |> to_list)
      in
      let children = List.nth chain 0 :: reviewers @ [ List.nth chain 2 ] in
      List.iter
        (fun child ->
          let acceptance = Yojson.Safe.Util.member "acceptance" child in
          if Yojson.Safe.Util.(acceptance |> member "level" |> to_string) <> "none"
          then failwith "headless child did not explicitly disable inferred acceptance";
          if
            Yojson.Safe.Util.(acceptance |> member "reason" |> to_string)
            <> Headless.acceptance_reason
          then failwith "headless child acceptance reason did not identify the review gate")
        children;
      if job_status (job_file "headless-one") <> "launch-requested" then
        failwith "headless begin did not claim its worker";
      if job_status (job_file "headless-two") <> "prepared" then
        failwith "headless begin changed another worker";
      (match
         Launcher.reserve_batch ~reject_requested:true stale_options
           stale_preflight
       with
      | Ok _ ->
          failwith
            "headless reservation accepted a worker claimed after stale preflight"
      | Error message ->
          require_contains "headless post-lock claim guard" message
            "became launch-requested");
      if job_status (job_file "headless-one") <> "launch-requested" then
        failwith "stale headless reservation reset a claimed worker";
      if job_status (job_file "headless-two") <> "prepared" then
        failwith "stale headless reservation mutated another worker";
      let duplicate =
        run ~root ~env 1955
          [ "headless"; "begin"; "headless-one"; "--home"; home ]
      in
      if duplicate.code = 0 then failwith "headless begin replayed a requested worker";
      require_contains "headless begin replay recovery" duplicate.stderr
        "headless resume";
      let state_fd = Unix.openfile state_lock [ Unix.O_CREAT; Unix.O_RDWR ] 0o600 in
      Unix.lockf state_fd Unix.F_LOCK 0;
      let log_size = String.length (Shell.read_file log) in
      let raced_resume =
        spawn ~root ~env 1959
          [ "headless"; "resume"; "headless-one"; "--home"; home ]
      in
      wait_for_new_wt log_size;
      set_worker_task_status "headless-one" "done";
      Unix.lockf state_fd Unix.F_ULOCK 0;
      Unix.close state_fd;
      let raced_resume = await raced_resume in
      if raced_resume.code = 0 then
        failwith "headless resume claimed a worker whose task closed under lock";
      require_contains "atomic headless resume task guard" raced_resume.stderr
        "not open";
      if job_status (job_file "headless-one") <> "launch-requested" then
        failwith "rejected headless resume changed launch-requested state";
      set_worker_task_status "headless-one" "open";
      let resumed =
        run ~root ~env 1956
          [ "headless"; "resume"; "headless-one"; "--home"; home ]
      in
      require_code 0 resumed;
      if job_status (job_file "headless-one") <> "launch-requested" then
        failwith "explicit headless resume changed the open launch state";
      require_no_terminal_script "headless-one";
      require_no_terminal_script "headless-two";
      let tasks = local_tasks_json home in
      if List.length tasks <> 2 then failwith "headless workers did not retain two local tasks";
      if
        List.exists
          (fun task ->
            not
              (String.equal
                 Yojson.Safe.Util.(task |> member "status" |> to_string)
                 "open"))
          tasks
      then failwith "headless execution completed a local task automatically";
      if string_contains (read_file log) "osascript" then
        failwith "headless begin or resume opened Ghostty")

let test_pi_task_snapshot_prepare_and_enter () =
  with_temp_root "pi-task" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let plan = Filename.concat root "plan.md" in
      init_git_repo repo;
      add_project ~root ~home ~env repo;
      require_code 0
        (run ~root ~env 1970
           [ "task"; "add"; "--home"; home; "--project"; "repo";
             "--title"; "Native Pi task" ]);
      let before = Shell.read_file (Filename.concat home ".monty/tasks.local.json") in
      let listed =
        run ~root ~env 1971
          [ "tasks"; "list"; "--json"; "--no-sync"; "--home"; home ]
      in
      require_code 0 listed;
      if Shell.read_file (Filename.concat home ".monty/tasks.local.json") <> before then
        failwith "read-only Pi task snapshot changed local task state";
      let list_json = Yojson.Safe.from_string listed.stdout in
      if Yojson.Safe.Util.(list_json |> member "schema" |> to_string) <> Pi_bridge.tasks_schema
      then failwith "Pi task snapshot returned the wrong schema";
      let first = Yojson.Safe.Util.(list_json |> member "tasks" |> to_list |> List.hd) in
      if Yojson.Safe.Util.(first |> member "action" |> to_string) <> "plan" then
        failwith "unstarted Pi task was not marked for planning";
      require_empty_log log;
      install_create_wt ~root ~log;
      Shell.write_file plan "Plan:\n1. Implement the native Pi task flow.\n";
      let prepared =
        run ~root ~env 1972
          [ "task"; "prepare"; "local-001"; "--plan"; plan; "--json";
            "--home"; home ]
      in
      require_code 0 prepared;
      let entry = Yojson.Safe.from_string prepared.stdout in
      if Yojson.Safe.Util.(entry |> member "schema" |> to_string) <> Pi_bridge.entry_schema
      then failwith "Pi task prepare returned the wrong schema";
      let worker = Yojson.Safe.Util.(entry |> member "worker") in
      if Yojson.Safe.Util.(worker |> member "id" |> to_string) <> "local-001"
         || Yojson.Safe.Util.(worker |> member "status" |> to_string) <> "prepared"
      then failwith "Pi task prepare returned the wrong worker";
      let cwd = Yojson.Safe.Util.(entry |> member "cwd" |> to_string) in
      if not (Sys.file_exists cwd && Sys.is_directory cwd) then
        failwith "Pi task prepare did not materialize its worktree";
      let opened =
        run ~root ~env 1973
          [ "task"; "enter"; "local:local-001"; "--json"; "--home"; home ]
      in
      require_code 0 opened;
      if Yojson.Safe.Util.(Yojson.Safe.from_string opened.stdout |> member "cwd" |> to_string) <> cwd
      then failwith "Pi task enter did not reuse its worktree";
      let job_file =
        Filename.concat home ".monty/runs/pi/workers/local-001/job.json"
      in
      let original_job = Yojson.Safe.from_file job_file in
      let worker_dir = Filename.dirname job_file in
      let transition =
        `Assoc
          [ ("operation", `String "complete");
            ("source", `String worker_dir);
            ( "target",
              `String
                (Filename.concat home
                   ".monty/runs/pi/archive/local-001") );
            ("task_key", `String "local:local-001");
            ("force", `Bool false);
            ("started_at", `String "2026-01-01T00:00:00Z") ]
      in
      Yojson.Safe.to_file job_file
        (original_job
        |> replace_assoc_field "transition" transition
        |> replace_assoc_field "status" (`String "completing"));
      Shell.write_file log "";
      let transitioning =
        run ~root ~env 1976
          [ "task"; "enter"; "local:local-001"; "--json"; "--home"; home ]
      in
      if transitioning.code = 0 then
        failwith "Pi task enter accepted a lifecycle transition";
      require_contains "Pi task transition diagnostic" transitioning.stderr
        "in a complete transition";
      require_empty_log log;
      Yojson.Safe.to_file job_file original_job;
      let tasks = local_tasks_json home in
      if List.length tasks <> 1
         || json_string "worker_id" (List.hd tasks) <> "local-001"
      then failwith "Pi task prepare did not retain one stable task link";
      let refreshed =
        run ~root ~env 1974
          [ "tasks"; "list"; "--json"; "--no-sync"; "--home"; home ]
      in
      require_code 0 refreshed;
      let item =
        Yojson.Safe.Util.(Yojson.Safe.from_string refreshed.stdout |> member "tasks"
                          |> to_list |> List.hd)
      in
      if Yojson.Safe.Util.(item |> member "action" |> to_string) <> "open" then
        failwith "prepared Pi task was not marked openable";
      if string_contains (read_file log) "osascript" || string_contains (read_file log) "/pi "
      then failwith "Pi task preparation launched a terminal or Pi")

let test_pi_task_enter_revalidates_open_task_under_lock () =
  with_temp_root "pi-task-enter-race" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let plan = Filename.concat root "plan.md" in
      init_git_repo repo;
      add_project ~root ~home ~env repo;
      install_create_wt ~root ~log;
      require_code 0
        (run ~root ~env 1990
           [ "task"; "add"; "--home"; home; "--project"; "repo";
             "--title"; "Atomic Pi entry" ]);
      Shell.write_file plan "Plan:\n1. Revalidate entry under the state lock.\n";
      let prepared =
        run ~root ~env 1991
          [ "task"; "prepare"; "local-001"; "--plan"; plan; "--json";
            "--home"; home ]
      in
      require_code 0 prepared;
      let prepared_cwd =
        Yojson.Safe.Util.
          (Yojson.Safe.from_string prepared.stdout |> member "cwd" |> to_string)
      in
      let job_file =
        Filename.concat home ".monty/runs/pi/workers/local-001/job.json"
      in
      let job = Yojson.Safe.from_file job_file in
      Yojson.Safe.to_file job_file
        (replace_assoc_field "last_known_worktree" (`String repo) job);
      let state_lock = Filename.concat home ".monty/state.lock" in
      let state_fd = Unix.openfile state_lock [ Unix.O_CREAT; Unix.O_RDWR ] 0o600 in
      Unix.lockf state_fd Unix.F_LOCK 0;
      let log_size = String.length (Shell.read_file log) in
      let entering =
        spawn ~root ~env 1992
          [ "task"; "enter"; "local:local-001"; "--json"; "--home"; home ]
      in
      let rec wait_for_wt attempts =
        if String.length (Shell.read_file log) > log_size then ()
        else if attempts = 0 then failwith "Pi task enter did not reach wt"
        else (
          Unix.sleepf 0.01;
          wait_for_wt (attempts - 1))
      in
      wait_for_wt 300;
      let tasks_path = Filename.concat home ".monty/tasks.local.json" in
      let tasks_json = Yojson.Safe.from_file tasks_path in
      let tasks = Yojson.Safe.Util.(tasks_json |> member "tasks" |> to_list) in
      let tasks =
        List.map
          (fun task ->
            if json_string "id" task = "local-001" then
              replace_assoc_field "status" (`String "done") task
            else task)
          tasks
      in
      Yojson.Safe.to_file tasks_path
        (replace_assoc_field "tasks" (`List tasks) tasks_json);
      Unix.lockf state_fd Unix.F_ULOCK 0;
      Unix.close state_fd;
      let entered = await entering in
      if entered.code = 0 then
        failwith "Pi task enter accepted a task closed while waiting for state lock";
      require_contains "atomic Pi task entry guard" entered.stderr "not open";
      if String.trim entered.stdout <> "" then
        failwith "rejected Pi task entry emitted session handoff JSON";
      let rejected_job = Yojson.Safe.from_file job_file in
      if json_string "status" rejected_job <> "prepared" then
        failwith "rejected Pi task entry changed worker launch state";
      if json_string "last_known_worktree" rejected_job <> repo then
        failwith "rejected Pi task entry committed a stale workspace update";
      if Yojson.Safe.Util.member "transition" rejected_job <> `Null then
        failwith "rejected Pi task entry started a lifecycle transition";
      if not (Sys.file_exists prepared_cwd && Sys.is_directory prepared_cwd) then
        failwith "rejected Pi task entry damaged the prepared worktree";
      if string_contains (read_file log) "osascript" || string_contains (read_file log) "/pi "
      then failwith "rejected Pi task entry launched a terminal or Pi")

let test_pi_task_exact_resolution_collision () =
  with_temp_root "pi-task-exact-collision" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let plan = Filename.concat root "plan.md" in
      init_git_repo repo;
      add_project ~root ~home ~env repo;
      install_create_wt ~root ~log;
      require_code 0
        (run ~root ~env 1977
           [ "task"; "add"; "--home"; home; "--project"; "repo";
             "--title"; "local-002" ]);
      require_code 0
        (run ~root ~env 1978
           [ "task"; "add"; "--home"; home; "--project"; "repo";
             "--title"; "Exact target" ]);
      Shell.write_file plan "Plan:\n1. Resolve the exact local ID.\n";
      require_code 0
        (run ~root ~env 1979
           [ "task"; "prepare"; "local-002"; "--plan"; plan; "--json";
             "--home"; home ]);
      let context = Filename.concat home ".monty/runs/pi/local-002.md" in
      require_contains "exact task context" (Shell.read_file context)
        "# Exact target")

let test_pi_task_prepare_preflight_context_cleanup () =
  with_temp_root "pi-task-missing-repo" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let plan = Filename.concat root "plan.md" in
      add_project ~root ~home ~env repo;
      require_code 0
        (run ~root ~env 1980
           [ "task"; "add"; "--home"; home; "--project"; "repo";
             "--title"; "Missing repo" ]);
      remove_tree repo;
      Shell.write_file plan "Plan:\n1. First plan.\n";
      let result =
        run ~root ~env 1981
          [ "task"; "prepare"; "local-001"; "--plan"; plan; "--json";
            "--home"; home ]
      in
      if result.code = 0 then failwith "task preparation accepted a missing repo";
      let context = Filename.concat home ".monty/runs/pi/local-001.md" in
      if Sys.file_exists context then
        failwith "repo preflight failure wrote a task context";
      require_empty_log log);
  with_temp_root "pi-task-revised-plan" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let plan = Filename.concat root "plan.md" in
      init_git_repo repo;
      add_project ~root ~home ~env repo;
      install_create_wt ~root ~log;
      require_code 0
        (run ~root ~env 1982
           [ "task"; "add"; "--home"; home; "--project"; "repo";
             "--title"; "Revised plan" ]);
      let context = Filename.concat home ".monty/runs/pi/local-001.md" in
      Shell.write_file plan "Plan:\n1. Rejected version.\n";
      let failed =
        run ~root
          ~env:(replace_env env [ ("MONTY_FAULT_INJECT", "reserve-after-stage") ])
          1983
          [ "task"; "prepare"; "local-001"; "--plan"; plan; "--json";
            "--home"; home ]
      in
      if failed.code = 0 then failwith "reservation fault unexpectedly succeeded";
      if Sys.file_exists context then
        failwith "pre-reservation failure retained an unclaimed task context";
      Shell.write_file plan "Plan:\n1. Accepted revision.\n";
      let retried =
        run ~root ~env 1984
          [ "task"; "prepare"; "local-001"; "--plan"; plan; "--json";
            "--home"; home ]
      in
      require_code 0 retried;
      require_contains "revised task context" (Shell.read_file context)
        "Accepted revision");
  with_temp_root "pi-task-prepared-context" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let plan = Filename.concat root "plan.md" in
      init_git_repo repo;
      add_project ~root ~home ~env repo;
      install_create_wt ~root ~log;
      let wt = Filename.concat root "fake-bin/wt" in
      Shell.write_file wt
        (String.concat "\n"
           [ "#!/bin/sh";
             "printf '%s\\n' \"$0 $*\" >> " ^ Shell.quote log;
             "exit 77";
             "" ]);
      Shell.chmod_executable wt;
      require_code 0
        (run ~root ~env 1985
           [ "task"; "add"; "--home"; home; "--project"; "repo";
             "--title"; "Prepared context" ]);
      Shell.write_file plan "Plan:\n1. Preserve after reservation.\n";
      let failed =
        run ~root ~env 1986
          [ "task"; "prepare"; "local-001"; "--plan"; plan; "--json";
            "--home"; home ]
      in
      if failed.code = 0 then failwith "worktree failure unexpectedly succeeded";
      let context = Filename.concat home ".monty/runs/pi/local-001.md" in
      if not (Sys.file_exists context) then
        failwith "post-reservation failure removed the canonical worker context";
      if
        not
          (Sys.file_exists
             (Filename.concat home
                ".monty/runs/pi/workers/local-001/job.json"))
      then failwith "worktree failure did not leave its prepared worker claim")

let test_pi_task_prepare_serializes_same_task () =
  with_temp_root "pi-task-serialized-prepare" (fun root ->
      let home, log, env = setup_environment root in
      let repo = Filename.concat root "repo" in
      let first_plan = Filename.concat root "first-plan.md" in
      let second_plan = Filename.concat root "second-plan.md" in
      let started = Filename.concat root "wt-started" in
      let worktrees = Filename.concat root "created-worktrees" in
      let wt = Filename.concat root "fake-bin/wt" in
      init_git_repo repo;
      add_project ~root ~home ~env repo;
      Shell.write_file wt
        (String.concat "\n"
           [ "#!/bin/sh";
             "set -eu";
             "printf '%s\\n' \"$0 $*\" >> " ^ Shell.quote log;
             "case \"$1\" in";
             "  b)";
             "    touch " ^ Shell.quote started;
             "    sleep 0.3";
             "    branch=$2";
             "    safe=$(printf '%s' \"$branch\" | tr '/ ' '__')";
             "    worktree=" ^ Shell.quote worktrees ^ "/$safe";
             "    if [ ! -d \"$worktree\" ]; then";
             "      mkdir -p " ^ Shell.quote worktrees;
             "      git worktree add -q -b \"$branch\" \"$worktree\"";
             "    fi";
             "    printf '%s\\n' \"$worktree\"";
             "    ;;";
             "  list) printf '%s\\n' 'repo:' ;;";
             "  *) exit 92 ;;";
             "esac";
             "" ]);
      Shell.chmod_executable wt;
      require_code 0
        (run ~root ~env 1987
           [ "task"; "add"; "--home"; home; "--project"; "repo";
             "--title"; "Serialized prepare" ]);
      Shell.write_file first_plan "Plan:\n1. Canonical prepared plan.\n";
      Shell.write_file second_plan "Plan:\n1. Concurrent revised plan.\n";
      let prepare_lock =
        Filename.concat home ".monty/prepare-local-001.lock"
      in
      let lock_fd = Unix.openfile prepare_lock [ Unix.O_CREAT; Unix.O_RDWR ] 0o600 in
      Unix.lockf lock_fd Unix.F_LOCK 0;
      let first =
        spawn ~root ~env 1988
          [ "task"; "prepare"; "local-001"; "--plan"; first_plan;
            "--json"; "--home"; home ]
      in
      Unix.sleepf 0.1;
      let tasks_path = Filename.concat home ".monty/tasks.local.json" in
      let tasks_json = Yojson.Safe.from_file tasks_path in
      let tasks = Yojson.Safe.Util.(tasks_json |> member "tasks" |> to_list) in
      let tasks =
        List.map
          (fun task ->
            if json_string "id" task = "local-001" then
              replace_assoc_field "title" (`String "Re-resolved prepare") task
            else task)
          tasks
      in
      Yojson.Safe.to_file tasks_path
        (replace_assoc_field "tasks" (`List tasks) tasks_json);
      Unix.lockf lock_fd Unix.F_ULOCK 0;
      Unix.close lock_fd;
      let rec wait attempts =
        if Sys.file_exists started then ()
        else if attempts = 0 then failwith "first preparation did not reach wt"
        else (
          Unix.sleepf 0.01;
          wait (attempts - 1))
      in
      wait 300;
      let second =
        spawn ~root ~env 1989
          [ "task"; "prepare"; "local-001"; "--plan"; second_plan;
            "--json"; "--home"; home ]
      in
      require_code 0 (await first);
      require_code 0 (await second);
      let context = Filename.concat home ".monty/runs/pi/local-001.md" in
      require_contains "serialized re-resolved task" (Shell.read_file context)
        "# Re-resolved prepare";
      require_contains "serialized canonical context" (Shell.read_file context)
        "Canonical prepared plan";
      if string_contains (Shell.read_file context) "Concurrent revised plan" then
        failwith "concurrent preparation replaced a prepared worker context")

let test_start_loads_native_pi_extension () =
  with_temp_root "pi-start" (fun root ->
      let home, _log, env = setup_environment root in
      let extension = Filename.concat home "pi-extension" in
      let pi = Filename.concat root "fake-bin/pi" in
      let seen = Filename.concat root "pi-start.log" in
      Shell.ensure_dir extension;
      Shell.write_file (Filename.concat extension "package.json") "{}\n";
      Shell.write_file pi
        (String.concat "\n"
           [ "#!/bin/sh";
             "printf 'home=%s\\ncommand=%s\\nargs=%s\\n' \"$MONTY_HOME\" \"$MONTY_COMMAND\" \"$*\" > "
             ^ Shell.quote seen;
             "" ]);
      Shell.chmod_executable pi;
      let started =
        run ~root ~env 1975
          [ "start"; "--home"; home; "--pi-command"; pi; "--name";
            "Native Monty" ]
      in
      require_code 0 started;
      let output = read_file seen in
      require_line "start home" output ("home=" ^ home);
      require_line "start command" output ("command=" ^ executable);
      require_contains "start extension" output
        ("--extension " ^ extension ^ " --name Native Monty"))

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

let run_named name test =
  try
    test ();
    Fmt.pr "PASS %s\n" name
  with exn -> failwith (Printf.sprintf "%s: %s" name (Printexc.to_string exn))

let () =
  [ ("cli_concurrent_task_adds_keep_unique_tasks", test_concurrent_task_adds_keep_unique_tasks);
    ("cli_malformed_json_is_not_overwritten", test_malformed_json_is_not_overwritten);
    ( "cli_dry_run_rejects_unsafe_manifest_before_side_effects",
      test_dry_run_rejects_unsafe_manifest_before_side_effects );
    ("cli_readme_worker_path_is_home_relative", test_readme_worker_path_is_home_relative);
    ("cli_atomic_fault_preserves_previous_json", test_cli_atomic_fault_preserves_previous_json);
    ("cli_concurrent_project_adds_keep_every_project", test_concurrent_project_adds_keep_every_project);
    ( "cli_rejects_unsafe_persisted_worker_before_external_commands",
      test_cli_rejects_unsafe_persisted_worker_before_external_commands );
    ("cli_rejects_state_parent_and_lock_symlinks", test_cli_rejects_state_parent_and_lock_symlinks);
    ( "cli_external_terminal_request_runs_without_state_lock",
      test_external_terminal_request_runs_without_state_lock );
    ( "cli_lifecycle_faults_recover_from_both_locations",
      test_lifecycle_faults_recover_from_both_locations );
    ( "cli_completion_persists_force_and_never_creates_worktree",
      test_completion_persists_force_and_never_creates_worktree );
    ( "cli_collision_task_failure_and_resume_dry_run_are_safe",
      test_collision_task_failure_and_resume_dry_run_are_safe );
    ( "cli_cleanup_stale_wt_doctor_and_transition_guards",
      test_cleanup_stale_wt_doctor_and_transition_guards );
    ( "cli_invalid_explicit_local_task_key_is_never_inferred",
      test_invalid_explicit_local_task_key_is_never_inferred );
    ( "cli_reconciliation_replay_idempotence_and_legacy_repair",
      test_reconciliation_replay_idempotence_and_legacy_repair );
    ( "cli_reconciliation_diagnostics_no_sync_and_unknown_launch",
      test_reconciliation_diagnostics_no_sync_and_unknown_launch );
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
    ( "cli_lifecycle_rejects_cross_project_and_owned_task_links",
      test_lifecycle_rejects_cross_project_and_owned_task_links );
    ( "cli_launch_state_race_preserves_completion_transition",
      test_launch_state_race_preserves_completion_transition );
    ( "cli_forged_launch_script_and_resume_mode_are_safe",
      test_forged_launch_script_and_resume_mode_are_safe );
    ( "cli_headless_prepare_begin_and_resume",
      test_headless_prepare_begin_and_resume );
    ( "cli_pi_task_snapshot_prepare_and_enter",
      test_pi_task_snapshot_prepare_and_enter );
    ( "cli_pi_task_enter_revalidates_open_task_under_lock",
      test_pi_task_enter_revalidates_open_task_under_lock );
    ( "cli_pi_task_exact_resolution_collision",
      test_pi_task_exact_resolution_collision );
    ( "cli_pi_task_prepare_preflight_context_cleanup",
      test_pi_task_prepare_preflight_context_cleanup );
    ( "cli_pi_task_prepare_serializes_same_task",
      test_pi_task_prepare_serializes_same_task );
    ( "cli_start_loads_native_pi_extension",
      test_start_loads_native_pi_extension );
    ( "cli_parser_and_doctor_contracts", test_cli_parser_and_doctor_contracts ) ]
  |> List.iter (fun (name, test) -> run_named name test)
