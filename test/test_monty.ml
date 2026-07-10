open Monty

let assert_equal label expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "%s: expected %S, got %S" label expected actual)

let assert_bool label value = if not value then failwith label

let string_contains haystack needle =
  let haystack_len = String.length haystack in
  let needle_len = String.length needle in
  let rec loop index =
    if needle_len = 0 then true
    else if index + needle_len > haystack_len then false
    else if String.sub haystack index needle_len = needle then true
    else loop (index + 1)
  in
  loop 0

let assert_contains label text expected =
  if not (string_contains text expected) then
    failwith (Printf.sprintf "%s: expected %S to contain %S" label text expected)

let assert_not_contains label text unexpected =
  if string_contains text unexpected then
    failwith (Printf.sprintf "%s: expected %S not to contain %S" label text unexpected)

let must = function Ok value -> value | Error msg -> failwith msg

let capture_stdout f =
  let path = Filename.temp_file "monty-test-stdout" ".txt" in
  let original_stdout = Unix.dup Unix.stdout in
  let output_fd = Unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600 in
  Fun.protect
    ~finally:(fun () ->
      flush stdout;
      Unix.dup2 original_stdout Unix.stdout;
      Unix.close original_stdout;
      if Sys.file_exists path then Sys.remove path)
    (fun () ->
      Unix.dup2 output_fd Unix.stdout;
      Unix.close output_fd;
      f ();
      flush stdout;
      Shell.read_file path)

let rec remove_tree path =
  try
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR ->
        Sys.readdir path
        |> Array.iter (fun name -> remove_tree (Filename.concat path name));
        Unix.rmdir path
    | _ -> Unix.unlink path
  with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> ()

let temp_roots = ref []
let () = at_exit (fun () -> List.iter remove_tree !temp_roots)

let temp_root name =
  let marker = Filename.temp_file ("monty-" ^ name ^ "-") ".tmp" in
  Sys.remove marker;
  Unix.mkdir marker 0o700;
  temp_roots := marker :: !temp_roots;
  marker

let test_slug () =
  assert_equal "slug" "fix-issue-123" (Slug.of_title "Fix issue #123");
  assert_equal "branch" "monty/02-fix-issue-123" (Slug.branch ~index:2 "Fix issue #123");
  assert_equal "custom branch prefix" "cto/02-fix-issue-123"
    (Slug.branch ~prefix:"cto" ~index:2 "Fix issue #123");
  assert_equal "trim branch prefix" "cto/02-fix-issue-123"
    (Slug.branch ~prefix:"/cto/" ~index:2 "Fix issue #123")

let test_shell_quote () =
  assert_equal "quote simple" "'hello'" (Shell.quote "hello");
  assert_equal "quote apostrophe" "'it'\\''s'" (Shell.quote "it's")

let test_manifest () =
  let root = temp_root "manifest" in
  let run_dir = Filename.concat root ".monty/runs/test" in
  Shell.ensure_dir run_dir;
  let context = Filename.concat run_dir "task.md" in
  Shell.write_file context "# Task\n";
  let manifest = Filename.concat run_dir "jobs.json" in
  Shell.write_file manifest
    "{\n  \"jobs\": [\n    {\n      \"title\": \"Task\",\n      \"repo\": \".\",\n      \"context\": \"task.md\",\n      \"task_key\": \"local:local-001\"\n    }\n  ]\n}\n";
  let old_cwd = Sys.getcwd () in
  Fun.protect
    ~finally:(fun () -> Sys.chdir old_cwd)
    (fun () ->
      Sys.chdir root;
      match Manifest.load manifest with
      | Error msg -> failwith msg
      | Ok [ (1, job) ] ->
          assert_equal "manifest title" "Task" job.Job.title;
          assert_equal "manifest context" context job.Job.context;
          assert_equal "manifest worker dir"
            (Filename.concat run_dir "workers/task")
            (Option.value ~default:"" job.Job.worker_dir);
          assert_equal "manifest task key" "local:local-001"
            (Option.value ~default:"" job.Job.task_key)
      | Ok _ -> failwith "expected exactly one job")

let setup_worker ?(last_known_worktree = None) root =
  let home = Filename.concat root "home" in
  let repo = Filename.concat root "repo" in
  Shell.ensure_dir home;
  Shell.ensure_dir repo;
  let context = Filename.concat root "context.md" in
  Shell.write_file context "# Task\n";
  let job =
    Job.make ~id:"issue-123" ~branch:"cto/issue-123" ~title:"Fix issue 123"
      ~repo ~context ()
  in
  let _id, worker_dir, instructions =
    Worker_memory.ensure ~home ~job ~branch:"cto/issue-123" ~repo ~context
      ~worktree_mode:"always" ~last_known_worktree
  in
  (home, repo, context, worker_dir, instructions)

let test_worker_memory_and_resume () =
  let root = temp_root "memory" in
  let home, _repo, _context, worker_dir, instructions =
    setup_worker ~last_known_worktree:(Some (Filename.concat root "wt")) root
  in
  if not (Sys.file_exists (Worker_memory.job_file worker_dir)) then
    failwith "expected job.json";
  if not (Sys.file_exists instructions) then failwith "expected MONTY.md";
  match Resume.find ~home "issue-123" with
  | Error msg -> failwith msg
  | Ok found ->
      assert_equal "resume title" "Fix issue 123" found.Job.title;
      assert_equal "resume branch" "cto/issue-123"
        (Option.value ~default:"" found.Job.branch)

let init_git_repo path =
  Shell.ensure_dir path;
  must (Process.run_quiet ~cwd:path "git init -q");
  must (Process.run_quiet ~cwd:path "git config user.email test@example.com");
  must (Process.run_quiet ~cwd:path "git config user.name Test");
  Shell.write_file (Filename.concat path "tracked.txt") "initial\n";
  must (Process.run_quiet ~cwd:path "git add tracked.txt");
  must (Process.run_quiet ~cwd:path "git commit -q -m initial")

let fake_prompting_wt ~dir ~branch ~repo_one ~repo_two =
  let path = Filename.concat dir "fake-wt" in
  Shell.write_file path
    (String.concat "\n"
       [ "#!/bin/sh";
         "selection=$(cat || true)";
         "case \"$1\" in";
         "  b)";
         "    if [ \"$selection\" = 2 ]; then";
         "      printf '%s\\n' " ^ Shell.quote repo_two;
         "      exit 0";
         "    fi";
         "    printf '%s\\n' " ^ Shell.quote ("Branch '" ^ branch ^ "' exists in multiple repos:") ^ " >&2";
         "    printf '%s\\n' " ^ Shell.quote ("  1) repo-one -> " ^ repo_one) ^ " >&2";
         "    printf '%s\\n' " ^ Shell.quote ("  2) repo-two -> " ^ repo_two) ^ " >&2";
         "    printf '%s' 'Select [1-2]: ' >&2";
         "    exit 2";
         "    ;;";
         "  *) exit 2 ;;";
         "esac";
         "" ]);
  Shell.chmod_executable path;
  path

let test_wt_disambiguates_repo_when_branch_name_collides () =
  let root = temp_root "wt-disambiguate" in
  let repo_one = Filename.concat root "repo-one" in
  let repo_two = Filename.concat root "repo-two" in
  let branch = "same-name" in
  init_git_repo repo_one;
  init_git_repo repo_two;
  let wt_command = fake_prompting_wt ~dir:root ~branch ~repo_one ~repo_two in
  let selected = must (Wt.create_or_reuse ~wt_command ~repo:repo_two ~branch) in
  assert_equal "selected repo" (Unix.realpath repo_two) selected;
  ignore (must (Wt.validate_worktree ~repo:repo_two selected))

let fake_git_wt root =
  let path = Filename.concat root "fake-wt" in
  let worktree = Filename.concat root "fake-worktree" in
  Shell.write_file path
    (String.concat "\n"
       [ "#!/bin/sh";
         "set -eu";
         "cmd=$1";
         "branch=${2:-}";
         "worktree=" ^ Shell.quote worktree;
         "case \"$cmd\" in";
         "  b)";
         "    if [ ! -d \"$worktree\" ]; then";
         "      git show-ref --verify --quiet \"refs/heads/$branch\" || git branch \"$branch\"";
         "      git worktree add -q \"$worktree\" \"$branch\"";
         "    fi";
         "    printf '%s\\n' \"$worktree\"";
         "    ;;";
         "  list)";
         "    printf 'repo:\\n'";
         "    if [ -d \"$worktree\" ]; then printf '  %s -> %s\\n' \"$branch\" \"$worktree\"; fi";
         "    ;;";
         "  db)";
         "    if [ -d \"$worktree\" ]; then git worktree remove --force \"$worktree\"; fi";
         "    git show-ref --verify --quiet \"refs/heads/$branch\" && git branch -D \"$branch\" >/dev/null || true";
         "    ;;";
         "  *) exit 2 ;;";
         "esac";
         "" ]);
  Shell.chmod_executable path;
  path

let setup_git_worker root =
  let home = Filename.concat root "home" in
  let repo = Filename.concat root "repo" in
  let context = Filename.concat root "context.md" in
  let branch = "cto/" ^ Filename.basename root in
  Shell.ensure_dir home;
  init_git_repo repo;
  let wt_command = fake_git_wt root in
  let worktree = must (Wt.create_or_reuse ~wt_command ~repo ~branch) in
  Shell.write_file context "# Task\n";
  let job =
    Job.make ~id:"issue-123" ~branch ~title:"Fix issue 123" ~repo ~context ()
  in
  let _id, worker_dir, instructions =
    Worker_memory.ensure ~home ~job ~branch ~repo ~context ~worktree_mode:"always"
      ~last_known_worktree:(Some worktree)
  in
  (home, repo, branch, worktree, worker_dir, instructions, wt_command)

let test_done_refuses_dirty_worktree () =
  let root = temp_root "done-dirty" in
  let home, repo, branch, worktree, worker_dir, _instructions, wt_command = setup_git_worker root in
  Shell.write_file (Filename.concat worktree "dirty.txt") "dirty\n";
  (match Done.complete ~worker:"issue-123" ~home ~wt_command ~force:false () with
  | Ok () -> failwith "expected dirty worktree to block done"
  | Error msg ->
      assert_contains "dirty error" msg "uncommitted or untracked";
      assert_bool "worker dir remains active" (Sys.file_exists worker_dir));
  must (Wt.force_clean ~worktree);
  must (Wt.delete_worktree_and_branch ~worktree ~wt_command ~repo ~branch ~force:true ())

let test_done_force_archives () =
  let root = temp_root "done-force" in
  let home, repo, branch, worktree, worker_dir, _instructions, wt_command = setup_git_worker root in
  Shell.write_file (Filename.concat worktree "dirty.txt") "dirty\n";
  must (Done.complete ~worker:"issue-123" ~home ~wt_command ~force:true ());
  let archive_dir =
    Filename.concat
      (Filename.concat (Filename.dirname (Filename.dirname worker_dir)) "archive")
      "issue-123"
  in
  assert_bool "worker dir moved" (not (Sys.file_exists worker_dir));
  assert_bool "archive dir exists" (Sys.file_exists archive_dir);
  assert_bool "worktree removed" (not (Sys.file_exists worktree));
  assert_bool "branch deleted" (not (Wt.branch_exists ~repo ~branch));
  let record = must (Job_store.parse_job_file (Filename.concat archive_dir "job.json")) in
  assert_equal "archived status" "done" record.Job_store.status;
  assert_equal "archived worker dir" archive_dir record.Job_store.worker_dir

let test_done_closes_linked_local_task () =
  let root = temp_root "done-local-task" in
  let home = Filename.concat root "home" in
  let repo = Filename.concat root "repo" in
  let context = Filename.concat root "context.md" in
  Shell.ensure_dir home;
  Shell.ensure_dir repo;
  Shell.write_file context "# Task\n";
  let _project = must (Project_overview.add_project ~home ~repo ()) in
  let task =
    must
      (Project_overview.add_local_task ~home ~project:"repo" ~title:"Fix local task" ())
  in
  let job =
    Job.make ~id:(task.Project_overview.id ^ "-fix-local-task")
      ~task_key:("local:" ^ task.Project_overview.id)
      ~branch:"cto/fix-local-task" ~title:"Fix local task" ~repo ~context ()
  in
  let _id, worker_dir, _instructions =
    Worker_memory.ensure ~home ~job ~branch:"cto/fix-local-task" ~repo ~context
      ~worktree_mode:"never" ~last_known_worktree:None
  in
  must (Done.complete ~worker:(task.Project_overview.id ^ "-fix-local-task") ~home
          ~wt_command:"wt" ~force:false ());
  let archive_dir =
    Filename.concat
      (Filename.concat (Filename.dirname (Filename.dirname worker_dir)) "archive")
      (task.Project_overview.id ^ "-fix-local-task")
  in
  assert_bool "worker dir moved" (not (Sys.file_exists worker_dir));
  assert_bool "archive dir exists" (Sys.file_exists archive_dir);
  let archived = must (Job_store.parse_job_file (Filename.concat archive_dir "job.json")) in
  assert_equal "archived task key" ("local:" ^ task.Project_overview.id)
    (Option.value ~default:"" archived.Job_store.job.Job.task_key);
  let open_tasks = must (Project_overview.load_tasks ~home ()) in
  assert_bool "linked local task hidden after archive" (open_tasks = []);
  let all_tasks = must (Project_overview.load_tasks ~home ~all:true ()) in
  assert_contains "linked local task done" (Project_overview.render_tasks all_tasks) "done"

let test_done_does_not_infer_legacy_local_task_by_title () =
  let root = temp_root "done-local-task-title" in
  let home = Filename.concat root "home" in
  let repo = Filename.concat root "repo" in
  let context = Filename.concat root "context.md" in
  Shell.ensure_dir home;
  Shell.ensure_dir repo;
  Shell.write_file context "# Task\n";
  let _project = must (Project_overview.add_project ~home ~repo ()) in
  let task =
    must
      (Project_overview.add_local_task ~home ~project:"repo" ~title:"Continue cto/legacy" ())
  in
  let job =
    Job.make ~id:"legacy-worker" ~branch:"cto/legacy" ~title:task.Project_overview.title
      ~repo ~context ()
  in
  let _id, worker_dir, _instructions =
    Worker_memory.ensure ~home ~job ~branch:"cto/legacy" ~repo ~context
      ~worktree_mode:"never" ~last_known_worktree:None
  in
  must (Done.complete ~worker:"legacy-worker" ~home ~wt_command:"wt" ~force:false ());
  assert_bool "worker dir moved" (not (Sys.file_exists worker_dir));
  let open_tasks = must (Project_overview.load_tasks ~home ()) in
  assert_equal "legacy task remains open without explicit repair" "open"
    (List.hd open_tasks).Project_overview.status;
  let archived = must (Job_store.find ~home ~scope:Job_store.Archived "legacy-worker") in
  assert_bool "ordinary done leaves legacy worker unlinked"
    (archived.Job_store.job.Job.task_key = None)

let test_resume_archived_reactivates () =
  let root = temp_root "resume-archived" in
  let home, _repo, _branch, _worktree, worker_dir, _instructions, wt_command = setup_git_worker root in
  must (Done.complete ~worker:"issue-123" ~home ~wt_command ~force:true ());
  (match Resume.find ~home "issue-123" with
  | Ok _ -> failwith "archived job should not be found by default resume"
  | Error _ -> ());
  let archived = must (Resume.find_record ~home ~scope:Job_store.Archived "issue-123") in
  let job = must (Resume.reactivate ~home archived) in
  assert_equal "reactivated worker dir" worker_dir
    (Option.value ~default:"" job.Job.worker_dir);
  let active = must (Resume.find ~home "issue-123") in
  assert_equal "active after reactivate" "Fix issue 123" active.Job.title

let test_launch_many_single_job_uses_single_job_defaults () =
  let root = temp_root "launch-many-single" in
  Shell.ensure_dir root;
  let context = Filename.concat root "context.md" in
  Shell.write_file context "# Task\n";
  let _project = must (Project_overview.add_project ~home:root ~repo:root ()) in
  let job = Job.make ~title:"Translate parking instructions" ~repo:root ~context () in
  let options =
    Launcher.{
      backend = Terminal.Dry_run;
      target = Terminal.Tab;
      pi_command = "/usr/bin/true --pi-test";
      wt_command = "/usr/bin/true --wt-test";
      worktree_mode = Always;
      branch_prefix = "cto";
      fork = None;
      home = root;
      script_dir = root;
      monty_command = "monty";
    }
  in
  let output = capture_stdout (fun () -> must (Launcher.launch_many options [ (1, job) ])) in
  assert_contains "single launch-many branch" output "--branch 'cto/translate-parking-instructions'";
  assert_not_contains "single launch-many should not number branch" output "cto/01-translate-parking-instructions"

let test_launch_many_multiple_jobs_keeps_numbered_defaults () =
  let root = temp_root "launch-many-multiple" in
  Shell.ensure_dir root;
  let context = Filename.concat root "context.md" in
  Shell.write_file context "# Task\n";
  let _project = must (Project_overview.add_project ~home:root ~repo:root ()) in
  let first = Job.make ~title:"First task" ~repo:root ~context () in
  let second = Job.make ~title:"Second task" ~repo:root ~context () in
  let options =
    Launcher.{
      backend = Terminal.Dry_run;
      target = Terminal.Tab;
      pi_command = "/usr/bin/true --pi-test";
      wt_command = "/usr/bin/true --wt-test";
      worktree_mode = Always;
      branch_prefix = "cto";
      fork = None;
      home = root;
      script_dir = root;
      monty_command = "monty";
    }
  in
  let output = capture_stdout (fun () -> must (Launcher.launch_many options [ (1, first); (2, second) ])) in
  assert_contains "first numbered branch" output "--branch 'cto/01-first-task'";
  assert_contains "second numbered branch" output "--branch 'cto/02-second-task'"

let test_ghostty_tab_launch_focuses_new_terminal () =
  let script = Ghostty.applescript ~target:Terminal.Tab ~workdir:"/tmp" ~script_path:"/tmp/monty.sh" in
  assert_contains "tab launch selects tab" script "select tab montyTab";
  assert_contains "tab launch focuses terminal" script "focus focused terminal of montyTab"

let test_list_jobs_render () =
  let root = temp_root "list" in
  let home, repo, _context, _worker_dir, _instructions = setup_worker root in
  let _project = must (Project_overview.add_project ~home ~repo ()) in
  let output = capture_stdout (fun () -> must (List_jobs.run ~home ~scope:Job_store.Active ())) in
  assert_contains "list worker id" output "issue-123";
  assert_not_contains "list should not show linked local task id" output "local:local-001";
  assert_contains "list project" output "repo";
  assert_contains "list status" output "open";
  assert_contains "list branch" output "cto/issue-123";
  assert_not_contains "list should not use job-only status" output "ACTIVE"

let test_tasks_sync_jobs_to_local_source () =
  let root = temp_root "tasks-sync" in
  let home = Filename.concat root "home" in
  let repo = Filename.concat root "repo" in
  let context = Filename.concat root "context.md" in
  Shell.ensure_dir home;
  Shell.ensure_dir repo;
  Shell.write_file context "# Task\n";
  let _project = must (Project_overview.add_project ~home ~repo ()) in
  let job =
    Job.make ~id:"issue-5250-localize-invoice-english"
      ~branch:"cto/5250-localize-invoice-english"
      ~title:"Issue 5250 - Localize invoice to English" ~repo ~context ()
  in
  let _id, worker_dir, _instructions =
    Worker_memory.ensure ~home ~job ~branch:"cto/5250-localize-invoice-english" ~repo
      ~context ~worktree_mode:"never" ~last_known_worktree:None
  in
  assert_bool "worker memory created" (Sys.file_exists worker_dir);
  let result = must (Project_overview.sync_jobs_to_local_tasks ~home) in
  assert_equal "sync created" "1" (string_of_int result.Project_overview.created);
  assert_equal "sync linked" "1" (string_of_int result.linked_jobs);
  let tasks = must (Project_overview.load_tasks ~home ()) in
  let rendered = Project_overview.render_tasks tasks in
  assert_contains "synced task title" rendered "Issue 5250 - Localize invoice to English";
  assert_contains "synced task branch" rendered "cto/5250-localize-invoice-english";
  assert_not_contains "synced task list hides local task id" rendered "local:local-001";
  let record = must (Job_store.parse_job_file (Filename.concat worker_dir "job.json")) in
  assert_equal "job task key" "local:local-001"
    (Option.value ~default:"" record.Job_store.job.Job.task_key);
  let second = must (Project_overview.sync_jobs_to_local_tasks ~home) in
  assert_equal "second sync created" "0" (string_of_int second.Project_overview.created);
  assert_equal "second sync updated" "0" (string_of_int second.updated);
  assert_equal "second sync linked" "0" (string_of_int second.linked_jobs)

let test_project_overview_local_tasks () =
  let root = temp_root "projects" in
  let home = Filename.concat root "home" in
  let repo = Filename.concat root "monty" in
  Shell.ensure_dir repo;
  let project = must (Project_overview.add_project ~home ~repo ()) in
  assert_equal "project id" "monty" project.Project_overview.id;
  assert_bool "project memory exists"
    (Sys.file_exists (Project_overview.project_memory_file ~home "monty"));
  let task =
    must
      (Project_overview.add_local_task ~home ~project:"monty"
         ~title:"Design overview" ())
  in
  assert_equal "local task id" "local-001" task.Project_overview.id;
  let tasks = must (Project_overview.load_tasks ~home ()) in
  let rendered = Project_overview.render_tasks tasks in
  assert_contains "local task rendered" rendered "local:local-001";
  must (Project_overview.done_local_task ~home "local-001");
  let open_tasks = must (Project_overview.load_tasks ~home ()) in
  assert_bool "done local task hidden" (open_tasks = []);
  let all_tasks = must (Project_overview.load_tasks ~home ~all:true ()) in
  assert_contains "done task visible with all" (Project_overview.render_tasks all_tasks) "done";
  let overview = must (Project_overview.overview ~home) in
  assert_contains "overview projects" overview "## Projects";
  assert_contains "overview active jobs" overview "## Active jobs"

let test_state_path_safe_components () =
  List.iter
    (fun value ->
      match State_path.safe_component ~label:"test id" value with
      | Ok _ -> failwith (Printf.sprintf "expected unsafe component %S to fail" value)
      | Error _ -> ())
    [ ""; "."; ".."; "../escape"; "a/b"; "a\000b"; " worker"; "worker " ];
  assert_equal "safe component" "local-001.worker"
    (must (State_path.safe_component ~label:"test id" "local-001.worker"))

let test_atomic_failure_before_rename_preserves_previous_json () =
  let root = temp_root "atomic-failure" in
  let home = Filename.concat root "home" in
  let path = Filename.concat home ".monty/tasks.local.json" in
  let previous = "{\"tasks\":[{\"id\":\"local-001\"}]}\n" in
  Shell.write_file path previous;
  State_store.set_before_rename_hook (fun () -> Error "injected before rename");
  Fun.protect
    ~finally:State_store.reset_before_rename_hook
    (fun () ->
      match State_store.write_json ~home ~path (`Assoc [ ("tasks", `List []) ]) with
      | Ok () -> failwith "expected injected atomic write failure"
      | Error msg -> assert_contains "fault error" msg "injected before rename");
  assert_equal "previous JSON bytes" previous (Shell.read_file path);
  let temp_files =
    Sys.readdir (Filename.dirname path) |> Array.to_list
    |> List.filter (fun name -> string_contains name "monty-tmp")
  in
  assert_bool "temporary JSON file cleaned" (temp_files = [])

let test_atomic_success_preserves_permissions_and_cleans_temp () =
  let root = temp_root "atomic-success" in
  let home = Filename.concat root "home" in
  let path = Filename.concat home ".monty/tasks.local.json" in
  Shell.write_file path "{\"tasks\":[]}\n";
  Unix.chmod path 0o640;
  must
    (State_store.write_json ~home ~path
       (`Assoc [ ("tasks", `List [ `Assoc [ ("id", `String "local-001") ] ]) ]));
  let mode = (Unix.stat path).Unix.st_perm land 0o777 in
  assert_equal "atomic permissions" "416" (string_of_int mode);
  ignore (Yojson.Safe.from_file path);
  let temp_files =
    Sys.readdir (Filename.dirname path) |> Array.to_list
    |> List.filter (fun name -> string_contains name "monty-tmp")
  in
  assert_bool "successful atomic write cleaned temp" (temp_files = [])

let write_legacy_job path ~id ~repo ~context extra =
  Shell.ensure_dir (Filename.dirname path);
  let fields =
    [ ("id", `String id);
      ("title", `String "Legacy task");
      ("repo", `String repo);
      ("branch", `String "cto/legacy");
      ("context", `String context) ]
    @ extra
  in
  Yojson.Safe.to_file path (`Assoc fields)

let test_job_store_uses_physical_canonical_paths () =
  let root = temp_root "job-paths" in
  let home = Filename.concat root "home" in
  let repo = Filename.concat root "repo" in
  let context = Filename.concat root "context.md" in
  Shell.ensure_dir repo;
  Shell.write_file context "# Context\n";
  let state = must (State_path.active ~home ~run_id:"run-1" ~id:"worker-1") in
  write_legacy_job state.State_path.job_file ~id:"worker-1" ~repo ~context [];
  let record = must (Job_store.parse_job_file ~home state.job_file) in
  assert_equal "derived worker dir" state.worker_dir record.Job_store.worker_dir;
  assert_equal "derived run dir" state.run_dir record.Job_store.run_dir;
  assert_equal "legacy default status" "active" record.Job_store.status;
  write_legacy_job state.job_file ~id:"worker-1" ~repo ~context
    [ ("worker_dir", `String (Filename.concat root "outside")) ];
  (match Job_store.parse_job_file ~home state.job_file with
  | Ok _ -> failwith "expected persisted worker path mismatch to fail"
  | Error msg -> assert_contains "worker path mismatch" msg "unsafe persisted worker_dir");
  write_legacy_job state.job_file ~id:"worker-1" ~repo ~context
    [ ("run_dir", `String (Filename.concat root "outside-run")) ];
  (match Job_store.parse_job_file ~home state.job_file with
  | Ok _ -> failwith "expected persisted run path mismatch to fail"
  | Error msg -> assert_contains "run path mismatch" msg "unsafe persisted run_dir");
  write_legacy_job state.job_file ~id:"other-worker" ~repo ~context [];
  (match Job_store.parse_job_file ~home state.job_file with
  | Ok _ -> failwith "expected persisted id mismatch to fail"
  | Error msg -> assert_contains "id mismatch" msg "does not match physical path id")

let test_archived_legacy_job_uses_physical_classification () =
  let root = temp_root "archived-physical" in
  let home = Filename.concat root "home" in
  let repo = Filename.concat root "repo" in
  let context = Filename.concat root "context.md" in
  Shell.ensure_dir repo;
  Shell.write_file context "# Context\n";
  let state = must (State_path.archived ~home ~run_id:"run-1" ~id:"worker-1") in
  write_legacy_job state.State_path.job_file ~id:"worker-1" ~repo ~context [];
  let record = must (Job_store.parse_job_file ~home state.job_file) in
  assert_equal "archived legacy default status" "done" record.Job_store.status;
  assert_bool "physical archive classification" (Job_store.is_archived record)

let test_archive_destination_rejects_symlink_escape () =
  let root = temp_root "archive-symlink" in
  let home = Filename.concat root "home" in
  let outside = Filename.concat root "outside" in
  let archive = Filename.concat home ".monty/runs/run-1/archive" in
  Shell.ensure_dir archive;
  Shell.ensure_dir outside;
  Unix.symlink outside (Filename.concat archive "worker-1");
  match State_path.archived ~home ~run_id:"run-1" ~id:"worker-1" with
  | Ok _ -> failwith "expected archive destination symlink to fail"
  | Error msg -> assert_contains "archive symlink" msg "symlink alias"

let test_transition_task_key_mismatch_is_rejected () =
  let root = temp_root "transition-task-key" in
  let home = Filename.concat root "home" in
  let repo = Filename.concat root "repo" in
  let context = Filename.concat root "context.md" in
  Shell.ensure_dir repo;
  Shell.write_file context "# Context\n";
  let active = must (State_path.active ~home ~run_id:"run-1" ~id:"worker-1") in
  let archived = must (State_path.archived ~home ~run_id:"run-1" ~id:"worker-1") in
  Shell.ensure_dir active.worker_dir;
  Yojson.Safe.to_file active.job_file
    (`Assoc
      [ ("id", `String "worker-1");
        ("title", `String "Transition task mismatch");
        ("repo", `String repo);
        ("branch", `String "cto/worker-1");
        ("context", `String context);
        ("worker_dir", `String active.worker_dir);
        ("run_dir", `String active.run_dir);
        ("task_key", `String "local:local-001");
        ("status", `String "completing");
        ( "transition",
          `Assoc
            [ ("operation", `String "complete");
              ("source", `String active.worker_dir);
              ("target", `String archived.worker_dir);
              ("task_key", `String "local:local-002");
              ("force", `Bool false);
              ("started_at", `String "2026-07-10T00:00:00Z") ] ) ]);
  match Job_store.parse_job_file ~home active.job_file with
  | Ok _ -> failwith "transition task key mismatch unexpectedly parsed"
  | Error msg -> assert_contains "transition task mismatch" msg "does not match top-level"

let test_job_store_rejects_symlink_escape () =
  let root = temp_root "job-symlink" in
  let home = Filename.concat root "home" in
  let repo = Filename.concat root "repo" in
  let context = Filename.concat root "context.md" in
  let outside = Filename.concat root "outside" in
  Shell.ensure_dir repo;
  Shell.ensure_dir outside;
  Shell.write_file context "# Context\n";
  write_legacy_job (Filename.concat outside "job.json") ~id:"worker-1" ~repo
    ~context [];
  let workers = Filename.concat home ".monty/runs/run-1/workers" in
  Shell.ensure_dir workers;
  let link = Filename.concat workers "worker-1" in
  Unix.symlink outside link;
  (match Job_store.parse_job_file ~home (Filename.concat link "job.json") with
  | Ok _ -> failwith "expected symlink escape to fail"
  | Error msg -> assert_contains "symlink escape" msg "symlink alias");
  match Job_store.load ~home ~scope:Job_store.All with
  | Ok _ -> failwith "expected discovery to reject a symlinked worker directory"
  | Error msg -> assert_contains "discovery symlink" msg "job discovery will not traverse"

let test_doctor_typed_checks_and_configuration () =
  let home = temp_root "doctor" in
  let operations =
    Doctor.
      {
        find_command =
          (fun command ->
            if List.mem command [ "pi --fixed"; "gh" ] then Ok ("/fake/" ^ command)
            else Error ("missing " ^ command));
      }
  in
  let dry_checks =
    Doctor.checks ~operations ~home ~pi_command:"pi --fixed" ~wt_command:"missing-wt"
      ~backend:Terminal.Dry_run ~worktree_mode:Launcher.Never ()
  in
  assert_bool "dry-run doctor has no required failure"
    (Doctor.exit_code dry_checks = 0);
  let dry_output = Doctor.render dry_checks in
  assert_contains "doctor pass" dry_output "PASS";
  assert_contains "doctor warn" dry_output "WARN";
  assert_not_contains "doctor dry-run ignores wt" dry_output "missing-wt";
  let real_checks =
    Doctor.checks ~operations ~home ~pi_command:"pi --fixed" ~wt_command:"missing-wt"
      ~backend:Terminal.Ghostty ~worktree_mode:Launcher.Always ()
  in
  assert_bool "real doctor fails required dependencies"
    (Doctor.exit_code real_checks = 1);
  let real_output = Doctor.render real_checks in
  assert_contains "doctor fail" real_output "FAIL";
  assert_contains "doctor configured wt" real_output "missing-wt";
  assert_contains "doctor recovery" real_output "Recovery:"

let test_cli_factory_injects_environment_and_dispatch () =
  let captured = ref [] in
  let operations =
    Cli.
      {
        default_operations with
        launch_one =
          (fun options job ->
            captured := (options, job) :: !captured;
            Ok ());
      }
  in
  let getenv values name = List.assoc_opt name values in
  let run suffix backend target worktree =
    let home = temp_root ("cli-factory-" ^ suffix) in
    let values =
      [ ("MONTY_HOME", home);
        ("MONTY_TERMINAL", backend);
        ("MONTY_TARGET", target);
        ("MONTY_WORKTREE", worktree);
        ("MONTY_PI_COMMAND", "pi-" ^ suffix ^ " --fixed");
        ("MONTY_WT_COMMAND", "wt-" ^ suffix ^ " --fixed");
        ("MONTY_BRANCH_PREFIX", "branch-" ^ suffix) ]
    in
    let argv =
      [| "monty"; "launch"; "--repo"; "/tmp/repo"; "--title";
         ("Task " ^ suffix); "--context"; "/tmp/context.md" |]
    in
    assert_bool ("injected CLI dispatch " ^ suffix)
      (Cli.eval ~getenv:(getenv values) ~operations argv = 0)
  in
  run "one" "dry-run" "split" "never";
  run "two" "ghostty" "window" "always";
  match List.rev !captured with
  | [ (one, _); (two, _) ] ->
      assert_contains "factory first home" one.Launcher.home "monty-cli-factory-one";
      assert_contains "factory second home" two.Launcher.home "monty-cli-factory-two";
      assert_equal "factory first pi" "pi-one --fixed" one.pi_command;
      assert_equal "factory second pi" "pi-two --fixed" two.pi_command;
      assert_equal "factory first wt" "wt-one --fixed" one.wt_command;
      assert_equal "factory second wt" "wt-two --fixed" two.wt_command;
      assert_equal "factory first prefix" "branch-one" one.branch_prefix;
      assert_equal "factory second prefix" "branch-two" two.branch_prefix;
      assert_bool "factory first backend" (one.backend = Terminal.Dry_run);
      assert_bool "factory second backend" (two.backend = Terminal.Ghostty);
      assert_bool "factory first target" (one.target = Terminal.Split);
      assert_bool "factory second target" (two.target = Terminal.Window);
      assert_bool "factory first worktree" (one.worktree_mode = Launcher.Never);
      assert_bool "factory second worktree" (two.worktree_mode = Launcher.Always)
  | _ -> failwith "injected CLI launch operation was not called twice"

let run_named name test =
  try
    test ();
    Fmt.pr "PASS %s\n" name
  with exn -> failwith (Printf.sprintf "%s: %s" name (Printexc.to_string exn))

let () =
  [ ("slug", test_slug);
    ("shell_quote", test_shell_quote);
    ("manifest", test_manifest);
    ("worker_memory_and_resume", test_worker_memory_and_resume);
    ("wt_repo_disambiguation", test_wt_disambiguates_repo_when_branch_name_collides);
    ("done_refuses_dirty_worktree", test_done_refuses_dirty_worktree);
    ("done_force_archives", test_done_force_archives);
    ("done_closes_linked_local_task", test_done_closes_linked_local_task);
    ("done_does_not_infer_legacy_title", test_done_does_not_infer_legacy_local_task_by_title);
    ("resume_archived_reactivates", test_resume_archived_reactivates);
    ("launch_many_single_defaults", test_launch_many_single_job_uses_single_job_defaults);
    ("launch_many_multiple_defaults", test_launch_many_multiple_jobs_keeps_numbered_defaults);
    ("ghostty_tab_focus", test_ghostty_tab_launch_focuses_new_terminal);
    ("list_jobs_render", test_list_jobs_render);
    ("tasks_sync", test_tasks_sync_jobs_to_local_source);
    ("project_overview_local_tasks", test_project_overview_local_tasks);
    ("state_path_safe_components", test_state_path_safe_components);
    ("atomic_failure_preserves_json", test_atomic_failure_before_rename_preserves_previous_json);
    ("atomic_success_permissions", test_atomic_success_preserves_permissions_and_cleans_temp);
    ("job_physical_paths", test_job_store_uses_physical_canonical_paths);
    ("archived_physical_classification", test_archived_legacy_job_uses_physical_classification);
    ("archive_symlink_escape", test_archive_destination_rejects_symlink_escape);
    ("transition_task_key_mismatch", test_transition_task_key_mismatch_is_rejected);
    ("job_symlink_escape", test_job_store_rejects_symlink_escape);
    ("doctor_typed_configuration", test_doctor_typed_checks_and_configuration);
    ("cli_factory_injection", test_cli_factory_injects_environment_and_dispatch) ]
  |> List.iter (fun (name, test) -> run_named name test)
