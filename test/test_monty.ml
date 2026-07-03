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

let temp_root name =
  Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "monty-%s-%d" name (Unix.getpid ()))

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

let setup_git_worker root =
  let home = Filename.concat root "home" in
  let repo = Filename.concat root "repo" in
  let context = Filename.concat root "context.md" in
  let branch = "cto/" ^ Filename.basename root in
  Shell.ensure_dir home;
  init_git_repo repo;
  let worktree = must (Wt.create_or_reuse ~wt_command:"wt" ~repo ~branch) in
  Shell.write_file context "# Task\n";
  let job =
    Job.make ~id:"issue-123" ~branch ~title:"Fix issue 123" ~repo ~context ()
  in
  let _id, worker_dir, instructions =
    Worker_memory.ensure ~home ~job ~branch ~repo ~context ~worktree_mode:"always"
      ~last_known_worktree:(Some worktree)
  in
  (home, repo, branch, worktree, worker_dir, instructions)

let test_done_refuses_dirty_worktree () =
  let root = temp_root "done-dirty" in
  let home, repo, branch, worktree, worker_dir, _instructions = setup_git_worker root in
  Shell.write_file (Filename.concat worktree "dirty.txt") "dirty\n";
  (match Done.complete ~worker:"issue-123" ~home ~wt_command:"wt" ~force:false () with
  | Ok () -> failwith "expected dirty worktree to block done"
  | Error msg ->
      assert_contains "dirty error" msg "uncommitted or untracked";
      assert_bool "worker dir remains active" (Sys.file_exists worker_dir));
  must (Wt.force_clean ~worktree);
  must (Wt.delete_worktree_and_branch ~worktree ~wt_command:"wt" ~repo ~branch ~force:true ())

let test_done_force_archives () =
  let root = temp_root "done-force" in
  let home, repo, branch, worktree, worker_dir, _instructions = setup_git_worker root in
  Shell.write_file (Filename.concat worktree "dirty.txt") "dirty\n";
  must (Done.complete ~worker:"issue-123" ~home ~wt_command:"wt" ~force:true ());
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

let test_done_closes_legacy_local_task_matched_by_title () =
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
  assert_bool "legacy linked local task hidden after archive" (open_tasks = []);
  let all_tasks = must (Project_overview.load_tasks ~home ~all:true ()) in
  assert_contains "legacy linked local task done" (Project_overview.render_tasks all_tasks)
    ("local:" ^ task.Project_overview.id)

let test_resume_archived_reactivates () =
  let root = temp_root "resume-archived" in
  let home, _repo, _branch, _worktree, worker_dir, _instructions = setup_git_worker root in
  must (Done.complete ~worker:"issue-123" ~home ~wt_command:"wt" ~force:true ());
  (match Resume.find ~home "issue-123" with
  | Ok _ -> failwith "archived job should not be found by default resume"
  | Error _ -> ());
  let archived = must (Resume.find_record ~home ~scope:Job_store.Archived "issue-123") in
  let job = must (Job_store.reactivate archived) in
  assert_equal "reactivated worker dir" worker_dir
    (Option.value ~default:"" job.Job.worker_dir);
  let active = must (Resume.find ~home "issue-123") in
  assert_equal "active after reactivate" "Fix issue 123" active.Job.title

let test_launch_many_single_job_uses_single_job_defaults () =
  let root = temp_root "launch-many-single" in
  Shell.ensure_dir root;
  let context = Filename.concat root "context.md" in
  Shell.write_file context "# Task\n";
  let job = Job.make ~title:"Translate parking instructions" ~repo:root ~context () in
  let options =
    Launcher.{
      backend = Terminal.Dry_run;
      target = Terminal.Tab;
      pi_command = "pi";
      wt_command = "wt";
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
  let first = Job.make ~title:"First task" ~repo:root ~context () in
  let second = Job.make ~title:"Second task" ~repo:root ~context () in
  let options =
    Launcher.{
      backend = Terminal.Dry_run;
      target = Terminal.Tab;
      pi_command = "pi";
      wt_command = "wt";
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
  let home, _repo, _context, _worker_dir, _instructions = setup_worker root in
  let records = must (Job_store.load ~home ~scope:Job_store.Active) in
  let output = List_jobs.render records in
  assert_contains "list id" output "issue-123";
  assert_contains "list status" output "ACTIVE"

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
         ~title:"Design overview" ~priority:"high" ())
  in
  assert_equal "local task id" "local-001" task.Project_overview.id;
  let tasks = must (Project_overview.load_tasks ~home ()) in
  let rendered = Project_overview.render_tasks tasks in
  assert_contains "local task rendered" rendered "local:local-001";
  assert_contains "local priority rendered" rendered "high";
  must (Project_overview.set_priority ~home ~task:"local-001" ~priority:"low");
  let reprioritized = must (Project_overview.load_tasks ~home ()) in
  assert_contains "local priority override rendered"
    (Project_overview.render_tasks reprioritized) "low";
  must (Project_overview.done_local_task ~home "local-001");
  let open_tasks = must (Project_overview.load_tasks ~home ()) in
  assert_bool "done local task hidden" (open_tasks = []);
  let all_tasks = must (Project_overview.load_tasks ~home ~all:true ()) in
  assert_contains "done task visible with all" (Project_overview.render_tasks all_tasks) "done";
  let overview = must (Project_overview.overview ~home) in
  assert_contains "overview projects" overview "## Projects";
  assert_contains "overview active jobs" overview "## Active jobs"

let () =
  test_slug ();
  test_shell_quote ();
  test_manifest ();
  test_worker_memory_and_resume ();
  test_wt_disambiguates_repo_when_branch_name_collides ();
  test_done_refuses_dirty_worktree ();
  test_done_force_archives ();
  test_done_closes_linked_local_task ();
  test_done_closes_legacy_local_task_matched_by_title ();
  test_resume_archived_reactivates ();
  test_launch_many_single_job_uses_single_job_defaults ();
  test_launch_many_multiple_jobs_keeps_numbered_defaults ();
  test_ghostty_tab_launch_focuses_new_terminal ();
  test_list_jobs_render ();
  test_project_overview_local_tasks ()
