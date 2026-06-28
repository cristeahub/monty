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

let must = function Ok value -> value | Error msg -> failwith msg

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
    "{\n  \"jobs\": [\n    {\n      \"title\": \"Task\",\n      \"repo\": \".\",\n      \"context\": \"task.md\"\n    }\n  ]\n}\n";
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
            (Option.value ~default:"" job.Job.worker_dir)
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

let fake_wt ~dir ~worktree =
  let path = Filename.concat dir "fake-wt" in
  let log = Filename.concat dir "fake-wt.log" in
  Shell.write_file path
    (String.concat "\n"
       [ "#!/bin/sh";
         "printf '%s %s\\n' \"$1\" \"${2-}\" >> " ^ Shell.quote log;
         "case \"$1\" in";
         "  b) printf '%s\\n' " ^ Shell.quote worktree ^ ";;";
         "  db) exit 0 ;;";
         "  *) exit 2 ;;";
         "esac";
         "" ]);
  Shell.chmod_executable path;
  (path, log)

let test_done_refuses_dirty_worktree () =
  let root = temp_root "done-dirty" in
  let worktree = Filename.concat root "worktree" in
  init_git_repo worktree;
  Shell.write_file (Filename.concat worktree "dirty.txt") "dirty\n";
  let wt_command, _log = fake_wt ~dir:root ~worktree in
  let home, _repo, _context, worker_dir, _instructions =
    setup_worker ~last_known_worktree:(Some worktree) root
  in
  match Done.complete ~worker:"issue-123" ~home ~wt_command ~force:false () with
  | Ok () -> failwith "expected dirty worktree to block done"
  | Error msg ->
      assert_contains "dirty error" msg "uncommitted or untracked";
      assert_bool "worker dir remains active" (Sys.file_exists worker_dir)

let test_done_force_archives () =
  let root = temp_root "done-force" in
  let worktree = Filename.concat root "worktree" in
  init_git_repo worktree;
  Shell.write_file (Filename.concat worktree "dirty.txt") "dirty\n";
  let wt_command, log = fake_wt ~dir:root ~worktree in
  let home, _repo, _context, worker_dir, _instructions =
    setup_worker ~last_known_worktree:(Some worktree) root
  in
  must (Done.complete ~worker:"issue-123" ~home ~wt_command ~force:true ());
  let archive_dir =
    Filename.concat
      (Filename.concat (Filename.dirname (Filename.dirname worker_dir)) "archive")
      "issue-123"
  in
  assert_bool "worker dir moved" (not (Sys.file_exists worker_dir));
  assert_bool "archive dir exists" (Sys.file_exists archive_dir);
  let record = must (Job_store.parse_job_file (Filename.concat archive_dir "job.json")) in
  assert_equal "archived status" "done" record.Job_store.status;
  assert_equal "archived worker dir" archive_dir record.Job_store.worker_dir;
  assert_contains "wt db called" (Shell.read_file log) "db cto/issue-123"

let test_resume_archived_reactivates () =
  let root = temp_root "resume-archived" in
  let worktree = Filename.concat root "worktree" in
  init_git_repo worktree;
  let wt_command, _log = fake_wt ~dir:root ~worktree in
  let home, _repo, _context, worker_dir, _instructions =
    setup_worker ~last_known_worktree:(Some worktree) root
  in
  must (Done.complete ~worker:"issue-123" ~home ~wt_command ~force:true ());
  (match Resume.find ~home "issue-123" with
  | Ok _ -> failwith "archived job should not be found by default resume"
  | Error _ -> ());
  let archived = must (Resume.find_record ~home ~scope:Job_store.Archived "issue-123") in
  let job = must (Job_store.reactivate archived) in
  assert_equal "reactivated worker dir" worker_dir
    (Option.value ~default:"" job.Job.worker_dir);
  let active = must (Resume.find ~home "issue-123") in
  assert_equal "active after reactivate" "Fix issue 123" active.Job.title

let test_list_jobs_render () =
  let root = temp_root "list" in
  let home, _repo, _context, _worker_dir, _instructions = setup_worker root in
  let records = must (Job_store.load ~home ~scope:Job_store.Active) in
  let output = List_jobs.render records in
  assert_contains "list id" output "issue-123";
  assert_contains "list status" output "ACTIVE"

let () =
  test_slug ();
  test_shell_quote ();
  test_manifest ();
  test_worker_memory_and_resume ();
  test_done_refuses_dirty_worktree ();
  test_done_force_archives ();
  test_resume_archived_reactivates ();
  test_list_jobs_render ()
