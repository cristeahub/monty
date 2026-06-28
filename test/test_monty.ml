open Monty

let assert_equal label expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "%s: expected %S, got %S" label expected actual)

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
  let root = Filename.concat (Filename.get_temp_dir_name ()) ("monty-test-" ^ string_of_int (Unix.getpid ())) in
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

let test_worker_memory_and_resume () =
  let root = Filename.concat (Filename.get_temp_dir_name ()) ("monty-memory-test-" ^ string_of_int (Unix.getpid ())) in
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
      ~worktree_mode:"always" ~last_known_worktree:(Some (Filename.concat root "wt"))
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

let () =
  test_slug ();
  test_shell_quote ();
  test_manifest ();
  test_worker_memory_and_resume ()
