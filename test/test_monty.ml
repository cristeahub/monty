open Monty

let assert_equal label expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "%s: expected %S, got %S" label expected actual)

let test_slug () =
  assert_equal "slug" "fix-issue-123" (Slug.of_title "Fix issue #123");
  assert_equal "branch" "monty/02-fix-issue-123" (Slug.branch ~index:2 "Fix issue #123")

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
          assert_equal "manifest context" context job.Job.context
      | Ok _ -> failwith "expected exactly one job")

let () =
  test_slug ();
  test_shell_quote ();
  test_manifest ()
