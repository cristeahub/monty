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
      fix = Some "Verify findings and fix valid issues." }

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
              ( "fix",
                match profile.fix with
                | None -> `Null
                | Some fix -> instruction "fix.md" fix ) ] ) ])

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
      | Ok _ -> failwith "expected exactly one job");
  let backend = Filename.concat root "django-backend" in
  let admin = Filename.concat root "admin" in
  Shell.ensure_dir backend;
  Shell.ensure_dir admin;
  let multi = Filename.concat run_dir "multi.json" in
  Yojson.Safe.to_file multi
    (`Assoc
      [ ( "jobs",
          `List
            [ `Assoc
                [ ("title", `String "Multi task");
                  ("context", `String "task.md");
                  ( "workspaces",
                    `List
                      [ `Assoc
                          [ ("repo", `String backend);
                            ("branch", `String "cto/backend") ];
                        `Assoc [ ("repo", `String admin) ] ] ) ] ] ) ]);
  (match Manifest.load multi with
  | Ok [ (_, job) ] ->
      assert_equal "multi manifest workspace count" "2"
        (List.length job.Job.workspaces |> string_of_int);
      assert_equal "multi manifest first repo" backend
        (List.hd job.workspaces).repo;
      assert_bool "multi manifest omitted branch remains derivable"
        ((List.nth job.workspaces 1).branch = None)
  | Ok _ -> failwith "expected one multi-workspace manifest job"
  | Error msg -> failwith msg);
  let rejects label json needle =
    Yojson.Safe.to_file multi json;
    match Manifest.load multi with
    | Ok _ -> failwith (label ^ " unexpectedly succeeded")
    | Error msg -> assert_contains label msg needle
  in
  rejects "mixed manifest forms"
    (`Assoc
      [ ( "jobs",
          `List
            [ `Assoc
                [ ("title", `String "Mixed"); ("repo", `String backend);
                  ("context", `String context);
                  ( "workspaces",
                    `List [ `Assoc [ ("repo", `String admin) ] ] ) ] ] ) ])
    "either top-level repo/branch or workspaces";
  rejects "bare manifest array" (`List []) "object with a \"jobs\" array";
  rejects "legacy memory directory"
    (`Assoc
      [ ( "jobs",
          `List
            [ `Assoc
                [ ("title", `String "Legacy"); ("repo", `String backend);
                  ("context", `String context);
                  ("memory_dir", `String "/tmp/legacy") ] ] ) ])
    "legacy manifest field \"memory_dir\"";
  rejects "legacy task key"
    (`Assoc
      [ ( "jobs",
          `List
            [ `Assoc
                [ ("title", `String "Legacy"); ("repo", `String backend);
                  ("context", `String context);
                  ("task", `String "local:local-001") ] ] ) ])
    "legacy manifest field \"task\"";
  rejects "relative workspace repo"
    (`Assoc
      [ ( "jobs",
          `List
            [ `Assoc
                [ ("title", `String "Relative");
                  ("context", `String context);
                  ( "workspaces",
                    `List [ `Assoc [ ("repo", `String "../admin") ] ] ) ] ] ) ])
    "must be an absolute path"

let test_agent_profile_validation () =
  let expect_error label needle = function
    | Ok _ -> failwith (label ^ " unexpectedly succeeded")
    | Error message -> assert_contains label message needle
  in
  let bundled_home =
    Option.value ~default:(Sys.getcwd ()) (Sys.getenv_opt "DUNE_SOURCEROOT")
  in
  let bundled = must (Agent_profile.discover ~home:bundled_home) in
  assert_bool "bundled reviewed profile"
    (List.exists
       (fun (profile : Agent_profile.t) -> profile.id = "reviewed") bundled);
  assert_bool "bundled solo profile"
    (List.exists
       (fun (profile : Agent_profile.t) -> profile.id = "solo") bundled);
  let valid = temp_root "profiles-valid" in
  install_profile valid reviewed_profile;
  let profiles = must (Agent_profile.discover ~home:valid) in
  assert_equal "profile discovery count" "1"
    (List.length profiles |> string_of_int);
  let duplicate = temp_root "profiles-duplicate" in
  install_profile duplicate reviewed_profile;
  install_profile (Filename.concat duplicate ".monty") reviewed_profile;
  expect_error "duplicate profile" "duplicate agent profile id"
    (Agent_profile.discover ~home:duplicate);
  let bad_schema = temp_root "profiles-schema" in
  install_profile bad_schema reviewed_profile;
  Shell.write_file
    (Filename.concat bad_schema "agent-profiles/reviewed/profile.json")
    "{\"schema\":\"bad\"}\n";
  expect_error "profile schema" "unsupported agent profile schema"
    (Agent_profile.discover ~home:bad_schema);
  let missing = temp_root "profiles-missing" in
  install_profile missing reviewed_profile;
  Unix.unlink
    (Filename.concat missing "agent-profiles/reviewed/implementation.md");
  expect_error "missing profile instruction" "instruction is missing"
    (Agent_profile.discover ~home:missing);
  let symlink = temp_root "profiles-symlink" in
  install_profile symlink reviewed_profile;
  let interactive =
    Filename.concat symlink "agent-profiles/reviewed/interactive.md"
  in
  Unix.unlink interactive;
  Unix.symlink "/tmp" interactive;
  expect_error "profile instruction symlink" "instruction is a symlink"
    (Agent_profile.discover ~home:symlink);
  let duplicate_reviewer = temp_root "profiles-reviewer" in
  let duplicate_reviews =
    { reviewed_profile with
      reviews =
        [ List.hd reviewed_profile.reviews;
          List.hd reviewed_profile.reviews ] }
  in
  install_profile duplicate_reviewer duplicate_reviews;
  expect_error "duplicate reviewer" "duplicate reviewer ids"
    (Agent_profile.discover ~home:duplicate_reviewer);
  let invalid_stages = temp_root "profiles-stages" in
  install_profile invalid_stages
    { reviewed_profile with reviews = []; fix = Some "Fix without reviews." };
  expect_error "invalid profile stages" "fix stage without reviewers"
    (Agent_profile.discover ~home:invalid_stages)

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
    Worker_memory.ensure ~home ~job ~profile:reviewed_profile
      ~branch:"cto/issue-123" ~repo ~context
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
  List.iter (fun repo ->
      must (Process.run_quiet ~cwd:repo
              ("git checkout -qb " ^ Shell.quote branch))) [ repo_one; repo_two ];
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
    Worker_memory.ensure ~home ~job ~profile:reviewed_profile ~branch ~repo
      ~context ~worktree_mode:"always"
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
    Job.make ~id:(task.Overview_types.id ^ "-fix-local-task")
      ~task_key:("local:" ^ task.Overview_types.id)
      ~branch:"cto/fix-local-task" ~title:"Fix local task" ~repo ~context ()
  in
  let _id, worker_dir, _instructions =
    Worker_memory.ensure ~home ~job ~profile:reviewed_profile
      ~branch:"cto/fix-local-task" ~repo ~context
      ~worktree_mode:"never" ~last_known_worktree:None
  in
  must (Done.complete ~worker:(task.Overview_types.id ^ "-fix-local-task") ~home
          ~wt_command:"wt" ~force:false ());
  let archive_dir =
    Filename.concat
      (Filename.concat (Filename.dirname (Filename.dirname worker_dir)) "archive")
      (task.Overview_types.id ^ "-fix-local-task")
  in
  assert_bool "worker dir moved" (not (Sys.file_exists worker_dir));
  assert_bool "archive dir exists" (Sys.file_exists archive_dir);
  let archived = must (Job_store.parse_job_file (Filename.concat archive_dir "job.json")) in
  assert_equal "archived task key" ("local:" ^ task.Overview_types.id)
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
    Job.make ~id:"legacy-worker" ~branch:"cto/legacy" ~title:task.Overview_types.title
      ~repo ~context ()
  in
  let _id, worker_dir, _instructions =
    Worker_memory.ensure ~home ~job ~profile:reviewed_profile
      ~branch:"cto/legacy" ~repo ~context
      ~worktree_mode:"never" ~last_known_worktree:None
  in
  must (Done.complete ~worker:"legacy-worker" ~home ~wt_command:"wt" ~force:false ());
  assert_bool "worker dir moved" (not (Sys.file_exists worker_dir));
  let open_tasks = must (Project_overview.load_tasks ~home ()) in
  assert_equal "legacy task remains open without explicit repair" "open"
    (List.hd open_tasks).Overview_types.status;
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
  install_profile root reviewed_profile;
  Shell.ensure_dir root;
  let context = Filename.concat root "context.md" in
  Shell.write_file context "# Task\n";
  let _project = must (Project_overview.add_project ~home:root ~repo:root ()) in
  let job = Job.make ~title:"Translate parking instructions" ~repo:root ~context () in
  let options =
    Launcher.{
      backend = Terminal.Dry_run;
      target = Terminal.Tab;
      harness = Harness.Pi;
      harness_command = "/usr/bin/true --pi-test";
      codex_yolo = false;
      wt_command = "/usr/bin/true --wt-test";
      worktree_mode = Always;
      branch_prefix = "cto";
      agent_profile = "reviewed";
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
  install_profile root reviewed_profile;
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
      harness = Harness.Pi;
      harness_command = "/usr/bin/true --pi-test";
      codex_yolo = false;
      wt_command = "/usr/bin/true --wt-test";
      worktree_mode = Always;
      branch_prefix = "cto";
      agent_profile = "reviewed";
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
    Worker_memory.ensure ~home ~job ~profile:reviewed_profile
      ~branch:"cto/5250-localize-invoice-english" ~repo
      ~context ~worktree_mode:"never" ~last_known_worktree:None
  in
  assert_bool "worker memory created" (Sys.file_exists worker_dir);
  let result = must (Project_overview.sync_jobs_to_local_tasks ~home) in
  assert_equal "sync created" "1" (string_of_int result.Overview_types.created);
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
  assert_equal "second sync created" "0" (string_of_int second.Overview_types.created);
  assert_equal "second sync updated" "0" (string_of_int second.updated);
  assert_equal "second sync linked" "0" (string_of_int second.linked_jobs)

let test_project_overview_local_tasks () =
  let root = temp_root "projects" in
  let home = Filename.concat root "home" in
  let repo = Filename.concat root "monty" in
  Shell.ensure_dir repo;
  let project = must (Project_overview.add_project ~home ~repo ()) in
  assert_equal "project id" "monty" project.Overview_types.id;
  assert_bool "project memory exists"
    (Sys.file_exists (Project_overview.project_memory_file ~home "monty"));
  let task =
    must
      (Project_overview.add_local_task ~home ~project:"monty"
         ~title:"Design overview" ())
  in
  assert_equal "local task id" "local-001" task.Overview_types.id;
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
  let find_command command =
    if List.mem command [ "pi --fixed"; "codex --fixed"; "gh" ] then
      Ok ("/fake/" ^ command)
    else Error ("missing " ^ command)
  in
  let dry_checks =
    Doctor.checks ~find_command ~home ~harness:Harness.Pi
      ~harness_command:"pi --fixed" ~wt_command:"missing-wt"
      ~backend:Terminal.Dry_run ~worktree_mode:Launcher.Never ()
  in
  assert_bool "dry-run doctor has no required failure"
    (Doctor.exit_code dry_checks = 0);
  let dry_output = Doctor.render dry_checks in
  assert_contains "doctor pass" dry_output "PASS";
  assert_contains "doctor warn" dry_output "WARN";
  assert_not_contains "doctor dry-run ignores wt" dry_output "missing-wt";
  let codex_checks =
    Doctor.checks ~find_command ~home ~harness:Harness.Codex
      ~harness_command:"codex --fixed" ~wt_command:"missing-wt"
      ~backend:Terminal.Dry_run ~worktree_mode:Launcher.Never ()
  in
  let codex_output = Doctor.render codex_checks in
  assert_bool "Codex doctor has no required failure"
    (Doctor.exit_code codex_checks = 0);
  assert_contains "Codex doctor names selected harness" codex_output "codex";
  assert_not_contains "Codex doctor does not require Pi" codex_output "pi --fixed";
  let real_checks =
    Doctor.checks ~find_command ~home ~harness:Harness.Pi
      ~harness_command:"pi --fixed" ~wt_command:"missing-wt"
      ~backend:Terminal.Ghostty ~worktree_mode:Launcher.Always ()
  in
  assert_bool "real doctor fails required dependencies"
    (Doctor.exit_code real_checks = 1);
  let real_output = Doctor.render real_checks in
  assert_contains "doctor fail" real_output "FAIL";
  assert_contains "doctor configured wt" real_output "missing-wt";
  assert_contains "doctor recovery" real_output "Recovery:"

let test_head_butler_continuation_commands () =
  let command harness continuation =
    Head_butler.continuation_command ~home:"/monty home" ~harness
      ~harness_command:
        (match harness with Harness.Pi -> "pi --fixed" | Harness.Codex -> "codex --fixed")
      ~codex_yolo:true continuation
  in
  assert_equal "Pi continuation picker"
    "cd '/monty home' && exec pi --fixed --resume"
    (command Harness.Pi Head_butler.Picker);
  assert_equal "Pi continuation last"
    "cd '/monty home' && exec pi --fixed --continue"
    (command Harness.Pi Head_butler.Last);
  assert_equal "Pi continuation exact selector"
    ("cd '/monty home' && exec pi --fixed --session "
    ^ Shell.quote "design's notes")
    (command Harness.Pi (Head_butler.Session "design's notes"));
  let codex_picker = command Harness.Codex Head_butler.Picker in
  assert_contains "Codex continuation picker" codex_picker
    "exec codex --fixed resume";
  assert_contains "continued Codex xhigh reasoning" codex_picker
    "model_reasoning_effort=\"xhigh\"";
  assert_contains "continued Codex Vim mode" codex_picker
    "tui.vim_mode_default=true";
  assert_contains "continued Codex command-local trust" codex_picker
    (String.trim (Codex_trust.argument "/monty home"));
  assert_contains "continued Codex YOLO" codex_picker
    "--dangerously-bypass-approvals-and-sandbox";
  let codex_safe =
    Head_butler.continuation_command ~home:"/monty home"
      ~harness:Harness.Codex ~harness_command:"codex --fixed"
      ~codex_yolo:false Head_butler.Picker
  in
  assert_not_contains "continued Codex YOLO defaults off" codex_safe
    "--dangerously-bypass-approvals-and-sandbox";
  assert_contains "continued Codex home cwd" codex_picker "-C .";
  assert_contains "Codex continuation last"
    (command Harness.Codex Head_butler.Last) "-C . --last";
  assert_contains "Codex continuation exact selector"
    (command Harness.Codex (Head_butler.Session "design's notes"))
    ("-C . " ^ Shell.quote "design's notes")

let test_codex_harness_command () =
  let job =
    Job.make ~worker_dir:"/monty/workers/task-1" ~title:"Codex task"
      ~repo:"/repo" ~context:"/monty/context.md" ()
  in
  let options =
    Harness_command.
      { harness = Harness.Codex;
        command = "codex --model fixed";
        codex_yolo = false;
        fork = None;
        script_dir = "/tmp";
        branch_prefix = "monty";
        monty_command = "monty" }
  in
  let command =
    Harness_command.build_command ~codex_hook:true
      ~codex_mode:Codex_session.Fresh
      ~codex_trusted_paths:[ "/repo" ] ~options ~home:"/monty"
      ~instructions:(Some "/monty/MONTY.md") ~job ~context:job.context
  in
  assert_contains "codex executable and fixed args" command
    "exec codex --model fixed";
  assert_contains "Codex xhigh reasoning effort" command
    "model_reasoning_effort=\"xhigh\"";
  assert_contains "Codex Vim mode default" command "tui.vim_mode_default=true";
  assert_contains "Codex command-local trust" command
    (String.trim (Codex_trust.argument "/repo"));
  assert_contains "Codex command-local SessionStart hook" command
    "hooks.SessionStart";
  assert_contains "Codex hook capture command" command
    "codex-session-capture";
  assert_contains "Codex hook explicit Monty home" command "/monty";
  let relative_home_command =
    Harness_command.build_command ~codex_hook:true
      ~codex_mode:Codex_session.Fresh ~codex_trusted_paths:[] ~options
      ~home:"." ~instructions:(Some "/monty/MONTY.md") ~job
      ~context:job.context
  in
  assert_contains "Codex hook resolves relative Monty home"
    relative_home_command (Unix.realpath ".");
  assert_contains "Codex hook feature" command "--enable 'hooks'";
  assert_contains "Codex scoped hook trust bypass" command
    "--dangerously-bypass-hook-trust";
  assert_contains "codex instruction path" command "/monty/MONTY.md";
  assert_contains "codex context path" command "/monty/context.md";
  assert_contains "codex worker memory" command "/monty/workers/task-1/memory.md";
  assert_not_contains "codex does not use pi file syntax" command "@/monty";
  assert_not_contains "Codex YOLO defaults off" command
    "--dangerously-bypass-approvals-and-sandbox";
  let picker_command =
    Harness_command.build_command ~codex_hook:true
      ~codex_mode:Codex_session.Picker
      ~codex_trusted_paths:[] ~options ~home:"/monty"
      ~instructions:(Some "/monty/MONTY.md") ~job ~context:job.context
  in
  assert_contains "Codex worker native picker" picker_command " resume -C .";
  assert_not_contains "Codex picker omits original prompt" picker_command
    "Read the files below before acting";
  assert_not_contains "Codex picker never uses last" picker_command "--last";
  let exact_command =
    Harness_command.build_command ~codex_hook:true
      ~codex_mode:(Codex_session.Exact "thread's exact id")
      ~codex_trusted_paths:[] ~options ~home:"/monty"
      ~instructions:(Some "/monty/MONTY.md") ~job ~context:job.context
  in
  assert_contains "Codex worker exact resume" exact_command
    (" resume -C . -- " ^ Shell.quote "thread's exact id");
  assert_not_contains "Codex exact resume omits original prompt" exact_command
    "Read the files below before acting";
  assert_not_contains "Codex exact resume never uses last" exact_command "--last";
  let single_always_script =
    Harness_command.launch_script_contents ~codex_hook:true
      ~codex_mode:Codex_session.Fresh
      ~codex_trusted_paths:[] ~options ~job ~id:"task-1"
      ~branch:"cto/task-1" ~source_repo:"/repo" ~initial_workdir:"/repo"
      ~home:"/monty" ~context:job.context ~instructions:"/monty/MONTY.md"
      ~worker_dir:"/monty/workers/task-1" ~worktree_mode:"always"
      ~wt_command:"wt" ()
  in
  assert_contains "single workspace uses raw ensure-worktree"
    single_always_script
    "ensure-worktree --repo '/repo' --branch 'cto/task-1' --wt-command 'wt'";
  assert_not_contains "raw ensure-worktree rejects global home option"
    single_always_script
    "ensure-worktree --repo '/repo' --branch 'cto/task-1' --home";
  let multi_job =
    Job.make_with_workspaces ~worker_dir:"/monty/workers/task-1"
      ~title:"Codex multi task"
      ~workspaces:
        [ Job.{ repo = "/repo"; branch = Some "cto/task-1" };
          Job.{ repo = "/admin"; branch = Some "cto/admin-task-1" } ]
      ~context:"/monty/context.md" ~task_key:"local:local-005" ()
  in
  let multi_command =
    Harness_command.build_command ~codex_hook:true
      ~codex_mode:Codex_session.Fresh
      ~codex_trusted_paths:[] ~options ~home:"/monty"
      ~instructions:(Some "/monty/MONTY.md") ~job:multi_job
      ~context:multi_job.context
  in
  assert_contains "Codex secondary workspace permission" multi_command
    "--add-dir \"$MONTY_WORKSPACE_2\"";
  let never_script =
    Harness_command.launch_script_contents ~codex_hook:true
      ~codex_mode:Codex_session.Fresh
      ~codex_trusted_paths:[] ~options ~job:multi_job ~id:"task-1"
      ~branch:"cto/task-1" ~source_repo:"/repo" ~initial_workdir:"/repo"
      ~home:"/monty" ~context:multi_job.context ~instructions:"/monty/MONTY.md"
      ~worker_dir:"/monty/workers/task-1" ~worktree_mode:"never"
      ~wt_command:"wt" ()
  in
  assert_contains "multi never first workspace" never_script
    "MONTY_WORKSPACE_1='/repo'";
  assert_contains "multi never second workspace" never_script
    "MONTY_WORKSPACE_2='/admin'";
  let always_script =
    Harness_command.launch_script_contents ~codex_hook:true
      ~codex_mode:Codex_session.Fresh
      ~codex_trusted_paths:[] ~options ~job:multi_job ~id:"task-1"
      ~branch:"cto/task-1" ~source_repo:"/repo" ~initial_workdir:"/repo"
      ~home:"/monty" ~context:multi_job.context ~instructions:"/monty/MONTY.md"
      ~worker_dir:"/monty/workers/task-1" ~worktree_mode:"always"
      ~wt_command:"wt" ()
  in
  assert_contains "multi workspace rehydration uses explicit Monty home"
    always_script
    "task workspace ensure 'local:local-005' --repo '/repo' --home '/monty'";
  let unlinked_multi_job =
    Job.make_with_workspaces ~worker_dir:"/monty/workers/task-1"
      ~title:"Unlinked Codex multi task"
      ~workspaces:
        [ Job.{ repo = "/repo"; branch = Some "cto/task-1" };
          Job.{ repo = "/admin"; branch = Some "cto/admin-task-1" } ]
      ~context:"/monty/context.md" ()
  in
  let unlinked_always_script =
    Harness_command.launch_script_contents ~codex_hook:true
      ~codex_mode:Codex_session.Fresh
      ~codex_trusted_paths:[] ~options ~job:unlinked_multi_job
      ~id:"task-1" ~branch:"cto/task-1" ~source_repo:"/repo"
      ~initial_workdir:"/repo" ~home:"/monty"
      ~context:unlinked_multi_job.context ~instructions:"/monty/MONTY.md"
      ~worker_dir:"/monty/workers/task-1" ~worktree_mode:"always"
      ~wt_command:"wt" ()
  in
  assert_contains "unlinked multi workspace uses raw ensure-worktree"
    unlinked_always_script
    "ensure-worktree --repo '/admin' --branch 'cto/admin-task-1' --wt-command 'wt'";
  assert_not_contains "unlinked raw ensure-worktree rejects global home option"
    unlinked_always_script
    "ensure-worktree --repo '/admin' --branch 'cto/admin-task-1' --home";
  let yolo_command =
    Harness_command.build_command ~codex_hook:true
      ~codex_mode:Codex_session.Fresh
      ~codex_trusted_paths:[]
      ~options:{ options with codex_yolo = true }
      ~home:"/monty" ~instructions:(Some "/monty/MONTY.md") ~job
      ~context:job.context
  in
  assert_contains "Codex YOLO flag" yolo_command
    "--dangerously-bypass-approvals-and-sandbox";
  let head_butler =
    Head_butler.command ~home:"/monty" ~harness:Harness.Codex
      ~harness_command:"codex" ~codex_yolo:true ~name:"Monty Head Butler"
  in
  assert_contains "head-butler Codex YOLO flag" head_butler
    "--dangerously-bypass-approvals-and-sandbox";
  assert_contains "head-butler Codex xhigh reasoning effort" head_butler
    "model_reasoning_effort=\"xhigh\"";
  assert_contains "head-butler Codex Vim mode default" head_butler
    "tui.vim_mode_default=true";
  assert_contains "head-butler Codex command-local trust" head_butler
    (String.trim (Codex_trust.argument "/monty"))

let test_codex_harness_rejects_fork () =
  let options =
    Launcher.
      { backend = Terminal.Dry_run;
        target = Terminal.Tab;
        harness = Harness.Codex;
        harness_command = "/usr/bin/true";
        codex_yolo = false;
        wt_command = "/usr/bin/true";
        worktree_mode = Never;
        branch_prefix = "monty";
        agent_profile = "reviewed";
        fork = Some "pi-session";
        home = "/tmp";
        script_dir = "/tmp";
        monty_command = "monty" }
  in
  match Launcher.check_dependencies options with
  | Ok () -> failwith "Codex unexpectedly accepted Pi fork semantics"
  | Error message -> assert_contains "Codex fork diagnostic" message "does not support --fork"

let test_settings_harness_roundtrip_and_precedence () =
  let home = temp_root "settings" in
  must (Settings.set_harness ~home Harness.Codex);
  let settings = must (Settings.load ~home) in
  assert_bool "persisted Codex harness"
    (settings.Settings.harness = Some Harness.Codex);
  assert_bool "Codex YOLO defaults off" (not settings.codex_yolo);
  assert_bool "branch prefix defaults to absent"
    (settings.branch_prefix = None);
  must (Settings.set_codex_yolo ~home true);
  must (Settings.set_branch_prefix ~home "cto");
  let settings = must (Settings.load ~home) in
  assert_bool "persisted Codex YOLO" settings.codex_yolo;
  assert_bool "setting Codex YOLO preserves harness"
    (settings.harness = Some Harness.Codex);
  assert_bool "persisted branch prefix"
    (settings.branch_prefix = Some "cto");
  let no_env _ = None in
  assert_bool "persisted harness is effective"
    (must (Settings.effective_harness ~getenv:no_env ~home None)
    = Harness.Codex);
  let pi_env name =
    if String.equal name "MONTY_HARNESS" then Some "pi" else None
  in
  assert_bool "environment overrides persisted harness"
    (must (Settings.effective_harness ~getenv:pi_env ~home None) = Harness.Pi);
  assert_bool "CLI overrides environment harness"
    (must
       (Settings.effective_harness ~getenv:pi_env ~home
          (Some Harness.Codex))
    = Harness.Codex);
  let branch_env name =
    if String.equal name "MONTY_BRANCH_PREFIX" then Some "environment" else None
  in
  assert_equal "persisted prefix overrides environment default" "cto"
    (must (Settings.effective_branch_prefix ~getenv:branch_env ~home None));
  assert_equal "CLI overrides persisted branch prefix" "explicit"
    (must
       (Settings.effective_branch_prefix ~getenv:branch_env ~home
          (Some "explicit")));
  let fallback_home = temp_root "settings-branch-fallback" in
  assert_equal "environment branch prefix fallback" "environment"
    (must
       (Settings.effective_branch_prefix ~getenv:branch_env
          ~home:fallback_home None));
  assert_equal "default branch prefix" "monty"
    (must
       (Settings.effective_branch_prefix ~getenv:no_env ~home:fallback_home
          None));
  assert_contains "settings rendering" (Settings.render settings)
    "harness       codex";
  assert_contains "YOLO settings rendering" (Settings.render settings)
    "codex-yolo    true";
  assert_contains "branch-prefix settings rendering" (Settings.render settings)
    "branch-prefix cto"

let test_headless_json_contract () =
  let dispatch =
    Headless.
      { id = "issue-123";
        title = "Fix issue 123";
        repo = "/repo";
        branch = "cto/issue-123";
        worktree = "/worktrees/issue-123";
        workspaces =
          [ Job_store.
              { repo = "/repo";
                branch = Some "cto/issue-123";
                worktree = Some "/worktrees/issue-123" };
            Job_store.
              { repo = "/admin";
                branch = Some "cto/admin-issue-123";
                worktree = Some "/worktrees/admin-issue-123" } ];
        worker_dir = "/monty/.monty/runs/run-1/workers/issue-123";
        instructions =
          "/monty/.monty/runs/run-1/workers/issue-123/MONTY.md";
        context = "/monty/.monty/runs/run-1/issue-123.md";
        home = "/monty";
        profile = reviewed_profile }
  in
  let json = Headless.dispatch_json ~attempt_id:"attempt-test" dispatch in
  assert_equal "headless dispatch schema" Headless.dispatch_schema
    Yojson.Safe.Util.(json |> member "schema" |> to_string);
  assert_equal "headless dispatch worktree" dispatch.worktree
    Yojson.Safe.Util.(json |> member "worker" |> member "worktree" |> to_string);
  assert_equal "headless dispatch workspace count" "2"
    (Yojson.Safe.Util.(json |> member "worker" |> member "workspaces" |> to_list)
    |> List.length |> string_of_int);
  assert_bool "headless dispatch excludes subagent runtime state"
    Yojson.Safe.Util.(json |> member "worker" |> member "run_id" = `Null);
  let harness_call = Yojson.Safe.Util.member "harness_call" json in
  assert_equal "headless harness tool" "subagent"
    Yojson.Safe.Util.(harness_call |> member "tool" |> to_string);
  let arguments = Yojson.Safe.Util.member "arguments" harness_call in
  assert_equal "headless harness context" "fresh"
    Yojson.Safe.Util.(arguments |> member "context" |> to_string);
  assert_equal "headless harness agent scope" "project"
    Yojson.Safe.Util.(arguments |> member "agentScope" |> to_string);
  assert_equal "headless harness cwd" dispatch.home
    Yojson.Safe.Util.(arguments |> member "cwd" |> to_string);
  assert_bool "headless harness is async"
    Yojson.Safe.Util.(arguments |> member "async" |> to_bool);
  assert_bool "headless harness disables clarify"
    (not Yojson.Safe.Util.(arguments |> member "clarify" |> to_bool));
  assert_bool "headless harness does not request Pi worktrees"
    Yojson.Safe.Util.(arguments |> member "worktree" = `Null);
  let chain = Yojson.Safe.Util.(arguments |> member "chain" |> to_list) in
  assert_bool "headless 1-2-1 chain has three phases" (List.length chain = 3);
  let implementation = List.nth chain 0 in
  let reviewers =
    Yojson.Safe.Util.(List.nth chain 1 |> member "parallel" |> to_list)
  in
  let fixer = List.nth chain 2 in
  assert_bool "headless has two reviewers" (List.length reviewers = 2);
  let attempt_root =
    "/monty/.monty/runs/run-1/workers/issue-123/artifacts/headless/attempt-test"
  in
  let assert_child label relative_path child =
    assert_equal (label ^ " output") (Filename.concat attempt_root relative_path)
      Yojson.Safe.Util.(child |> member "output" |> to_string);
    assert_bool (label ^ " progress disabled")
      (not Yojson.Safe.Util.(child |> member "progress" |> to_bool));
    let acceptance = Yojson.Safe.Util.member "acceptance" child in
    assert_equal (label ^ " acceptance level") "none"
      Yojson.Safe.Util.(acceptance |> member "level" |> to_string);
    assert_equal (label ^ " acceptance reason") Headless.acceptance_reason
      Yojson.Safe.Util.(acceptance |> member "reason" |> to_string)
  in
  assert_child "headless implementation" "implementation.md" implementation;
  assert_child "headless correctness review" "reviews/correctness.md"
    (List.nth reviewers 0);
  assert_child "headless quality review" "reviews/quality.md"
    (List.nth reviewers 1);
  assert_child "headless final" "final.md" fixer;
  let review_only_profile =
    Agent_profile.
      { id = "review-only";
        description = "one review without a fixer";
        interactive = "Review once.";
        implementation = "Implement.";
        reviews =
          [ { id = "edge"; title = "Edge review";
              instructions = "Inspect edge cases." } ];
        fix = None }
  in
  let review_only =
    Headless.dispatch_json ~attempt_id:"attempt-review-only"
      { dispatch with profile = review_only_profile }
  in
  let review_only_chain =
    Yojson.Safe.Util.(
      review_only |> member "harness_call" |> member "arguments"
      |> member "chain" |> to_list)
  in
  assert_bool "review-only chain omits fixer"
    (List.length review_only_chain = 2);
  assert_equal "review-only implementation is final" "final.md"
    Yojson.Safe.Util.(
      List.hd review_only_chain |> member "output" |> to_string
      |> Filename.basename);
  let colliding_ids_profile =
    { reviewed_profile with
      reviews =
        [ Agent_profile.
            { id = "edge-case"; title = "Hyphen review";
              instructions = "Review." };
          Agent_profile.
            { id = "edge_case"; title = "Underscore review";
              instructions = "Review." } ] }
  in
  let colliding_ids =
    Headless.dispatch_json ~attempt_id:"attempt-aliases"
      { dispatch with profile = colliding_ids_profile }
  in
  let aliases =
    Yojson.Safe.Util.(
      colliding_ids |> member "harness_call" |> member "arguments"
      |> member "chain" |> index 1 |> member "parallel" |> to_list)
    |> List.map Yojson.Safe.Util.(fun json -> json |> member "as" |> to_string)
  in
  assert_bool "distinct reviewer ids keep distinct output aliases"
    (List.sort_uniq String.compare aliases |> List.length = 2);
  let prepared =
    Headless.
      { id = dispatch.id;
        title = dispatch.title;
        branch = dispatch.branch;
        worktree = Some dispatch.worktree;
        workspaces = dispatch.workspaces;
        worker_dir = dispatch.worker_dir;
        status = "prepared";
        profile = reviewed_profile }
  in
  let prepared_json =
    Headless.prepare_json ~harness:Harness.Codex ~codex_yolo:true [ prepared ]
  in
  assert_equal "headless prepare schema" Headless.prepare_schema
    Yojson.Safe.Util.(prepared_json |> member "schema" |> to_string);
  assert_equal "headless prepare harness" "codex"
    Yojson.Safe.Util.(prepared_json |> member "harness" |> to_string);
  assert_bool "headless prepare YOLO"
    Yojson.Safe.Util.(prepared_json |> member "codex_yolo" |> to_bool);
  assert_equal "headless prepare status" "prepared"
    Yojson.Safe.Util.(
      prepared_json |> member "jobs" |> index 0 |> member "status" |> to_string)

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
  [ ("slug", test_slug);
    ("shell_quote", test_shell_quote);
    ("manifest", test_manifest);
    ("agent_profile_validation", test_agent_profile_validation);
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
    ("head_butler_continuation_commands", test_head_butler_continuation_commands);
    ("codex_harness_command", test_codex_harness_command);
    ("codex_harness_rejects_fork", test_codex_harness_rejects_fork);
    ( "settings_harness_roundtrip_and_precedence",
      test_settings_harness_roundtrip_and_precedence );
    ("headless_json_contract", test_headless_json_contract) ]
  |> List.iter (fun (name, test) -> run_named name test)
