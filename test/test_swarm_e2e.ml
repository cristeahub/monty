(* Checkout-binary E2E coverage for the swarm agent profile: an isolated
   MONTY_HOME, a fake pi harness that commits in the child's worktree, a fake
   wt over real temporary Git repositories, and cleanup of everything. *)

open Monty

let failf fmt = Printf.ksprintf failwith fmt
let ( / ) = Filename.concat

let contains text needle = Home.contains_substring text needle

let require_contains label text needle =
  if not (contains text needle) then failf "%s: expected %S to contain %S" label text needle

let rec remove_tree path =
  try
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR ->
        Sys.readdir path |> Array.iter (fun name -> remove_tree (path / name));
        Unix.rmdir path
    | _ -> Unix.unlink path
  with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> ()

let executable =
  let test_dir = Shell.abs_path Sys.executable_name |> Filename.dirname in
  let candidate = test_dir / "../bin/main.exe" |> Shell.normalize in
  if not (Sys.file_exists candidate) then failf "built Monty binary is missing: %s" candidate;
  Unix.realpath candidate

let bundled_profiles =
  Option.value ~default:(Sys.getcwd ()) (Sys.getenv_opt "DUNE_SOURCEROOT") / "agent-profiles"

let rec copy_tree source destination =
  match (Unix.lstat source).Unix.st_kind with
  | Unix.S_DIR ->
      Shell.ensure_dir destination;
      Sys.readdir source |> Array.iter (fun name -> copy_tree (source / name) (destination / name))
  | _ -> Shell.write_file destination (Shell.read_file source)

type result = { code : int; stdout : string; stderr : string }

let counter = ref 0

let spawn ~root ~env args =
  incr counter;
  let stdout_path = root / Printf.sprintf "out-%d" !counter in
  let stderr_path = root / Printf.sprintf "err-%d" !counter in
  let fd path = Unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600 in
  let stdout_fd = fd stdout_path and stderr_fd = fd stderr_path in
  let stdin_fd = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
  let pid =
    Unix.create_process_env executable (Array.of_list (executable :: args)) env stdin_fd stdout_fd
      stderr_fd
  in
  Unix.close stdin_fd; Unix.close stdout_fd; Unix.close stderr_fd;
  fun () ->
    let _, status = Unix.waitpid [] pid in
    let code = match status with Unix.WEXITED code -> code | _ -> 128 in
    { code; stdout = Shell.read_file stdout_path; stderr = Shell.read_file stderr_path }

let run ~root ~env args = spawn ~root ~env args ()

let require_code label expected result =
  if result.code <> expected then
    failf "%s: exited %d instead of %d\nstdout:\n%s\nstderr:\n%s" label result.code expected
      result.stdout result.stderr

let git ~cwd arguments =
  match Process.run_capture ~cwd ("git " ^ String.concat " " (List.map Shell.quote arguments)) with
  | Ok { Process.status = `Exited 0; stdout } -> String.trim stdout
  | Ok { stdout; _ } -> failf "git %s failed in %s:\n%s" (String.concat " " arguments) cwd stdout
  | Error message -> failwith message

let init_repo path =
  Shell.ensure_dir path;
  ignore (git ~cwd:path [ "init"; "-q" ]);
  ignore (git ~cwd:path [ "config"; "user.email"; "monty@example.invalid" ]);
  ignore (git ~cwd:path [ "config"; "user.name"; "Monty" ]);
  Shell.write_file (path / "tracked.txt") "tracked\n";
  ignore (git ~cwd:path [ "add"; "tracked.txt" ]);
  ignore (git ~cwd:path [ "commit"; "-qm"; "initial" ])

(* A wt that creates worktrees for new and existing branches and lists and
   deletes them, the way the real one does for Monty. *)
let install_wt ~root =
  let worktrees = root / "worktrees" in
  let wt = root / "fake-bin/wt" in
  Shell.write_file wt
    (String.concat "\n"
       [ "#!/bin/sh"; "set -eu"; "repo=$(pwd -P)";
         "case \"$1\" in";
         "  b)";
         "    branch=$2";
         "    safe=$(printf '%s' \"$branch\" | tr '/ ' '__')";
         "    worktree=" ^ Shell.quote worktrees ^ "/$safe";
         "    if [ ! -d \"$worktree\" ]; then";
         "      mkdir -p " ^ Shell.quote worktrees;
         "      if git show-ref --verify --quiet \"refs/heads/$branch\"; then";
         "        git worktree add -q \"$worktree\" \"$branch\"";
         "      else";
         "        git worktree add -q -b \"$branch\" \"$worktree\"";
         "      fi";
         "    fi";
         "    printf '%s\\n' \"$worktree\" ;;";
         "  list)";
         "    printf 'repo:\\n'";
         "    git worktree list --porcelain | awk '/^worktree /{path=$2} /^branch /{sub(\"refs/heads/\",\"\",$2); printf \"  %s -> %s\\n\", $2, path}' ;;";
         "  db)";
         "    branch=$2";
         "    path=$(git worktree list --porcelain | awk -v b=\"refs/heads/$branch\" '/^worktree /{path=$2} /^branch /{if ($2==b) print path}')";
         "    if [ -n \"$path\" ]; then git worktree remove --force \"$path\"; fi";
         "    git branch -D \"$branch\" >/dev/null 2>&1 || true ;;";
         "  *) exit 92 ;;";
         "esac"; "" ]);
  Shell.chmod_executable wt

(* A pi that behaves like a swarm child: it commits in its worktree, blocks
   when asked, breaks the check for one name, and fails for another. *)
let install_pi ~root =
  let pi = root / "fake-bin/pi" in
  Shell.write_file pi
    ("#!/bin/sh\nset -eu\nroot=" ^ Shell.quote root ^ "\n"
    ^ {|prompt=$(cat)
printf '%s\n' "$MONTY_JOB_ID $MONTY_SWARM_PARENT $MONTY_SWARM_NAME $*" >> "$root/phases.log"
printf '%s\n' "$prompt" > "$root/prompt-$MONTY_SWARM_NAME.txt"
cd "$MONTY_JOB_WORKTREE"
case "$MONTY_SWARM_NAME" in
  block*)
    count=0
    while [ ! -f "$root/release" ]; do
      count=$((count + 1)); [ "$count" -lt 1500 ] || exit 86
      sleep 0.02
    done ;;
  crash)
    printf '{"type":"message_end","message":{"role":"assistant","stopReason":"error","errorMessage":"boom","content":[]}}\n'
    printf '%s\n' '{"type":"agent_end"}'
    exit 0 ;;
esac
if [ -f BROKEN ]; then
  git rm -q BROKEN
  git commit -qm "Fix the check"
else
  printf 'work by %s\n' "$MONTY_SWARM_NAME" > "$MONTY_SWARM_NAME.txt"
  git add -A
  git commit -qm "Add $MONTY_SWARM_NAME"
  if [ "$MONTY_SWARM_NAME" = broken ]; then
    printf 'broken\n' > BROKEN
    git add -A
    git commit -qm "Break the check"
  fi
fi
printf '{"type":"message_end","message":{"role":"assistant","stopReason":"stop","content":[{"type":"text","text":"CHILD HANDOFF %s"}]}}\n' "$MONTY_SWARM_NAME"
printf '%s\n' '{"type":"agent_end"}'
|});
  Shell.chmod_executable pi

let env_with overrides =
  let overridden key = List.exists (fun (name, _) -> String.equal key name) overrides in
  (Unix.environment () |> Array.to_list
  |> List.filter (fun entry ->
         match String.index_opt entry '=' with
         | None -> true
         | Some index ->
             let key = String.sub entry 0 index in
             not (String.starts_with ~prefix:"MONTY_" key || overridden key)))
  @ List.map (fun (name, value) -> name ^ "=" ^ value) overrides
  |> Array.of_list

let json_string field json =
  match Yojson.Safe.Util.member field json with `String value -> value | _ -> ""

let wait_until label predicate =
  let rec loop remaining =
    if predicate () then ()
    else if remaining = 0 then failwith ("timed out: " ^ label)
    else (Unix.sleepf 0.05; loop (remaining - 1))
  in
  loop 600

let with_root name f =
  let marker = Filename.temp_file ("monty-swarm-" ^ name ^ "-") ".tmp" in
  Sys.remove marker;
  Unix.mkdir marker 0o700;
  let root = Unix.realpath marker in
  Fun.protect ~finally:(fun () -> remove_tree root) (fun () -> f root)

type fixture = {
  root : string;
  home : string;
  repo : string;
  env : string array;
  run_dir : string;
}

let setup root =
  let home = root / "home" in
  let repo = root / "repo" in
  let user_home = root / "user-home" in
  Shell.ensure_dir (root / "fake-bin");
  Shell.ensure_dir user_home;
  Shell.ensure_dir home;
  copy_tree bundled_profiles (home / "agent-profiles");
  init_repo repo;
  install_wt ~root;
  install_pi ~root;
  let env =
    env_with
      [ ("MONTY_HOME", home); ("HOME", user_home); ("NO_COLOR", "1");
        ("PI_CODING_AGENT_DIR", root / "pi-agent"); ("MANTLE_CONTEXT", "");
        ("PATH", root / "fake-bin" ^ ":/usr/bin:/bin"); ("MONTY_WT_COMMAND", "wt");
        ("MONTY_HARNESS", "pi"); ("MONTY_PI_COMMAND", "pi"); ("MONTY_TERMINAL", "ghostty") ]
  in
  require_code "projects add" 0 (run ~root ~env [ "projects"; "add"; "--home"; home; "--repo"; repo ]);
  { root; home; repo; env; run_dir = home / ".monty/runs/swarm" }

let driver_id = "driver"
let driver_branch = "cto/driver"

(* The driver is reserved and materialized like any worker; its interactive
   session is simulated by exporting the worker environment. *)
let create_driver ?(swarm = []) fixture =
  let context = fixture.root / "goal.md" in
  Shell.write_file context "# Goal\n\nShip the swarm feature.\n";
  let manifest = fixture.run_dir / "jobs.json" in
  Shell.ensure_dir fixture.run_dir;
  Yojson.Safe.to_file manifest
    (`Assoc
      [ ( "jobs",
          `List
            [ `Assoc
                ([ ("id", `String driver_id); ("title", `String "Swarm driver");
                   ("repo", `String fixture.repo); ("branch", `String driver_branch);
                   ("context", `String context); ("agent_profile", `String "swarm") ]
                @ if swarm = [] then [] else [ ("swarm", `Assoc swarm) ]) ] ) ]);
  let dry =
    run ~root:fixture.root ~env:fixture.env
      [ "launch-many"; "--terminal"; "dry-run"; "--manifest"; manifest; "--home"; fixture.home ]
  in
  require_code "swarm launch dry-run" 0 dry;
  require_contains "dry-run profile" dry.stdout "agent profile: swarm";
  require_code "driver prepare" 0
    (run ~root:fixture.root ~env:fixture.env
       [ "headless"; "prepare-many"; "--manifest"; manifest; "--home"; fixture.home ]);
  let worker_dir = fixture.run_dir / "workers" / driver_id in
  let job = Yojson.Safe.from_file (worker_dir / "job.json") in
  let worktree = json_string "last_known_worktree" job in
  if not (Sys.file_exists worktree) then failwith "driver worktree is missing";
  (worker_dir, worktree)

let driver_env fixture worker_dir =
  Array.append fixture.env
    [| "MONTY_WORKER_DIR=" ^ worker_dir; "MONTY_JOB_ID=" ^ driver_id; "MONTY_RUN_DIR=" ^ fixture.run_dir |]

let ledger fixture = Yojson.Safe.from_file (fixture.run_dir / "workers" / driver_id / "swarm/ledger.json")

let child_states fixture =
  Yojson.Safe.Util.(ledger fixture |> member "children" |> to_list)
  |> List.map (fun child -> (json_string "name" child, json_string "state" child))

let status_json ~root ~env =
  let result = run ~root ~env [ "swarm"; "status"; "--json" ] in
  require_code "swarm status" 0 result;
  Yojson.Safe.from_string result.stdout

let state_of ~root ~env name =
  Yojson.Safe.Util.(status_json ~root ~env |> member "children" |> to_list)
  |> List.find (fun child -> json_string "name" child = name)
  |> json_string "state"

let test_swarm_lifecycle () =
  with_root "lifecycle" (fun root ->
      let fixture = setup root in
      let worker_dir, driver_worktree =
        create_driver ~swarm:[ ("max_workers", `Int 2) ] fixture
      in
      let env = driver_env fixture worker_dir in
      let swarm args = run ~root ~env ([ "swarm" ] @ args) in
      (* A swarm driver never runs headless. *)
      let refused = run ~root ~env:fixture.env [ "headless"; "run"; driver_id; "--home"; fixture.home ] in
      require_code "headless run of a driver" 1 refused;
      require_contains "headless refusal" refused.stderr "swarm driver runs interactively";
      let brief = root / "brief.md" in
      Shell.write_file brief "# Area\n\nImplement your area and commit.\n";
      let unset = swarm [ "spawn"; "alpha"; "--brief"; brief ] in
      require_code "spawn before init" 1 unset;
      require_contains "check required" unset.stderr "monty swarm init --check";
      require_code "init" 0 (swarm [ "init"; "--check"; "test ! -f BROKEN" ]);
      let bad_name = swarm [ "spawn"; "bad/name"; "--brief"; brief ] in
      require_code "unsafe child name" 1 bad_name;
      (* Two blocking children fill the limit; a third is refused. *)
      let spawn name =
        let result = swarm [ "spawn"; name; "--brief"; brief ] in
        require_code ("spawn " ^ name) 0 result;
        let json = Yojson.Safe.from_string result.stdout in
        if json_string "worker_id" json <> driver_id ^ "--" ^ name then failwith "child id shape changed";
        if json_string "branch" json <> driver_branch ^ "--" ^ name then failwith "child branch shape changed";
        if json_string "run_status" (Yojson.Safe.Util.member "receipt" json) <> "started" then
          failwith "spawn did not return a startup receipt";
        json
      in
      ignore (spawn "block-one");
      ignore (spawn "block-two");
      let third = swarm [ "spawn"; "alpha"; "--brief"; brief ] in
      require_code "spawn past the limit" 1 third;
      require_contains "limit message" third.stderr "swarm limit reached: 2 of 2";
      let duplicate = swarm [ "spawn"; "block-one"; "--brief"; brief ] in
      require_code "duplicate name" 1 duplicate;
      (* Children see their swarm identity and the commit rule. *)
      wait_until "children started" (fun () -> Sys.file_exists (root / "prompt-block-one.txt"));
      let prompt = Shell.read_file (root / "prompt-block-one.txt") in
      require_contains "commit rule" prompt "Commit all your work on the current branch";
      require_contains "child instructions" prompt "swarm child `block-one` of driver `driver`";
      let child_dir = fixture.run_dir / "workers" / (driver_id ^ "--block-one") in
      let child_job = Yojson.Safe.from_file (child_dir / "job.json") in
      if json_string "swarm_parent" child_job <> driver_id || json_string "swarm_name" child_job <> "block-one"
      then failwith "child record lost its swarm identity";
      if json_string "agent_profile" child_job <> "solo" then failwith "child did not pin the worker profile";
      (* monty swarm is refused from a child and from a non-swarm worker. *)
      let child_env =
        Array.append fixture.env
          [| "MONTY_WORKER_DIR=" ^ child_dir; "MONTY_JOB_ID=" ^ driver_id ^ "--block-one" |]
      in
      let from_child = run ~root ~env:child_env [ "swarm"; "status" ] in
      require_code "swarm from a child" 1 from_child;
      require_contains "child refusal" from_child.stderr "swarm child block-one of driver driver";
      (* Children are not tasks: hidden by default, indented with --swarm. *)
      let listed = run ~root ~env:fixture.env [ "list"; "--home"; fixture.home ] in
      require_code "list" 0 listed;
      if contains listed.stdout "driver--block-one" then failwith "list showed swarm children by default";
      let with_children = run ~root ~env:fixture.env [ "list"; "--swarm"; "--home"; fixture.home ] in
      require_code "list --swarm" 0 with_children;
      require_contains "indented child" with_children.stdout "  driver--block-one";
      let tasks = Yojson.Safe.from_file (fixture.home / ".monty/tasks.local.json") in
      let task_count = Yojson.Safe.Util.(tasks |> member "tasks" |> to_list |> List.length) in
      if task_count <> 1 then failf "reconciliation created %d tasks for one driver" task_count;
      let busy_status = status_json ~root ~env in
      if Yojson.Safe.Util.(busy_status |> member "running" |> to_int) <> 2 then
        failwith "status did not count two running children";
      (* Release the children and let them finish. *)
      Shell.write_file (root / "release") "go\n";
      wait_until "children finished" (fun () ->
          List.for_all (fun name -> state_of ~root ~env name = "finished") [ "block-one"; "block-two" ]);
      (* Child handoffs land in the driver inbox, not the head butler's. *)
      let driver_pending = run ~root ~env [ "handoff"; "pending"; "--format"; "json" ] in
      require_code "driver pending" 0 driver_pending;
      let pending_count output =
        Yojson.Safe.Util.(Yojson.Safe.from_string output |> member "pending" |> to_list |> List.length)
      in
      if pending_count driver_pending.stdout <> 2 then failwith "driver inbox did not receive both child handoffs";
      let butler_pending = run ~root ~env:fixture.env [ "handoff"; "pending"; "--format"; "json"; "--home"; fixture.home ] in
      require_code "head-butler pending" 0 butler_pending;
      if pending_count butler_pending.stdout <> 0 then failwith "child handoffs leaked into the head-butler inbox";
      let explicit = run ~root ~env:fixture.env [ "handoff"; "pending"; "--inbox"; driver_id; "--format"; "json"; "--home"; fixture.home ] in
      require_code "explicit inbox" 0 explicit;
      if pending_count explicit.stdout <> 2 then failwith "--inbox did not select the driver inbox";
      (* Retire refuses an unlanded child; land then retire works. *)
      let early = swarm [ "retire"; "block-one" ] in
      require_code "retire before landing" 1 early;
      require_contains "unlanded refusal" early.stderr "not in the driver branch";
      let landed = swarm [ "land"; "block-one" ] in
      require_code "land" 0 landed;
      require_contains "landing output" landed.stdout "Landed swarm child block-one";
      if not (contains (git ~cwd:driver_worktree [ "log"; "--oneline"; "-1" ]) "Land swarm child block-one") then
        failwith "driver branch did not receive the landing merge";
      if not (Sys.file_exists (driver_worktree / "block-one.txt")) then failwith "landed work is missing";
      if state_of ~root ~env "block-one" <> "landed" then failwith "ledger did not record the landing";
      (* Done refuses the driver while children remain, and children directly. *)
      let child_done = run ~root ~env:fixture.env [ "done"; driver_id ^ "--block-one"; "--home"; fixture.home ] in
      require_code "done on a child" 1 child_done;
      require_contains "child done refusal" child_done.stderr "monty swarm retire";
      let driver_done = run ~root ~env:fixture.env [ "done"; driver_id; "--home"; fixture.home ] in
      require_code "done with children" 1 driver_done;
      require_contains "done lists retire commands" driver_done.stderr "monty swarm retire 'block-one'";
      require_contains "done lists forced retire" driver_done.stderr "monty swarm retire 'block-two' --force";
      require_code "retire landed" 0 (swarm [ "retire"; "block-one" ]);
      if state_of ~root ~env "block-one" <> "retired" then failwith "retire did not update the ledger";
      if not (Sys.file_exists (fixture.run_dir / "archive" / (driver_id ^ "--block-one") / "job.json")) then
        failwith "retired child was not archived";
      if git ~cwd:fixture.repo [ "branch"; "--list"; driver_branch ^ "--block-one" ] <> "" then
        failwith "retired child branch survived";
      (* A freed slot lets the next child run; a broken check fails landing. *)
      ignore (spawn "broken");
      wait_until "broken finished" (fun () -> state_of ~root ~env "broken" = "finished");
      let failed = swarm [ "land"; "broken" ] in
      require_code "land with failing check" 1 failed;
      require_contains "check failure names the output" failed.stderr "artifacts/swarm/broken/check-";
      let log_path =
        Yojson.Safe.Util.(status_json ~root ~env |> member "children" |> to_list)
        |> List.find (fun child -> json_string "name" child = "broken")
        |> json_string "last_check"
      in
      if not (Sys.file_exists log_path) then failwith "check output was not captured";
      require_contains "check log" (Shell.read_file log_path) "test ! -f BROKEN";
      if contains (git ~cwd:driver_worktree [ "log"; "--oneline" ]) "broken" then
        failwith "a failed check was merged into the driver branch";
      (* A follow-up brief resumes the child as a fresh chain. *)
      let fix = root / "fix.md" in
      Shell.write_file fix "Remove BROKEN and commit.\n";
      let resumed = swarm [ "resume"; "broken"; "--brief"; fix ] in
      require_code "resume" 0 resumed;
      require_contains "follow-up appended" (Shell.read_file (worker_dir / "swarm/briefs/broken.md")) "Follow-up from the driver";
      wait_until "broken resumed and finished" (fun () -> state_of ~root ~env "broken" = "finished");
      let phases = Shell.read_file (root / "phases.log") in
      if List.length (List.filter (fun line -> contains line "driver--broken ") (String.split_on_char '\n' phases)) <> 2 then
        failwith "resume did not run a second chain";
      (* Forced completion retires every remaining child and archives the driver. *)
      let forced = run ~root ~env:fixture.env [ "done"; driver_id; "--force"; "--home"; fixture.home ] in
      require_code "forced done" 0 forced;
      if Sys.file_exists worker_dir then failwith "driver was not archived";
      List.iter
        (fun name ->
          if Sys.file_exists (fixture.run_dir / "workers" / (driver_id ^ "--" ^ name)) then
            failf "child %s survived forced completion" name)
        [ "block-two"; "broken" ];
      let archived_ledger = Yojson.Safe.from_file (fixture.run_dir / "archive" / driver_id / "swarm/ledger.json") in
      Yojson.Safe.Util.(archived_ledger |> member "children" |> to_list)
      |> List.iter (fun child ->
             if json_string "state" child <> "retired" then
               failf "child %s was not retired by forced completion" (json_string "name" child)))

let test_failed_chain_concurrency_and_doctor () =
  with_root "failures" (fun root ->
      let fixture = setup root in
      let worker_dir, _ = create_driver ~swarm:[ ("check", `String "true"); ("max_workers", `Int 1) ] fixture in
      let env = driver_env fixture worker_dir in
      let brief = root / "brief.md" in
      Shell.write_file brief "# Area\n";
      (* Two concurrent spawns against a limit of one: exactly one wins and
         the ledger stays valid JSON with one child. *)
      let first = spawn ~root ~env [ "swarm"; "spawn"; "block-a"; "--brief"; brief ] in
      let second = spawn ~root ~env [ "swarm"; "spawn"; "block-b"; "--brief"; brief ] in
      let results = [ first (); second () ] in
      let winners = List.filter (fun result -> result.code = 0) results in
      if List.length winners <> 1 then
        failf "concurrent spawns: %d succeeded\n%s"
          (List.length winners)
          (String.concat "\n" (List.map (fun result -> result.stdout ^ result.stderr) results));
      let states = child_states fixture in
      if List.length states <> 1 then failf "ledger has %d children after a race" (List.length states);
      let winner = fst (List.hd states) in
      Shell.write_file (root / "release") "go\n";
      wait_until "race child finished" (fun () -> state_of ~root ~env winner = "finished");
      require_code "retire race child" 0 (run ~root ~env [ "swarm"; "retire"; winner; "--force" ]);
      (* A failed chain publishes a failure handoff to the driver inbox. *)
      require_code "spawn crash" 0 (run ~root ~env [ "swarm"; "spawn"; "crash"; "--brief"; brief ]);
      wait_until "crash failed" (fun () -> state_of ~root ~env "crash" = "failed");
      let pending = run ~root ~env [ "handoff"; "pending"; "--format"; "json" ] in
      require_code "driver pending" 0 pending;
      let entries = Yojson.Safe.Util.(Yojson.Safe.from_string pending.stdout |> member "pending" |> to_list) in
      let crash =
        List.find_opt
          (fun entry ->
            json_string "id" (Yojson.Safe.Util.member "handoff" entry |> Yojson.Safe.Util.member "worker")
            = driver_id ^ "--crash")
          entries
      in
      (match crash with
      | Some entry ->
          if json_string "outcome" (Yojson.Safe.Util.member "handoff" entry) <> "failed" then
            failwith "crash handoff is not a failure"
      | None -> failwith "failed child chain did not reach the driver inbox");
      let butler = run ~root ~env:fixture.env [ "handoff"; "pending"; "--format"; "json"; "--home"; fixture.home ] in
      require_code "butler pending" 0 butler;
      if Yojson.Safe.Util.(Yojson.Safe.from_string butler.stdout |> member "pending" |> to_list) <> [] then
        failwith "a child failure reached the head-butler inbox";
      (* Doctor explains a running child without a supervisor and an orphan. *)
      let ledger_path = worker_dir / "swarm/ledger.json" in
      let stale =
        match Yojson.Safe.from_file ledger_path with
        | `Assoc fields ->
            `Assoc
              (List.map
                 (function
                   | "children", `List children ->
                       ( "children",
                         `List
                           (List.map
                              (function
                                | `Assoc child when json_string "name" (`Assoc child) = "crash" ->
                                    `Assoc (("state", `String "running") :: List.remove_assoc "state" child)
                                | child -> child)
                              children) )
                   | field -> field)
                 fields)
        | _ -> failwith "ledger is not an object"
      in
      Yojson.Safe.to_file ledger_path stale;
      let doctor = run ~root ~env:fixture.env [ "doctor"; "--terminal"; "dry-run"; "--home"; fixture.home ] in
      require_code "doctor warns" 0 doctor;
      require_contains "stale running child" doctor.stdout "records crash as running but no supervisor holds its worker lock";
      require_contains "stale recovery" doctor.stdout "monty swarm resume 'crash'";
      (* A non-swarm worker cannot run monty swarm. *)
      let solo_context = root / "solo.md" in
      Shell.write_file solo_context "# Solo\n";
      let solo_manifest = fixture.run_dir / "solo.json" in
      Yojson.Safe.to_file solo_manifest
        (`Assoc
          [ ( "jobs",
              `List
                [ `Assoc
                    [ ("id", `String "plain"); ("title", `String "Plain worker");
                      ("repo", `String fixture.repo); ("branch", `String "cto/plain");
                      ("context", `String solo_context); ("agent_profile", `String "solo") ] ] ) ]);
      require_code "plain prepare" 0
        (run ~root ~env:fixture.env [ "headless"; "prepare-many"; "--manifest"; solo_manifest; "--home"; fixture.home ]);
      let plain_env =
        Array.append fixture.env [| "MONTY_WORKER_DIR=" ^ fixture.run_dir / "workers/plain"; "MONTY_JOB_ID=plain" |]
      in
      let refused = run ~root ~env:plain_env [ "swarm"; "status" ] in
      require_code "swarm from a solo worker" 1 refused;
      require_contains "solo refusal" refused.stderr "pinned to agent profile solo, not swarm";
      (* An orphaned child whose driver vanished is reported with its repair. *)
      remove_tree worker_dir;
      let orphan = run ~root ~env:fixture.env [ "doctor"; "--terminal"; "dry-run"; "--home"; fixture.home ] in
      require_code "doctor orphan" 0 orphan;
      require_contains "orphan warning" orphan.stdout "driver driver is missing or archived";
      require_contains "orphan recovery" orphan.stdout ("monty done 'driver--crash' --force");
      let repaired = run ~root ~env:fixture.env [ "done"; driver_id ^ "--crash"; "--force"; "--home"; fixture.home ] in
      require_code "orphan repair" 0 repaired)

let () =
  Unix.putenv "GIT_CONFIG_NOSYSTEM" "1";
  Unix.putenv "GIT_CONFIG_GLOBAL" "/dev/null";
  List.iter
    (fun (name, test) ->
      test ();
      Printf.printf "PASS %s\n%!" name)
    [ ("swarm_lifecycle", test_swarm_lifecycle);
      ("swarm_failed_chain_concurrency_and_doctor", test_failed_chain_concurrency_and_doctor) ]
