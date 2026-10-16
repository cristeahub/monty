open Test_support

let contains = Monty.Home.contains_substring
let quote = Monty.Shell.quote
let self = Unix.realpath Sys.executable_name
let put path value = mkdir (Filename.dirname path); write path value
let executable path value = put path value; Unix.chmod path 0o700
let exists path = try ignore (Unix.lstat path); true with Unix.Unix_error (Unix.ENOENT,_,_) -> false
let strip_newlines text =
  let rec length n=if n>0 && text.[n-1]='\n' then length(n-1) else n in
  String.sub text 0 (length(String.length text))
let contents label path expected =
  check (exists path && strip_newlines (read path) = expected) (label ^ ": " ^ path)
let absent label path = check (not (exists path)) (label ^ " should be absent: " ^ path)
let value name = Option.value ~default:"" (Sys.getenv_opt name)
let wait_file path = wait ~seconds:12. (fun () -> exists path)
let fixture_wrapper mode = "#!/bin/sh\nexec " ^ quote self ^ " --fixture " ^ mode ^ " \"$@\"\n"

let fixture mode args =
  match mode, args with
  | "dune", ["--version"] -> print_endline "3.24.2"
  | "dune", "build" :: _ ->
      let release = strip_newlines (read "binary-release.txt") in
      executable "_build/default/bin/main.exe"
        ("#!/bin/sh\nexec " ^ quote self ^ " --fixture runtime " ^ quote release ^ " \"$@\"\n")
  | "runtime", release :: rest ->
      if release = "broken runtime" && (match rest with "doctor" :: _ -> true | _ -> false) then exit 139;
      print_endline release
  | "mv", source :: _ ->
      let name = Filename.basename source in
      if (String.starts_with ~prefix:".monty-wrapper-install-" name && value "FAIL_WRAPPER" = "1") then exit 47;
      if (String.starts_with ~prefix:".monty-shell-rc." name && value "FAIL_SHELL_RC" = "1") then exit 48;
      let real_mv = value "REAL_MV" in
      if source = value "INTERRUPT_AFTER_SOURCE" && source <> "" then (
        let moved = run real_mv args in
        if moved.code <> 0 then exit moved.code;
        Unix.kill (int_of_string (String.trim (read (value "INSTALL_PID_FILE")))) Sys.sigterm;
        pause 1.)
      else if source = value "PAUSE_SOURCE" && source <> "" then (
        let moved = run real_mv args in
        if moved.code <> 0 then exit moved.code;
        put (value "PAUSE_READY") "";
        wait_file (value "PAUSE_RELEASE"))
      else Unix.execv real_mv (Array.of_list (real_mv :: args))
  | _ -> failwith "invalid installer fixture command"

let suite installer source_version = with_temp_dir ~prefix:"monty-install-" (fun root ->
  let repo = root / "repo" and prefix = root / "prefix" and fake = root / "fake-bin" and shell_home = root / "home" in
  List.iter mkdir [repo / ".monty"; fake; shell_home];
  let installed = prefix / "share/monty" in
  let state = installed / ".monty" and real_binary = prefix / "libexec/monty/monty-real" and wrapper = prefix / "bin/monty" in
  put (repo / "install.sh") (read installer); Unix.chmod (repo / "install.sh") 0o700;
  put (repo / ".monty/version") (read source_version); Unix.chmod (repo / ".monty/version") 0o600;
  copy_tree (Filename.dirname installer / "agent-profiles") (repo / "agent-profiles");
  let real_mv = Monty.Process.command_exists_with_arguments "mv" |> Result.get_ok in
  List.iter (fun mode -> executable (fake / mode) (fixture_wrapper mode)) ["dune"; "mv"];
  let fail_wrapper = ref false and fail_rc = ref false and interrupt = ref "" and paused = ref "" in
  let environment install_prefix = merge_env env
    ["PATH", fake ^ ":" ^ Option.value ~default:"" (List.assoc_opt "PATH" env);
     "HOME", shell_home; "REAL_MV", real_mv;
     "FAIL_WRAPPER", (if !fail_wrapper then "1" else "0");
     "FAIL_SHELL_RC", (if !fail_rc then "1" else "0");
     "INTERRUPT_AFTER_SOURCE", !interrupt; "PAUSE_SOURCE", !paused;
     "PAUSE_READY", root / "paused"; "PAUSE_RELEASE", root / "released";
     "INSTALL_PID_FILE", install_prefix / ".monty-install-lock/pid"] in
  let start ?(install_prefix=prefix) ?home args =
    spawn ~cwd:repo ~env:(environment install_prefix) ~input:"" ~new_session:true "/bin/sh"
      ([repo / "install.sh"; "--prefix"; install_prefix; "--no-shell-rc"]
       @ (match home with None -> [] | Some path -> ["--monty-home"; path]) @ args) in
  let install ?install_prefix ?home ?(code=0) args =
    let result = await (start ?install_prefix ?home args) in
    check (result.code = code) (Printf.sprintf "installer exit %d, expected %d:\n%s\n%s" result.code code result.stdout result.stderr);
    result.stdout ^ result.stderr in
  let rejects ?install_prefix ?home args =
    let result = await (start ?install_prefix ?home args) in
    check (result.code <> 0) "installer accepted an invalid operation";
    result.stdout ^ result.stderr in
  let release expected =
    List.iter (fun path -> let result = run path [] in
      check (result.code=0) result.stderr;
      check (strip_newlines result.stdout=expected) ("installed release differs: " ^ path))
      [real_binary; wrapper] in
  let state_kept label = contents label (state / "tasks.local.json") "{\"tasks\":[{\"id\":\"keep-me\"}]}" in
  let set_release text binary = put (repo / "control-room.txt") (text ^ "\n"); put (repo / "binary-release.txt") (binary ^ "\n") in
  contents "source state version" source_version "2";
  set_release "release one" "binary one"; ignore (install []);
  contents "fresh state version" (state / "version") "2";
  contents "fresh control room" (installed / "control-room.txt") "release one";
  List.iter (fun profile ->
    let rec compare_tree source destination =
      let entries path=Sys.readdir path |> Array.to_list |> List.sort String.compare in
      equal ~label:("installed profile entries differ: " ^ destination) (entries source) (entries destination);
      Array.iter (fun name ->
        if Sys.is_directory (source/name) then compare_tree (source/name) (destination/name)
        else check (read (source/name)=read (destination/name)) ("profile differs: " ^ name)) (Sys.readdir source) in
    compare_tree (repo/"agent-profiles"/profile) (installed/"agent-profiles"/profile)) ["simplified";"reviewed";"solo"];
  let profile = run ~env:(merge_env env ["MONTY_HOME",installed]) bin ["agent-profiles";"show"] in
  check (profile.code=0 && contains profile.stdout "\"id\": \"simplified\"" && contains profile.stdout "\"fix_role\": \"simplifier\"") "installed default profile lost simplifier";
  release "binary one";
  put (state / "tasks.local.json") "{\"tasks\":[{\"id\":\"keep-me\"}]}\n";
  put (state / "memory.md") "durable memory\n";
  put (state / "agent-profiles/personal/profile.json") "{\"id\":\"personal\"}\n";
  let state_inode = (Unix.stat state).st_ino in
  set_release "release two" "binary two"; ignore (install []);
  state_kept "matching upgrade task state";
  contents "worker memory" (state / "memory.md") "durable memory";
  contents "personal profile" (state / "agent-profiles/personal/profile.json") "{\"id\":\"personal\"}";
  contents "matching version" (state / "version") "2";
  check ((Unix.stat state).st_ino=state_inode) "matching state directory was replaced";
  contents "updated control room" (installed / "control-room.txt") "release two"; release "binary two";
  set_release "must not install" "broken runtime";
  check (contains (rejects []) "built binary failed its runtime check") "broken runtime was not diagnosed";
  release "binary two"; contents "runtime failure source" (installed / "control-room.txt") "release two";
  state_kept "runtime failure task state"; contents "runtime failure memory" (state/"memory.md") "durable memory";
  absent "runtime failure install lock" (prefix/".monty-install-lock");
  set_release "release two" "binary two";
  paused := installed / "control-room.txt";
  let upgrading = start [] in
  Fun.protect ~finally:(fun () -> put (root/"released") "";
    if poll upgrading=None then (terminate upgrading;ignore(await upgrading))) (fun () ->
    wait_file (root/"paused");
    let concurrent = run ~env:(merge_env env ["MONTY_HOME",installed]) bin ["settings";"set";"branch-prefix";"concurrent";"--home";installed] in
    put (root/"released") "";
    check (concurrent.code=0) concurrent.stderr;
    let upgraded = await upgrading in check (upgraded.code=0) upgraded.stderr);
  paused := "";
  state_kept "concurrent upgrade task state";
  check (contains (read (state/"settings.json")) "concurrent") "concurrent registry write was lost";
  check ((Unix.stat state).st_ino=state_inode) "concurrent upgrade replaced state directory";
  absent "nested state" (state/".monty"); absent "nested control room" (installed/"monty");
  set_release "interrupted release" "interrupted binary";
  interrupt := installed / "control-room.txt";
  ignore (install ~code:143 []); interrupt := "";
  state_kept "interrupted task state"; contents "interrupted version" (state/"version") "2";
  contents "interrupted source" (installed/"control-room.txt") "release two"; release "binary two";
  absent "interrupted prefix lock" (prefix/".monty-install-lock"); absent "interrupted home lock" (installed ^ ".monty-install-lock");
  Unix.unlink (state/"version"); set_release "release three" "binary three"; ignore (install []);
  contents "adopted legacy version" (state/"version") "2"; state_kept "adopted legacy tasks"; release "binary three";
  put (state/"version") "1\n"; set_release "migration release" "migration binary";
  check (contains (install ["--dry-run"]) "migrate compatible .monty state from version 1 to 2 without replacing it") "migration preview missing";
  contents "dry-run migration version" (state/"version") "1";
  ignore (install []); contents "migrated version" (state/"version") "2"; state_kept "migrated tasks";
  contents "migration source" (installed/"control-room.txt") "migration release"; release "migration binary";
  put (state/"version") "99\n"; set_release "release four" "binary four";
  let preview = install ["--dry-run"] in
  check (contains preview "WARNING: Monty state version mismatch" && contains preview "explicit confirmation or --replace-state would be required") "mismatch preview missing";
  contents "dry-run mismatched version" (state/"version") "99";
  contents "dry-run source" (installed/"control-room.txt") "migration release"; release "migration binary";
  check (contains (install ["--dry-run";"--replace-state"]) "state replacement is explicitly authorized but dry-run makes no changes") "authorized replacement preview missing";
  contents "authorized dry-run version" (state/"version") "99";
  contents "authorized dry-run source" (installed/"control-room.txt") "migration release"; release "migration binary";
  let refused = rejects [] in
  check (contains refused "WARNING: Monty state version mismatch" && contains refused "will delete the current .monty folder") "mismatch refusal warning missing";
  contents "refused version" (state/"version") "99"; state_kept "refused tasks";
  contents "refused source" (installed/"control-room.txt") "migration release"; release "migration binary";
  ignore (install ["--replace-state"]); contents "replacement version" (state/"version") "2";
  absent "replaced tasks" (state/"tasks.local.json"); absent "replaced memory" (state/"memory.md");
  contents "replacement source" (installed/"control-room.txt") "release four"; release "binary four";
  put (state/"tasks.local.json") "survive rollback\n"; put (state/"version") "1\n";
  set_release "release five" "binary five"; fail_wrapper := true; ignore (rejects []); fail_wrapper := false;
  contents "rollback task state" (state/"tasks.local.json") "survive rollback";
  contents "rollback version" (state/"version") "1"; contents "rollback source" (installed/"control-room.txt") "release four"; release "binary four";
  ignore (install []); contents "retry version" (state/"version") "2";
  contents "retry tasks" (state/"tasks.local.json") "survive rollback";
  contents "retry source" (installed/"control-room.txt") "release five"; release "binary five";
  put (repo/".monty/version") "99\n"; put (repo/".monty/tasks.local.json") "same-home state\n";
  let same_prefix = root/"same-home-prefix" in
  ignore (rejects ~install_prefix:same_prefix ~home:repo []);
  contents "same-home refused version" (repo/".monty/version") "99";
  contents "same-home refused tasks" (repo/".monty/tasks.local.json") "same-home state";
  ignore (install ~install_prefix:same_prefix ~home:repo ["--replace-state"]);
  contents "same-home replacement version" (repo/".monty/version") "2";
  absent "same-home replaced tasks" (repo/".monty/tasks.local.json");
  contents "same-home source" (repo/"control-room.txt") "release five";
  check (exists (repo/"install.sh")) "same-home installation replaced checkout";
  put (repo/".git/source-marker") "source marker\n";
  check (contains (rejects ~install_prefix:same_prefix ~home:(root/"nonexistent/../repo") []) "paths must not contain . or .. components") "unsafe alias was not diagnosed";
  contents "unsafe alias source marker" (repo/".git/source-marker") "source marker";
  contents "unsafe alias source" (repo/"control-room.txt") "release five";
  let assignments = "MONTY_HOME=/plain\n  export MONTY_HOME=/exported\ntypeset -gx MONTY_HOME=/typeset\ndeclare -x MONTY_HOME=/declared\nset -gx MONTY_HOME /fish\n" in
  let rc = shell_home/"existing.zshrc" in
  let original = assignments ^ "# >>> monty >>>\nexport MONTY_HOME=/managed\n# <<< monty <<<\n" in
  put rc original;
  check (contains (install ["--shell-rc";rc;"--dry-run"]) "would ask before overriding existing MONTY_HOME") "shell conflict preview missing";
  check (read rc=original) "dry-run changed shell config";
  let skipped = install ["--shell-rc";rc] in
  check (contains skipped "Skipping shell config update because stdin is not interactive.") "noninteractive conflict was accepted";
  String.split_on_char '\n' assignments |> List.filter ((<>) "") |> List.iter (fun line -> check (contains skipped line) ("unreported shell assignment: " ^ line));
  check (not (contains skipped "MONTY_HOME=/managed")) "managed assignment was reported as conflict";
  check (read rc=original) "unconfirmed shell config changed";
  let rc = shell_home/".zshrc" and target = shell_home/"dotfiles/zshrc" in
  mkdir (Filename.dirname target); Unix.symlink "dotfiles/zshrc" rc;
  let kept = "# MONTY_HOME=/comment\nMONTY_HOME_EXTRA=/keep\nexport OTHER=/keep # MONTY_HOME=/comment\ntypeset OTHER=/keep # MONTY_HOME=/comment\nset -gx MONTY_HOME_EXTRA /keep\n" in
  let original = kept ^ "# >>> monty >>>\nexport MONTY_HOME=/old\nexport MONTY_BRANCH_PREFIX=old\n# <<< monty <<<\n# >>> monty >>>\nexport MONTY_HOME=/duplicate\n# <<< monty <<<\n" in
  put target original; Unix.chmod target 0o640;
  let permissions = (Unix.stat target).st_perm in
  Unix.link target (root/"rc-original-inode");
  let updated = install ["--shell-rc";rc;"--branch-prefix";"cto"] in
  check (contains updated ("Updated shell config: " ^ rc)) "shell update receipt missing";
  check ((Unix.lstat rc).st_kind=Unix.S_LNK && Unix.readlink rc="dotfiles/zshrc") "shell config symlink changed";
  check ((Unix.stat target).st_perm=permissions) "shell config permissions changed";
  check (read (root/"rc-original-inode")=original) "shell config was updated in place";
  let expected = kept ^ "\n# >>> monty >>>\nexport MONTY_HOME=" ^ quote installed ^ "\nexport MONTY_BRANCH_PREFIX='cto'\n# <<< monty <<<\n" in
  check (read target=expected) "managed shell blocks were not replaced correctly";
  fail_rc := true; ignore (rejects ["--shell-rc";rc;"--branch-prefix";"changed"]); fail_rc := false;
  check (read target=expected) "failed shell publication changed config";
  Sys.readdir (Filename.dirname target) |> Array.iter (fun name -> check (not (String.starts_with ~prefix:".monty-shell-rc." name)) "shell publication left temporary file");
  let fish = shell_home/"fish/conf.d/monty.fish" in
  ignore (install ["--shell-rc";fish;"--branch-prefix";"cto";"--dry-run"]);
  absent "dry-run fish directory" (shell_home/"fish");
  ignore (install ["--shell-rc";fish;"--branch-prefix";"cto"]);
  check (read fish = "\n# >>> monty >>>\nset -gx MONTY_HOME " ^ quote installed ^ "\nset -gx MONTY_BRANCH_PREFIX 'cto'\n# <<< monty <<<\n") "new fish shell config differs";
  print_endline "PASS hardened installer, state versioning, rollback, concurrency and shell config")

let () = match Array.to_list Sys.argv with
  | _ :: "--fixture" :: mode :: args -> fixture mode args
  | _ :: _binary :: installer :: version :: [] -> suite (Unix.realpath installer) (Unix.realpath version)
  | _ -> failwith "usage: test_install BIN INSTALLER VERSION"
