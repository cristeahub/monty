#!/bin/sh
set -eu

installer=$1
root=$(mktemp -d "${TMPDIR:-/tmp}/monty-install-test.XXXXXX")
trap 'rm -rf "$root"' 0 HUP INT TERM

repo=$root/repo
prefix=$root/prefix
fake_bin=$root/fake-bin
home=$root/home
installed_home=$prefix/share/monty
state_dir=$installed_home/.monty
real_binary=$prefix/libexec/monty/monty-real
wrapper=$prefix/bin/monty
log=$root/install.log
real_tar=$(command -v tar)
real_mv=$(command -v mv)
real_ln=$(command -v ln)
fake_dune_state_version=
fake_tar_fail_create=0
fake_mv_fail_stage_name=
fake_mv_interrupt_stage_name=
fake_mv_stage_state_version=
fake_mv_stage_sleep=0
fake_ln_fail_version=0
fake_ln_interrupt_version=0
fake_mv_fail_binary=0
fake_mv_fail_wrapper=0
install_monty_home=
install_shell_rc=

mkdir -p "$repo" "$fake_bin" "$home"
cp "$installer" "$repo/install.sh"

cat > "$fake_bin/dune" <<'EOF'
#!/bin/sh
set -eu
case "${1:-}" in
  --version)
    printf '3.20.0\n'
    ;;
  build)
    mkdir -p _build/default/bin
    release=$(cat binary-release.txt)
    cat > _build/default/bin/main.exe <<INNER
#!/bin/sh
printf '%s\n' '$release'
INNER
    chmod +x _build/default/bin/main.exe
    if [ -n "${FAKE_DUNE_STATE_VERSION:-}" ]; then
      mkdir -p "$FAKE_STATE_DIR"
      printf '%s\n' "$FAKE_DUNE_STATE_VERSION" > "$FAKE_STATE_DIR/version"
    fi
    ;;
  *)
    exit 2
    ;;
esac
EOF
chmod +x "$fake_bin/dune"

cat > "$fake_bin/tar" <<'EOF'
#!/bin/sh
set -eu
if [ "${FAKE_TAR_FAIL_CREATE:-0}" -eq 1 ]; then
  for arg in "$@"; do
    if [ "$arg" = -cf ]; then
      exit 41
    fi
  done
fi
exec "$REAL_TAR" "$@"
EOF
chmod +x "$fake_bin/tar"

cat > "$fake_bin/mv" <<'EOF'
#!/bin/sh
set -eu
source_path=${1:-}
case "$source_path" in
  */.monty-install-stage-*/*)
    source_name=${source_path##*/}
    if [ "${FAKE_MV_STAGE_SLEEP:-0}" -eq 1 ]; then
      sleep 1
    fi
    if [ -n "${FAKE_MV_STAGE_STATE_VERSION:-}" ]; then
      mkdir -p "$FAKE_STATE_DIR"
      printf '%s\n' "$FAKE_MV_STAGE_STATE_VERSION" > "$FAKE_STATE_DIR/version"
    fi
    if [ -n "${FAKE_MV_FAIL_STAGE_NAME:-}" ] &&
       [ "$source_name" = "$FAKE_MV_FAIL_STAGE_NAME" ]; then
      exit 42
    fi
    if [ -n "${FAKE_MV_INTERRUPT_STAGE_NAME:-}" ] &&
       [ "$source_name" = "$FAKE_MV_INTERRUPT_STAGE_NAME" ]; then
      kill -TERM "$(cat "$FAKE_INSTALL_PID_FILE")"
      sleep 1
      exit 43
    fi
    ;;
  */.monty-real-install-*)
    if [ "${FAKE_MV_FAIL_BINARY:-0}" -eq 1 ]; then
      exit 45
    fi
    ;;
  */.monty-wrapper-install-*)
    if [ "${FAKE_MV_FAIL_WRAPPER:-0}" -eq 1 ]; then
      exit 46
    fi
    ;;
esac
exec "$REAL_MV" "$@"
EOF
chmod +x "$fake_bin/mv"

cat > "$fake_bin/ln" <<'EOF'
#!/bin/sh
set -eu
case "${1:-}" in
  */.monty/.version.monty-install-*/version)
    if [ "${FAKE_LN_FAIL_VERSION:-0}" -eq 1 ]; then
      exit 44
    fi
    if [ "${FAKE_LN_INTERRUPT_VERSION:-0}" -eq 1 ]; then
      "$REAL_LN" "$@"
      kill -TERM "$(cat "$FAKE_INSTALL_PID_FILE")"
      sleep 1
      exit 47
    fi
    ;;
esac
exec "$REAL_LN" "$@"
EOF
chmod +x "$fake_bin/ln"

fail() {
  printf 'FAIL install: %s\n' "$1" >&2
  if [ -f "$log" ]; then
    printf '%s\n' '--- install output ---' >&2
    cat "$log" >&2
  fi
  exit 1
}

assert_contents() {
  label=$1
  path=$2
  expected=$3
  if [ ! -f "$path" ]; then
    fail "$label is missing: $path"
  fi
  actual=$(cat "$path")
  if [ "$actual" != "$expected" ]; then
    fail "$label: expected $expected, found $actual"
  fi
}

assert_absent() {
  label=$1
  path=$2
  if [ -e "$path" ] || [ -L "$path" ]; then
    fail "$label should be absent: $path"
  fi
}

assert_contains_file() {
  label=$1
  path=$2
  expected=$3
  if ! grep -F "$expected" "$path" >/dev/null; then
    fail "$label: expected $path to contain $expected"
  fi
}

assert_public_executable() {
  label=$1
  path=$2
  # shellcheck disable=SC2012
  permissions=$(LC_ALL=C ls -ld "$path" | awk '{ print substr($1, 2, 9) }')
  if [ "$permissions" != rwxr-xr-x ]; then
    fail "$label: expected mode 755, found $permissions"
  fi
}

assert_binary_release() {
  label=$1
  expected=$2
  if [ ! -x "$real_binary" ]; then
    fail "$label is not executable: $real_binary"
  fi
  actual=$($real_binary)
  if [ "$actual" != "$expected" ]; then
    fail "$label: expected $expected, found $actual"
  fi
  wrapper_actual=$($wrapper)
  if [ "$wrapper_actual" != "$expected" ]; then
    fail "$label through wrapper: expected $expected, found $wrapper_actual"
  fi
}

expect_interrupted_install() {
  label=$1
  install_status=0
  run_install || install_status=$?
  if [ "$install_status" -ne 143 ]; then
    fail "$label: expected exit 143, found $install_status"
  fi
}

run_install() {
  (
    cd "$repo"
    set -- --prefix "$prefix"
    if [ -n "$install_monty_home" ]; then
      set -- "$@" --monty-home "$install_monty_home"
    fi
    if [ -n "$install_shell_rc" ]; then
      set -- "$@" --shell-rc "$install_shell_rc"
    else
      set -- "$@" --no-shell-rc
    fi
    PATH="$fake_bin:$PATH" HOME="$home" \
      REAL_TAR="$real_tar" REAL_MV="$real_mv" REAL_LN="$real_ln" \
      FAKE_STATE_DIR="$state_dir" \
      FAKE_INSTALL_PID_FILE="$installed_home.monty-install-lock/pid" \
      FAKE_DUNE_STATE_VERSION="$fake_dune_state_version" \
      FAKE_TAR_FAIL_CREATE="$fake_tar_fail_create" \
      FAKE_MV_FAIL_STAGE_NAME="$fake_mv_fail_stage_name" \
      FAKE_MV_INTERRUPT_STAGE_NAME="$fake_mv_interrupt_stage_name" \
      FAKE_MV_STAGE_STATE_VERSION="$fake_mv_stage_state_version" \
      FAKE_MV_STAGE_SLEEP="$fake_mv_stage_sleep" \
      FAKE_LN_FAIL_VERSION="$fake_ln_fail_version" \
      FAKE_LN_INTERRUPT_VERSION="$fake_ln_interrupt_version" \
      FAKE_MV_FAIL_BINARY="$fake_mv_fail_binary" \
      FAKE_MV_FAIL_WRAPPER="$fake_mv_fail_wrapper" \
      ./install.sh "$@"
  ) > "$log" 2>&1
}

printf 'release one\n' > "$repo/control-room.txt"
printf 'binary one\n' > "$repo/binary-release.txt"
run_install
assert_contents 'new state version' "$state_dir/version" '1'
assert_binary_release 'new binary' 'binary one'
assert_public_executable 'real binary permissions' "$real_binary"
assert_public_executable 'wrapper permissions' "$wrapper"
# shellcheck disable=SC2012
state_inode=$(ls -di "$state_dir" | awk '{ print $1 }')

mkdir -p "$state_dir/runs/run-001/workers/task-001"
printf '{"tasks":[{"id":"local-001","title":"Keep me"}]}\n' \
  > "$state_dir/tasks.local.json"
printf 'durable worker memory\n' \
  > "$state_dir/runs/run-001/workers/task-001/memory.md"

printf 'release two\n' > "$repo/control-room.txt"
printf 'binary two\n' > "$repo/binary-release.txt"
run_install
assert_contents 'matching state version' "$state_dir/version" '1'
# shellcheck disable=SC2012
matching_state_inode=$(ls -di "$state_dir" | awk '{ print $1 }')
if [ "$matching_state_inode" != "$state_inode" ]; then
  fail 'matching state directory was replaced instead of preserved'
fi
assert_contents 'preserved task state' "$state_dir/tasks.local.json" \
  '{"tasks":[{"id":"local-001","title":"Keep me"}]}'
assert_contents 'preserved worker memory' \
  "$state_dir/runs/run-001/workers/task-001/memory.md" 'durable worker memory'
assert_contents 'updated control room' "$installed_home/control-room.txt" 'release two'
assert_binary_release 'upgraded binary' 'binary two'

install_monty_home=$installed_home/
run_install
install_monty_home=
assert_absent 'lock inside trailing-slash Monty home' \
  "$installed_home/.monty-install-lock"
assert_absent 'sibling lock after trailing-slash install' \
  "$installed_home.monty-install-lock"
assert_contents 'task state after trailing-slash install' \
  "$state_dir/tasks.local.json" \
  '{"tasks":[{"id":"local-001","title":"Keep me"}]}'

install_monty_home=$prefix/
if run_install; then
  fail 'overlapping install prefix and Monty home were accepted'
fi
install_monty_home=
assert_contents 'control room after overlap refusal' \
  "$installed_home/control-room.txt" 'release two'
assert_binary_release 'binary after overlap refusal' 'binary two'

install_monty_home=$wrapper
if run_install; then
  fail 'Monty home colliding with the installed wrapper was accepted'
fi
install_monty_home=
assert_binary_release 'binary after wrapper collision refusal' 'binary two'
assert_contents 'control room after wrapper collision refusal' \
  "$installed_home/control-room.txt" 'release two'

wrapper_alias=$root/wrapper-alias
ln -s "$wrapper" "$wrapper_alias"
install_monty_home=$wrapper_alias
if run_install; then
  fail 'Monty home aliased to the installed wrapper was accepted'
fi
install_monty_home=
assert_binary_release 'binary after wrapper alias refusal' 'binary two'
rm "$wrapper_alias"

bin_alias=$root/bin-alias
ln -s "$prefix/bin" "$bin_alias"
install_monty_home=$bin_alias/task-home
if run_install; then
  fail 'Monty home inside a physical bin alias was accepted'
fi
install_monty_home=
assert_absent 'state under physical bin alias' "$prefix/bin/task-home"
assert_binary_release 'binary after physical alias refusal' 'binary two'
rm "$bin_alias"

rm "$state_dir/version"
printf 'release three\n' > "$repo/control-room.txt"
printf 'binary three\n' > "$repo/binary-release.txt"
run_install
assert_contents 'adopted legacy state version' "$state_dir/version" '1'
assert_contents 'legacy task state' "$state_dir/tasks.local.json" \
  '{"tasks":[{"id":"local-001","title":"Keep me"}]}'
assert_contents 'legacy control-room update' "$installed_home/control-room.txt" \
  'release three'
assert_binary_release 'binary after legacy adoption' 'binary three'

printf 'release four\n' > "$repo/control-room.txt"
printf 'binary four\n' > "$repo/binary-release.txt"
printf '2\n' > "$state_dir/version"
if run_install; then
  fail 'mismatched state version was accepted'
fi
assert_contents 'mismatched state version remains untouched' "$state_dir/version" '2'
assert_contents 'mismatched task state remains untouched' "$state_dir/tasks.local.json" \
  '{"tasks":[{"id":"local-001","title":"Keep me"}]}'
assert_contents 'mismatched control room remains untouched' \
  "$installed_home/control-room.txt" 'release three'
assert_binary_release 'binary after version mismatch' 'binary three'

printf '1\n' > "$state_dir/version"
fake_dune_state_version=2
if run_install; then
  fail 'state mismatch introduced during the build was accepted'
fi
fake_dune_state_version=
assert_contents 'revalidated state version' "$state_dir/version" '2'
assert_contents 'control room after build race' \
  "$installed_home/control-room.txt" 'release three'
assert_binary_release 'binary after build race' 'binary three'
printf '1\n' > "$state_dir/version"

fake_mv_stage_state_version=2
if run_install; then
  fail 'state mismatch introduced during activation was accepted'
fi
fake_mv_stage_state_version=
assert_contents 'activation revalidated state version' "$state_dir/version" '2'
assert_contents 'control room after activation race' \
  "$installed_home/control-room.txt" 'release three'
assert_binary_release 'binary after activation race' 'binary three'
printf '1\n' > "$state_dir/version"

fake_tar_fail_create=1
if run_install; then
  fail 'failed archive creation was accepted'
fi
fake_tar_fail_create=0
assert_contents 'control room after archive failure' \
  "$installed_home/control-room.txt" 'release three'
assert_contents 'state after archive failure' "$state_dir/version" '1'
assert_binary_release 'binary after archive failure' 'binary three'

mkdir "$installed_home.monty-install-lock"
if run_install; then
  fail 'an existing installer lock was ignored'
fi
rmdir "$installed_home.monty-install-lock"
assert_contents 'control room after lock refusal' \
  "$installed_home/control-room.txt" 'release three'
assert_contents 'state after lock refusal' "$state_dir/version" '1'

mkdir "$prefix/.monty-install-lock"
if run_install; then
  fail 'an existing prefix installer lock was ignored'
fi
rmdir "$prefix/.monty-install-lock"
assert_contents 'control room after prefix lock refusal' \
  "$installed_home/control-room.txt" 'release three'
assert_contents 'state after prefix lock refusal' "$state_dir/version" '1'
printf 'release three\n' > "$repo/control-room.txt"
printf 'binary three\n' > "$repo/binary-release.txt"

stale_pid=99999999
mkdir "$prefix/.monty-install-lock" "$installed_home.monty-install-lock"
printf '%s\n' "$stale_pid" > "$prefix/.monty-install-lock/pid"
printf '%s\n' "$stale_pid" > "$installed_home.monty-install-lock/pid"
if ! run_install; then
  fail 'stale installer locks were not recovered'
fi
assert_contains_file 'stale lock recovery diagnostic' "$log" \
  'Recovered stale installer lock'
assert_absent 'recovered prefix lock' "$prefix/.monty-install-lock"
assert_absent 'recovered home lock' "$installed_home.monty-install-lock"
assert_contents 'control room after stale lock recovery' \
  "$installed_home/control-room.txt" 'release three'
assert_contents 'state after stale lock recovery' "$state_dir/version" '1'

sleep 30 &
live_lock_pid=$!
mkdir "$prefix/.monty-install-lock"
printf '%s\n' "$live_lock_pid" > "$prefix/.monty-install-lock/pid"
if run_install; then
  kill "$live_lock_pid" 2>/dev/null || true
  wait "$live_lock_pid" 2>/dev/null || true
  fail 'a live installer lock was recovered'
fi
kill "$live_lock_pid" 2>/dev/null || true
wait "$live_lock_pid" 2>/dev/null || true
if ! run_install; then
  fail 'installer lock was not recovered after its owner exited'
fi
assert_contains_file 'dead live-lock recovery diagnostic' "$log" \
  'Recovered stale installer lock'
assert_absent 'recovered dead live lock' "$prefix/.monty-install-lock"
assert_contents 'control room after live lock checks' \
  "$installed_home/control-room.txt" 'release three'
assert_contents 'state after live lock checks' "$state_dir/version" '1'
printf 'release four\n' > "$repo/control-room.txt"
printf 'binary four\n' > "$repo/binary-release.txt"

fake_mv_fail_stage_name=control-room.txt
if run_install; then
  fail 'injected control-room activation failure was accepted'
fi
fake_mv_fail_stage_name=
assert_contents 'rolled-back control room after activation failure' \
  "$installed_home/control-room.txt" 'release three'
assert_contents 'state after activation failure' "$state_dir/version" '1'
assert_contents 'task state after activation failure' "$state_dir/tasks.local.json" \
  '{"tasks":[{"id":"local-001","title":"Keep me"}]}'
assert_binary_release 'binary after activation failure' 'binary three'

fake_mv_interrupt_stage_name=control-room.txt
expect_interrupted_install 'injected activation interruption'
fake_mv_interrupt_stage_name=
assert_contents 'rolled-back control room after interruption' \
  "$installed_home/control-room.txt" 'release three'
assert_contents 'state after interruption' "$state_dir/version" '1'
assert_binary_release 'binary after interruption' 'binary three'

rm "$state_dir/version"
fake_ln_fail_version=1
if run_install; then
  fail 'injected state-version activation failure was accepted'
fi
fake_ln_fail_version=0
assert_absent 'legacy version after activation rollback' "$state_dir/version"
assert_contents 'control room after state-version activation failure' \
  "$installed_home/control-room.txt" 'release three'
assert_contents 'legacy task state after activation rollback' \
  "$state_dir/tasks.local.json" \
  '{"tasks":[{"id":"local-001","title":"Keep me"}]}'
assert_binary_release 'binary after state-version activation failure' 'binary three'

fake_ln_interrupt_version=1
expect_interrupted_install 'interruption after state-version creation'
fake_ln_interrupt_version=0
assert_absent 'version after interrupted legacy adoption' "$state_dir/version"
assert_contents 'control room after interrupted legacy adoption' \
  "$installed_home/control-room.txt" 'release three'
assert_contents 'task state after interrupted legacy adoption' \
  "$state_dir/tasks.local.json" \
  '{"tasks":[{"id":"local-001","title":"Keep me"}]}'
assert_binary_release 'binary after interrupted legacy adoption' 'binary three'
printf '1\n' > "$state_dir/version"

unsafe_version_target=$root/unsafe-version-target
printf '1\n' > "$unsafe_version_target"
rm "$state_dir/version"
ln -s "$unsafe_version_target" "$state_dir/version"
if run_install; then
  fail 'symlinked state version was accepted'
fi
assert_contents 'unsafe version target remains untouched' "$unsafe_version_target" '1'
assert_contents 'control room after unsafe state refusal' \
  "$installed_home/control-room.txt" 'release three'
rm "$state_dir/version"
printf '1\n' > "$state_dir/version"

fake_mv_fail_binary=1
if run_install; then
  fail 'injected real-binary rename failure was accepted'
fi
fake_mv_fail_binary=0
assert_contents 'control room rolled back after binary rename failure' \
  "$installed_home/control-room.txt" 'release three'
assert_contents 'state after binary rename failure' "$state_dir/version" '1'
assert_binary_release 'atomic binary after rename failure' 'binary three'

fake_mv_fail_wrapper=1
if run_install; then
  fail 'injected wrapper rename failure was accepted'
fi
fake_mv_fail_wrapper=0
assert_contents 'control room rolled back after wrapper rename failure' \
  "$installed_home/control-room.txt" 'release three'
assert_contents 'state after wrapper rename failure' "$state_dir/version" '1'
assert_binary_release 'binary rolled back after wrapper failure' 'binary three'

external_bin=$root/external-state-bin
mkdir "$external_bin"
ln -s "$external_bin" "$state_dir/bin"
run_install
assert_absent 'write through preserved state bin symlink' "$external_bin/monty-real"
if [ ! -L "$state_dir/bin" ]; then
  fail 'state bin symlink was not preserved'
fi
assert_contents 'final control room' "$installed_home/control-room.txt" 'release four'
assert_contents 'final retained task state' "$state_dir/tasks.local.json" \
  '{"tasks":[{"id":"local-001","title":"Keep me"}]}'
assert_binary_release 'final upgraded binary' 'binary four'

quoted_home=$root/monty\'s-home
install_monty_home=$quoted_home
run_install
install_monty_home=
assert_contents 'quoted Monty home state version' "$quoted_home/.monty/version" '1'
assert_binary_release 'wrapper with quoted Monty home' 'binary four'
run_install
assert_binary_release 'wrapper restored to normal Monty home' 'binary four'

shell_rc_target=$home/managed.zshrc
shell_rc_file=$home/test.zshrc
printf 'export KEEP_ME=1\n' > "$shell_rc_target"
ln -s "$(basename "$shell_rc_target")" "$shell_rc_file"
install_shell_rc=$shell_rc_file
run_install
install_shell_rc=
if [ ! -L "$shell_rc_file" ]; then
  fail 'shell config symlink was replaced'
fi
assert_contains_file 'shell config preserves existing content' \
  "$shell_rc_target" 'export KEEP_ME=1'
assert_contains_file 'shell config records Monty home' \
  "$shell_rc_target" "export MONTY_HOME='$installed_home'"
for shell_temp in "$home"/.monty-shell-rc.*; do
  if [ -e "$shell_temp" ] || [ -L "$shell_temp" ]; then
    fail "shell config temporary file remains: $shell_temp"
  fi
done

start_concurrent_install() {
  concurrent_repo=$1
  concurrent_home=$2
  concurrent_log=$3
  (
    cd "$concurrent_repo"
    PATH="$fake_bin:$PATH" HOME="$home" \
      REAL_TAR="$real_tar" REAL_MV="$real_mv" REAL_LN="$real_ln" \
      FAKE_STATE_DIR="$concurrent_home/.monty" \
      FAKE_INSTALL_PID_FILE="$concurrent_home.monty-install-lock/pid" \
      FAKE_MV_STAGE_SLEEP=1 \
      ./install.sh --prefix "$concurrent_prefix" \
        --monty-home "$concurrent_home" --no-shell-rc
  ) > "$concurrent_log" 2>&1 &
  concurrent_pid=$!
}

concurrent_prefix=$root/concurrent-prefix
concurrent_home_a=$root/concurrent-home-a
concurrent_home_b=$root/concurrent-home-b
concurrent_repo_a=$root/concurrent-repo-a
concurrent_repo_b=$root/concurrent-repo-b
concurrent_log_a=$root/concurrent-a.log
concurrent_log_b=$root/concurrent-b.log
cp -R "$repo" "$concurrent_repo_a"
cp -R "$repo" "$concurrent_repo_b"
printf 'concurrent control a\n' > "$concurrent_repo_a/control-room.txt"
printf 'concurrent binary a\n' > "$concurrent_repo_a/binary-release.txt"
printf 'concurrent control b\n' > "$concurrent_repo_b/control-room.txt"
printf 'concurrent binary b\n' > "$concurrent_repo_b/binary-release.txt"
start_concurrent_install "$concurrent_repo_a" "$concurrent_home_a" "$concurrent_log_a"
concurrent_pid_a=$concurrent_pid
start_concurrent_install "$concurrent_repo_b" "$concurrent_home_b" "$concurrent_log_b"
concurrent_pid_b=$concurrent_pid
concurrent_status_a=0
concurrent_status_b=0
wait "$concurrent_pid_a" || concurrent_status_a=$?
wait "$concurrent_pid_b" || concurrent_status_b=$?
if [ "$concurrent_status_a" -eq 0 ] && [ "$concurrent_status_b" -eq 0 ]; then
  fail 'same-prefix concurrent installers both acquired the lock'
fi
if [ "$concurrent_status_a" -ne 0 ] && [ "$concurrent_status_b" -ne 0 ]; then
  fail 'same-prefix concurrent installers both failed'
fi
concurrent_binary=$concurrent_prefix/libexec/monty/monty-real
concurrent_wrapper=$concurrent_prefix/bin/monty
concurrent_binary_release=$($concurrent_binary)
concurrent_wrapper_release=$($concurrent_wrapper)
if [ "$concurrent_binary_release" != "$concurrent_wrapper_release" ]; then
  fail 'same-prefix concurrent install mixed wrapper and binary releases'
fi
case "$concurrent_binary_release" in
  'concurrent binary a'|'concurrent binary b') ;;
  *) fail "unexpected concurrent binary release: $concurrent_binary_release" ;;
esac

printf 'PASS install transaction and state version preservation\n'
