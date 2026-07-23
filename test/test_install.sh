#!/bin/sh
set -eu

installer=$1
source_version=$2
root=$(mktemp -d "${TMPDIR:-/tmp}/monty-install-test.XXXXXX")
root=$(CDPATH='' cd -- "$root" && pwd -P)
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
real_mv=$(command -v mv)
fail_wrapper=0
interrupt_after_source=
same_home_argument=$repo

mkdir -p "$repo/.monty" "$fake_bin" "$home"
cp "$installer" "$repo/install.sh"
cp "$source_version" "$repo/.monty/version"
chmod 600 "$repo/.monty/version"

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
    ;;
  *)
    exit 2
    ;;
esac
EOF
chmod +x "$fake_bin/dune"

cat > "$fake_bin/mv" <<'EOF'
#!/bin/sh
set -eu
source_path=${1:-}
case "$source_path" in
  */.monty-wrapper-install-*)
    if [ "${FAIL_WRAPPER:-0}" -eq 1 ]; then
      exit 47
    fi
    ;;
esac
if [ -n "${INTERRUPT_AFTER_SOURCE:-}" ] &&
   [ "$source_path" = "$INTERRUPT_AFTER_SOURCE" ]; then
  "$REAL_MV" "$@"
  kill -TERM "$(cat "$INSTALL_PID_FILE")"
  sleep 1
  exit 0
fi
exec "$REAL_MV" "$@"
EOF
chmod +x "$fake_bin/mv"

fail() {
  printf 'FAIL install: %s\n' "$1" >&2
  if [ -f "$log" ]; then
    printf '%s\n' '--- install output ---' >&2
    sed -n '1,240p' "$log" >&2
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

assert_log_contains() {
  expected=$1
  if ! grep -F "$expected" "$log" >/dev/null; then
    fail "installer output did not contain: $expected"
  fi
}

assert_release() {
  expected=$1
  if [ ! -x "$real_binary" ] || [ ! -x "$wrapper" ]; then
    fail 'installed binary or wrapper is not executable'
  fi
  actual=$($real_binary)
  if [ "$actual" != "$expected" ]; then
    fail "real binary: expected $expected, found $actual"
  fi
  actual=$($wrapper)
  if [ "$actual" != "$expected" ]; then
    fail "wrapper: expected $expected, found $actual"
  fi
}

run_install() {
  (
    cd "$repo"
    PATH="$fake_bin:$PATH" HOME="$home" REAL_MV="$real_mv" \
      FAIL_WRAPPER="$fail_wrapper" \
      INTERRUPT_AFTER_SOURCE="$interrupt_after_source" \
      INSTALL_PID_FILE="$prefix/.monty-install-lock/pid" \
      ./install.sh --prefix "$prefix" --no-shell-rc "$@"
  ) > "$log" 2>&1
}

run_same_home_install() {
  same_home_prefix=$root/same-home-prefix
  (
    cd "$repo"
    PATH="$fake_bin:$PATH" HOME="$home" REAL_MV="$real_mv" FAIL_WRAPPER=0 \
      INTERRUPT_AFTER_SOURCE='' INSTALL_PID_FILE="$same_home_prefix/.monty-install-lock/pid" \
      ./install.sh --prefix "$same_home_prefix" --monty-home "$same_home_argument" \
        --no-shell-rc "$@"
  ) > "$log" 2>&1
}

assert_contents 'source state version' "$source_version" '1'

printf 'release one\n' > "$repo/control-room.txt"
printf 'binary one\n' > "$repo/binary-release.txt"
run_install
assert_contents 'new state version' "$state_dir/version" '1'
assert_contents 'new control room' "$installed_home/control-room.txt" 'release one'
assert_release 'binary one'

printf '{"tasks":[{"id":"keep-me"}]}\n' > "$state_dir/tasks.local.json"
printf 'durable memory\n' > "$state_dir/memory.md"
# shellcheck disable=SC2012
state_inode=$(ls -di "$state_dir" | awk '{ print $1 }')
printf 'release two\n' > "$repo/control-room.txt"
printf 'binary two\n' > "$repo/binary-release.txt"
run_install
assert_contents 'preserved task state' "$state_dir/tasks.local.json" \
  '{"tasks":[{"id":"keep-me"}]}'
assert_contents 'preserved worker memory' "$state_dir/memory.md" 'durable memory'
assert_contents 'matching state version' "$state_dir/version" '1'
# shellcheck disable=SC2012
matching_inode=$(ls -di "$state_dir" | awk '{ print $1 }')
if [ "$matching_inode" != "$state_inode" ]; then
  fail 'matching .monty directory was replaced instead of preserved'
fi
assert_contents 'updated control room' "$installed_home/control-room.txt" 'release two'
assert_release 'binary two'

printf 'interrupted release\n' > "$repo/control-room.txt"
printf 'interrupted binary\n' > "$repo/binary-release.txt"
interrupt_after_source=$installed_home
interrupt_status=0
run_install || interrupt_status=$?
interrupt_after_source=
if [ "$interrupt_status" -ne 143 ]; then
  fail "interrupted home activation: expected exit 143, found $interrupt_status"
fi
assert_contents 'task state after interrupted rename' "$state_dir/tasks.local.json" \
  '{"tasks":[{"id":"keep-me"}]}'
assert_contents 'version after interrupted rename' "$state_dir/version" '1'
assert_contents 'control room after interrupted rename' \
  "$installed_home/control-room.txt" 'release two'
assert_release 'binary two'
assert_absent 'prefix lock after interruption' "$prefix/.monty-install-lock"
assert_absent 'home lock after interruption' "$installed_home.monty-install-lock"

rm "$state_dir/version"
printf 'release three\n' > "$repo/control-room.txt"
printf 'binary three\n' > "$repo/binary-release.txt"
run_install
assert_contents 'adopted legacy state version' "$state_dir/version" '1'
assert_contents 'legacy task state' "$state_dir/tasks.local.json" \
  '{"tasks":[{"id":"keep-me"}]}'
assert_release 'binary three'

printf '2\n' > "$state_dir/version"
printf 'release four\n' > "$repo/control-room.txt"
printf 'binary four\n' > "$repo/binary-release.txt"
run_install --dry-run
assert_log_contains 'WARNING: Monty state version mismatch'
assert_log_contains 'explicit confirmation or --replace-state would be required'
assert_contents 'mismatched state after dry run' "$state_dir/version" '2'
assert_contents 'control room after dry run' \
  "$installed_home/control-room.txt" 'release three'
assert_release 'binary three'
run_install --dry-run --replace-state
assert_log_contains 'state replacement is explicitly authorized but dry-run makes no changes'
assert_contents 'authorized dry-run state' "$state_dir/version" '2'
assert_contents 'authorized dry-run control room' \
  "$installed_home/control-room.txt" 'release three'
assert_release 'binary three'
if run_install; then
  fail 'non-interactive version mismatch was accepted without explicit replacement'
fi
assert_log_contains 'WARNING: Monty state version mismatch'
assert_log_contains "will delete the current .monty folder"
assert_contents 'mismatched state remains' "$state_dir/version" '2'
assert_contents 'mismatched task state remains' "$state_dir/tasks.local.json" \
  '{"tasks":[{"id":"keep-me"}]}'
assert_contents 'control room remains after refusal' \
  "$installed_home/control-room.txt" 'release three'
assert_release 'binary three'

run_install --replace-state
assert_contents 'replacement state version' "$state_dir/version" '1'
assert_absent 'replaced task state' "$state_dir/tasks.local.json"
assert_absent 'replaced worker memory' "$state_dir/memory.md"
assert_contents 'control room after replacement' \
  "$installed_home/control-room.txt" 'release four'
assert_release 'binary four'

printf 'survive rollback\n' > "$state_dir/tasks.local.json"
printf 'release five\n' > "$repo/control-room.txt"
printf 'binary five\n' > "$repo/binary-release.txt"
fail_wrapper=1
if run_install; then
  fail 'injected wrapper activation failure was accepted'
fi
fail_wrapper=0
assert_contents 'state after rollback' "$state_dir/tasks.local.json" 'survive rollback'
assert_contents 'version after rollback' "$state_dir/version" '1'
assert_contents 'control room after rollback' \
  "$installed_home/control-room.txt" 'release four'
assert_release 'binary four'

printf '2\n' > "$repo/.monty/version"
printf 'same-home state\n' > "$repo/.monty/tasks.local.json"
if run_same_home_install; then
  fail 'same-home state mismatch was accepted without confirmation'
fi
assert_contents 'same-home mismatched state' "$repo/.monty/version" '2'
assert_contents 'same-home task state' \
  "$repo/.monty/tasks.local.json" 'same-home state'
run_same_home_install --replace-state
assert_contents 'same-home replacement version' "$repo/.monty/version" '1'
assert_absent 'same-home replaced task state' "$repo/.monty/tasks.local.json"
assert_contents 'same-home source checkout' "$repo/control-room.txt" 'release five'
if [ ! -f "$repo/install.sh" ]; then
  fail 'same-home install replaced the source checkout'
fi
mkdir -p "$repo/.git"
printf 'source marker\n' > "$repo/.git/source-marker"
same_home_argument=$root/nonexistent/../repo
if run_same_home_install; then
  fail 'a Monty home containing an unresolved .. component was accepted'
fi
assert_log_contains 'paths must not contain . or .. components'
assert_contents 'rejected alias source marker' \
  "$repo/.git/source-marker" 'source marker'
assert_contents 'rejected alias control room' \
  "$repo/control-room.txt" 'release five'

printf 'PASS hardened installer and state versioning\n'
