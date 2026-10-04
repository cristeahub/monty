#!/bin/sh
set -eu

installer=$1
source_version=$2
checkout_binary=$(CDPATH='' cd -- "$(dirname "$3")" && pwd -P)/$(basename "$3")
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
fail_shell_rc=0
interrupt_after_source=
pause_source=
same_home_argument=$repo

mkdir -p "$repo/.monty" "$fake_bin" "$home"
cp "$installer" "$repo/install.sh"
cp "$source_version" "$repo/.monty/version"
cp -R "$(dirname "$installer")/agent-profiles" "$repo/agent-profiles"
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
if [ '$release' = 'broken runtime' ] && [ "\${1:-}" = doctor ]; then exit 139; fi
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
  */.monty-shell-rc.*)
    if [ "${FAIL_SHELL_RC:-0}" -eq 1 ]; then
      exit 48
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
if [ -n "${PAUSE_SOURCE:-}" ] && [ "$source_path" = "$PAUSE_SOURCE" ]; then
  "$REAL_MV" "$@"
  touch "$PAUSE_READY"
  count=0
  while [ ! -f "$PAUSE_RELEASE" ]; do
    count=$((count + 1))
    [ "$count" -lt 1000 ] || exit 48
    sleep 0.01
  done
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
      FAIL_WRAPPER="$fail_wrapper" FAIL_SHELL_RC="$fail_shell_rc" \
      INTERRUPT_AFTER_SOURCE="$interrupt_after_source" \
      PAUSE_SOURCE="$pause_source" PAUSE_READY="$root/paused" PAUSE_RELEASE="$root/released" \
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

assert_contents 'source state version' "$source_version" '2'

printf 'release one\n' > "$repo/control-room.txt"
printf 'binary one\n' > "$repo/binary-release.txt"
run_install
assert_contents 'new state version' "$state_dir/version" '2'
assert_contents 'new control room' "$installed_home/control-room.txt" 'release one'
for profile in simplified reviewed solo; do
  diff -r "$repo/agent-profiles/$profile" "$installed_home/agent-profiles/$profile" ||
    fail "installed $profile instructions differ"
done
MONTY_HOME="$installed_home" "$checkout_binary" agent-profiles show > "$root/default-profile.json"
grep -F '"id": "simplified"' "$root/default-profile.json" >/dev/null ||
  fail 'fresh installation did not default to simplified'
grep -F '"fix_role": "simplifier"' "$root/default-profile.json" >/dev/null ||
  fail 'installed simplifier lost its role'
assert_release 'binary one'

printf '{"tasks":[{"id":"keep-me"}]}\n' > "$state_dir/tasks.local.json"
printf 'durable memory\n' > "$state_dir/memory.md"
mkdir -p "$state_dir/agent-profiles/personal"
printf '%s\n' '{"id":"personal"}' > \
  "$state_dir/agent-profiles/personal/profile.json"
# shellcheck disable=SC2012
state_inode=$(ls -di "$state_dir" | awk '{ print $1 }')
printf 'release two\n' > "$repo/control-room.txt"
printf 'binary two\n' > "$repo/binary-release.txt"
run_install
assert_contents 'preserved task state' "$state_dir/tasks.local.json" \
  '{"tasks":[{"id":"keep-me"}]}'
assert_contents 'preserved worker memory' "$state_dir/memory.md" 'durable memory'
assert_contents 'preserved personal profile' \
  "$state_dir/agent-profiles/personal/profile.json" '{"id":"personal"}'
assert_contents 'matching state version' "$state_dir/version" '2'
# shellcheck disable=SC2012
matching_inode=$(ls -di "$state_dir" | awk '{ print $1 }')
if [ "$matching_inode" != "$state_inode" ]; then
  fail 'matching .monty directory was replaced instead of preserved'
fi
assert_contents 'updated control room' "$installed_home/control-room.txt" 'release two'
assert_release 'binary two'

# A binary may start successfully but crash when its Unix runtime is used.
printf 'broken runtime\n' > "$repo/binary-release.txt"
printf 'must not install\n' > "$repo/control-room.txt"
if run_install; then fail 'installer accepted a broken compiler runtime'; fi
assert_log_contains 'built binary failed its runtime check'
assert_release 'binary two'
assert_contents 'runtime failure preserves source' "$installed_home/control-room.txt" 'release two'
assert_contents 'runtime failure preserves tasks' "$state_dir/tasks.local.json" \
  '{"tasks":[{"id":"keep-me"}]}'
assert_contents 'runtime failure preserves memory' "$state_dir/memory.md" 'durable memory'
assert_absent 'runtime failure leaves no install lock' "$prefix/.monty-install-lock"
printf 'binary two\n' > "$repo/binary-release.txt"
printf 'release two\n' > "$repo/control-room.txt"

# Hold source activation open while a real Monty process writes registry state.
# Compatible upgrades must never move either the home or its .monty directory.
pause_source=$installed_home/control-room.txt
run_install &
install_child=$!
count=0
while [ ! -f "$root/paused" ]; do
  count=$((count + 1))
  if [ "$count" -ge 500 ]; then
    touch "$root/released"
    wait "$install_child" || true
    fail 'installer did not reach the source activation barrier'
  fi
  sleep 0.01
done
runtime_status=0
MONTY_HOME="$installed_home" "$checkout_binary" settings set branch-prefix concurrent \
  --home "$installed_home" > "$root/runtime.log" 2>&1 || runtime_status=$?
touch "$root/released"
wait "$install_child" || fail 'concurrent install failed'
pause_source=
[ "$runtime_status" -eq 0 ] || fail 'concurrent registry write failed'
assert_contents 'concurrent install retains tasks' "$state_dir/tasks.local.json" \
  '{"tasks":[{"id":"keep-me"}]}'
grep -F 'concurrent' "$state_dir/settings.json" >/dev/null || fail 'runtime write was lost'
# shellcheck disable=SC2012
[ "$(ls -di "$state_dir" | awk '{ print $1 }')" = "$state_inode" ] || \
  fail 'concurrent install replaced the state directory'
assert_absent 'nested state after concurrent install' "$state_dir/.monty"
assert_absent 'nested control room after concurrent install' "$installed_home/monty"

printf 'interrupted release\n' > "$repo/control-room.txt"
printf 'interrupted binary\n' > "$repo/binary-release.txt"
interrupt_after_source=$installed_home/control-room.txt
interrupt_status=0
run_install || interrupt_status=$?
interrupt_after_source=
if [ "$interrupt_status" -ne 143 ]; then
  fail "interrupted home activation: expected exit 143, found $interrupt_status"
fi
assert_contents 'task state after interrupted rename' "$state_dir/tasks.local.json" \
  '{"tasks":[{"id":"keep-me"}]}'
assert_contents 'version after interrupted rename' "$state_dir/version" '2'
assert_contents 'control room after interrupted rename' \
  "$installed_home/control-room.txt" 'release two'
assert_release 'binary two'
assert_absent 'prefix lock after interruption' "$prefix/.monty-install-lock"
assert_absent 'home lock after interruption' "$installed_home.monty-install-lock"

rm "$state_dir/version"
printf 'release three\n' > "$repo/control-room.txt"
printf 'binary three\n' > "$repo/binary-release.txt"
run_install
assert_contents 'adopted legacy state version' "$state_dir/version" '2'
assert_contents 'legacy task state' "$state_dir/tasks.local.json" \
  '{"tasks":[{"id":"keep-me"}]}'
assert_release 'binary three'

printf '1\n' > "$state_dir/version"
printf 'migration release\n' > "$repo/control-room.txt"
printf 'migration binary\n' > "$repo/binary-release.txt"
run_install --dry-run
assert_log_contains 'migrate compatible .monty state from version 1 to 2 without replacing it'
assert_contents 'dry-run migration state' "$state_dir/version" '1'
run_install
assert_contents 'migrated state version' "$state_dir/version" '2'
assert_contents 'migrated task state' "$state_dir/tasks.local.json" \
  '{"tasks":[{"id":"keep-me"}]}'
assert_contents 'migration control room' \
  "$installed_home/control-room.txt" 'migration release'
assert_release 'migration binary'

printf '99\n' > "$state_dir/version"
printf 'release four\n' > "$repo/control-room.txt"
printf 'binary four\n' > "$repo/binary-release.txt"
run_install --dry-run
assert_log_contains 'WARNING: Monty state version mismatch'
assert_log_contains 'explicit confirmation or --replace-state would be required'
assert_contents 'mismatched state after dry run' "$state_dir/version" '99'
assert_contents 'control room after dry run' \
  "$installed_home/control-room.txt" 'migration release'
assert_release 'migration binary'
run_install --dry-run --replace-state
assert_log_contains 'state replacement is explicitly authorized but dry-run makes no changes'
assert_contents 'authorized dry-run state' "$state_dir/version" '99'
assert_contents 'authorized dry-run control room' \
  "$installed_home/control-room.txt" 'migration release'
assert_release 'migration binary'
if run_install; then
  fail 'non-interactive version mismatch was accepted without explicit replacement'
fi
assert_log_contains 'WARNING: Monty state version mismatch'
assert_log_contains "will delete the current .monty folder"
assert_contents 'mismatched state remains' "$state_dir/version" '99'
assert_contents 'mismatched task state remains' "$state_dir/tasks.local.json" \
  '{"tasks":[{"id":"keep-me"}]}'
assert_contents 'control room remains after refusal' \
  "$installed_home/control-room.txt" 'migration release'
assert_release 'migration binary'

run_install --replace-state
assert_contents 'replacement state version' "$state_dir/version" '2'
assert_absent 'replaced task state' "$state_dir/tasks.local.json"
assert_absent 'replaced worker memory' "$state_dir/memory.md"
assert_contents 'control room after replacement' \
  "$installed_home/control-room.txt" 'release four'
assert_release 'binary four'

printf 'survive rollback\n' > "$state_dir/tasks.local.json"
printf '1\n' > "$state_dir/version"
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

run_install
assert_contents 'version after successful migration retry' "$state_dir/version" '2'
assert_contents 'state after successful migration retry' \
  "$state_dir/tasks.local.json" 'survive rollback'
assert_contents 'control room after successful migration retry' \
  "$installed_home/control-room.txt" 'release five'
assert_release 'binary five'

printf '99\n' > "$repo/.monty/version"
printf 'same-home state\n' > "$repo/.monty/tasks.local.json"
if run_same_home_install; then
  fail 'same-home state mismatch was accepted without confirmation'
fi
assert_contents 'same-home mismatched state' "$repo/.monty/version" '99'
assert_contents 'same-home task state' \
  "$repo/.monty/tasks.local.json" 'same-home state'
run_same_home_install --replace-state
assert_contents 'same-home replacement version' "$repo/.monty/version" '2'
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

rc_file=$home/existing.zshrc
cat > "$root/rc-assignments" <<'EOF'
MONTY_HOME=/plain
  export MONTY_HOME=/exported
typeset -gx MONTY_HOME=/typeset
declare -x MONTY_HOME=/declared
set -gx MONTY_HOME /fish
EOF
cat "$root/rc-assignments" > "$rc_file"
cat >> "$rc_file" <<'EOF'
# >>> monty >>>
export MONTY_HOME=/managed
# <<< monty <<<
EOF
cp "$rc_file" "$root/rc-before"
run_install --shell-rc "$rc_file" --dry-run </dev/null
assert_log_contains 'would ask before overriding existing MONTY_HOME'
cmp -s "$rc_file" "$root/rc-before" || fail 'shell config changed during dry run'
run_install --shell-rc "$rc_file" </dev/null
assert_log_contains 'Skipping shell config update because stdin is not interactive.'
while IFS= read -r assignment; do
  assert_log_contains "$assignment"
done < "$root/rc-assignments"
if grep -F 'MONTY_HOME=/managed' "$log" >/dev/null; then
  fail 'managed shell assignment was reported as an override conflict'
fi
cmp -s "$rc_file" "$root/rc-before" || fail 'unconfirmed shell config changed'

rc_file=$home/.zshrc
rc_target=$home/dotfiles/zshrc
mkdir -p "$(dirname "$rc_target")"
ln -s dotfiles/zshrc "$rc_file"
cat > "$root/rc-kept" <<'EOF'
# MONTY_HOME=/comment
MONTY_HOME_EXTRA=/keep
export OTHER=/keep # MONTY_HOME=/comment
typeset OTHER=/keep # MONTY_HOME=/comment
set -gx MONTY_HOME_EXTRA /keep
EOF
cat "$root/rc-kept" > "$rc_target"
cat >> "$rc_target" <<'EOF'
# >>> monty >>>
export MONTY_HOME=/old
export MONTY_BRANCH_PREFIX=old
# <<< monty <<<
# >>> monty >>>
export MONTY_HOME=/duplicate
# <<< monty <<<
EOF
chmod 640 "$rc_target"
rc_permissions=$(ls -l "$rc_target" | awk '{ print $1 }')
cp "$rc_target" "$root/rc-before"
ln "$rc_target" "$root/rc-original-inode"
run_install --shell-rc "$rc_file" --branch-prefix cto </dev/null
assert_log_contains "Updated shell config: $rc_file"
[ -L "$rc_file" ] || fail 'shell config symlink was replaced'
[ "$(readlink "$rc_file")" = dotfiles/zshrc ] || fail 'shell config symlink target changed'
[ "$(ls -l "$rc_target" | awk '{ print $1 }')" = "$rc_permissions" ] ||
  fail 'shell config permissions changed'
cmp -s "$root/rc-original-inode" "$root/rc-before" ||
  fail 'shell config was changed in place instead of atomically replaced'
cat "$root/rc-kept" > "$root/rc-expected"
cat >> "$root/rc-expected" <<EOF

# >>> monty >>>
export MONTY_HOME='$installed_home'
export MONTY_BRANCH_PREFIX='cto'
# <<< monty <<<
EOF
cmp -s "$rc_target" "$root/rc-expected" || fail 'managed shell blocks were not replaced correctly'

cp "$rc_target" "$root/rc-before"
fail_shell_rc=1
if run_install --shell-rc "$rc_file" --branch-prefix changed </dev/null; then
  fail 'injected shell config publication failure was accepted'
fi
fail_shell_rc=0
cmp -s "$rc_target" "$root/rc-before" || fail 'shell config changed after failed publication'
for rc_temp in "$(dirname "$rc_target")"/.monty-shell-rc.*; do
  assert_absent 'shell config temporary file' "$rc_temp"
done

rc_file=$home/fish/conf.d/monty.fish
run_install --shell-rc "$rc_file" --branch-prefix cto --dry-run </dev/null
assert_absent 'shell config directory after dry run' "$home/fish"
run_install --shell-rc "$rc_file" --branch-prefix cto </dev/null
cat > "$root/rc-expected" <<EOF

# >>> monty >>>
set -gx MONTY_HOME '$installed_home'
set -gx MONTY_BRANCH_PREFIX 'cto'
# <<< monty <<<
EOF
cmp -s "$rc_file" "$root/rc-expected" || fail 'new fish shell config is incorrect'

printf 'PASS hardened installer, state versioning, and shell config\n'
