#!/bin/sh
set -eu

prefix=${MONTY_INSTALL_PREFIX:-"$HOME/.local"}
monty_home=${MONTY_INSTALL_HOME:-}
monty_home_arg_set=0
dev_install=0
refresh_lock=0
dry_run=0
replace_state=0
write_shell_rc=${MONTY_WRITE_SHELL_RC:-1}
shell_rc=${MONTY_SHELL_RC:-}
branch_prefix=${MONTY_BRANCH_PREFIX:-monty}
monty_state_version=1

prefix_lock=
home_lock=
prefix_lock_held=0
home_lock_held=0
transaction_dir=
stage_dir=
backup_home=
dev_state_backup=
binary_tmp=
wrapper_tmp=
binary_backup=
wrapper_backup=
shell_rc_tmp=
version_tmp=
transaction_active=0
home_backup_present=0
home_activated=0
state_moved=0
dev_state_reset=0
dev_state_created=0
version_created=0
binary_had_previous=0
binary_activated=0
wrapper_had_previous=0
wrapper_activated=0
rollback_incomplete=0

usage() {
  cat <<'EOF'
Install the monty CLI with Dune.

Usage:
  ./install.sh [options]

Options:
  --prefix DIR       Install prefix. Default: $HOME/.local
  --monty-home DIR   Monty home. Default: PREFIX/share/monty
  --dev-install      Use the current checkout as MONTY_HOME instead of copying it
  --branch-prefix P  Branch prefix for generated worker branches. Default: monty
  --refresh-lock     Run dune pkg lock before building
  --replace-state    Delete incompatible .monty state after displaying a warning
  --shell-rc FILE    Shell startup file to update. Defaults from $SHELL
  --no-shell-rc      Do not write MONTY_HOME to a shell startup file
  --dry-run          Print commands without running them
  -h, --help         Show this help

Environment:
  MONTY_INSTALL_PREFIX   Default install prefix
  MONTY_INSTALL_HOME     Default Monty home
  MONTY_SHELL_RC         Default shell startup file to update
  MONTY_WRITE_SHELL_RC   Set to 0 to skip shell startup file updates
  MONTY_BRANCH_PREFIX    Default branch prefix for generated worker branches

Examples:
  ./install.sh
  ./install.sh --prefix ~/.local
EOF
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --prefix)
      if [ "$#" -lt 2 ]; then
        echo "install.sh: --prefix requires a directory" >&2
        exit 2
      fi
      prefix=$2
      shift 2
      ;;
    --prefix=*)
      prefix=${1#--prefix=}
      shift
      ;;
    --monty-home)
      if [ "$#" -lt 2 ]; then
        echo "install.sh: --monty-home requires a directory" >&2
        exit 2
      fi
      monty_home=$2
      monty_home_arg_set=1
      shift 2
      ;;
    --monty-home=*)
      monty_home=${1#--monty-home=}
      monty_home_arg_set=1
      shift
      ;;
    --dev-install)
      dev_install=1
      shift
      ;;
    --branch-prefix)
      if [ "$#" -lt 2 ]; then
        echo "install.sh: --branch-prefix requires a prefix" >&2
        exit 2
      fi
      branch_prefix=$2
      shift 2
      ;;
    --branch-prefix=*)
      branch_prefix=${1#--branch-prefix=}
      shift
      ;;
    --refresh-lock)
      refresh_lock=1
      shift
      ;;
    --replace-state)
      replace_state=1
      shift
      ;;
    --shell-rc)
      if [ "$#" -lt 2 ]; then
        echo "install.sh: --shell-rc requires a file" >&2
        exit 2
      fi
      shell_rc=$2
      write_shell_rc=1
      shift 2
      ;;
    --shell-rc=*)
      shell_rc=${1#--shell-rc=}
      write_shell_rc=1
      shift
      ;;
    --no-shell-rc)
      write_shell_rc=0
      shift
      ;;
    --dry-run)
      dry_run=1
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "install.sh: unknown option: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

path_exists() {
  [ -e "$1" ] || [ -L "$1" ]
}

expand_home_path() {
  case "$1" in
    ~/*) printf '%s\n' "$HOME/${1#~/}" ;;
    ~) printf '%s\n' "$HOME" ;;
    *) printf '%s\n' "$1" ;;
  esac
}

strip_trailing_slashes() {
  stripped=$1
  while [ "$stripped" != / ] && [ "${stripped%/}" != "$stripped" ]; do
    stripped=${stripped%/}
  done
  printf '%s\n' "$stripped"
}

absolute_path() {
  expanded=$(strip_trailing_slashes "$(expand_home_path "$1")")
  case "$expanded" in
    /*) absolute=$expanded ;;
    *) absolute=$PWD/$expanded ;;
  esac
  case "$absolute/" in
    */./*|*/../*)
      echo "install.sh: paths must not contain . or .. components: $absolute" >&2
      return 1
      ;;
  esac
  printf '%s\n' "$absolute"
}

physical_path() {
  physical_input=$(absolute_path "$1")
  physical_links=0
  while [ -L "$physical_input" ]; do
    physical_link=$(readlink "$physical_input") || return 1
    case "$physical_link" in
      /*) physical_input=$physical_link ;;
      *) physical_input=$(dirname -- "$physical_input")/$physical_link ;;
    esac
    physical_input=$(strip_trailing_slashes "$physical_input")
    physical_links=$((physical_links + 1))
    if [ "$physical_links" -gt 40 ]; then
      return 1
    fi
  done
  physical_probe=$physical_input
  physical_suffix=
  while [ ! -d "$physical_probe" ]; do
    physical_name=$(basename -- "$physical_probe")
    physical_parent=$(dirname -- "$physical_probe")
    if [ "$physical_parent" = "$physical_probe" ]; then
      return 1
    fi
    physical_suffix=/$physical_name$physical_suffix
    physical_probe=$physical_parent
  done
  physical_root=$(CDPATH='' cd -- "$physical_probe" && pwd -P) || return 1
  if [ "$physical_root" = / ] && [ -z "$physical_suffix" ]; then
    printf '/\n'
  else
    printf '%s%s\n' "${physical_root%/}" "$physical_suffix"
  fi
}

paths_overlap() {
  case "$1" in
    "$2"|"$2"/*) return 0 ;;
  esac
  case "$2" in
    "$1"|"$1"/*) return 0 ;;
  esac
  return 1
}

script_dir=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd)
repo_root=$script_dir
prefix=$(absolute_path "$prefix")
shell_rc=$(expand_home_path "$shell_rc")

if [ "$dev_install" -eq 1 ] && [ "$monty_home_arg_set" -eq 1 ]; then
  echo "install.sh: --dev-install uses the current checkout as MONTY_HOME, so it cannot be combined with --monty-home" >&2
  exit 2
fi

if [ "$dev_install" -eq 1 ]; then
  monty_home=$repo_root
elif [ -z "$monty_home" ]; then
  monty_home=$prefix/share/monty
fi
monty_home=$(absolute_path "$monty_home")

bin_dir=$prefix/bin
runtime_bin_dir=$prefix/libexec/monty
monty_bin=$bin_dir/monty
monty_real_bin=$runtime_bin_dir/monty-real
built_monty_bin=$repo_root/_build/default/bin/main.exe
state_dir=$monty_home/.monty
version_file=$state_dir/version

physical_path "$prefix" >/dev/null || {
  echo "install.sh: could not resolve install prefix: $prefix" >&2
  exit 2
}
monty_home_physical=$(physical_path "$monty_home") || {
  echo "install.sh: could not resolve Monty home: $monty_home" >&2
  exit 2
}
repo_physical=$(physical_path "$repo_root") || exit 2
if [ "$repo_physical" != "$monty_home_physical" ] &&
   paths_overlap "$repo_physical" "$monty_home_physical"; then
  echo "install.sh: Monty home and source checkout must not overlap" >&2
  exit 2
fi
if [ "$monty_home_physical" = / ]; then
  echo "install.sh: Monty home must not be the filesystem root" >&2
  exit 2
fi
for install_path in "$bin_dir" "$runtime_bin_dir" "$monty_bin" "$monty_real_bin"; do
  install_physical=$(physical_path "$install_path") || exit 2
  if paths_overlap "$monty_home_physical" "$install_physical"; then
    printf 'install.sh: Monty home must not overlap install runtime path %s\n' \
      "$install_path" >&2
    exit 2
  fi
done

if ! command -v dune >/dev/null 2>&1; then
  echo "install.sh: dune is required and was not found on PATH" >&2
  exit 1
fi

dune_version=$(dune --version 2>/dev/null || true)
dune_major=$(printf '%s' "$dune_version" | cut -d. -f1)
dune_minor=$(printf '%s' "$dune_version" | cut -d. -f2)
case "$dune_major:$dune_minor" in
  *[!0-9:]*|:*|*:)
    echo "install.sh: could not parse dune version: $dune_version" >&2
    exit 1
    ;;
esac
if [ "$dune_major" -lt 3 ] || { [ "$dune_major" -eq 3 ] && [ "$dune_minor" -lt 20 ]; }; then
  echo "install.sh: dune 3.20 or newer is required, found $dune_version" >&2
  exit 1
fi

shell_quote() {
  printf '%s' "$1" | sed "s/'/'\\\\''/g; s/^/'/; s/$/'/"
}

print_command() {
  for argument in "$@"; do
    printf ' %s' "$(shell_quote "$argument")"
  done
}

run_in_repo() {
  printf '+ cd %s &&' "$(shell_quote "$repo_root")"
  print_command "$@"
  printf '\n'
  if [ "$dry_run" -eq 0 ]; then
    (cd "$repo_root" && "$@")
  fi
}

same_dir() {
  [ -d "$1" ] && [ -d "$2" ] &&
    [ "$(CDPATH='' cd -- "$1" && pwd -P)" = "$(CDPATH='' cd -- "$2" && pwd -P)" ]
}

ensure_directory() {
  directory=$1
  label=$2
  if [ -L "$directory" ]; then
    echo "install.sh: $label must not be a symlink: $directory" >&2
    return 1
  fi
  mkdir -p "$directory"
  if [ ! -d "$directory" ] || [ -L "$directory" ]; then
    echo "install.sh: $label is not a safe directory: $directory" >&2
    return 1
  fi
}

state_kind=none
installed_state_version=
inspect_state() {
  state_kind=none
  installed_state_version=
  if [ -L "$state_dir" ]; then
    echo "install.sh: Monty state directory must not be a symlink: $state_dir" >&2
    return 1
  fi
  if ! path_exists "$state_dir"; then
    return 0
  fi
  if [ ! -d "$state_dir" ]; then
    echo "install.sh: Monty state path is not a directory: $state_dir" >&2
    return 1
  fi
  if [ -L "$version_file" ]; then
    echo "install.sh: Monty state version must not be a symlink: $version_file" >&2
    return 1
  fi
  if ! path_exists "$version_file"; then
    state_kind=legacy
    return 0
  fi
  if [ ! -f "$version_file" ]; then
    echo "install.sh: Monty state version is not a regular file: $version_file" >&2
    return 1
  fi
  installed_state_version=$(cat "$version_file") || return 1
  if [ "$installed_state_version" = "$monty_state_version" ]; then
    state_kind=matching
  else
    state_kind=mismatch
  fi
}

warn_state_mismatch() {
  printf '%s\n' 'WARNING: Monty state version mismatch.' >&2
  printf 'Installer state version: %s\n' "$monty_state_version" >&2
  printf 'Current state version: %s\n' "${installed_state_version:-<empty>}" >&2
  printf 'Continuing will delete the current .monty folder: %s\n' "$state_dir" >&2
}

authorize_state() {
  if [ "$state_kind" != mismatch ]; then
    return 0
  fi
  warn_state_mismatch
  if [ "$dry_run" -eq 1 ]; then
    if [ "$replace_state" -eq 1 ]; then
      printf '%s\n' '+ state replacement is explicitly authorized but dry-run makes no changes' >&2
    else
      printf '%s\n' '+ explicit confirmation or --replace-state would be required' >&2
    fi
    return 0
  fi
  if [ "$replace_state" -eq 1 ]; then
    return 0
  fi
  if [ ! -t 0 ]; then
    printf '%s\n' 'install.sh: refusing to delete state without confirmation; rerun with --replace-state' >&2
    return 1
  fi
  printf 'Delete this .monty folder and continue? [y/N] ' >&2
  IFS= read -r answer
  case "$answer" in
    y|Y|yes|YES|Yes) return 0 ;;
    *)
      echo "install.sh: installation cancelled; existing state was preserved" >&2
      return 1
      ;;
  esac
}

write_state_version() {
  ensure_directory "$state_dir" "Monty state directory" || return 1
  version_tmp=$(mktemp "$state_dir/.version-install.XXXXXX") || return 1
  if ! (umask 077; printf '%s\n' "$monty_state_version" > "$version_tmp"); then
    rm -f "$version_tmp"
    return 1
  fi
  if ! mv "$version_tmp" "$version_file"; then
    rm -f "$version_tmp"
    version_tmp=
    return 1
  fi
  version_tmp=
}

lock_pid() {
  candidate=$1
  if [ -L "$candidate" ] || [ ! -d "$candidate" ] ||
     [ -L "$candidate/pid" ] || [ ! -f "$candidate/pid" ]; then
    return 1
  fi
  for entry in "$candidate"/* "$candidate"/.[!.]* "$candidate"/..?*; do
    if ! path_exists "$entry"; then
      continue
    fi
    if [ "$entry" != "$candidate/pid" ]; then
      return 1
    fi
  done
  owner=$(cat "$candidate/pid") || return 1
  case "$owner" in
    ''|*[!0-9]*) return 1 ;;
  esac
  [ "$owner" -gt 0 ] || return 1
  printf '%s\n' "$owner"
}

create_lock() {
  lock=$1
  if ! mkdir "$lock" 2>/dev/null; then
    owner=$(lock_pid "$lock") || {
      echo "install.sh: installer lock is malformed or unsafe: $lock" >&2
      return 1
    }
    if kill -0 "$owner" 2>/dev/null; then
      printf 'install.sh: another installation is active with PID %s: %s\n' \
        "$owner" "$lock" >&2
      return 1
    fi
    current=$(lock_pid "$lock") || return 1
    if [ "$current" != "$owner" ] || ! rm "$lock/pid" || ! rmdir "$lock"; then
      echo "install.sh: stale installer lock changed while being recovered: $lock" >&2
      return 1
    fi
    printf 'Recovered stale installer lock from dead PID %s: %s\n' "$owner" "$lock"
    mkdir "$lock" 2>/dev/null || {
      echo "install.sh: another installation acquired the lock: $lock" >&2
      return 1
    }
  fi
  if ! (umask 077; printf '%s\n' "$$" > "$lock/pid"); then
    rm -f "$lock/pid"
    rmdir "$lock" 2>/dev/null || true
    return 1
  fi
}

remove_lock() {
  lock=$1
  owner=$(lock_pid "$lock") || return 1
  if [ "$owner" != "$$" ]; then
    printf 'install.sh: refusing to remove installer lock owned by PID %s: %s\n' \
      "$owner" "$lock" >&2
    return 1
  fi
  rm "$lock/pid" && rmdir "$lock"
}

acquire_locks() {
  prefix_lock=$prefix/.monty-install-lock
  home_lock=$monty_home.monty-install-lock
  create_lock "$prefix_lock" || return 1
  prefix_lock_held=1
  if [ "$home_lock" != "$prefix_lock" ]; then
    create_lock "$home_lock" || return 1
    home_lock_held=1
  fi
}

release_locks() {
  if [ "$home_lock_held" -eq 1 ]; then
    remove_lock "$home_lock"
    home_lock_held=0
  fi
  if [ "$prefix_lock_held" -eq 1 ]; then
    remove_lock "$prefix_lock"
    prefix_lock_held=0
  fi
}

rollback_files() {
  failed=0
  if [ "$wrapper_activated" -eq 1 ]; then
    rm -f "$monty_bin" || failed=1
  fi
  if [ "$wrapper_had_previous" -eq 1 ] && path_exists "$wrapper_backup"; then
    mv "$wrapper_backup" "$monty_bin" || failed=1
  fi
  if [ "$binary_activated" -eq 1 ]; then
    rm -f "$monty_real_bin" || failed=1
  fi
  if [ "$binary_had_previous" -eq 1 ] && path_exists "$binary_backup"; then
    mv "$binary_backup" "$monty_real_bin" || failed=1
  fi
  return "$failed"
}

rollback_home() {
  failed=0
  if [ "$dev_install" -eq 1 ]; then
    if [ "$dev_state_reset" -eq 1 ]; then
      if path_exists "$dev_state_backup"; then
        rm -rf "$state_dir" || failed=1
        if [ "$failed" -eq 0 ]; then
          mv "$dev_state_backup" "$state_dir" || failed=1
        fi
      elif ! path_exists "$state_dir"; then
        failed=1
      fi
    elif [ "$dev_state_created" -eq 1 ]; then
      rm -rf "$state_dir" || failed=1
    elif [ "$version_created" -eq 1 ]; then
      rm -f "$version_file" || failed=1
    fi
    return "$failed"
  fi

  if [ "$home_activated" -eq 1 ]; then
    if [ "$state_moved" -eq 1 ] && ! path_exists "$backup_home/.monty"; then
      if [ "$version_created" -eq 1 ]; then
        rm -f "$version_file" || failed=1
      fi
      if [ "$failed" -eq 0 ] && path_exists "$state_dir"; then
        mv "$state_dir" "$backup_home/.monty" || failed=1
      else
        failed=1
      fi
    fi
    if [ "$failed" -eq 0 ]; then
      rm -rf "$monty_home" || failed=1
    fi
  fi
  if [ "$home_backup_present" -eq 1 ] && path_exists "$backup_home" &&
     ! path_exists "$monty_home"; then
    mv "$backup_home" "$monty_home" || failed=1
  fi
  return "$failed"
}

rollback_install() {
  rollback_failed=0
  rollback_files || rollback_failed=1
  rollback_home || rollback_failed=1
  transaction_active=0
  if [ "$rollback_failed" -ne 0 ]; then
    rollback_incomplete=1
    echo "install.sh: failed to restore the previous installation completely" >&2
    return 1
  fi
}

cleanup_install() {
  status=$1
  trap - 0 HUP INT TERM
  set +e
  if [ "$transaction_active" -eq 1 ]; then
    rollback_install || status=1
  fi
  [ -z "$binary_tmp" ] || rm -f "$binary_tmp"
  [ -z "$wrapper_tmp" ] || rm -f "$wrapper_tmp"
  [ -z "$shell_rc_tmp" ] || rm -f "$shell_rc_tmp"
  [ -z "$version_tmp" ] || rm -f "$version_tmp"
  if [ -n "$transaction_dir" ] && [ "$rollback_incomplete" -eq 0 ]; then
    rm -rf "$transaction_dir"
  elif [ -n "$transaction_dir" ]; then
    printf 'install.sh: recovery files were preserved at %s\n' "$transaction_dir" >&2
  fi
  if [ "$home_lock_held" -eq 1 ]; then
    remove_lock "$home_lock" >/dev/null 2>&1 || true
  fi
  if [ "$prefix_lock_held" -eq 1 ]; then
    remove_lock "$prefix_lock" >/dev/null 2>&1 || true
  fi
  exit "$status"
}

trap 'cleanup_install $?' 0
trap 'exit 129' HUP
trap 'exit 130' INT
trap 'exit 143' TERM

prepare_transaction() {
  parent_dir=$(dirname -- "$monty_home")
  ensure_directory "$parent_dir" "Monty home parent" || return 1
  transaction_dir=$(mktemp -d "$parent_dir/.monty-install.XXXXXX") || return 1
  backup_home=$transaction_dir/previous-home
  dev_state_backup=$transaction_dir/previous-state

  if [ "$dev_install" -eq 0 ]; then
    stage_dir=$transaction_dir/control-room
    archive=$transaction_dir/control-room.tar
    mkdir "$stage_dir"
    (
      cd "$repo_root"
      tar --exclude './_build' --exclude './.git' --exclude './.monty' \
        --exclude './.pi-subagents' -cf "$archive" .
    )
    (cd "$stage_dir" && tar -xf "$archive")
    rm "$archive"
  fi
}

prepare_install_files() {
  ensure_directory "$prefix" "install prefix" || return 1
  ensure_directory "$runtime_bin_dir" "runtime binary directory" || return 1
  ensure_directory "$bin_dir" "wrapper directory" || return 1
  if [ -d "$monty_real_bin" ] && [ ! -L "$monty_real_bin" ]; then
    echo "install.sh: real binary path is a directory: $monty_real_bin" >&2
    return 1
  fi
  if [ -d "$monty_bin" ] && [ ! -L "$monty_bin" ]; then
    echo "install.sh: wrapper path is a directory: $monty_bin" >&2
    return 1
  fi

  binary_tmp=$(mktemp "$runtime_bin_dir/.monty-real-install-XXXXXX") || return 1
  cp "$built_monty_bin" "$binary_tmp"
  chmod 755 "$binary_tmp"

  wrapper_tmp=$(mktemp "$bin_dir/.monty-wrapper-install-XXXXXX") || return 1
  cat > "$wrapper_tmp" <<EOF
#!/bin/sh
MONTY_HOME=$(shell_quote "$monty_home")
MONTY_BRANCH_PREFIX=$(shell_quote "$branch_prefix")
export MONTY_HOME MONTY_BRANCH_PREFIX
exec $(shell_quote "$monty_real_bin") "\$@"
EOF
  chmod 755 "$wrapper_tmp"

  binary_backup=$(mktemp "$runtime_bin_dir/.monty-real-backup-XXXXXX") || return 1
  wrapper_backup=$(mktemp "$bin_dir/.monty-wrapper-backup-XXXXXX") || return 1
  rm "$binary_backup" "$wrapper_backup"
}

activate_control_room() {
  if [ "$dev_install" -eq 1 ]; then
    case "$state_kind" in
      none)
        dev_state_created=1
        write_state_version
        ;;
      legacy)
        version_created=1
        write_state_version
        ;;
      matching) ;;
      mismatch)
        dev_state_reset=1
        mv "$state_dir" "$dev_state_backup"
        write_state_version
        ;;
    esac
    return 0
  fi

  if path_exists "$monty_home"; then
    if [ -L "$monty_home" ] || [ ! -d "$monty_home" ]; then
      echo "install.sh: Monty home is not a safe directory: $monty_home" >&2
      return 1
    fi
    home_backup_present=1
    mv "$monty_home" "$backup_home"
  fi
  home_activated=1
  mv "$stage_dir" "$monty_home"
  stage_dir=

  case "$state_kind" in
    matching)
      state_moved=1
      mv "$backup_home/.monty" "$state_dir"
      ;;
    legacy)
      state_moved=1
      mv "$backup_home/.monty" "$state_dir"
      version_created=1
      write_state_version
      ;;
    none|mismatch)
      write_state_version
      ;;
  esac
}

activate_install_files() {
  if path_exists "$monty_real_bin"; then
    binary_had_previous=1
    mv "$monty_real_bin" "$binary_backup"
  fi
  binary_activated=1
  mv "$binary_tmp" "$monty_real_bin"
  binary_tmp=

  if path_exists "$monty_bin"; then
    wrapper_had_previous=1
    mv "$monty_bin" "$wrapper_backup"
  fi
  wrapper_activated=1
  mv "$wrapper_tmp" "$monty_bin"
  wrapper_tmp=
}

commit_install() {
  transaction_active=0
  [ "$binary_had_previous" -eq 0 ] || rm -f "$binary_backup"
  [ "$wrapper_had_previous" -eq 0 ] || rm -f "$wrapper_backup"
  rm -rf "$transaction_dir"
  transaction_dir=
}

shell_name() {
  basename "${SHELL:-sh}"
}

default_shell_rc() {
  case "$(shell_name)" in
    zsh) printf '%s\n' "$HOME/.zshrc" ;;
    bash) printf '%s\n' "$HOME/.bashrc" ;;
    fish) printf '%s\n' "$HOME/.config/fish/conf.d/monty.fish" ;;
    *) printf '%s\n' "$HOME/.profile" ;;
  esac
}

shell_rc_kind() {
  case "$1" in
    *.fish) printf '%s\n' fish ;;
    *) printf '%s\n' posix ;;
  esac
}

existing_monty_home_assignments() {
  if [ ! -f "$1" ]; then
    return 0
  fi
  awk '
    function is_monty_home_assignment(line) {
      return \
        line ~ /^[[:space:]]*(export[[:space:]]+)?MONTY_HOME[[:space:]]*=/ || \
        line ~ /^[[:space:]]*(typeset|declare)[[:space:]][^#]*MONTY_HOME[[:space:]]*=/ || \
        line ~ /^[[:space:]]*set[[:space:]][^#]*[[:space:]]MONTY_HOME([[:space:]]|$)/
    }
    $0 == "# >>> monty >>>" { skip = 1; next }
    $0 == "# <<< monty <<<" { skip = 0; next }
    skip != 1 && is_monty_home_assignment($0) { print }
  ' "$1"
}

confirm_shell_rc_override() {
  rc_file=$1
  existing=$2
  if [ -z "$existing" ]; then
    return 0
  fi
  printf '\nFound an existing MONTY_HOME setting in %s:\n' "$rc_file"
  printf '%s\n' "$existing"
  if [ ! -t 0 ]; then
    printf 'Skipping shell config update because stdin is not interactive.\n'
    printf 'Set Monty environment manually if needed:\n'
    printf '  export MONTY_HOME=%s\n' "$(shell_quote "$monty_home")"
    printf '  export MONTY_BRANCH_PREFIX=%s\n' "$(shell_quote "$branch_prefix")"
    return 1
  fi
  printf 'Override it with MONTY_HOME=%s? [y/N] ' "$(shell_quote "$monty_home")"
  IFS= read -r answer
  case "$answer" in
    y|Y|yes|YES|Yes) return 0 ;;
    *)
      printf 'Skipping shell config update.\n'
      return 1
      ;;
  esac
}

resolve_shell_rc_target() {
  target=$1
  links=0
  while [ -L "$target" ]; do
    link=$(readlink "$target") || return 1
    case "$link" in
      /*) target=$link ;;
      *) target=$(dirname -- "$target")/$link ;;
    esac
    links=$((links + 1))
    [ "$links" -le 40 ] || return 1
  done
  printf '%s\n' "$target"
}

write_monty_shell_rc() {
  if [ "$write_shell_rc" = 0 ]; then
    return 0
  fi
  rc_file=$shell_rc
  if [ -z "$rc_file" ]; then
    rc_file=$(default_shell_rc)
  fi
  existing=$(existing_monty_home_assignments "$rc_file")
  if [ "$dry_run" -eq 1 ]; then
    if [ -n "$existing" ]; then
      printf '+ would ask before overriding existing MONTY_HOME in %s\n' \
        "$(shell_quote "$rc_file")"
    fi
    printf '+ update %s with MONTY_HOME=%s and MONTY_BRANCH_PREFIX=%s\n' \
      "$(shell_quote "$rc_file")" "$(shell_quote "$monty_home")" \
      "$(shell_quote "$branch_prefix")"
    return 0
  fi

  remove_existing=0
  if [ -n "$existing" ]; then
    if confirm_shell_rc_override "$rc_file" "$existing"; then
      remove_existing=1
    else
      return 0
    fi
  fi

  rc_target=$(resolve_shell_rc_target "$rc_file") || {
    echo "install.sh: could not resolve shell config: $rc_file" >&2
    return 1
  }
  rc_dir=$(dirname -- "$rc_target")
  ensure_directory "$rc_dir" "shell config directory" || return 1
  shell_rc_tmp=$(mktemp "$rc_dir/.monty-shell-rc.XXXXXX") || return 1
  if [ -f "$rc_target" ]; then
    cp -p "$rc_target" "$shell_rc_tmp"
    awk -v remove_existing="$remove_existing" '
      function is_monty_home_assignment(line) {
        return \
          line ~ /^[[:space:]]*(export[[:space:]]+)?MONTY_HOME[[:space:]]*=/ || \
          line ~ /^[[:space:]]*(typeset|declare)[[:space:]][^#]*MONTY_HOME[[:space:]]*=/ || \
          line ~ /^[[:space:]]*set[[:space:]][^#]*[[:space:]]MONTY_HOME([[:space:]]|$)/
      }
      $0 == "# >>> monty >>>" { skip = 1; next }
      $0 == "# <<< monty <<<" { skip = 0; next }
      skip == 1 { next }
      remove_existing == "1" && is_monty_home_assignment($0) { next }
      { print }
    ' "$rc_target" > "$shell_rc_tmp"
  fi
  {
    printf '\n# >>> monty >>>\n'
    case "$(shell_rc_kind "$rc_file")" in
      fish)
        printf 'set -gx MONTY_HOME %s\n' "$(shell_quote "$monty_home")"
        printf 'set -gx MONTY_BRANCH_PREFIX %s\n' "$(shell_quote "$branch_prefix")"
        ;;
      *)
        printf 'export MONTY_HOME=%s\n' "$(shell_quote "$monty_home")"
        printf 'export MONTY_BRANCH_PREFIX=%s\n' "$(shell_quote "$branch_prefix")"
        ;;
    esac
    printf '# <<< monty <<<\n'
  } >> "$shell_rc_tmp"
  mv "$shell_rc_tmp" "$rc_target"
  shell_rc_tmp=
  printf 'Updated shell config: %s\n' "$rc_file"
}

if same_dir "$repo_root" "$monty_home"; then
  dev_install=1
fi

printf 'Installing monty to %s\n' "$prefix"
printf 'Using repo root %s\n' "$repo_root"
printf 'Using Monty home %s\n' "$monty_home"
printf 'Using branch prefix %s\n' "$branch_prefix"
printf 'Using Monty state version %s\n' "$monty_state_version"
printf 'Using dune %s\n' "$dune_version"

inspect_state
if [ "$dry_run" -eq 1 ] && [ "$state_kind" = mismatch ]; then
  authorize_state
fi

if [ "$refresh_lock" -eq 1 ]; then
  run_in_repo dune pkg lock
fi
run_in_repo dune build bin/main.exe

if [ "$dry_run" -eq 1 ]; then
  if [ "$state_kind" = legacy ]; then
    printf '+ adopt existing unversioned .monty state as version %s\n' "$monty_state_version"
  elif [ "$state_kind" = none ] || [ "$state_kind" = mismatch ]; then
    printf '+ initialize .monty state version %s\n' "$monty_state_version"
  else
    printf '+ preserve matching .monty state version %s\n' "$monty_state_version"
  fi
  if [ "$dev_install" -eq 0 ]; then
    printf '+ transactionally replace control room at %s while preserving compatible state\n' \
      "$(shell_quote "$monty_home")"
  fi
  printf '+ atomically install binary at %s\n' "$(shell_quote "$monty_real_bin")"
  printf '+ atomically install wrapper at %s\n' "$(shell_quote "$monty_bin")"
  write_monty_shell_rc
  printf 'Dry run complete.\n'
  exit 0
fi

if [ ! -x "$built_monty_bin" ]; then
  echo "install.sh: expected built binary at $built_monty_bin" >&2
  exit 1
fi

prepare_transaction
prepare_install_files
acquire_locks
inspect_state
authorize_state
transaction_active=1
activate_control_room
activate_install_files
commit_install
release_locks
write_monty_shell_rc

printf '\nInstalled: %s\n' "$monty_bin"
printf 'Real binary: %s\n' "$monty_real_bin"
printf 'Default MONTY_HOME: %s\n' "$monty_home"
printf 'Default MONTY_BRANCH_PREFIX: %s\n' "$branch_prefix"
printf 'Monty state version: %s\n' "$monty_state_version"

case ":$PATH:" in
  *":$bin_dir:"*) ;;
  *)
    printf '\n%s is not on PATH. Add this to your shell config:\n' "$bin_dir"
    # shellcheck disable=SC2016
    printf '  export PATH="%s:$PATH"\n' "$bin_dir"
    ;;
esac

printf '\nRunning monty doctor:\n'
"$monty_bin" doctor
