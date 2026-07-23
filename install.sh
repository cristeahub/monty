#!/bin/sh
set -eu

prefix=${MONTY_INSTALL_PREFIX:-"$HOME/.local"}
monty_home=${MONTY_INSTALL_HOME:-}
monty_home_arg_set=0
dev_install=0
refresh_lock=0
dry_run=0
write_shell_rc=${MONTY_WRITE_SHELL_RC:-1}
shell_rc=${MONTY_SHELL_RC:-}
branch_prefix=${MONTY_BRANCH_PREFIX:-monty}
monty_state_version=1
install_lock_dir=
prefix_lock_dir=
staging_dir=
archive_file=
backup_dir=
file_backup_dir=
binary_tmp=
wrapper_tmp=
shell_rc_tmp=
version_tmp=
version_tmp_dir=
lock_held=0
prefix_lock_held=0
activation_in_progress=0
activation_phase=
binary_activated=0
binary_had_previous=0
wrapper_activated=0
wrapper_had_previous=0
version_linked=0
state_dir_created=0
monty_home_created=0

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

expand_home_path() {
  case "$1" in
    ~/*)
      printf '%s\n' "$HOME/${1#~/}"
      ;;
    ~)
      printf '%s\n' "$HOME"
      ;;
    *)
      printf '%s\n' "$1"
      ;;
  esac
}

strip_trailing_slashes() {
  path=$1
  while [ "$path" != / ] && [ "${path%/}" != "$path" ]; do
    path=${path%/}
  done
  printf '%s\n' "$path"
}

physical_path() {
  physical_input=$1
  case "$physical_input" in
    /*) ;;
    *) physical_input=$PWD/$physical_input ;;
  esac
  physical_input=$(strip_trailing_slashes "$physical_input")
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

prefix=$(strip_trailing_slashes "$(expand_home_path "$prefix")")
shell_rc=$(expand_home_path "$shell_rc")

script_dir=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd)
repo_root=$script_dir

if [ "$dev_install" -eq 1 ] && [ "$monty_home_arg_set" -eq 1 ]; then
  echo "install.sh: --dev-install uses the current checkout as MONTY_HOME, so it cannot be combined with --monty-home" >&2
  exit 2
fi

if [ "$dev_install" -eq 1 ]; then
  monty_home=$repo_root
elif [ -z "$monty_home" ]; then
  monty_home=$prefix/share/monty
fi
monty_home=$(strip_trailing_slashes "$(expand_home_path "$monty_home")")
prefix_physical=$(physical_path "$prefix") || {
  echo "install.sh: could not resolve install prefix: $prefix" >&2
  exit 2
}
monty_home_physical=$(physical_path "$monty_home") || {
  echo "install.sh: could not resolve Monty home: $monty_home" >&2
  exit 2
}
if [ "$monty_home_physical" = / ]; then
  echo "install.sh: Monty home must not be the filesystem root" >&2
  exit 2
fi
case "$prefix_physical" in
  "$monty_home_physical"|"$monty_home_physical"/*)
    echo "install.sh: install prefix must not equal or be inside Monty home" >&2
    exit 2
    ;;
esac

bin_dir=$prefix/bin
runtime_bin_dir=$prefix/libexec/monty
monty_bin=$bin_dir/monty
monty_real_bin=$runtime_bin_dir/monty-real
built_monty_bin=$repo_root/_build/default/bin/main.exe
bin_dir_physical=$(physical_path "$bin_dir")
runtime_bin_dir_physical=$(physical_path "$runtime_bin_dir")
monty_bin_physical=$(physical_path "$monty_bin")
monty_real_bin_physical=$(physical_path "$monty_real_bin")
for install_path in "$bin_dir_physical" "$runtime_bin_dir_physical" \
  "$monty_bin_physical" "$monty_real_bin_physical"; do
  if paths_overlap "$monty_home_physical" "$install_path"; then
    printf 'install.sh: Monty home must not overlap an install runtime path: %s and %s\n' \
      "$monty_home" "$install_path" >&2
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

case "$dune_major" in
  ''|*[!0-9]*)
    echo "install.sh: could not parse dune version: $dune_version" >&2
    exit 1
    ;;
esac

case "$dune_minor" in
  ''|*[!0-9]*)
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
  for arg in "$@"; do
    printf ' %s' "$(shell_quote "$arg")"
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

run_command() {
  printf '+'
  print_command "$@"
  printf '\n'

  if [ "$dry_run" -eq 0 ]; then
    "$@"
  fi
}

same_dir() {
  [ -d "$1" ] && [ -d "$2" ] && [ "$(cd "$1" && pwd -P)" = "$(cd "$2" && pwd -P)" ]
}

path_exists() {
  [ -e "$1" ] || [ -L "$1" ]
}

inspect_monty_state() {
  state_dir=$monty_home/.monty
  version_file=$state_dir/version
  preserve_monty_state=0
  monty_state_needs_version=0

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

  preserve_monty_state=1
  if [ -L "$version_file" ]; then
    echo "install.sh: Monty state version must not be a symlink: $version_file" >&2
    return 1
  fi
  if ! path_exists "$version_file"; then
    monty_state_needs_version=1
    return 0
  fi
  if [ ! -f "$version_file" ]; then
    echo "install.sh: Monty state version is not a regular file: $version_file" >&2
    return 1
  fi

  installed_state_version=$(cat "$version_file") || return 1
  if [ "$installed_state_version" != "$monty_state_version" ]; then
    printf 'install.sh: Monty state version mismatch at %s: installer requires %s, found %s\n' \
      "$version_file" "$monty_state_version" "$installed_state_version" >&2
    printf 'Refusing to replace the control room or modify its state.\n' >&2
    return 1
  fi
}

install_lock_pid() {
  candidate_lock=$1
  if [ -L "$candidate_lock" ] || [ ! -d "$candidate_lock" ]; then
    return 1
  fi
  candidate_pid_file=$candidate_lock/pid
  if [ -L "$candidate_pid_file" ] || [ ! -f "$candidate_pid_file" ]; then
    return 1
  fi
  for candidate_entry in "$candidate_lock"/* "$candidate_lock"/.[!.]* \
    "$candidate_lock"/..?*; do
    if ! path_exists "$candidate_entry"; then
      continue
    fi
    if [ "$candidate_entry" != "$candidate_pid_file" ]; then
      return 1
    fi
  done
  candidate_pid=$(cat "$candidate_pid_file") || return 1
  case "$candidate_pid" in
    ''|*[!0-9]*) return 1 ;;
  esac
  if [ "$candidate_pid" -le 0 ]; then
    return 1
  fi
  printf '%s\n' "$candidate_pid"
}

install_lock_inode() {
  # shellcheck disable=SC2012
  LC_ALL=C ls -di "$1" 2>/dev/null | awk 'NR == 1 { print $1 }'
}

restore_install_lock() {
  restore_from=$1
  restore_to=$2
  if ! path_exists "$restore_to"; then
    mv "$restore_from" "$restore_to" 2>/dev/null || true
  fi
}

recover_stale_install_lock() {
  lock_dir=$1
  stale_pid=$(install_lock_pid "$lock_dir") || {
    printf 'install.sh: installer lock is malformed or unsafe and must be inspected: %s\n' \
      "$lock_dir" >&2
    return 1
  }
  if kill -0 "$stale_pid" 2>/dev/null; then
    printf 'install.sh: another installation is active with PID %s: %s\n' \
      "$stale_pid" "$lock_dir" >&2
    return 1
  fi

  stale_inode=$(install_lock_inode "$lock_dir")
  if [ -z "$stale_inode" ]; then
    return 1
  fi
  stale_lock=$lock_dir.stale.$$
  stale_suffix=0
  while path_exists "$stale_lock"; do
    stale_suffix=$((stale_suffix + 1))
    stale_lock=$lock_dir.stale.$$.$stale_suffix
  done
  if ! mv "$lock_dir" "$stale_lock" 2>/dev/null; then
    printf 'install.sh: installer lock changed while checking it: %s\n' \
      "$lock_dir" >&2
    return 1
  fi

  moved_inode=$(install_lock_inode "$stale_lock")
  moved_pid=$(install_lock_pid "$stale_lock" 2>/dev/null || true)
  if [ "$moved_inode" != "$stale_inode" ] || [ "$moved_pid" != "$stale_pid" ] || \
     kill -0 "$stale_pid" 2>/dev/null; then
    restore_install_lock "$stale_lock" "$lock_dir"
    printf 'install.sh: installer lock became active while checking it: %s\n' \
      "$lock_dir" >&2
    return 1
  fi

  if ! rm "$stale_lock/pid" || ! rmdir "$stale_lock"; then
    printf 'install.sh: could not remove stale installer lock: %s\n' \
      "$stale_lock" >&2
    return 1
  fi
  printf 'Recovered stale installer lock from dead PID %s: %s\n' \
    "$stale_pid" "$lock_dir"
}

create_install_lock() {
  lock_dir=$1
  if ! mkdir "$lock_dir" 2>/dev/null; then
    recover_stale_install_lock "$lock_dir" || return 1
    if ! mkdir "$lock_dir" 2>/dev/null; then
      printf 'install.sh: another installation acquired the lock: %s\n' \
        "$lock_dir" >&2
      return 1
    fi
  fi
  if ! (umask 077; printf '%s\n' "$$" > "$lock_dir/pid"); then
    rm -f "$lock_dir/pid"
    rmdir "$lock_dir" 2>/dev/null
    return 1
  fi
}

remove_install_lock() {
  lock_dir=$1
  lock_owner=$(install_lock_pid "$lock_dir") || {
    printf 'install.sh: refusing to remove malformed installer lock: %s\n' \
      "$lock_dir" >&2
    return 1
  }
  if [ "$lock_owner" != "$$" ]; then
    printf 'install.sh: refusing to remove installer lock owned by PID %s: %s\n' \
      "$lock_owner" "$lock_dir" >&2
    return 1
  fi
  rm -f "$lock_dir/pid" || return 1
  rmdir "$lock_dir" || return 1
}

acquire_install_lock() {
  parent_dir=$(dirname -- "$monty_home")
  prefix_lock_dir=$prefix_physical/.monty-install-lock
  install_lock_dir=$monty_home_physical.monty-install-lock
  mkdir -p "$parent_dir"

  create_install_lock "$prefix_lock_dir" || return 1
  prefix_lock_held=1
  create_install_lock "$install_lock_dir" || return 1
  lock_held=1
}

release_install_lock() {
  if [ "$lock_held" -eq 1 ]; then
    remove_install_lock "$install_lock_dir" || return 1
    lock_held=0
  fi
  if [ "$prefix_lock_held" -eq 1 ]; then
    remove_install_lock "$prefix_lock_dir" || return 1
    prefix_lock_held=0
  fi
}

ensure_install_directory() {
  directory=$1
  label=$2
  if [ -L "$directory" ]; then
    echo "install.sh: $label must not be a symlink: $directory" >&2
    return 1
  fi
  mkdir -p "$directory" || return 1
  if [ ! -d "$directory" ] || [ -L "$directory" ]; then
    echo "install.sh: $label is not a safe directory: $directory" >&2
    return 1
  fi
}

prepare_install_files() {
  ensure_install_directory "$prefix" "install prefix" || return 1
  ensure_install_directory "$runtime_bin_dir" "runtime binary directory" || return 1
  ensure_install_directory "$bin_dir" "wrapper directory" || return 1

  file_backup_dir=$(mktemp -d "$prefix/.monty-install-files.XXXXXX") || return 1
  binary_tmp=$(mktemp "$runtime_bin_dir/.monty-real-install-XXXXXX") || return 1
  cp "$built_monty_bin" "$binary_tmp" || return 1
  chmod 755 "$binary_tmp" || return 1

  wrapper_tmp=$(mktemp "$bin_dir/.monty-wrapper-install-XXXXXX") || return 1
  cat > "$wrapper_tmp" <<EOF
#!/bin/sh
MONTY_HOME=$(shell_quote "$monty_home")
MONTY_BRANCH_PREFIX=$(shell_quote "$branch_prefix")
export MONTY_HOME MONTY_BRANCH_PREFIX
exec $(shell_quote "$monty_real_bin") "\$@"
EOF
  chmod 755 "$wrapper_tmp" || return 1
}

activate_install_files() {
  if path_exists "$monty_real_bin"; then
    binary_had_previous=1
    if ! mv "$monty_real_bin" "$file_backup_dir/monty-real"; then
      binary_had_previous=0
      return 1
    fi
  fi
  binary_activated=1
  mv "$binary_tmp" "$monty_real_bin" || return 1
  binary_tmp=

  if path_exists "$monty_bin"; then
    wrapper_had_previous=1
    if ! mv "$monty_bin" "$file_backup_dir/monty"; then
      wrapper_had_previous=0
      return 1
    fi
  fi
  wrapper_activated=1
  mv "$wrapper_tmp" "$monty_bin" || return 1
  wrapper_tmp=
}

rollback_install_files() {
  install_file_rollback_failed=0

  if [ "$wrapper_activated" -eq 1 ]; then
    rm -f "$monty_bin" || install_file_rollback_failed=1
  fi
  if [ "$wrapper_had_previous" -eq 1 ] &&
     path_exists "$file_backup_dir/monty"; then
    mv "$file_backup_dir/monty" "$monty_bin" || install_file_rollback_failed=1
  fi
  if [ "$binary_activated" -eq 1 ]; then
    rm -f "$monty_real_bin" || install_file_rollback_failed=1
  fi
  if [ "$binary_had_previous" -eq 1 ] &&
     path_exists "$file_backup_dir/monty-real"; then
    mv "$file_backup_dir/monty-real" "$monty_real_bin" || install_file_rollback_failed=1
  fi
  binary_activated=0
  binary_had_previous=0
  wrapper_activated=0
  wrapper_had_previous=0
  [ "$install_file_rollback_failed" -eq 0 ]
}

move_control_entries() {
  source_dir=$1
  destination_dir=$2

  for entry in "$source_dir"/* "$source_dir"/.[!.]* "$source_dir"/..?*; do
    if ! path_exists "$entry"; then
      continue
    fi
    if [ "${entry##*/}" = .monty ]; then
      continue
    fi
    if ! mv "$entry" "$destination_dir/"; then
      return 1
    fi
  done
}

remove_control_entries() {
  for entry in "$monty_home"/* "$monty_home"/.[!.]* "$monty_home"/..?*; do
    if ! path_exists "$entry"; then
      continue
    fi
    if [ "$entry" = "$monty_home/.monty" ]; then
      continue
    fi
    rm -rf "$entry" || return 1
  done
}

rollback_activation() {
  rollback_failed=0

  rollback_install_files || rollback_failed=1
  if [ "$activation_phase" = installing ]; then
    remove_control_entries || rollback_failed=1
  fi
  if [ -d "$backup_dir" ]; then
    move_control_entries "$backup_dir" "$monty_home" || rollback_failed=1
  fi
  if [ "$version_linked" -eq 1 ] && [ -e "$version_tmp" ] &&
     [ -e "$monty_home/.monty/version" ] &&
     [ "$version_tmp" -ef "$monty_home/.monty/version" ]; then
    rm -f "$monty_home/.monty/version" || rollback_failed=1
  fi
  version_linked=0
  if [ -n "$version_tmp" ]; then
    rm -f "$version_tmp" || rollback_failed=1
    version_tmp=
  fi
  if [ -n "$version_tmp_dir" ]; then
    rmdir "$version_tmp_dir" || rollback_failed=1
    version_tmp_dir=
  fi
  if [ "$state_dir_created" -eq 1 ]; then
    rmdir "$monty_home/.monty" 2>/dev/null || true
    state_dir_created=0
  fi
  if [ "$monty_home_created" -eq 1 ]; then
    rmdir "$monty_home" 2>/dev/null || true
    monty_home_created=0
  fi
  activation_in_progress=0

  if [ "$rollback_failed" -ne 0 ]; then
    echo "install.sh: failed to restore the previous installation completely" >&2
    return 1
  fi
}

commit_activation() {
  activation_in_progress=0
  activation_phase=
  version_linked=0
  state_dir_created=0
  monty_home_created=0
  if [ -n "$version_tmp" ]; then
    rm -f "$version_tmp"
    version_tmp=
  fi
  if [ -n "$version_tmp_dir" ]; then
    rmdir "$version_tmp_dir"
    version_tmp_dir=
  fi
  if [ -n "$backup_dir" ]; then
    rm -rf "$backup_dir"
    backup_dir=
  fi
  if [ -n "$file_backup_dir" ]; then
    rm -rf "$file_backup_dir"
    file_backup_dir=
  fi
  if [ -n "$staging_dir" ]; then
    rmdir "$staging_dir"
    staging_dir=
  fi
  binary_activated=0
  binary_had_previous=0
  wrapper_activated=0
  wrapper_had_previous=0
}

cleanup_install() {
  status=$1
  set +e
  trap - 0 HUP INT TERM

  if [ "$activation_in_progress" -eq 1 ]; then
    rollback_activation || status=1
  fi
  if [ -n "$version_tmp" ]; then
    rm -f "$version_tmp"
  fi
  if [ -n "$version_tmp_dir" ]; then
    rmdir "$version_tmp_dir" 2>/dev/null
  fi
  if [ -n "$binary_tmp" ]; then
    rm -f "$binary_tmp"
  fi
  if [ -n "$wrapper_tmp" ]; then
    rm -f "$wrapper_tmp"
  fi
  if [ -n "$shell_rc_tmp" ]; then
    rm -f "$shell_rc_tmp"
  fi
  if [ -n "$archive_file" ]; then
    rm -f "$archive_file"
  fi
  if [ -n "$staging_dir" ]; then
    rm -rf "$staging_dir"
  fi
  if [ -n "$backup_dir" ]; then
    rmdir "$backup_dir" 2>/dev/null
  fi
  if [ -n "$file_backup_dir" ]; then
    rmdir "$file_backup_dir" 2>/dev/null
  fi
  if [ "$lock_held" -eq 1 ]; then
    remove_install_lock "$install_lock_dir" 2>/dev/null || true
  fi
  if [ "$prefix_lock_held" -eq 1 ]; then
    remove_install_lock "$prefix_lock_dir" 2>/dev/null || true
  fi
  exit "$status"
}

trap 'cleanup_install $?' 0
trap 'exit 129' HUP
trap 'exit 130' INT
trap 'exit 143' TERM

ensure_monty_state_version() {
  inspect_monty_state || return 1
  state_dir=$monty_home/.monty
  version_file=$state_dir/version

  if [ "$monty_state_needs_version" -eq 1 ]; then
    printf '+ adopt existing Monty state as version %s at %s\n' \
      "$monty_state_version" "$(shell_quote "$state_dir")"
  elif [ "$preserve_monty_state" -eq 0 ]; then
    printf '+ initialize Monty state version %s at %s\n' \
      "$monty_state_version" "$(shell_quote "$state_dir")"
  else
    return 0
  fi

  if [ "$dry_run" -eq 1 ]; then
    return 0
  fi

  if [ "$preserve_monty_state" -eq 0 ]; then
    if mkdir "$state_dir" 2>/dev/null; then
      state_dir_created=1
    else
      inspect_monty_state || return 1
      if [ "$monty_state_needs_version" -eq 0 ]; then
        return 0
      fi
    fi
  fi

  version_tmp_dir=$(mktemp -d "$state_dir/.version.monty-install-XXXXXX") || return 1
  version_tmp=$version_tmp_dir/version
  if ! (umask 077; set -C; printf '%s\n' "$monty_state_version" > "$version_tmp"); then
    return 1
  fi
  version_linked=1
  if ln "$version_tmp" "$version_file" 2>/dev/null; then
    return 0
  fi
  version_linked=0

  rm -f "$version_tmp"
  version_tmp=
  rmdir "$version_tmp_dir"
  version_tmp_dir=
  inspect_monty_state || return 1
  if [ "$monty_state_needs_version" -eq 0 ]; then
    return 0
  fi
  echo "install.sh: could not create Monty state version: $version_file" >&2
  return 1
}

activate_staged_control_room() {
  if ! path_exists "$monty_home"; then
    mkdir "$monty_home"
    monty_home_created=1
  elif [ ! -d "$monty_home" ] || [ -L "$monty_home" ]; then
    echo "install.sh: Monty home is not a safe directory: $monty_home" >&2
    return 1
  fi

  backup_dir=$(mktemp -d "$(dirname -- "$monty_home")/.monty-install-backup.XXXXXX")
  activation_in_progress=1
  activation_phase=backing-up

  move_control_entries "$monty_home" "$backup_dir" || return 1
  activation_phase=installing
  move_control_entries "$staging_dir" "$monty_home" || return 1

  # A cooperating installer cannot change state while the lock is held. This
  # second check also catches unsafe state created by another process while the
  # non-state entries were being activated, before installer metadata changes.
  inspect_monty_state || return 1
  ensure_monty_state_version || return 1
}

copy_monty_home() {
  if same_dir "$repo_root" "$monty_home"; then
    printf 'Monty home already contains this checkout: %s\n' "$monty_home"
    if [ "$dry_run" -eq 0 ]; then
      acquire_install_lock
      inspect_monty_state
      activation_in_progress=1
      activation_phase=files
    fi
    ensure_monty_state_version
    return 0
  fi

  parent_dir=$(dirname -- "$monty_home")

  printf '+ copy control room to %s\n' "$(shell_quote "$monty_home")"
  if [ "$dry_run" -eq 1 ]; then
    ensure_monty_state_version
    return 0
  fi

  mkdir -p "$parent_dir"
  archive_file=$(mktemp "${TMPDIR:-/tmp}/monty-install-archive.XXXXXX")
  (
    cd "$repo_root"
    tar \
      --exclude './_build' \
      --exclude './.git' \
      --exclude './.monty' \
      --exclude './.pi-subagents' \
      -cf "$archive_file" .
  )
  staging_dir=$(mktemp -d "$parent_dir/.monty-install-stage-XXXXXX")
  (cd "$staging_dir" && tar -xf "$archive_file")
  rm -f "$archive_file"
  archive_file=

  acquire_install_lock
  inspect_monty_state
  if [ "$preserve_monty_state" -eq 1 ]; then
    printf '+ preserve Monty state version %s at %s\n' \
      "$monty_state_version" "$(shell_quote "$monty_home/.monty")"
  fi
  activate_staged_control_room
}

shell_name() {
  basename "${SHELL:-sh}"
}

default_shell_rc() {
  case "$(shell_name)" in
    zsh)
      printf '%s\n' "$HOME/.zshrc"
      ;;
    bash)
      printf '%s\n' "$HOME/.bashrc"
      ;;
    fish)
      printf '%s\n' "$HOME/.config/fish/conf.d/monty.fish"
      ;;
    *)
      printf '%s\n' "$HOME/.profile"
      ;;
  esac
}

shell_rc_kind() {
  case "$1" in
    *.fish)
      printf '%s\n' fish
      ;;
    *)
      printf '%s\n' posix
      ;;
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

resolve_shell_rc_target() {
  target_path=$1
  link_count=0
  while [ -L "$target_path" ]; do
    link_target=$(readlink "$target_path") || return 1
    case "$link_target" in
      /*) target_path=$link_target ;;
      *) target_path=$(dirname -- "$target_path")/$link_target ;;
    esac
    link_count=$((link_count + 1))
    if [ "$link_count" -gt 40 ]; then
      echo "install.sh: too many shell config symlinks: $1" >&2
      return 1
    fi
  done
  printf '%s\n' "$target_path"
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
    y|Y|yes|YES|Yes)
      return 0
      ;;
    *)
      printf 'Skipping shell config update.\n'
      return 1
      ;;
  esac
}

write_monty_shell_rc() {
  if [ "$write_shell_rc" = 0 ]; then
    return 0
  fi

  rc_file=$shell_rc
  if [ -z "$rc_file" ]; then
    rc_file=$(default_shell_rc)
  fi

  rc_target=$(resolve_shell_rc_target "$rc_file") || return 1
  rc_dir=$(dirname -- "$rc_target")
  existing=$(existing_monty_home_assignments "$rc_file")

  if [ "$dry_run" -eq 1 ]; then
    if [ -n "$existing" ]; then
      printf '+ would ask before overriding existing MONTY_HOME in %s\n' "$(shell_quote "$rc_file")"
    fi
    printf '+ update %s with MONTY_HOME=%s and MONTY_BRANCH_PREFIX=%s\n' "$(shell_quote "$rc_file")" "$(shell_quote "$monty_home")" "$(shell_quote "$branch_prefix")"
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

  ensure_install_directory "$rc_dir" "shell config directory" || return 1
  shell_rc_tmp=$(mktemp "$rc_dir/.monty-shell-rc.XXXXXX") || return 1
  if [ -f "$rc_target" ]; then
    cp -p "$rc_target" "$shell_rc_tmp" || return 1
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

printf 'Installing monty to %s\n' "$prefix"
printf 'Using repo root %s\n' "$repo_root"
printf 'Using Monty home %s\n' "$monty_home"
printf 'Using branch prefix %s\n' "$branch_prefix"
printf 'Using Monty state version %s\n' "$monty_state_version"
printf 'Using dune %s\n' "$dune_version"

inspect_monty_state

if [ "$refresh_lock" -eq 1 ]; then
  run_in_repo dune pkg lock
fi

run_in_repo dune build bin/main.exe

if [ "$dry_run" -eq 1 ]; then
  copy_monty_home
  printf '+ mkdir -p %s %s\n' "$(shell_quote "$runtime_bin_dir")" "$(shell_quote "$bin_dir")"
  printf '+ install %s atomically at %s\n' \
    "$(shell_quote "$built_monty_bin")" "$(shell_quote "$monty_real_bin")"
  printf '+ write wrapper %s with MONTY_HOME=%s and MONTY_BRANCH_PREFIX=%s\n' "$(shell_quote "$monty_bin")" "$(shell_quote "$monty_home")" "$(shell_quote "$branch_prefix")"
  write_monty_shell_rc
  printf 'Dry run complete.\n'
  exit 0
fi

if [ ! -x "$built_monty_bin" ]; then
  echo "install.sh: expected built binary at $built_monty_bin" >&2
  exit 1
fi

prepare_install_files
copy_monty_home
activate_install_files
commit_activation
release_install_lock
write_monty_shell_rc

printf '\nInstalled: %s\n' "$monty_bin"
printf 'Real binary: %s\n' "$monty_real_bin"
printf 'Default MONTY_HOME: %s\n' "$monty_home"
printf 'Default MONTY_BRANCH_PREFIX: %s\n' "$branch_prefix"

case ":$PATH:" in
  *":$bin_dir:"*)
    ;;
  *)
    printf '\n%s is not on PATH. Add this to your shell config:\n' "$bin_dir"
    # shellcheck disable=SC2016
    printf '  export PATH="%s:$PATH"\n' "$bin_dir"
    ;;
esac

printf '\nRunning monty doctor:\n'
"$monty_bin" doctor
