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

usage() {
  cat <<'EOF'
Install the monty CLI with Dune.

Usage:
  ./install.sh [options]

Options:
  --prefix DIR       Install prefix. Default: $HOME/.local
  --monty-home DIR   Monty home. Default: PREFIX/share/monty
  --dev-install      Use the current checkout as MONTY_HOME instead of copying it
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

prefix=$(expand_home_path "$prefix")
shell_rc=$(expand_home_path "$shell_rc")

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
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
monty_home=$(expand_home_path "$monty_home")

bin_dir=$prefix/bin
if [ "$dev_install" -eq 1 ]; then
  runtime_bin_dir=$prefix/libexec/monty
else
  runtime_bin_dir=$monty_home/.monty/bin
fi
monty_bin=$bin_dir/monty
monty_real_bin=$runtime_bin_dir/monty-real
built_monty_bin=$repo_root/_build/default/bin/main.exe

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
  printf '%s' "$1" | sed "s/'/'\\''/g; s/^/'/; s/$/'/"
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

copy_monty_home() {
  if same_dir "$repo_root" "$monty_home"; then
    printf 'Monty home already contains this checkout: %s\n' "$monty_home"
    return 0
  fi

  parent_dir=$(dirname -- "$monty_home")
  tmp_dir=$parent_dir/.monty-install-$$

  printf '+ copy control room to %s\n' "$(shell_quote "$monty_home")"
  if [ "$dry_run" -eq 1 ]; then
    return 0
  fi

  mkdir -p "$parent_dir"
  rm -rf "$tmp_dir"
  mkdir -p "$tmp_dir"
  (
    cd "$repo_root"
    tar \
      --exclude './_build' \
      --exclude './.git' \
      --exclude './.monty' \
      -cf - .
  ) | (cd "$tmp_dir" && tar -xf -)
  rm -rf "$monty_home"
  mv "$tmp_dir" "$monty_home"
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

write_monty_shell_rc() {
  if [ "$write_shell_rc" = 0 ]; then
    return 0
  fi

  rc_file=$shell_rc
  if [ -z "$rc_file" ]; then
    rc_file=$(default_shell_rc)
  fi

  rc_dir=$(dirname -- "$rc_file")
  tmp_file=$rc_file.monty.$$

  if [ "$dry_run" -eq 1 ]; then
    printf '+ update %s with MONTY_HOME=%s\n' "$(shell_quote "$rc_file")" "$(shell_quote "$monty_home")"
    return 0
  fi

  mkdir -p "$rc_dir"
  if [ -f "$rc_file" ]; then
    awk '
      $0 == "# >>> monty >>>" { skip = 1; next }
      $0 == "# <<< monty <<<" { skip = 0; next }
      skip != 1 { print }
    ' "$rc_file" > "$tmp_file"
  else
    : > "$tmp_file"
  fi

  {
    printf '\n# >>> monty >>>\n'
    case "$(shell_rc_kind "$rc_file")" in
      fish)
        printf 'set -gx MONTY_HOME %s\n' "$(shell_quote "$monty_home")"
        ;;
      *)
        printf 'export MONTY_HOME=%s\n' "$(shell_quote "$monty_home")"
        ;;
    esac
    printf '# <<< monty <<<\n'
  } >> "$tmp_file"

  mv "$tmp_file" "$rc_file"
  printf 'Updated shell config: %s\n' "$rc_file"
}

printf 'Installing monty to %s\n' "$prefix"
printf 'Using repo root %s\n' "$repo_root"
printf 'Using Monty home %s\n' "$monty_home"
printf 'Using dune %s\n' "$dune_version"

if [ "$refresh_lock" -eq 1 ]; then
  run_in_repo dune pkg lock
fi

run_in_repo dune build bin/main.exe

if [ "$dry_run" -eq 1 ]; then
  copy_monty_home
  printf '+ mkdir -p %s %s\n' "$(shell_quote "$runtime_bin_dir")" "$(shell_quote "$bin_dir")"
  printf '+ cp %s %s\n' "$(shell_quote "$built_monty_bin")" "$(shell_quote "$monty_real_bin")"
  printf '+ write wrapper %s with MONTY_HOME=%s\n' "$(shell_quote "$monty_bin")" "$(shell_quote "$monty_home")"
  write_monty_shell_rc
  printf 'Dry run complete.\n'
  exit 0
fi

if [ ! -x "$built_monty_bin" ]; then
  echo "install.sh: expected built binary at $built_monty_bin" >&2
  exit 1
fi

copy_monty_home
mkdir -p "$runtime_bin_dir" "$bin_dir"
cp "$built_monty_bin" "$monty_real_bin"
chmod +x "$monty_real_bin"
cat > "$monty_bin" <<EOF
#!/bin/sh
MONTY_HOME=$(shell_quote "$monty_home")
export MONTY_HOME
exec $(shell_quote "$monty_real_bin") "\$@"
EOF
chmod +x "$monty_bin"
write_monty_shell_rc

printf '\nInstalled: %s\n' "$monty_bin"
printf 'Real binary: %s\n' "$monty_real_bin"
printf 'Default MONTY_HOME: %s\n' "$monty_home"

case ":$PATH:" in
  *":$bin_dir:"*)
    ;;
  *)
    printf '\n%s is not on PATH. Add this to your shell config:\n' "$bin_dir"
    printf '  export PATH="%s:$PATH"\n' "$bin_dir"
    ;;
esac

printf '\nRunning monty doctor:\n'
"$monty_bin" doctor
