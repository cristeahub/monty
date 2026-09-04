#!/bin/sh
set -eu

state=${MONTY_FAKE_CONTAINER_STATE:?}
log=${MONTY_FAKE_CONTAINER_LOG:?}
mkdir -p "$state/containers" "$state/volumes"
printf '%s\n' "$0 $*" >> "$log"

fail_once() {
  [ "${MONTY_FAKE_CONTAINER_FAIL_ONCE:-}" = "$1" ] || return 0
  marker="$state/failed-$1"
  [ -e "$marker" ] && return 0
  : > "$marker"
  exit 88
}

json_image() {
  printf '%s\n' '[{"configuration":{"descriptor":{"digest":"sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"}},"variants":[{"platform":{"architecture":"arm64","os":"linux"},"config":{"config":{"Cmd":["sleep","infinity"],"Env":["HOME=/monty/home"],"Labels":{"com.monty.image":"apple-worker-v1"},"User":"monty"}}}]}]'
}

container_json() {
  dir=$1
  owner=$(cat "$dir/owner")
  volume=$(cat "$dir/volume")
  printf '[{"configuration":{"labels":{"com.monty.worker":"%s"},"image":{"descriptor":{"digest":"sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"}},"platform":{"architecture":"arm64","os":"linux"},"useInit":true,"capAdd":["CAP_CHOWN","CAP_DAC_OVERRIDE"],"capDrop":["ALL"],"initProcess":{"executable":"/usr/bin/sleep","arguments":["infinity"],"environment":["HOME=/monty/home","CODEX_HOME=/monty/home/.codex"],"user":{"id":{"uid":10001,"gid":10001}}},"publishedPorts":[],"publishedSockets":[],"rosetta":false,"ssh":false,"virtualization":false,"mounts":[{"destination":"/monty","type":{"volume":{"name":"%s"}}},{"destination":"/tmp","type":{"tmpfs":{}}},{"destination":"/run/monty-secrets","type":{"tmpfs":{}}}]}}]\n' "$owner" "$volume"
}

case "$1" in
  system)
    [ "$2" = status ]
    exit 0
    ;;
  image)
    [ "$2" = inspect ]
    json_image
    exit 0
    ;;
  volume)
    action=$2
    shift 2
    case "$action" in
      list)
        for dir in "$state"/volumes/*; do
          [ -d "$dir" ] && basename "$dir"
        done
        ;;
      create)
        owner=
        while [ "$#" -gt 1 ]; do
          case "$1" in
            --label) owner=${2#com.monty.worker=}; shift 2 ;;
            --opt) shift 2 ;;
            *) shift ;;
          esac
        done
        name=$1
        mkdir -p "$state/volumes/$name"
        printf '%s\n' "$owner" > "$state/volumes/$name/owner"
        ;;
      inspect)
        dir="$state/volumes/$1"
        owner=$(cat "$dir/owner")
        printf '[{"configuration":{"labels":{"com.monty.worker":"%s"}}}]\n' "$owner"
        ;;
      delete)
        rm -rf "$state/volumes/$1"
        ;;
      *) exit 91 ;;
    esac
    exit 0
    ;;
  list)
    all=false
    [ "${2:-}" = --all ] && all=true
    for dir in "$state"/containers/*; do
      [ -d "$dir" ] || continue
      if $all || [ -e "$dir/running" ]; then basename "$dir"; fi
    done
    exit 0
    ;;
  create)
    shift
    name=
    owner=
    volume=
    cap_chown=false
    cap_dac_override=false
    while [ "$#" -gt 0 ]; do
      case "$1" in
        --name) name=$2; shift 2 ;;
        --label) owner=${2#com.monty.worker=}; shift 2 ;;
        --mount)
          case "$2" in type=volume,source=*,target=/monty)
            volume=${2#type=volume,source=}; volume=${volume%,target=/monty} ;;
          esac
          shift 2
          ;;
        --cap-add)
          case "$2" in
            CAP_CHOWN) cap_chown=true ;;
            CAP_DAC_OVERRIDE) cap_dac_override=true ;;
          esac
          shift 2 ;;
        --cpus|--memory|--platform|--user|--env|--cap-drop|--entrypoint)
          shift 2 ;;
        --init) shift ;;
        *) shift ;;
      esac
    done
    $cap_chown
    $cap_dac_override
    dir="$state/containers/$name"
    mkdir -p "$dir/rootfs"
    printf '%s\n' "$owner" > "$dir/owner"
    printf '%s\n' "$volume" > "$dir/volume"
    fail_once create
    exit 0
    ;;
  inspect)
    container_json "$state/containers/$2"
    exit 0
    ;;
  start)
    : > "$state/containers/$2/running"
    exit 0
    ;;
  stop)
    name=${4:-$2}
    rm -f "$state/containers/$name/running"
    rm -rf "$state/containers/$name/secrets"
    fail_once stop
    exit 0
    ;;
  delete)
    rm -rf "$state/containers/$2"
    exit 0
    ;;
  copy|cp)
    source=$2
    destination=$3
    case "$source" in
      *:*)
        name=${source%%:*}
        path=${source#*:}
        volume=$(cat "$state/containers/$name/volume")
        case "$path" in
          /monty/*) actual="$state/volumes/$volume/${path#/monty/}" ;;
          /*) actual="$state/containers/$name/rootfs$path" ;;
        esac
        cp "$actual" "$destination"
        fail_once collect
        ;;
      *)
        name=${destination%%:*}
        path=${destination#*:}
        mkdir -p "$state/containers/$name/rootfs/$(dirname "$path")"
        cp "$source" "$state/containers/$name/rootfs$path"
        ;;
    esac
    exit 0
    ;;
  exec)
    shift
    while [ "$#" -gt 0 ]; do
      case "$1" in
        --interactive) shift ;;
        --user|--workdir|--env) shift 2 ;;
        *) break ;;
      esac
    done
    name=$1
    shift
    container_dir="$state/containers/$name"
    volume=$(cat "$container_dir/volume")
    volume_dir="$state/volumes/$volume"
    case "$1" in
      true) exit 0 ;;
      monty-inspect)
        internal=$(cat "$volume_dir/context/worktree")
        branch=$(cat "$volume_dir/context/branch")
        worktree="$volume_dir/${internal#/monty/}"
        case "${2:-}" in
          --dirty|--changes) mode=$2; expected=$3; expected_branch=$4 ;;
          *) mode=inspect; expected=$2; expected_branch=$3 ;;
        esac
        test "$internal" = "$expected"
        test "$branch" = "$expected_branch"
        test "$(git -C "$worktree" branch --show-current)" = "$branch"
        fail_once inspection
        if [ "$mode" = --dirty ]; then
          [ -z "$(git -C "$worktree" status --porcelain=v1 --untracked-files=all)" ] || exit 3
          exit 0
        fi
        if [ "$mode" = --changes ]; then
          temp=$(mktemp -d)
          git -C "$worktree" diff --numstat -z HEAD -- > "$temp/numstat"
          git -C "$worktree" diff --name-only -z HEAD -- > "$temp/tracked"
          git -C "$worktree" ls-files --others --exclude-standard -z > "$temp/untracked"
          numstat_size=$(wc -c < "$temp/numstat")
          tracked_size=$(wc -c < "$temp/tracked")
          untracked_size=$(wc -c < "$temp/untracked")
          printf 'monty-changes-v1 %s %s %s\n' "$numstat_size" "$tracked_size" "$untracked_size"
          cat "$temp/numstat" "$temp/tracked" "$temp/untracked"
          rm -rf "$temp"
          exit 0
        fi
        printf '%s\n' '== workspace =='
        git -C "$worktree" status --short --branch
        printf '%s\n' '== diff stat ==' '== processes ==' '1 sleep sleep infinity'
        exit 0
        ;;
      rm)
        path=$3
        rm -f "$container_dir/rootfs$path"
        exit 0
        ;;
      sh)
        [ "$2" = -c ]
        script=$3
        shift 3
        case "$script" in
          *'CapInh CapPrm CapEff CapAmb'*)
            :
            ;;
          *'chown monty:monty /monty/home /monty/home/.codex /run/monty-secrets'*)
            mkdir -p "$container_dir/secrets" "$volume_dir/home/.codex"
            fail_once auth
            ;;
          *'cat > /run/monty-secrets/auth.json'*)
            mkdir -p "$container_dir/secrets"
            cat > "$container_dir/secrets/auth.json"
            mkdir -p "$volume_dir/home/.codex"
            ln -sfn "$container_dir/secrets/auth.json" "$volume_dir/home/.codex/auth.json"
            shasum -a 256 "$container_dir/secrets/auth.json" | awk '{print $1}' >> "$state/auth-hashes"
            ;;
          *'cat > '*'/monty/context/task.md'*)
            mkdir -p "$volume_dir/context"; cat > "$volume_dir/context/task.md"
            ;;
          *'cat > '*'/monty/context/MONTY.md'*)
            mkdir -p "$volume_dir/context"; cat > "$volume_dir/context/MONTY.md"
            ;;
          *'cat > '*'/monty/context/job.json'*)
            mkdir -p "$volume_dir/context"; cat > "$volume_dir/context/job.json"
            ;;
          *'chown -R monty:monty /monty'*)
            mkdir -p "$volume_dir/home/.cache" "$volume_dir/home/.codex" \
              "$volume_dir/home/.local/share" "$volume_dir/repos" \
              "$volume_dir/worktrees" "$volume_dir/context" \
              "$volume_dir/outbox" "$volume_dir/artifacts"
            ;;
          *'monty-seed.bundle'*)
            marker=$1; branch=$2; internal=$3; head=$4
            worktree="$volume_dir/${internal#/monty/}"
            mkdir -p "$volume_dir/home/.cache" "$volume_dir/home/.codex" \
              "$volume_dir/repos" "$volume_dir/worktrees/task" "$volume_dir/context" \
              "$volume_dir/outbox" "$volume_dir/artifacts"
            cp "$container_dir/rootfs/monty-seed.bundle" "$volume_dir/context/source.bundle"
            if [ ! -d "$volume_dir/repos/task/.git" ]; then
              git clone -q "$volume_dir/context/source.bundle" "$volume_dir/repos/task"
            fi
            if [ ! -d "$worktree" ]; then
              if git -C "$volume_dir/repos/task" show-ref --verify --quiet "refs/heads/$branch"; then
                git -C "$volume_dir/repos/task" worktree add -q "$worktree" "$branch"
              else
                git -C "$volume_dir/repos/task" worktree add -q -b "$branch" "$worktree" "$head"
              fi
            fi
            printf '%s\n' "$internal" > "$volume_dir/context/worktree"
            printf '%s\n' "$branch" > "$volume_dir/context/branch"
            printf '%s\n' "$head" > "$volume_dir/context/seeded"
            printf '%s\n' "fake-guest wt b $branch" >> "$log"
            ;;
          *'chown root:root /monty /monty/context'*)
            :
            ;;
          *'wc -c < '*)
            marker=$1; guest=$2; stage=$3
            case "$guest" in /monty/*) actual="$volume_dir/${guest#/monty/}" ;; esac
            test -f "$actual"
            test ! -L "$actual"
            test "$(wc -c < "$actual")" -le 5242880
            cp "$actual" "$container_dir/rootfs$stage"
            ;;
          *'codex exec'*)
            translated=$(printf '%s' "$script" | sed "s#/monty#$volume_dir#g")
            HOME="$volume_dir/home" CODEX_HOME="$volume_dir/home/.codex" sh -c "$translated"
            ;;
          *) exit 92 ;;
        esac
        exit 0
        ;;
      *) exit 93 ;;
    esac
    ;;
  *) exit 94 ;;
esac
