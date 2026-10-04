#!/bin/bash
# Build/test tracked Monty source without host mounts or credentials.
set -euo pipefail
cd "$(dirname "$0")/../.."

git ls-files -z | COPYFILE_DISABLE=1 tar --no-xattrs --null -T - -cf - | container run --rm --interactive \
  --platform linux/arm64 --cpus 4 --memory 8g --init --cap-drop ALL \
  --user monty --env HOME=/home/monty --env CODEX_HOME=/home/monty/.codex \
  --entrypoint bash monty-apple-worker:1 -euc '
    umask 077
    mkdir -p "$CODEX_HOME"
    test "$(uname -m)" = aarch64
    test "$(id -u)" = 10001
    test "$(sed -n "s/^CapEff:[[:space:]]*//p" /proc/self/status)" = 0000000000000000
    for tool in git wt codex pi ocamlc dune cc make bash curl jq rg node npm \
      python3.14 uv rustc cargo terraform psql gdalinfo redis-server chromium java gh shasum zsh forge cast anvil hnc; do
      command -v "$tool"
    done
    codex --version
    pi --version
    codex sandbox -- /bin/echo sandbox-ready
    bash -lc '"'"'dune --version && ocamlc -version && cargo --version && rustc --version'"'"'
    dune --version
    ocamlc -version
    python3.14 --version
    uv --version
    rustc --version
    cargo --version
    forge --version
    hnc --version
    /opt/node-24.19.0/bin/node --version
    terraform version
    mkdir -p "$HOME/source"
    tar -xf - -C "$HOME/source"
    cd "$HOME/source"
    dune build @all
    dune runtest --force
  '
