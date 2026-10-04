# Monty Apple worker image

This image provides a native Linux ARM64 development environment managed by
[Apple container](https://github.com/apple/container). It uses Debian 13 so
projects can use glibc-based Python wheels and browser binaries. Ghostty and
the Monty supervisor stay on macOS.

| Workload | Tools in the image |
| --- | --- |
| OCaml projects | OCaml 5.4, Dune 3.24.2, C/C++ compiler, Make, pkg-config; Dune installs each project's locked compiler and dependencies |
| Web and mobile JavaScript | Node 24, npm 11.12.1, native Chromium, Java 21 for Gradle |
| Django/backend | Python 3.14, uv 0.11.3, PostgreSQL 17/PostGIS, Redis, GDAL/GEOS/PROJ, gettext, Poppler |
| Rust | Rust 1.85.1 with Cargo, Clippy, rustfmt |
| Contract benchmarks | Foundry 1.7.1 and Huff Neo 1.5.17; Foundry installs the project's pinned Solidity compiler |
| Infrastructure | Terraform 1.13.5, GitHub CLI, Git, SSH and GnuPG |
| Worker and shell tools | Pi 1.0.2, Codex, `wt`, Bash, Zsh, curl, jq, ripgrep, Perl, shellcheck, archive utilities |

Project dependencies stay in their own manifests and lockfiles. PostgreSQL and
Redis are installed but are not started automatically; workers can start
task-local services inside their private volume. No ports are published to the
host.

The auxiliary `mev-lab/tools/robinhood-alchemy` project pins Node 24.19.0 and
npm 11.17.0. Select its installed pair with
`export PATH=/opt/node-24.19.0/bin:$PATH` before running that project's commands.
The default Node 24.15.0/npm 11.12.1 pair matches the web/mobile toolchain.

For `web-apps`, generate the API client with `make src/api/index.ts` before
building and set `NODE_OPTIONS=--max-old-space-size=6144` for its large bundles.
Use `CYPRESS_INSTALL_BINARY=0 npm ci` for builds and Vitest checks when the
project's pinned Cypress binary does not support Linux ARM64.

Native iOS and Android builds remain on macOS. The ARM64 containers cover the
mobile app's JavaScript/TypeScript checks; Apple's Xcode toolchain is macOS-only,
and Google's [Linux NDK distribution](https://developer.android.com/ndk/downloads)
is x86. Older Cypress versions may also lack Linux ARM64 binaries; this image
provides native Chromium but does not replace a project's pinned test runner.

Install Apple `container` on an Apple silicon Mac running macOS 26 or newer,
then explicitly start its service with `container system start`. For native-only
builds on container 1.3.1, set `[build] rosetta = false` in
`~/.config/container/config.toml` before starting the service. Check the merged
configuration with `container system property list`.

Build and register the one pilot image explicitly; worker launch never builds,
pulls, loads, or replaces it:

```sh
container build --platform linux/arm64 --cpus 4 --memory 6g \
  --file container/apple-worker/Containerfile \
  --tag monty-apple-worker:1 container/apple-worker
monty container-image register
```

The build context is only `container/apple-worker`, so host repositories,
worker state, and credentials are not sent to the builder. The base image,
`wt` revision, and Pi/Codex tarballs are pinned and checksum-verified. Pi's npm
dependencies are resolved during the build with lifecycle scripts disabled.
OS packages use Debian's stable
repositories. The registered image digest and exported OCI archive pin the
complete installed toolchain for worker execution and recovery.

Run the toolchain and complete Monty test suite in a disposable native container:

```sh
bash container/apple-worker/verify.sh
```

This tests tracked source files, including local edits, as the unprivileged
worker user, without host mounts or credentials, and removes the container on
exit. The image build also runs
every required tool before producing the image, including `pi --version` and `codex --version`.
Codex uses its bundled Bubblewrap. The Debian system version emits a different
proc-mount error that Codex 0.153.0 cannot recognize for its container fallback.
The verification script tests the actual sandbox as the worker user.

Registration validates the Linux ARM64 image contract and records its full
digest under the Monty home lock. Apple 1.3.1 resolves locally built images by
tag, so Monty checks the digest before creation and again on the created,
stopped container before starting it. Create a recoverable, checksummed OCI copy
after review:

```sh
container image save --platform linux/arm64 \
  --output monty-apple-worker-1-linux-arm64.oci.tar monty-apple-worker:1
shasum -a 256 monty-apple-worker-1-linux-arm64.oci.tar \
  > monty-apple-worker-1-linux-arm64.oci.tar.sha256
```

Recovery remains an explicit operator action:

```sh
shasum -a 256 -c monty-apple-worker-1-linux-arm64.oci.tar.sha256
container image load --input monty-apple-worker-1-linux-arm64.oci.tar
monty container-image register
```

Apple 1.3.1 cannot copy directly from the named volume, so the container
rootfs stays writable only for a checked, size-limited report staging file.
The agent runs as `monty` with zero effective capabilities. The container keeps
only `CAP_CHOWN` and `CAP_DAC_OVERRIDE` available to Monty's root setup execs so
the root-owned Apple mounts can be initialized and reports can be staged.

For a real worker, use a registered project and a linked local task in a
dedicated one-job manifest. Container workers support headless Pi and Codex,
with exactly one repository. Sign in to the selected harness on the host first.
Monty reads its regular, user-owned `auth.json`, which must have mode `0600`:

- Pi: `PI_CODING_AGENT_DIR/auth.json`, or `~/.pi/agent/auth.json`.
- Codex: `CODEX_HOME/auth.json`, or `~/.codex/auth.json`.

Authentication is injected through stdin into private tmpfs. Pi's entire agent
directory lives there, including any refreshed credentials, and disappears when
the container stops. Refreshes do not update the host login. Pi host settings,
packages, custom providers, and environment credentials are not copied. Use a
built-in provider with stored OAuth credentials or a literal API key in
`auth.json`; host secret-manager commands and ambient cloud credentials are
not available in the guest. Each start or explicit resume reads the host login
again. Collected logs and reports are checked for credential values.

Pi uses the same profiles, JSON completion checks, and reviewer tool restrictions
as host workers. Select a model with, for example,
`--pi-command 'pi --provider openai-codex --model gpt-6.1-sol'` on `prepare-many` and
`run`/`run-many` (or set `MONTY_PI_COMMAND`). The executable must be `pi` or
`/usr/local/bin/pi` from the image. Existing images without Pi fail preflight;
rebuild and register the image before preparing new Pi workers.

After explicitly choosing headless execution, enable container workers and run:

```sh
MONTY_LIVE_SMOKE_MANIFEST=/absolute/path/to/one-job-smoke.json sh -eu -c '
  test -f "$MONTY_LIVE_SMOKE_MANIFEST"
  monty container-image register
  monty settings set harness pi
  monty settings set container-workers true
  monty headless prepare-many --dry-run --manifest "$MONTY_LIVE_SMOKE_MANIFEST"
  monty headless prepare-many --manifest "$MONTY_LIVE_SMOKE_MANIFEST"
  monty headless run-many --manifest "$MONTY_LIVE_SMOKE_MANIFEST"
'
```

Choose `codex` instead of `pi` to use Codex. The effective harness follows the
usual CLI flag, environment, then persisted-setting precedence. For either
harness, use `headless run`/`run-many` and `headless resume`; the legacy Pi
`headless begin` extension path cannot run container workers.

That command intentionally launches the selected harness and leaves the smoke task open
for `monty inspect`, `monty stop`, and an explicit `monty done` after review.
Codex uses the existing approval/sandbox setting. You do not need to enable
`codex-yolo` to build or verify the image. With `codex-yolo=false`, Codex's
workspace sandbox also restricts dependency downloads and writes to the home
cache; a cold build needs dependencies prepared within its permitted paths.
Monty preserves this policy. Use `--home /absolute/control-room`
on each Monty command when validating in an isolated control room. Setting
`container-workers` back to `false` affects new tasks; existing container tasks
retain their recorded runtime.
