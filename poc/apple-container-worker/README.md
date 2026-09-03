# Apple container worker PoC

This experiment uses Apple `container` 1.3.1 to put all task-owned state in
one named ext4 volume. Apple's `--init` forwards shutdown signals to an inert
`sleep infinity` child; host commands seed and inspect the task with
`container cp` and `container exec`.

Requirements: macOS 26, Apple `container`, Git, and Python 3. Run from the
repository root. Apple `container` 1.3.1 enables Rosetta for builders by
default; on a native-only host, set this in `~/.config/container/config.toml`
and restart the service first:

```toml
[build]
rosetta = false
```

Then run:

```sh
python3 poc/apple-container-worker/poc.py --self-check
python3 poc/apple-container-worker/poc.py \
  --output "$MONTY_WORKER_DIR/artifacts/apple-container-worker-poc"
```

The full run requires the service to be running with `build.rosetta=false` and
reuses `monty-poc-apple-worker:1` only when it matches the verified native arm64
digest and Containerfile configuration. If absent, it restores the exact image
only from a supplied OCI archive after verifying the adjacent `.sha256` file;
it deletes the known invalid fallback tag only when its full digest matches and
the recovery archive has already passed verification. The runner never builds
an image or starts/stops Apple's unlabelled global builder. It then runs 20
samples for each warm benchmark. Under Monty,
the default output is
`$MONTY_WORKER_DIR/artifacts/apple-container-worker-poc`; outside Monty,
`--output` is required. Output inside the ephemeral code worktree is rejected.
Under Monty, overrides must remain below `$MONTY_WORKER_DIR/artifacts`.

If the verified local tag is absent, recover it while writing fresh evidence:

```sh
python3 poc/apple-container-worker/poc.py \
  --image-archive /durable/path/monty-poc-apple-worker-1-linux-arm64.oci.tar \
  --output "$MONTY_WORKER_DIR/artifacts/apple-container-worker-poc"
```

The pinned Containerfile records the source recipe for a future deliberately
versioned image. A fresh build is not accepted as the historical digest; build
and review a new tag/digest separately when that recipe changes.

The runner cleans up only the exact `monty-poc-` container and volume names it
created and still carrying its unique invocation label. It retains the
versioned PoC image, proves a second inert container can reuse it without a
build or pull, saves it as `monty-poc-apple-worker-1-linux-arm64.oci.tar`, and
writes the archive SHA-256 beside it. Restore that exact image with:

```sh
container image load --input \
  /durable/path/apple-container-worker-poc/monty-poc-apple-worker-1-linux-arm64.oci.tar
```

The benchmark commands use 2 CPUs and 1 GiB RAM. Raw inspection-cycle samples
separate start/readiness, inspection exec, time-to-result, stop, and total
time-to-return-to-stopped. Each inspection result is emitted as JSONL and
stdout is flushed before teardown begins; `time_to_result` includes that
delivery. Startup acceptance covers cached create/start,
stopped wake, and stopped wake-to-inspection-result; stop remains a separately
visible lifecycle diagnostic. Override only the sample count (minimum 20) or
evidence directory when needed:

```sh
python3 poc/apple-container-worker/poc.py \
  --iterations 30 \
  --output /durable/path/apple-container-worker-poc
```

The deterministic resource names are serialized with an exclusive run lock.
The script refuses to touch pre-existing containers or volumes, verifies its
unique invocation label before deletion, confirms cleanup, and never invokes a
prune command. Each invocation requires a new output path so failed or repeated
runs cannot mix evidence. Local `evidence*` directories are ignored historical
review snapshots; only a fresh external output directory is canonical and
durable.
