# Monty Apple worker image

Build and register the one pilot image explicitly; worker launch never builds,
pulls, loads, or replaces it:

```sh
container build --platform linux/arm64 \
  --file container/apple-worker/Containerfile \
  --tag monty-apple-worker:1 .
monty container-image register
```

Registration validates the Linux ARM64 image contract and records its full
digest under the Monty home lock. Create a recoverable, checksummed OCI copy
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

After this revision is installed and reviewed, the head butler can opt into a
real credential-using smoke with a dedicated one-job manifest:

```sh
MONTY_LIVE_SMOKE_MANIFEST=/absolute/path/to/one-job-smoke.json sh -eu -c '
  test -f "$MONTY_LIVE_SMOKE_MANIFEST"
  monty container-image register
  monty settings set harness codex
  monty settings set codex-yolo true
  monty settings set container-workers true
  monty headless prepare-many --dry-run --manifest "$MONTY_LIVE_SMOKE_MANIFEST"
  monty headless prepare-many --manifest "$MONTY_LIVE_SMOKE_MANIFEST"
  monty headless run-many --manifest "$MONTY_LIVE_SMOKE_MANIFEST"
'
```

That command intentionally launches real Codex and leaves the smoke task open
for `monty inspect`, `monty stop`, and an explicit `monty done` after review.
