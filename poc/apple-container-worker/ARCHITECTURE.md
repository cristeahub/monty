# Proven boundary and production gaps

The host supplies only control-plane operations: start/stop, `exec`, and
`cp`. The task container has one named ext4 volume mounted at `/monty` and no
bind mount, forwarded socket, SSH agent, credential directory, or host
repository. Repository input crosses the boundary as a Git bundle copied to
`/monty/context`; reports and artifacts leave through `/monty/outbox` with
`container cp`.

Inside the volume, the PoC creates `home`, `repos`, `worktrees`, `context`,
`outbox`, and `artifacts`. PID 1 is Apple's signal-forwarding init around an
inert `sleep infinity` child, so start/wake does not resume an agent and stop
does not wait for a forced kill. Inspection runs Git with optional index locks
disabled and compares a whole-volume content digest before and after the loop.
The PoC uses `git worktree`; it does not exercise `wt` because packaging a
Linux `wt` binary would expand this runtime-focused slice into a toolchain
build.

Before production integration, Monty still needs credential delivery with a
strict lifetime, a Linux `wt` package, non-root worker execution, network and
resource policy, authenticated/versioned report handling, crash recovery,
and lifecycle integration under Monty's state lock. `container exec` is a
powerful control channel rather than an intrinsically read-only API, so the
production inspection command must remain narrowly allow-listed. This PoC
does not run Codex or Pi inside the VM and deliberately does not change Monty
settings, schemas, or worker lifecycle code.

The latest durable generated `REPORT.md`, `benchmark.json`, and `samples.csv`
are the canonical measurements; this architecture note intentionally does not
duplicate run-specific figures. The decision remains **PROCEED WITH THE
STOP-OFF-CRITICAL-PATH WORKAROUND** when those startup gates pass. Keep active
task containers warm. For a stopped task, start it, inspect it, emit and flush
the result, and only then request stop asynchronously or through an idle policy.
Explicit off/remove operations still wait for and report Apple's teardown
result. Stop samples remain visible for capacity planning even though they are
not part of the user-visible startup gate.

The runner requires the service to be running with `[build] rosetta = false`;
it never changes host configuration. It removes the historical fallback tag
only after matching its complete known-bad digest and checksum-verifying the
recovery archive. It loads and verifies the exact native arm64 image rather
than rebuilding a historical digest from mutable repositories. A second
labeled inert container proves cached-tag reuse. The retained tag is also saved
as a checksummed OCI archive so the next run can restore it without rebuilding.
Apple's builder is global and has no per-run ownership label, so this runner
only records its observed state and never starts or stops it. Producing a new
versioned image remains a separately coordinated production workflow.

Apple `container` 1.3.1 also cannot `container cp` directly from a named-volume
mount: its copy implementation looks under the container rootfs snapshot and
reports the mounted path as missing. The runner preserves that error, uses
`container exec cp` to stage each outbox file in the private VM rootfs, and
then retrieves it with `container cp`. Production collection needs this
staging step or an upstream fix.
