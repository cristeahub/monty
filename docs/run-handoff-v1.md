# Monty run handoff v1

`monty:run-handoff:v1` is the canonical record for one finished interactive,
Pi, or Codex worker run. Publishing this record does not change `job.json`, the
linked task status, or any worktree lifecycle.

Canonical files use this immutable identity:

```text
.monty/handoffs/<worker-run-id>/<worker-id>/<handoff-id>.json
```

Monty writes a deterministic Markdown sibling with the `.md` suffix. The JSON
record has these fields:

| Field | Meaning |
| --- | --- |
| `schema` | Always `monty:run-handoff:v1`. |
| `id`, `finished_at` | Stable idempotency key and UTC publication time. |
| `source` | `interactive`, `headless-pi`, or `headless-codex`. |
| `outcome` | `ready-for-review`, `needs-attention`, or `failed`. |
| `worker` | Canonical worker run id, worker id, title, task key when present, and open job status observed at publication. |
| `summary` | Dense bounded “what I did” summary. |
| `changes` | Per-workspace repo, branch, worktree, changed files, insertions, deletions, and any Git inspection error. |
| `validation` | Worker-reported validation summaries and result classification. |
| `review` | Accepted, fixed, rejected, and unresolved findings plus an optional overall summary. |
| `risks` | Residual risks, blockers, and decisions requiring attention. |
| `last_phase`, `error` | Failure phase and useful error when applicable. |
| `workspaces` | Every absolute repo, branch, and materialized worktree known to the task. |
| `evidence` | Canonical JSON/Markdown, worker memory, task context, and artifact paths. Artifact entries retain a safe worker-relative path for archive/reopen remapping. |
| `next_actions` | Stable action identifiers for detail, continuation, another pass, draft PR, and later completion. |

Git inspection happens before Monty takes its state lock. Publication then
reloads the exact worker identity and atomically writes the canonical record,
rendering, and notice while holding the one-home lock. Reusing the same handoff
id returns the existing canonical run and never reopens an acknowledged notice.
Interactive publication prints the compact rendering directly in the worker's
Ghostty session and creates an already-acknowledged delivery receipt, so it is
not surfaced again by the head butler. Headless publication leaves its notice
pending for head-butler delivery.

## Pending delivery

`.monty/inbox/run-handoffs/<notice-id>.json` uses
`monty:run-handoff-notice:v1`. It contains a stable worker/run/handoff identity,
the canonical path derived from that identity, creation time, and optional
acknowledgement time. Monty rejects malformed identities, forged paths,
symlinked inboxes, symlinked records, and references outside the canonical
handoff hierarchy.

`monty handoff pending` is at-least-once for headless delivery. It repairs a
canonical publication interrupted before rendering or its correctly classified
delivery receipt. Recovered interactive receipts remain acknowledged and are
not displayed by the head butler. When a Pi attempt has a durable
`final.md` but its live callback was missed, it publishes a `needs-attention`
receipt without inferring success from the artifact alone. It never launches or
resumes work and never changes task status. The head butler displays
the referenced handoff at a safe message boundary and then calls
`monty handoff acknowledge <notice-id>`. Repeating acknowledgement succeeds
without changing the run or task.

## Read-only follow-up

`monty handoff follow-up` emits `monty:run-handoff-follow-up:v1`. The dispatch
contains the user's question, `read_only: true`, the current workspace map, a
read-only working directory, and availability-checked paths to the canonical
handoff, rendered card, current worker memory, task context, and remapped
artifacts. It carries no harness session or process identity and performs no
code or lifecycle mutation.

Changing code requires an explicit `monty resume` or `monty headless resume`.
Committing, pushing, opening a draft PR, and `monty done` remain separate,
explicitly approved actions.
