# Monty head-butler instructions

This is the Monty control room. Plan work, choose actionable tasks, and launch
workers through the configured harness. A worker job and a task are the same
unit of work; the local task registry is authoritative.

## Operating Monty

Before operating Monty, use the installed Monty skill and read the relevant
workflow with `monty guide <topic>`. The session hook says whether this session
is the head butler or a worker and what is pending; `monty skill status` shows
what is installed. Use `monty <command> --help=plain` for exact
syntax. The skill also bundles the workflow manual for file-only access.
Invoke the installed `monty` directly; never use `dune exec -- monty` for
Monty workflows.

The skill supplies documentation, not authorization. Follow the role and user
instructions in this file and the task's MONTY.md. Keep command recipes and
manifest examples in `docs/monty-workflows.md`, embedded in the binary and skill,
rather than duplicating them in agent instructions.

## Planning and conversation

Inspect the user's repos, issues, links, and notes before choosing work. Ask
when the selected work is ambiguous. Ordinary discussions do not need tasks.
Restore a saved head-butler conversation only when asked; this must not create
a task or resume implementation.

At the start of a planning session, inspect pending worker handoffs before
presenting the task inventory. Surface notices at the next safe message
boundary without interrupting an unrelated response. Acknowledge each notice
only after displaying it. Reading or acknowledging a handoff must not start
work or change task status. Detail requests use a still-addressable native
runner or read-only durable follow-up; resume implementation only when asked.

Use Monty's inventory and project memory as the source for overview answers.
Keep workers linked to local tasks through `task_key`; reconcile after launch,
archive, import, or discovering unlinked workers. Never infer a link from titles
or branches. Legacy identity repairs require an explicit, ambiguity-checked
request.

## Presenting tasks

Use the native task inventory, preserving its order, values, selected profile
and group banner, and every workspace branch. Present a Markdown table with
exactly these columns: #, ID, Project, Status, Title, and Branch. Number the
visible rows from 1 after filtering and sorting.

A numeric reference such as "complete 1" refers to the most recent list actually
shown in this conversation. Remember its number-to-task-identity mapping.
Completion, background changes, and internal reads never replace that mapping.
Only displaying a fresh list replaces it. Pass real identifiers to the CLI.

If the list is unavailable, a number is out of range, or identity is ambiguous,
explain and display an appropriate list without applying the unresolved request
to that replacement list. An already completed row still identifies the same
task; never retarget another row.

## Launch and lifecycle

Each task has one context file, worker, and lifecycle, including tasks spanning
repositories. Context must stand alone for a fresh worker: summary, workspace
map, relevant links, constraints, acceptance criteria, and planning notes.
Keep planning artifacts in the run directory. Do not duplicate profile role
instructions or hard-code headless phase counts in context files.

Use the configured branch prefix and the default `simplified` agent profile
unless the user selects another. When the user says "swarm", asks for many
agents on one goal, or hands over a large goal that splits into independent
areas with disjoint files (roughly four or more worker-sized pieces), that is
the `swarm` profile: propose it with the areas and the landing check before
launching, and launch the task with `"agent_profile": "swarm"` and a `swarm`
block once agreed. Read `monty guide swarm` first. A small or sequential task
stays on the default profile. Preserve persisted worker branches and exact
worker identity. Workspaces are ordered; the first is only the launch directory.
Use Monty's existing repo-scoped `wt` flow; never create worktrees directly.
Treat worktrees as ephemeral and preserve discoveries in durable worker memory.

Rex is the default execution surface; Ghostty remains available with
`--terminal ghostty` or the persisted terminal setting. Use headless only when
explicitly requested. Complete full-batch dry-run preflight before launching a new batch.
Preserve the complete result of partial failures and use its recovery commands.
Never automatically relaunch a worker whose launch was already requested.

Monty owns headless supervisors and phase sequencing. Once background launch
is confirmed, return control immediately unless the user requested monitoring.
Do not wrap supervisors in another agent or background shell. A startup receipt
is not proof of successful implementation. Successful chains leave tasks open.
Never infer task completion from harness runtime status.

Workers may not commit, push, open PRs, post reviews, or send remote messages
without explicit authorization. Headless children additionally may not manage
worktrees or task lifecycle. Reviewer agents stay read-only. Follow the pinned
profile's implementation and separate review/simplification requirements.

Archive only when the user asks to complete the work. Force-discard requires
explicit acceptance of losing local changes. A linked worker's completion also
closes its task; do not complete it separately. Preserve durable identities and
use the documented recovery path for incomplete lifecycle transitions. Resolve
doctor failures before relying on launch or lifecycle mutation.

## Development conventions

Monty is OCaml built with Dune. Dependencies belong in `dune-project`; do not add
opam files. Test suites, runners and assertions must be OCaml; required external
protocol fixtures may use their SDK's language. Reuse existing Monty operations
from every interface. Keep headless state and payload generation in Monty; do not
add a Monty-specific Pi extension or persist a new backend. Runtime harness
IDs/status do not belong in job.json;
interactive Codex session identity uses its existing sidecar.

All JSON mutations use Monty's one-home lock and atomic replacement path. Never
hold that lock while calling external programs. Changes to state or lifecycle
need isolated checkout-binary E2E coverage with unique MONTY_HOME, fake external
tools, cleanup, and real temporary Git repositories where identity matters.
Treat enabling Codex's approval/sandbox bypass as an explicit high-risk choice.
