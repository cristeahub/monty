# Monty CLI workflows

This manual ships inside the Monty binary and its automatically installed CLI skill.
Use `monty guide <topic>` for a workflow or read the skill’s bundled reference.
Use `monty <command> --help=plain` for current syntax.
Documentation describes operations; your role and user authorization determine
which operations you may perform.

## Inventory

This repo is the Monty control room.
Use this control room to plan work, choose actionable tasks, and launch worker agent sessions through the configured Pi, Codex or Claude harness.
At the start of a head-butler planning session, run `monty handoff pending` before presenting the task inventory.
Surface each pending run handoff at the next safe message boundary; never interrupt, cancel, or replace an unrelated response already in progress.
After a handoff has actually been displayed to the user, run `monty handoff acknowledge <notice-id>`.
Acknowledgement is idempotent, and reading a pending handoff must never start another run or change task status.
When the user asks for more detail, use a still-addressable native runner when available; otherwise run `monty handoff follow-up <worker> --question <question>` and inspect only its read-only durable context.
Use an explicit `monty resume` or `monty headless resume` only when the user asks to continue implementation.
General head-butler discussions do not need to become tasks merely to remain resumable.
Use `monty continue`, `monty continue --last`, or `monty continue <session>` only when the user asks to restore a saved head-butler conversation; these commands select registered native sessions and do not create a task or resume implementation. Import selected older native history explicitly before continuation.
In Monty conversations, a worker job and a task are the same unit of work.
Do not treat active worker jobs and local tasks as separate concepts in user-facing replies.
The local task registry is the source of truth for task status.
Keep worker jobs linked to local tasks with `task_key` and run `monty tasks sync` after launching, archiving, importing, or noticing unlinked worker jobs.
`monty list` and `monty tasks list` are equivalent task-listing views and must show the same task inventory.
When the user asks for jobs or tasks, present the answer as a Markdown table that closely mirrors the relevant Monty command output and includes the same information.
For task and job lists, use exactly these columns: #, ID, Project, Status, Title, and Branch.
Preserve any profile and group selection banner printed by Monty above the table.
Lists default to the active Mantle profile's project group; use `--group` to
override it or `--all-groups` to include every group. `--all` still includes done tasks.
Use `monty list` or `monty tasks list` for the task inventory, not an ad-hoc merge of local tasks and worker jobs.
Number the visible rows from 1 after filtering and sorting, preserving the command's order and values, including multi-workspace branches.
A numeric conversational reference such as `complete 1` refers to the most recent task list actually shown to the user in this conversation.
Keep that list's number-to-real-task-identity mapping in conversation context; numbers are temporary list positions, never task IDs or a persisted/global last-list registry.
Resolve the remembered row to its real task/worker identifier before invoking the existing CLI command, preserving completion routing and safeguards.
Completing a task, background changes, and internal reads do not replace or renumber that mapping: after `complete 1`, `complete 2` still targets the original row 2.
Only displaying a fresh task list replaces the mapping, with numbering restarted at 1 for that list, including filtered and archived/all views.
If the referenced list is unavailable, the number is outside its range, or the remembered task cannot be resolved unambiguously, explain the issue and obtain/display an appropriate list instead of guessing.
Do not apply that unresolved numeric request to the replacement list; let the user choose from it.
An already-completed row still refers to the same task; report that it is already complete and never retarget another task.
Pass real identifiers to the CLI; do not pass row numbers or invent a `complete` CLI command.
Always invoke the globally installed `monty` executable directly.
Never use `dune exec -- monty` for Monty workflows.

## Launch

When the user asks what to work on, inspect the provided repos, issues, links, or notes.
Ask clarifying questions when the selected work is ambiguous.
Keep planning artifacts under `.monty/runs/<run-id>/`.
Use a short run id such as `2026-06-27-issues` or `run-001`.

When the user chooses tasks to execute, create one Markdown context file per worker task.
Each context file should be specific enough that a fresh worker agent can start without reading the whole planning conversation.
Include the task summary, repo path, issue or PR links, relevant constraints, acceptance criteria, and any important planning notes.
Do not duplicate interactive review wording or hard-code a headless phase count in task context; the selected agent profile supplies those role instructions.
Use the default `simplified` profile unless the user selects another profile for the invocation or manifest job.
Workers must not post review comments, push, or open PRs unless explicitly approved.

Create `.monty/runs/<run-id>/jobs.json` with this shape:

```json
{
  "jobs": [
    {
      "id": "issue-123",
      "title": "Fix issue 123",
      "repo": "/absolute/path/to/repo",
      "branch": "monty/issue-123",
      "context": ".monty/runs/<run-id>/issue-123.md",
      "worker_dir": ".monty/runs/<run-id>/workers/issue-123",
      "task_key": "local:local-001"
    }
  ]
}
```

When one task spans repositories, keep one task, one context file, one worker,
and one lifecycle. Replace the top-level `repo` and `branch` fields with an
ordered `workspaces` array:

```json
{
  "jobs": [
    {
      "id": "invoice-sonnet-5",
      "title": "Upgrade invoice parsing and admin reprocessing",
      "workspaces": [
        {
          "repo": "/absolute/path/to/django-backend",
          "branch": "monty/invoice-parser-sonnet-5-django-backend"
        },
        {
          "repo": "/absolute/path/to/admin",
          "branch": "monty/admin-invoice-sonnet-5-admin"
        }
      ],
      "context": ".monty/runs/<run-id>/invoice-sonnet-5.md",
      "worker_dir": ".monty/runs/<run-id>/workers/invoice-sonnet-5",
      "task_key": "local:local-001"
    }
  ]
}
```

Every workspace repo must be an absolute registered project path. The array
order is stable; its first entry is only the initial launch directory, not a
`primary_project`, and every entry belongs to the same task. Use the `monty task
workspace add` command to attach planned repo-plus-branch metadata before launch.
Use `monty task show <task>` to see every repo, branch, and absolute materialized
worktree path. Once the task has a linked worker, the `monty task workspace ensure` command
rehydrates all workspaces through `wt` and persists their
absolute paths; launch and resume do this automatically.
For persisted `never` workers, ensure returns the existing source repositories without invoking `wt`.
If planning accidentally created separate unlaunched local tasks
for one feature, use the `monty task merge <source> --into <target>` command after
checking both identities; the source becomes historical and the target owns the
combined workspace set.

The `branch` field is optional.
Prefer setting it when the issue number or task name gives a clear branch name.
Use the configured branch prefix for branch names.
The default prefix is `monty`, but users may persist another value with `monty settings set branch-prefix <prefix>` or use the `MONTY_BRANCH_PREFIX` fallback, for example `cto`.
When omitting `branch`, Monty derives `<branch-prefix>/<title-slug>` automatically.
For new tasks spanning multiple repositories, Monty appends the slugged registered
project id to every generated or explicit branch, for example
`cto/my-feature-django-backend` and `cto/my-feature-admin`. An already matching
suffix stays unchanged, as do explicit prefixes and feature stems. Adding a second
planned workspace or merging unlaunched plans updates all planned branches together;
use those branches in the manifest. Single-repository naming stays unchanged.
Persisted worker branches are never renamed by retry, resume, ensure, reconciliation,
completion, or reopening.
Create or sync a local task for every worker before launch so the local task registry remains the source of truth.
Set `task_key` for workers launched from local tasks, for example `local:local-001`.
When `task_key` is present, `monty done <worker-id>` closes the linked local task while archiving the worker.
Ordinary launch and reconciliation never infer a task from a worker title or branch.
Use `monty tasks repair-worker <worker-id>` only for an explicit, ambiguity-checked legacy repair.

After writing the manifest and context files, launch workers with:

```sh
monty launch-many --manifest .monty/runs/<run-id>/jobs.json
```

Use dry-run first when checking the generated manifest or when the user asks for a preview.

```sh
monty launch-many --terminal dry-run --manifest .monty/runs/<run-id>/jobs.json
```

Dry-run runs the same complete preflight as real launch and performs no mutation.
Rex is the default terminal, so the batch opens through the installed Rex CLI; see the Rex terminals topic for targets and recovery, and pass `--terminal ghostty` for Ghostty.
Do not launch the real batch until every repo, context, project, task link, dependency, canonical path, and full-batch identity passes preflight.
If a real batch partially fails, preserve Monty's full result in the handoff.
Use the printed batch command to retry `prepared` and `launch-failed` workers.
Never automatically relaunch a `launch-requested` worker.
Use the printed `monty resume <worker-id>` command only when the user intentionally wants another terminal request.

## Rex terminals

Rex is the default native terminal backend; Ghostty remains available with `--terminal ghostty`, `MONTY_TERMINAL=ghostty`, or `monty settings set terminal ghostty`. `--terminal` wins over the environment, which wins over the setting, which wins over the Rex default. Check it with `monty doctor --terminal rex`, which requires the `rex` CLI and a reachable server (the Rex app runs it). Never script the app, type into shells, or talk to the server socket directly; every tab, pane and session goes through `rex run`, `rex split` and `rex new`.

Terminology: a Rex session groups work and survives while no app views it; a Rex window is a tab inside a session; a block is a pane. `--target tab` creates a window in a session, `--target split` creates a block to the right of the caller's own pane, and `--target window` creates one dedicated Rex session per worker and shows it in the app. Rex sets `REX_SESSION`, `REX_BLOCK` and `REX_SERVER` in every terminal; Monty uses those exact IDs for the whole batch and never follows moving focus or infers anything from labels.

```sh
monty launch-many --terminal rex --manifest .monty/runs/<run-id>/jobs.json          # tabs in your session
monty launch-many --terminal rex --target split --manifest .monty/runs/<run-id>/jobs.json
monty launch-many --terminal rex --target window --manifest .monty/runs/<run-id>/jobs.json
monty resume --terminal rex <worker-id>      # focuses a still-live Rex terminal, else opens a new tab
monty rex status <worker-id>                 # recorded IDs, liveness and program status; read-only
```

From outside Rex (a plain shell or a service-owned chat) `tab` follows the active Mantle profile. It lands in the session named by `monty settings set rex-session <session-id-or-label>` for that profile when one is set (resolved through `rex session inspect` on every launch, recorded by full ID, and refused in preflight when it is not live). Otherwise it lands in a session labelled after the profile, for example `work`: a live session the user already labelled that way, else one Monty creates and records under `.monty/rex-session.<profile>.json`, verified live before reuse. Without a Mantle profile the same applies with the label `monty` and `.monty/rex-session.json`. `rex-session` addresses the active profile and `rex-session.<profile>` any profile; an empty value clears it, and `monty doctor --terminal rex` reports whether the active profile's session resolves. `split` is refused in preflight, and `window` creates a session. Showing a session needs exactly one connected Rex app (or `REX_CLIENT`) with Remote Control enabled; otherwise preflight fails before any worker is reserved and names the fix.

Each worker records `rex-terminal.json` (server, session, window, block, client, target, attempt) beside `job.json`; `job.json` never carries Rex IDs. A `rex` failure or garbled response after `launch-requested` keeps that state with the error; use the printed `monty resume` command only when another terminal request is intended, because the tab may already exist. Resume checks the recorded block: live means focus, exited or gone means a new tab for the same conversation, unreachable server means stop. Workers running inside Rex receive their own `REX_*` context through the service and may use the Rex agent guide (`rex skill show`) for their own tabs; label and ID conventions there do not identify Monty workers. Program status (OSC 7501) from the terminal client mirrors attachment, handoff outcome and exit; none of it completes a task.

## Headless

Rex is the default execution surface; Ghostty remains available with `--terminal ghostty`.
Use the headless harness flow only when the user explicitly requests headless execution.
Headless commands must resolve the effective harness through the normal CLI, environment, and persisted-settings precedence.
When `codex` is persisted as the selected harness, do not add a redundant `--harness codex` flag.
The Monty server owns native Pi, Codex and Claude processes and their supervisors. Configure the local service before execution; an unavailable service never falls back to an independent native launch. See docs/server-owned-agents.md for current rollout limitations.
For every harness, preflight must confirm the configured executable is available before state mutation.
No parent subagent tool is needed.
Run headless dry-run first when checking a new batch or when the user asks for a preview:

```sh
monty headless prepare-many --dry-run --manifest .monty/runs/<run-id>/jobs.json
```

Then prepare the real batch:

```sh
monty headless prepare-many --manifest .monty/runs/<run-id>/jobs.json
```

Headless preparation reserves every job and materializes its Monty-managed `wt` worktree while leaving the job `prepared`.
Run `monty headless run <worker-id>` or `monty headless run-many --manifest <manifest>` for the selected harness.
Both submit independent supervisors to the Monty service and return after the worker is claimed and its first phase prompt is ready.
The startup receipt references the future canonical handoff; it does not assert that model execution succeeded.
Pi uses non-interactive print/JSON mode, Codex uses `codex exec`, and Claude uses print/stream-JSON mode.
None opens a terminal. Monty owns phase sequencing, logs, results, and final handoff publication.
Do not wrap these default background commands in another agent or background shell.
Use `--wait` only when explicitly waiting for the final result is useful, such as scripts or requested monitoring.
Independent chains can run concurrently without waiting for earlier tasks to finish.
Once launch is confirmed, return control to the user immediately; do not wait for,
synchronously poll, or babysit the run unless the user explicitly asks for
monitoring. Let the process finish independently and recover its result from the
durable handoff inbox at the next safe message boundary.

Each chain gets fresh minimal context and follows its pinned agent profile: one implementer, zero or more mutually isolated reviewers in parallel, then an optional final writer (simplifier or fixer).
The default `simplified` profile runs one implementer followed by one separate editing simplifier, sequentially in the same workspaces. The explicit `reviewed` profile runs one implementer, two reviewers, and one fixer; `solo` runs only the implementer with self-validation. Legacy workers without profile metadata retain `reviewed` behavior.
Monty captures reviewer reports outside the worktree. Pi reviewers use read-only file tools without bash; Codex reviewers use a read-only sandbox with hooks, MCP integrations and delegation disabled; Claude reviewers expose only read/search tools with those integrations disabled.
No child may create worktrees, stage, commit, push, open a PR, post remotely, or run `monty done`.
A successful chain leaves the task open and its worktree intact.
Never infer task completion from native runtime status.

If a process fails after its worker is claimed, leave the worker `launch-requested`.
Run `monty headless resume <worker-id>` only when the user intentionally requests a fresh successor run.
Resume submits the selected Pi, Codex or Claude profile to the service in the background; `--wait` waits for its result.
Never persist a backend, Pi run ID, Codex session ID, async status, or runtime state in `job.json`.
Interactive Codex workers record their exact conversation only in the durable
`codex-session-id` sidecar; headless Codex chains must not claim or update it.
Never automatically run `monty done` after a headless chain.

## Lifecycle

At the start of a day or planning session, review active jobs with:

```sh
monty list
```

When a feature is complete, archive it with:

```sh
monty done <worker-id>
```

This deletes every worker worktree and branch, closes any linked Monty-owned local task, marks the job done, and moves durable worker memory to `.monty/runs/<run-id>/archive/<worker-id>/`.
Do not run a separate `monty task done` for a linked local worker unless repairing old data from before this behavior existed.
Use `--force` only when the user explicitly accepts discarding local worktree changes.
Use `monty list --archived` or `monty list --all` when reviewing archived work.

## Projects

When the user asks about current projects, project context, task overview, or what Monty knows about their work, get the information from Monty first.
Use `monty overview` for a cross-project summary.
Use `monty projects list` and `monty projects show <project>` for project memory.
Use `monty tasks sync` to reconcile worker jobs into local tasks.
Reconciliation is deterministic, uses stable worker identity, and keeps local task status authoritative over remote issue state.
Use `monty tasks list` for task summaries.
Use `monty tasks list --no-sync` or `monty list --no-sync` when an explicitly read-only inventory is required.
Use `monty projects add --repo <repo> --github <owner/repo>` when the user wants Monty to fetch GitHub issue metadata, but keep local tasks as the status source of truth.
Use `monty task add --project <project> --title <title>` for local tracking records, including work that originates from GitHub issues or other external systems.

## Workers

Worker sessions are launched in repo-scoped worktrees created by Monty's `ensure-worktree` flow.
For a multi-workspace task Monty materializes every declared repo-plus-branch pair, launches in the first, and supplies all absolute paths to the harness.
Headless child agents receive the first Monty-managed worktree as their explicit `cwd` and every workspace through Monty instructions; they must never request Pi-managed worktrees.
Monty validates that any `wt` result belongs to the requested repo, because different repos may use the same branch name.
Treat wt worktrees as ephemeral.
Durable session memory belongs in the worker directory under `.monty/runs/<run-id>/workers/<worker-id>/`.
Each worker receives Monty instructions and its context file through the selected harness's supported prompt mechanism.
Do not assume the worker can see the full head-butler planning conversation.
Put all essential information in the worker context file.
Workers are instructed to write important discoveries, blockers, and handoff notes back to their worker directory.

Resume an existing worker with:

```sh
monty resume <worker-id>
```

Interactive Codex resume uses the worker's exact `codex-session-id` sidecar,
or Monty's registered conversation picker when that sidecar is absent. Both
select only sessions with that worker's exact durable identity and ordered
workspace map; a foreign sidecar never authorizes attaching another worker. Explicitly
import a selected older native session before resuming it. Use `--fresh` only
when the user deliberately wants a new Codex conversation; it replaces the old
sidecar only after the service durably registers the new native session.

Resume uses the durable worker's persisted worktree mode even when current CLI or environment defaults differ.

Resume an archived worker and move it back to active memory with:

```sh
monty resume --archived <worker-id>
```

Durable worker identity comes from the canonical physical `job.json` location.
Active state belongs under `.monty/runs/<run-id>/workers/<worker-id>/`.
Archived state belongs under `.monty/runs/<run-id>/archive/<worker-id>/`.
Do not silently move or rewrite unsafe legacy paths.
Completion and reopening are recoverable transitions, so retry the exact command reported by `monty doctor` when either is incomplete.

Run `monty doctor` when launch dependencies or durable state look unhealthy.
PASS and WARN-only output exits zero.
Any FAIL exits nonzero and must be resolved before relying on launch or lifecycle mutation.

## Settings

The implementation is OCaml built with Dune.
Use Dune package management and dependencies in `dune-project`.
Do not add opam files.
Rex is the default terminal backend; `monty settings set terminal ghostty|rex` persists another default and `--terminal` or `MONTY_TERMINAL` override it per command. `monty settings set rex-session <session>` names the Rex session (ID or label from `rex ls`) that tabs requested from outside a Rex terminal land in for the active Mantle profile (`rex-session.<profile>` addresses another profile); an empty value clears it, and without a mapping tabs go to a session labelled after the profile. Use `monty settings set harness pi|codex|claude` to persist the default agent harness; `--harness` remains the per-command override. Use `monty settings set branch-prefix <prefix>` to persist the default generated-branch prefix; `--branch-prefix` remains the per-command override.
Use `monty settings set codex-yolo true|false` to control whether Monty-launched Codex sessions bypass approvals and sandboxing. Treat the enabled value as an explicit high-risk user choice.
Keep headless state and payload generation in Monty.
Pi, Codex and Claude headless execution use their configured non-interactive commands under Monty's server-owned background runner.
Do not add a Monty-specific Pi extension or a new persisted backend.
Use Monty's repo-scoped `ensure-worktree` flow for every workspace's worktree creation and reuse.
It must use the existing `wt` CLI, validate each selected repo, and automatically answer `wt` repo-selection prompts when branch names collide across repos.
Never bypass `wt` with direct `git worktree` commands.
All JSON mutations must use Monty's one-home lock and atomic replacement path.
Never hold that lock while invoking `gh`, `wt`, Ghostty, `rex`, pi, `osascript`, git, or other slow external commands.
When changing state or lifecycle behavior, add isolated checkout-binary E2E coverage with a unique `MONTY_HOME`, fake external tools, reliable cleanup, and real temporary Git repositories where identity matters.

## Handoffs

At the start of a planning session, inspect `monty handoff pending`. Display
each notice at a safe message boundary, then acknowledge its exact notice ID:

```sh
monty handoff acknowledge <notice-id>
monty handoff show <worker-id>
monty handoff follow-up <worker-id> --question 'What remains uncertain?'
```

Follow-up reads durable evidence; it does not resume implementation. Use a
still-addressable native runner when available. Only explicit implementation
requests authorize `monty resume` or `monty headless resume`.

Interactive workers publish once after implementation, required review or
simplification, and validation. Include the result, checks, verified findings,
remaining risks, and durable artifact paths. For example:

```sh
monty handoff publish "$MONTY_JOB_ID" --summary 'Implemented and validated the change.' --check 'dune runtest: passed'
```

Publication leaves the task open and its worktrees intact. Headless children
return their handoff to the supervisor instead; the supervisor publishes.

## Skills

Monty automatically installs or refreshes its `monty` CLI skill before starting
Pi, Codex or Claude sessions. This covers head-butler start/continue, worker launch/resume,
headless phases, and service-managed native sessions. Installation uses the
selected native profile: `${CODEX_HOME:-$HOME/.codex}/skills/monty`,
`${PI_CODING_AGENT_DIR:-$HOME/.pi/agent}/skills/monty` or
`${CLAUDE_CONFIG_DIR:-$HOME/.claude}/skills/monty`. Service sessions use
their configured native profile rather than the service process's ambient account.
Read-only reviewers can read `references/workflows.md` without a shell or host CLI.

`monty guide` lists topics; `monty guide workers` reads a topic without needing
an initialized Monty home. To install explicitly without starting an agent:

```sh
monty skills install --harness codex
monty skills install --harness pi
monty skills install --harness claude
```

The skill and manual are embedded in the executable. Identical files are left
untouched, generated files are refreshed atomically, and unmanaged files or
unsafe symlinks are rejected instead of overwritten. Read-only inventory, help,
and dry-run commands do not install skills. Skills do not grant new permissions
or override role instructions. Native settings that disable skills remain in effect.

## Client service

The version 1 client API coordinates conversations independently of tasks. It
uses Cohttp/Lwt, server-owned native processes and persistent private transports.
The Codex protocol baseline is 0.160.0/0.160.1; the Pi RPC baseline is 1.0.2.
Claude uses the explicitly packaged SDK version described in docs/server-owned-agents.md.
Codex versions are informational: Monty attempts the native connection regardless
of version and reports connection or protocol failures. Other Pi runtime versions
fail closed. Pi also supports read-only native JSONL
history mirroring without starting its runtime. CLI guidance comes from the Monty skill.

Codex connects through a private server-owned app-server, rather than adopting
the account's independently launched daemon. `/health` lists protocol baselines in
`codex_tested_versions`; `codex_version` retains the original compatibility baseline.
New threads explicitly request `historyMode: "legacy"`, matching Monty's existing
full-history adapter instead of inheriting Codex's changing storage default.

### Startup, authentication and rollout

Build and test the checkout before replacing an installed binary. Do not migrate
live chats, install startup services, change shell aliases, or replace vendor
`codex` as an incidental part of rollout. First select the intended Monty home,
account homes and workspace roots explicitly. A trusted configuration is:

```json
{
  "profiles": [
    {
      "id": "default",
      "harness": "codex",
      "command": "/absolute/path/to/codex",
      "codex_home": "/absolute/path/to/account/codex",
      "roots": ["/absolute/path/to/repos", "/absolute/path/to/wt", "/absolute/path/to/monty"]
    }
  ]
}
```

Each Codex profile requires an explicit account `CODEX_HOME`; its private runtime
is fenced within that scope. Do not point automated test configurations at personal
accounts. Native processes inherit a minimal system environment, not
Monty worker variables or ambient API keys. Use account configuration in the
selected native home. Per-thread workspace roots, instructions, shell Monty
variables are supplied in `thread/start` config. The Monty CLI skill is installed
in the selected native profile before connecting.
Approvals default to `on-request` and sandboxing to `workspace-write`. Managed
terminals honor the existing `--codex-yolo`, `MONTY_CODEX_YOLO` and persisted
`codex-yolo` setting, in that order. When explicitly enabled, new threads use
`never` approvals and `danger-full-access`. The terminal attaches without CLI
permission overrides, which Codex rejects for remote resumes. The service does not inherit the
operator's setting for unrelated API requests: Codex `start` and `switch`
requests must explicitly supply the boolean `codex_yolo: true` to opt in.
Rejoining an already-running thread preserves its native execution policy;
changing a launch setting does not reconfigure that running thread. The YOLO
launch setting applies to fresh threads.
Existing custom provider, hook and other native account settings remain owned
by Codex. Per-thread configuration of arbitrary external MCP server environments
is not an API capability; configure those servers within their account scope.

```sh
monty service start --home /absolute/monty-home --config /absolute/profiles.json --port 8765
```

Run this once to select the trusted configuration and start in the background. Monty remembers its absolute
path and bound port in `.monty/client-service.json`. Later native launches,
including plain `monty`, automatically start the service in the background if
needed and wait for authenticated health before opening the terminal. A healthy
service is reused; concurrent launches share the existing service lock. Logs go
to `.monty/client-service.log`, and the service survives terminal exit. No login
agent or background supervisor is installed. A first launch without a saved
configuration gives the setup command instead of guessing account homes or roots.

```sh
monty service start  # Reuse the running service or start from saved configuration.
monty service stop   # Wait for shutdown; also safe if already stopped.
```

Use the same `--home` when managing a nondefault Monty home. Stop before changing
the saved configuration or port with `start --config FILE --port PORT`.
`service serve --config FILE` remains available for foreground operation with
Ctrl-C to stop. `service stop` also works for foreground services, authenticates
to the current service epoch, and never signals a PID from saved metadata. The
next managed launch starts it again. Stopping the service disconnects clients
and shuts down its private Codex, Pi, Claude and terminal processes. Independently
launched native daemons are left alone. Restart with the same configuration and
home to inspect retained history; control requires explicit resume. Unfinished
turns are never silently replayed. Retained terminal bytes remain readable through
the API after restart, with an unknown outcome when the owner died mid-session.

The listener binds IPv4 loopback only. Port zero (the default) chooses a free
port. `.monty/client-service.json` records the address, PID and service epoch;
`.monty/client-service-token` contains a generated bearer token with mode 0600.
Never put the token in URLs, command-line flags, screenshots or archives. The
service requires `Authorization: Bearer TOKEN`, exact `Host: 127.0.0.1:PORT`,
and either no Origin or the identical HTTP origin. There is no remote binding,
pairing, relay, arbitrary shell endpoint, filesystem browsing or CORS wildcard.
Deleting/replacing the token while the service is stopped rotates it. Only one
service may run per Monty home; a separate process lock excludes duplicates.

`examples/client_api.py` is an independent Python standard-library client. It
reads the private token file and polls durable receipts. For example:

```sh
python3 examples/client_api.py --home /absolute/monty-home health
python3 examples/client_api.py --home /absolute/monty-home inventory
python3 examples/client_api.py --home /absolute/monty-home command start-001 @request.json
```

### Version 1 operations

All paths below are relative to `/v1`. JSON commands require exactly
`Content-Type: application/json`. Requests are limited to 256 KiB and receive a
10-second body timeout. IDs are bounded alphanumeric, underscore or hyphen
components; paths cannot select arbitrary files. HTTP connection timeout is
30 seconds. Native frames/snapshots are limited to 16 MiB.

| Method/path | Result or action |
| --- | --- |
| `GET /health` | Capabilities, account scope hashes, service epoch and current cursor |
| `POST /shutdown` | Authenticate and supply `{"epoch":"CURRENT_HEALTH_EPOCH"}`; HTTP 202 acknowledges shutdown through the shared cleanup path |
| `GET /inventory` | Read-only CLI inventory, exact ordering/values, canonical keys, every workspace, profile/group banner and warnings |
| `GET /projects` | Registered projects for task creation |
| `GET /tasks/KEY` | Task details, worker identity and the revision required for actions |
| `GET /discovery?profile=ID` | Native conversations in one configured account; follow `next_cursor` with `cursor` |
| `GET /discovery/history?profile=ID` | Read native history without importing or resuming it |
| `GET /conversations` | Monty conversation manifests and native provenance |
| `GET /conversations/ID` | One manifest, revision, selected session, transfers and coverage |
| `GET /conversations/ID/history` | Owned raw batches, normalized item revisions and effective item identities; `?offset=N` pages eight batches per session |
| `GET /blobs/SHA256` | Owned attachment bytes |
| `POST /commands/REQUEST_ID` | Reserve a command; return a durable receipt with HTTP 202 |
| `GET /commands/REQUEST_ID` | Read its current outcome without dispatch |
| `GET /events` | SSE invalidations/provisional native activity; optional `Last-Event-ID` |

Inventory accepts `scope=active|archived|all`, `project`, `group`, `all_groups=true`
and `run`, following CLI selection rules. Queries never reconcile tasks,
acknowledge handoffs, import/resume chats or complete work. Existing CLI lifecycle
commands remain authoritative. The service detects changes to CLI inventory by
polling and emits invalidations. Conversation runtime completion is unrelated
to task completion, and task-free chats create no worker, task or worktree.

Unlinked diagnostic worker rows remain read-only until explicitly reconciled.
Native discovery supports Codex `archived=true` and `search`; Pi reports unreadable
or skipped sources in `errors` with partial coverage. History queries use
`native_id` for Codex and `session_file` for Pi. Viewing history does not import,
attach, resume, or create tasks.

Task actions use `POST /v1/commands/ID` and its existing durable receipts:

- `task_create`: `project`, `title`, optional `notes`. Creation saves a task;
  it does not start a worker.
- `task_preflight`: canonical `task`, its `revision`, account `profile`, and
  `action` (`launch` or `resume`). Launch also requires standalone Markdown
  `context`. Monty adds the title, task identity and ordered workspace map, saves
  the plan in its run directory, and runs doctor plus the complete dry-run.
- `task_apply`: successful `preflight` command ID and `confirm:true`. Monty checks
  the task, account, role, settings, terminal and context again, repeats the dry-run, and
  requests the selected terminal once. The preflight result and plan carry
  `terminal` (`ghostty` or `rex`, from the persisted setting or the service's
  `MONTY_TERMINAL`); an apply whose terminal changed since preflight is refused. Resume uses the exact saved worker and original native
  account. A consumed plan cannot be submitted again under a different ID.
- `task_complete`: canonical `task`, its `revision` and `confirm:true`. Linked
  workers go through `monty done`; dirty worktrees are refused. There is no
  force-discard API action.

Task process results retain `exit_code`, `stdout` and `stderr`, including partial
failure and recovery commands. A finished command receipt means the adapter
finished: inspect the process result. A launch request does not establish that
implementation succeeded or that the task is complete. Task API launches use
managed worktrees and the selected terminal. Rex uses the configured Monty session
because the service has no caller pane.
Codex task API launches keep approvals and sandboxing enabled even when saved settings
or the terminal's login environment enable bypass.

A fresh task-free conversation request is:

```json
{
  "op": "start",
  "profile": "default",
  "title": "Planning",
  "context": {
    "workspaces": ["/allowed/repository"],
    "instructions": "Current trusted instructions",
    "objective": "Investigate the current issue",
    "environment": {}
  },
  "task_keys": []
}
```

Workspace paths must exist inside configured roots, with the first as cwd.
There must be 1–16 ordered workspaces. Instructions are bounded at 64 KiB.
Optional `model` and `modelProvider` are separate fields. Optional `task_keys`
must be exact existing inventory keys. Task links have no lifecycle side effect.
The response supplies the conversation ID, session ID, exact native identity
and profile. Private native endpoints remain server-owned. Naming a fresh Codex
thread persists its empty rollout so another client can resume it before any model
work. Starting a conversation by
itself does not send a model turn.

Other commands use `op` as follows. Stateful conversation commands require its
current `revision` unless explicitly noted. Fetch a fresh manifest after a 409;
a rejected command ID remains rejected, so a deliberately revised command uses
a new ID. Repeating an identical accepted command uses its original ID.

| `op` | Additional fields and behavior |
| --- | --- |
| `import` | `profile`, `context`, optional `task_keys`; Codex and Claude use `native_id`, Pi uses absolute `session_file`. Pi continues mirroring the selected file without starting a runtime |
| `attach` | Codex only: same fields; verifies the thread is loaded in the selected server, rejoins it and mirrors live activity |
| `resume` | `conversation`, `revision`, `detached:true`; explicit operator confirmation that prior runtimes were detached, with an idle source check |
| `send` | `conversation`, `revision` or exact `selected_session`, `text` (at most 8192 bytes); Pi additionally accepts `delivery:"prompt"`, `"steer"` or `"follow_up"` |
| `interrupt` | `conversation`, `revision` or exact `selected_session`; Codex requires exact `turn_id`; Pi clears queued inputs and aborts active work |
| `set_model` | Pi only: `conversation`, `revision`, `provider`, `model`; requires idle state and no pending dialogs |
| `detach` | `conversation`, `revision`; acknowledges client detachment without cancelling execution or releasing its runtime |
| `release_runtime` | `conversation`, `revision`; explicitly releases an idle runtime; exit its native terminal first |
| `respond` | `conversation`, `revision`, `pending_id`, `pending_revision`, native `response` object; reserves exactly one response on that connection |
| `sync` | `conversation`; explicit snapshot refresh, no model work; no revision required |
| `switch` | `conversation`, `revision`, destination `profile`, `context`, `mode:"branch"`, optional model/provider |
| `recover_switch` | `conversation`, `revision`, `transfer`, `confirm_branch:true`; selects an already recorded accepted destination after reading it, without creating/replaying it |
| `archive` | `conversation`; marks retained archive metadata; no native/task mutation or revision required |
| `purge` | `conversation`, `confirm:true`; permanent local retention removal, no revision required; rejects owned runtimes or live terminals |
| `handoff` | Rejected with a precise unsupported-capability explanation; there is no automatic standalone process takeover |

Responses to native approval/user requests preserve the upstream JSON shape;
Monty does not translate an approval into arbitrary command execution. Pending
requests identify the exact native connection generation and revision. A second
or stale response conflicts. A lost response is never retried against a newer
connection's reused numeric request ID.

Clients can fence sends and interruptions with `selected_session`, so unrelated
transcript updates do not invalidate the action. A changed session is rejected.
Clients omitting `selected_session` retain the conversation revision check.
The service epoch changes at every restart. Service startup reuses its last
bound port unless explicitly overridden.

Receipts distinguish `accepted`, `running`, `finished`, `rejected` and
`outcome_unknown`. `finished` means the command/RPC finished, not that the model
turn or task finished. Submission reserves the ID before dispatch; concurrent
repeats return that receipt. Reusing an ID with different content conflicts.
A crash/lost native reply after possible acceptance is `outcome_unknown`; Monty
never automatically resubmits it. Inspect native identity/history and the receipt
before deliberately continuing. Exactly-once external execution is not claimed.
Service restart conservatively converts interrupted receipts to unknown.

SSE retains only the latest 256 events, each at most 4 KiB; larger events emit
resync hints. Slow readers do not block execution or allocate independent queues.
Expired or previous-epoch cursors produce HTTP 409 before streaming, or a final
`resync` event during streaming. Read authoritative snapshots, then reconnect
using `/health`'s cursor. Provisional token deltas are not durable message copies.
Disconnecting an HTTP client ends that client's stream; the service keeps
observing and executing. Use `interrupt` for a native turn or `cancel_execution`
with the execution receipt's current `updated_at` to stop a headless supervisor.

### Pi history and RPC

Add a Pi profile to the same configuration:

```json
{
  "id": "pi",
  "harness": "pi",
  "command": "/absolute/path/to/pi",
  "pi_home": "/absolute/path/to/pi/agent",
  "roots": ["/absolute/path/to/repos"],
  "session_roots": ["/absolute/path/to/pi/agent/sessions"],
  "tools": ["read", "bash", "edit", "write"]
}
```

`pi_home` is the native `PI_CODING_AGENT_DIR`, pinning account/configuration
scope. `session_roots` defaults to its `sessions` directory; new managed files
use a `monty` subdirectory of the first root. Optional `tools` selects native
built-in tools; omit it for Pi defaults, or use `[]` to disable them. Roots
validate selected workspaces, logs and copied attachment files; they do not
provide an OS sandbox for Pi's native tools. Pi owns account credentials,
extensions and other native resources. The service passes `--offline` and
`--no-approve`, which ignores unapproved project-local executable configuration;
it does not approve project code. Only explicit per-session Monty environment
variables and a minimal system environment reach the process. No ambient provider
keys or unrelated Monty worker variables are inherited.

To mirror an existing native Pi chat, submit this command using a fresh request
ID and an explicitly selected file:

```json
{
  "op": "import",
  "profile": "pi",
  "session_file": "/absolute/path/to/pi/agent/sessions/selected.jsonl",
  "context": {"workspaces": ["/absolute/path/to/repos/project"]}
}
```

This starts no Pi process and never writes the source. Monty copies complete
native records and available embedded image bytes into its existing archive,
then polls for new records even without API clients. Repeated copying deduplicates
native entry IDs; raw batches contain entry deltas plus source header/leaf metadata.
Source truncation, replacement, deletion or unavailability never removes captured
history. A changed session header ID is rejected. Symlink paths and files outside
configured session roots are rejected. Version 2/3 JSONL files are supported up
to 16 MiB; each sync currently reads the bounded file. Embedded images use the
shared 8 MiB per-attachment limit. Incomplete trailing records wait for a newline
and report partial coverage; malformed complete records report failure.

Native `id`/`parentId` relationships retain branches and pre-compaction records.
Without RPC, effective history follows the last persisted entry; unsaved branch
navigation and transient activity cannot be inferred from a log. With an owned
RPC runtime, `get_entries` supplies the active leaf and entries not yet flushed
to disk. Pi does not create its file until the first user/assistant message;
an empty session remains readable in Monty, but cannot be resumed after its
runtime exits before that file exists. Missing or deleted native files are never
silently recreated for resume.

Use `start` with `profile:"pi"` and the usual context to create a service-owned
`pi --mode rpc` process. API clients share it. `send` defaults to `prompt`;
`steer` and `follow_up` queue native input. Command acknowledgement does not mean
turn completion: Monty uses `agent_settled` to observe settled execution. `interrupt`
clears queues before aborting, returning what was cleared. Native extension
dialogs appear in `pending`; `respond` supplies exactly `{"confirmed":true}` for
confirm, `{"value":"answer"}` for select/input/editor, or `{"cancelled":true}`.
Responses reserve one exact dialog/revision/connection, reject expired replies,
and can resolve a dialog while its originating prompt is awaiting acknowledgement.

Pi's standalone TUI has no remote attach transport. Monty-owned terminals run
that native TUI under the service. To control a session from an unrelated native
terminal through RPC, stop every native writer and explicitly `resume` the imported conversation with
`detached:true`. Resume verifies the exact native ID, source path and saved cwd.
That confirmation is the operator's ownership assertion; Monty cannot prove an
unrelated native writer stopped. Conversely, `release_runtime` archives a final
RPC snapshot before closing an idle owned runtime, retaining unflushed setup
entries even when no native file exists. `detach` only acknowledges the client's
detachment and leaves accepted work running. Native Pi can continue an existing
file after the owned runtime is released.
Do not run both writers together.
Service restart resumes file mirroring, clears stale dialog state and leaves RPC
runtimes disconnected until an explicit resume; uncertain commands are never
replayed. No Monty-specific Pi extension or second conversation database is used.

### Native terminals and existing chats

`monty terminal` starts a task-free conversation on the configured service. The
service opens the native process in an owned PTY and the client forwards bytes
and terminal size changes. Codex uses
`codex resume --remote unix://EXACT_SOCKET ... EXACT_THREAD`; Pi and Claude use
their native session flags. An unavailable service never falls back to an
independent harness process. By default it selects the configured Codex
profile matching the terminal's canonical `CODEX_HOME` (or `~/.codex` when unset),
so Mantle switches select the corresponding account without another environment
variable. Add each account home to the trusted service configuration once, with
its own profile ID and allowed roots, then restart the service. For example,
Mantle work normally uses `~/.local/share/mantle/profiles/work/codex`.
`--profile` overrides `MONTY_API_PROFILE`; either explicitly selects a configured
profile whose account scope must still match `CODEX_HOME`. Missing or ambiguous
matches fail before creating a thread; Monty never falls back to another account.
Sessions pin that account scope independently of the profile name. Reconfiguring
a profile to another account blocks native observation/control of its older
sessions while their owned history remains readable. Explicit attachment under
a new name can rebind the same scope; renaming does not bypass purge tombstones.
`--request-id` permits receipt recovery. `--mode picker|last|exact --session ID`
selects registered Monty conversations. Task-free picker/last modes stay within
the current workspace; an explicit ID may select another task-free workspace.
Worker selectors require the exact durable worker identity and ordered workspace
map. `--prompt` sends initial native TUI input.
Worker starts/resumes and head-butler start/continue use this entry point;
Ghostty environment and Monty-managed workspace creation stay in the existing
launcher. Before attaching, Monty enables `tui.vim_mode_default` in the selected
Codex account through its native configuration API and passes the same terminal
override. This also makes Vim the default for later Codex sessions in that account.
Managed Codex terminals enter Vim Normal mode automatically. Codex 0.160.0
forces Insert mode during startup despite that setting, so the Python PTY guard
sends Escape once the native Vim footer appears. It preserves later user mode changes, terminal resizing and native exit codes.
Generated worker commands pass `--worker` to preserve their task link,
workspace environment and `codex-session-id` sidecar. Task-free/head-butler
terminals ignore ambient worker identity even when started from a worker shell.
Every attachment also forwards the caller's `REX_SESSION`, `REX_BLOCK`,
`REX_SERVER` and `REX_CLIENT` as `terminal_environment`; the service validates
them and applies them to that native process (and, for Codex, to the thread's
tool shell environment at start) without storing them in the session context.
Native runtime IDs are not written to `job.json`. Interactive workers use
harness-specific session sidecars. Launch scripts must match the current
Monty-generated template before resume can rewrite them.

For ad hoc use, invoke `monty terminal --home ...` explicitly. There is no vendor
binary replacement or shell alias. A stopped service starts from its saved
configuration; missing setup or startup failure produces actionable guidance.
The native stored-session picker is replaced by Monty's registered catalog;
explicitly import a selected older session before resuming it.

For a chat already on the selected server, use `attach`. A standalone chat is
not transferable merely because its history is readable. Reach idle, close/detach
the old runtime, explicitly import that one native ID and request `resume` with
`detached:true`. That field is operator confirmation, not protocol proof of
exclusive ownership. Monty does not kill a runtime or silently start a second
writer. Codex 0.160.0 cannot prove standalone release or revoke another client's
write access. Exclusive handoff and exclusive switching are therefore unavailable.

The opt-in real integration test uses a fresh native home and a local fake model
HTTP endpoint. It verifies an empty-thread rejoin, two clients on the same active
thread, native TUI-originated messages, a TUI SIGHUP preserving active execution,
and the old client's continued ability to send. This is evidence for those tested
paths, not a guarantee that every native exit key detaches without interruption;
Ctrl-C may explicitly request cancellation through the native UI.

### Owned archive, switching and recovery

A conversation manifest atomically commits references to immutable bounded
batches under `.monty/conversations/ID/sessions/SESSION/batches/`. It includes
native lineage, transfer records and the selected session. Batches retain exposed
native payloads (with credential fields redacted), retrieval provenance and schema/
normalizer versions. Normalized items have native turn/item identity, source order,
revision, structured content and per-turn provider/model only when supplied.
Unknown types remain opaque with visible coverage warnings. `effective` identifies
selected source history; older revisions/rolled-back items remain captured.
Completed live items are normalized with provisional order until reconciled; errors and native requests are also retained raw.
When an unchanged snapshot reconciles live revisions or selected history, the
new batch references its retained raw payload by `raw_sha256` and sets `raw` to
null. Repeated catch-up therefore preserves revisions without copying that payload.
History pages have per-session `next_offset` values. Follow them to read retained
history beyond the first eight batches; the example client exposes `history(ID)`.

Monty remains subscribed with no HTTP clients present. It mirrors completed live
events and periodically reconciles snapshots after rejoin. Only manifest-referenced
batches are committed; flush precedes manifest publication under the one-home lock.
Startup discards unreferenced batches. Repeated snapshots update source identities
rather than inventing messages. Last successful sync, source failure, gaps and
unsupported content remain visible. History reads use only Monty's files, including
when the native source is unavailable. Paginated Codex storage is not supported;
failed native reads retain old history and report unavailable coverage.

Available local attachment files inside validated workspace roots are copied up
to 8 MiB and addressed by SHA-256 under `.monty/conversation-blobs/`. Unsafe,
missing, oversized or remote URL bytes are explicitly unavailable. Remote URLs are
not fetched, and arbitrary filesystem reads are not exposed. Shared blobs survive
purging another conversation that references them. Purge removes owned conversation
records and related receipt content, garbage-collects unshared blobs, and retains
native-source tombstones preventing immediate reimport. Native deletion never
implicitly purges Monty's archive. These files are private local storage, not
end-to-end encrypted remote storage.
Purge rejects live owned Pi/Claude RPC runtimes, live native terminals and running
Codex worker leases. Release those runtimes and exit their terminals first; purge
does not cancel execution. Codex history observed without an owned worker lease
can still be purged. Native session files are never deleted.

Switching creates a visible independent branch in the same Monty conversation.
It requires idle source history and no pending requests. It records the exact
bounded package: current objective/instructions, workspaces, source checkpoint,
up to 100 effective items, explicitly selected available attachment bytes and explicit
omissions. Optional `attachments` on the switch request selects source attachment
SHA-256 hashes; their exact base64 bytes count toward the package limit. Unselected
bytes are omitted. Transferred context is normalized as a `contextTransfer`
boundary instead of duplicated original dialogue. Historical material stays marked as data; tool calls are never replayed
as commands. Destination permissions are rebuilt from its configuration. Hidden
state, running processes and approvals are not transferred. The package is limited
to 128 KiB; exceeding it rejects before destination creation.

Destination acceptance is recorded before input routing changes. A partially
accepted transfer remains inspectable; `recover_switch` selects that known session
without redispatch. If destination identity is unknown, automatic recovery is
unavailable; do not issue another switch merely because a response was lost.
Native continuations receive the package as explicit user context and may run
one continuation turn. Cross-harness switching uses the same archive and
transfer contract. A Pi source with only file mirroring additionally requires
`detached:true`, since the log cannot prove idle state. The differently shaped
fixture adapter remains test-only, enabled with `allow_fixture:true`.
A native client on the previous branch can continue writing,
and its records remain associated with that source session.

For development validation:

```sh
dune build @all
dune runtest
_build/default/test/test_client_native.exe _build/default/bin/main.exe
_build/default/test/test_pi_native.exe _build/default/bin/main.exe
# Optional installed native CLI/SDK protocol checks, with local model fixtures:
_build/default/test/test_codex_native.exe _build/default/bin/main.exe /path/to/codex
_build/default/test/test_claude_native.exe _build/default/bin/main.exe /path/to/claude /path/to/sdk-python
_build/default/test/test_skills_native.exe _build/default/bin/main.exe
```

All suites and assertions are OCaml. The Python fake Claude SDK is a protocol
fixture for the production bridge. The default suite uses isolated homes,
temporary Git repositories and fake native servers. `test_client_native.exe`
additionally requires installed Codex and a PTY; it verifies
the server's private native runtime and terminal, with all inference served by
a local fixture. The private runtime test passed with Codex 0.162.1.
The opt-in Pi suite requires Pi 1.0.2, fresh native/system homes and a local
model fixture; it verifies RPC send/interrupt, shared API observation, exact
server-owned PTY resume, native file mirroring and fork identity.
No private transcript, real provider call, installed-binary replacement or live
chat migration is needed. Crash injection checkpoints are
`conversation-after-batch` and `conversation-after-destination` via
`MONTY_FAULT_INJECT`; use them only in disposable test homes.
