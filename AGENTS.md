# Monty head-butler instructions

This repo is the Monty control room.
At the start of a head-butler planning session, run `monty handoff pending` before presenting the task inventory.
Surface each pending run handoff at the next safe message boundary; never interrupt, cancel, or replace an unrelated response already in progress.
After a handoff has actually been displayed to the user, run `monty handoff acknowledge <notice-id>`.
Acknowledgement is idempotent, and reading a pending handoff must never start another run or change task status.
When the user asks for more detail, use a still-addressable native runner when available; otherwise run `monty handoff follow-up <worker> --question <question>` and inspect only its read-only durable context.
Use an explicit `monty resume` or `monty headless resume` only when the user asks to continue implementation.
Use it to plan work, choose actionable tasks, and launch worker agent sessions through the configured Pi or Codex harness.
In Monty conversations, a worker job and a task are the same unit of work.
Do not treat active worker jobs and local tasks as separate concepts in user-facing replies.
The local task registry is the source of truth for task status.
Keep worker jobs linked to local tasks with `task_key` and run `monty tasks sync` after launching, archiving, importing, or noticing unlinked worker jobs.
`monty list` and `monty tasks list` are equivalent task-listing views and must show the same task inventory.
When the user asks for jobs or tasks, present the answer as a Markdown table that closely mirrors the relevant Monty command output and includes the same information.
For task and job lists, use exactly these columns: ID, Project, Status, Title, and Branch.
Use `monty list` or `monty tasks list` for the task inventory, not an ad-hoc merge of local tasks and worker jobs.
Always invoke the globally installed `monty` executable directly.
Never use `dune exec -- monty` for Monty workflows.

## Head-butler workflow

When the user asks what to work on, inspect the provided repos, issues, links, or notes.
Ask clarifying questions when the selected work is ambiguous.
Keep planning artifacts under `.monty/runs/<run-id>/`.
Use a short run id such as `2026-06-27-issues` or `run-001`.

When the user chooses tasks to execute, create one Markdown context file per worker task.
Each context file should be specific enough that a fresh worker agent can start without reading the whole planning conversation.
Include the task summary, repo path, issue or PR links, relevant constraints, acceptance criteria, and any important planning notes.
For implementation jobs that will run in Ghostty, include a `Review loop` section in the context file.
That section must instruct the worker to run `/review` after the initial implementation and focused validation, verify each concrete finding, fix valid findings, rerun affected tests, and record the review findings plus fixes in worker memory.
For explicitly headless jobs, use a `Headless review chain` section instead.
State that Monty's fixed chain supplies one implementer, two independent parallel reviewers, and one fixer, so the implementer must not invoke `/review` or launch subagents itself.
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
          "branch": "monty/invoice-parser-sonnet-5"
        },
        {
          "repo": "/absolute/path/to/admin",
          "branch": "monty/admin-invoice-sonnet-5"
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
absolute paths; launch and resume do this automatically. If planning accidentally
created separate unlaunched local tasks
for one feature, use the `monty task merge <source> --into <target>` command after
checking both identities; the source becomes historical and the target owns the
combined workspace set.

The `branch` field is optional.
Prefer setting it when the issue number or task name gives a clear branch name.
Use the configured branch prefix for branch names.
The default prefix is `monty`, but users may persist another value with `monty settings set branch-prefix <prefix>` or use the `MONTY_BRANCH_PREFIX` fallback, for example `cto`.
When omitting `branch`, Monty derives `<branch-prefix>/<title-slug>` automatically.
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
Do not launch the real batch until every repo, context, project, task link, dependency, canonical path, and full-batch identity passes preflight.
If a real batch partially fails, preserve Monty's full result in the handoff.
Use the printed batch command to retry `prepared` and `launch-failed` workers.
Never automatically relaunch a `launch-requested` worker.
Use the printed `monty resume <worker-id>` command only when the user intentionally wants another terminal request.

Ghostty remains the default execution surface.
Use the headless harness flow only when the user explicitly requests headless or Pi-subagent execution.
Headless commands must resolve the effective harness through the normal CLI, environment, and persisted-settings precedence.
When `codex` is persisted as the selected harness, do not add a redundant `--harness codex` flag.
Monty does not provide a custom Pi extension; for Pi it emits complete JSON arguments for the harness's existing `subagent` tool.

Before any mutating Pi headless command, call the harness `subagent` tool with `action: "list"` and confirm that the tool and required agents are available.
If they are unavailable, stop without mutating Monty state.
For Codex headless execution, Monty's preflight must confirm that the configured Codex command is available before state mutation.
Run headless dry-run first when checking a new batch or when the user asks for a preview:

```sh
monty headless prepare-many --dry-run --manifest .monty/runs/<run-id>/jobs.json
```

Then prepare the real batch:

```sh
monty headless prepare-many --manifest .monty/runs/<run-id>/jobs.json
```

Headless preparation reserves every job and materializes its Monty-managed `wt` worktree while leaving the job `prepared`.
With Pi selected, run `monty headless begin <worker-id>` immediately before the harness call.
Read the returned `harness_call.tool` and pass `harness_call.arguments` unchanged to that exposed harness tool; do not manually reconstruct, simplify, or enrich the generated chain JSON.
The same dispatch includes a versioned `completion` contract.
After a successful Pi callback, run its exact `success_command`; after a failed callback, run the `failure_command` with the concrete last phase and error substituted.
Completion finalization writes the shared durable handoff and pending notice but does not mark the task done.
With Codex selected, run `monty headless run <worker-id>` or `monty headless run-many --manifest <manifest>`.
Codex headless execution uses non-interactive `codex exec` processes and must not open Ghostty.
A small native Codex supervisor may run the blocking Monty command and return its result to the main conversation; the returned result and durable inbox must both reference the same canonical handoff.
Independent Pi or Codex chains can run concurrently without waiting for earlier jobs to finish.
Run long-lived headless commands under a background terminal or native supervisor.
Once launch is confirmed, return control to the user immediately; do not wait for,
synchronously poll, or babysit the run unless the user explicitly asks for
monitoring. Let the process finish independently and recover its result from the
durable handoff inbox at the next safe message boundary.

Each chain gets fresh minimal context and runs one implementer, two mutually isolated reviewers in parallel, and one fixer.
Reviewers may write only their separate reports outside the worktree.
No child may create worktrees, stage, commit, push, open a PR, post remotely, or run `monty done`.
A successful chain leaves the task open and its worktree intact.
Never infer completion from Pi runtime status.

If a Pi harness call fails after `begin`, or a Codex process fails after the worker is claimed, leave the worker `launch-requested`.
Run `monty headless resume <worker-id>` only when the user intentionally requests a fresh successor chain.
With Pi selected, pass the resumed `harness_call.arguments` unchanged to the harness tool; with Codex selected, `resume` directly runs the successor chain.
Never persist a backend, Pi run ID, Codex session ID, async status, or runtime state in `job.json`.
Never automatically run `monty done` after a headless chain.

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

## Project overview workflow

When the user asks about current projects, project context, task overview, or what Monty knows about their work, get the information from Monty first.
Use `monty overview` for a cross-project summary.
Use `monty projects list` and `monty projects show <project>` for project memory.
Use `monty tasks sync` to reconcile worker jobs into local tasks.
Reconciliation is deterministic, uses stable worker identity, and keeps local task status authoritative over remote issue state.
Use `monty tasks list` for task summaries.
Use `monty tasks list --no-sync` or `monty list --no-sync` when an explicitly read-only inventory is required.
Use `monty projects add --repo <repo> --github <owner/repo>` when the user wants Monty to fetch GitHub issue metadata, but keep local tasks as the status source of truth.
Use `monty task add --project <project> --title <title>` for local tracking records, including work that originates from GitHub issues or other external systems.

## Worker expectations

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

## Project conventions

The implementation is OCaml built with Dune.
Use Dune package management and dependencies in `dune-project`.
Do not add opam files.
Use Ghostty as the default terminal backend. Use `monty settings set harness pi|codex` to persist the default agent harness; `--harness` remains the per-command override. Use `monty settings set branch-prefix <prefix>` to persist the default generated-branch prefix; `--branch-prefix` remains the per-command override.
Use `monty settings set codex-yolo true|false` to control whether Monty-launched Codex sessions bypass approvals and sandboxing. Treat the enabled value as an explicit high-risk user choice.
Keep headless state and payload generation in Monty.
Pi execution uses the harness's existing subagent tool, while Codex headless execution uses the configured non-interactive Codex command.
Do not add a Monty-specific Pi extension or a new persisted backend.
Use Monty's repo-scoped `ensure-worktree` flow for every workspace's worktree creation and reuse.
It must use the existing `wt` CLI, validate each selected repo, and automatically answer `wt` repo-selection prompts when branch names collide across repos.
Never bypass `wt` with direct `git worktree` commands.
All JSON mutations must use Monty's one-home lock and atomic replacement path.
Never hold that lock while invoking `gh`, `wt`, Ghostty, pi, `osascript`, git, or other slow external commands.
When changing state or lifecycle behavior, add isolated checkout-binary E2E coverage with a unique `MONTY_HOME`, fake external tools, reliable cleanup, and real temporary Git repositories where identity matters.
