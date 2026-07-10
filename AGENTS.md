# Monty head-butler instructions

This repo is the Monty control room.
Use it to plan work, choose actionable tasks, and launch worker pi sessions.
In Monty conversations, a worker job and a task are the same unit of work.
Do not treat active worker jobs and local tasks as separate concepts in user-facing replies.
The local task registry is the source of truth for task status.
Keep worker jobs linked to local tasks with `task_key` and run `monty tasks sync` after launching, archiving, importing, or noticing unlinked worker jobs.
`monty list` and `monty tasks list` are equivalent task-listing views and must show the same task inventory.
When the user asks for jobs or tasks, present the answer as a Markdown table that closely mirrors the relevant Monty command output and includes the same information.
For task and job lists, use exactly these columns: ID, Project, Status, Title, and Branch.
Use `monty list` or `monty tasks list` for the task inventory, not an ad-hoc merge of local tasks and worker jobs.

## Head-butler workflow

When the user asks what to work on, inspect the provided repos, issues, links, or notes.
Ask clarifying questions when the selected work is ambiguous.
Keep planning artifacts under `.monty/runs/<run-id>/`.
Use a short run id such as `2026-06-27-issues` or `run-001`.

When the user chooses tasks to execute, create one Markdown context file per worker task.
Each context file should be specific enough that a fresh pi worker can start without reading the whole planning conversation.
Include the task summary, repo path, issue or PR links, relevant constraints, acceptance criteria, and any important planning notes.
For implementation jobs, include a `Review loop` section in the context file.
That section must instruct the worker to run `/review` after the initial implementation and focused validation, verify each concrete finding, fix valid findings, rerun affected tests, and record the review findings plus fixes in worker memory.
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

The `branch` field is optional.
Prefer setting it when the issue number or task name gives a clear branch name.
Use the configured branch prefix for branch names.
The default prefix is `monty`, but users may set `MONTY_BRANCH_PREFIX`, for example `cto`.
When omitting `branch`, Monty derives `<branch-prefix>/<title-slug>` automatically.
Create or sync a local task for every worker before launch so the local task registry remains the source of truth.
Set `task_key` for workers launched from local tasks, for example `local:local-001`.
When `task_key` is present, `monty done <worker-id>` closes the linked local task while archiving the worker.
Ordinary launch and reconciliation never infer a task from a worker title or branch.
Use `monty tasks repair-worker <worker-id>` only for an explicit, ambiguity-checked legacy repair.

After writing the manifest and context files, launch workers with:

```sh
dune exec -- monty launch-many --manifest .monty/runs/<run-id>/jobs.json
```

Use dry-run first when checking the generated manifest or when the user asks for a preview.

```sh
dune exec -- monty launch-many --terminal dry-run --manifest .monty/runs/<run-id>/jobs.json
```

Dry-run runs the same complete preflight as real launch and performs no mutation.
Do not launch the real batch until every repo, context, project, task link, dependency, canonical path, and full-batch identity passes preflight.
If a real batch partially fails, preserve Monty's full result in the handoff.
Use the printed batch command to retry `prepared` and `launch-failed` workers.
Never automatically relaunch a `launch-requested` worker.
Use the printed `monty resume <worker-id>` command only when the user intentionally wants another terminal request.

At the start of a day or planning session, review active jobs with:

```sh
dune exec -- monty list
```

When a feature is complete, archive it with:

```sh
dune exec -- monty done <worker-id>
```

This deletes the worker worktree and branch, closes any linked Monty-owned local task, marks the job done, and moves durable worker memory to `.monty/runs/<run-id>/archive/<worker-id>/`.
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
Monty validates that any `wt` result belongs to the requested repo, because different repos may use the same branch name.
Treat wt worktrees as ephemeral.
Durable session memory belongs in the worker directory under `.monty/runs/<run-id>/workers/<worker-id>/`.
Each worker receives Monty instructions and its context file as pi `@file` arguments.
Do not assume the worker can see the full head-butler planning conversation.
Put all essential information in the worker context file.
Workers are instructed to write important discoveries, blockers, and handoff notes back to their worker directory.

Resume an existing worker with:

```sh
dune exec -- monty resume <worker-id>
```

Resume uses the durable worker's persisted worktree mode even when current CLI or environment defaults differ.

Resume an archived worker and move it back to active memory with:

```sh
dune exec -- monty resume --archived <worker-id>
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
Use Ghostty as the default terminal backend.
Use Monty's repo-scoped `ensure-worktree` flow for worktree creation and reuse.
It must use the existing `wt` CLI, validate the selected repo, and automatically answer `wt` repo-selection prompts when branch names collide across repos.
Never bypass `wt` with direct `git worktree` commands.
All JSON mutations must use Monty's one-home lock and atomic replacement path.
Never hold that lock while invoking `gh`, `wt`, Ghostty, pi, `osascript`, git, or other slow external commands.
When changing state or lifecycle behavior, add isolated checkout-binary E2E coverage with a unique `MONTY_HOME`, fake external tools, reliable cleanup, and real temporary Git repositories where identity matters.
