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
Always invoke the globally installed `monty` executable directly.
Never use `dune exec -- monty` for Monty workflows.

## Native Pi handoff

When Monty is running through `monty start`, use Monty's task registry first and the native Pi slash commands for workflow and navigation.
Do not inspect Monty's implementation, reconstruct manifests, read raw state files, or research how task dispatch works before handling an ordinary task request.

When the user asks to begin a concrete task:

1. Check `monty list` so an existing task is not duplicated.
2. If the task is absent, create it with `monty task add --project <project> --title <title>` once its project and title are unambiguous.
3. Run `monty list` again and present the task inventory with exactly the columns ID, Project, Status, Title, and Branch.
4. End with: `Jump to the task by running /monty.`

Ask a focused clarifying question instead of inventing a project or title when either is ambiguous.
The model cannot invoke Pi slash commands or switch sessions itself, so do not claim that a conversational request already opened or navigated to a task.
After the user enters `/monty`, the native selector owns task choice, planning, and session replacement.
Use `/monty-start` only after the user approves the current plan, `/monty-run resume` only when the user explicitly requests a successor run, `/monty-head-butler` to go to the head session, and `/monty-plan-cancel` to abandon planning.
Do not infer approval to complete a task, run a successor chain, push, or perform another remote action from a general request to start or inspect work.
In native plan mode, inspect only the selected task's relevant project context, remain read-only, and finish with a concrete numbered `Plan:` section.
Do not create manual manifests or context files for the native conversational flow because `/monty-start` creates the canonical task context and worker state.

## Head-butler workflow

When the user asks what to work on, inspect the provided repos, issues, links, or notes.
Ask clarifying questions when the selected work is ambiguous.
Keep planning artifacts under `.monty/runs/<run-id>/`.
Use a short run id such as `2026-06-27-issues` or `run-001`.

Inside the native Monty Pi extension, use `/monty` to select work, `/monty-open <task>` to open named work, `/monty-start` to start an approved plan, and `/monty-head-butler` to go to the exact head session.
Direct session-navigation commands cancel an active foreground Pi response and show a spinner while cancellation settles, but they must never stop or await detached Monty worker chains.
An unstarted task remains in the head-butler session and enters read-only plan mode.
After the user approves the plan, the native start action prepares a task subsession and launches its asynchronous chain while the head butler remains available.

For explicit Ghostty or manual batch workflows, create one Markdown context file per worker task.
Each context file should be specific enough that a fresh pi worker can start without reading the whole planning conversation.
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

After writing a manual batch manifest and its context files, launch workers with:

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

`monty start` loads Monty's process-wide Pi extension.
The extension owns task selection, read-only plan mode, native task subsessions, minimal location status, and return navigation.
It uses Monty's JSON descriptors and the existing `pi-subagents` RPC rather than duplicating session or agent runtime state.
Ghostty remains available through explicit `launch`, `launch-many`, and `resume` commands.
The direct headless CLI flow remains available for manual and recovery use.

Before any mutating headless command, call the harness `subagent` tool with `action: "list"` and confirm that the tool and required agents are available.
If they are unavailable, stop without mutating Monty state.
Run headless dry-run first when checking a new batch or when the user asks for a preview:

```sh
monty headless prepare-many --dry-run --manifest .monty/runs/<run-id>/jobs.json
```

Then prepare the real batch:

```sh
monty headless prepare-many --manifest .monty/runs/<run-id>/jobs.json
```

Headless preparation reserves every job and materializes its Monty-managed `wt` worktree while leaving the job `prepared`.
For each prepared worker, run `monty headless begin <worker-id>` immediately before the harness call.
Read the returned `harness_call.tool` and pass `harness_call.arguments` unchanged to that exposed harness tool.
Do not manually reconstruct, simplify, or enrich the generated chain JSON.
The generated asynchronous chains can run concurrently without waiting for earlier jobs to finish.

Each chain gets fresh minimal context and runs one implementer, two mutually isolated reviewers in parallel, and one fixer.
Reviewers may write only their separate reports outside the worktree.
No child may create worktrees, stage, commit, push, open a PR, post remotely, or run `monty done`.
A successful chain leaves the task open and its worktree intact.
Never infer completion from Pi runtime status.

If a harness call fails after `begin`, leave the worker `launch-requested`.
Run `monty headless resume <worker-id>` only when the user intentionally requests a fresh successor chain, then pass its generated `harness_call.arguments` unchanged to the harness tool.
Never persist a backend, Pi run ID, async status, or runtime state in `job.json`.
Never automatically run `monty done` after a headless chain.

At the start of a day or planning session, review active jobs with:

```sh
monty list
```

When a feature is complete, archive it with:

```sh
monty done <worker-id>
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
Headless child agents receive the exact same Monty-managed worktree as their explicit `cwd`; they must never request Pi-managed worktrees.
Monty validates that any `wt` result belongs to the requested repo, because different repos may use the same branch name.
Treat wt worktrees as ephemeral.
Durable session memory belongs in the worker directory under `.monty/runs/<run-id>/workers/<worker-id>/`.
Each worker receives Monty instructions and its context file as pi `@file` arguments.
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

The deterministic CLI and state layer is OCaml built with Dune.
The bundled native Pi adapter lives under `pi-extension/` and uses Pi's public extension and session APIs.
Use Dune package management and dependencies in `dune-project`.
Do not add opam files.
Keep headless state and payload generation in Monty, while execution uses the harness's existing subagent tool or stable RPC.
Do not add a new persisted backend.
Use Monty's repo-scoped `ensure-worktree` flow for worktree creation and reuse.
It must use the existing `wt` CLI, validate the selected repo, and automatically answer `wt` repo-selection prompts when branch names collide across repos.
Never bypass `wt` with direct `git worktree` commands.
All JSON mutations must use Monty's one-home lock and atomic replacement path.
Never hold that lock while invoking `gh`, `wt`, Ghostty, pi, `osascript`, git, or other slow external commands.
When changing state or lifecycle behavior, add isolated checkout-binary E2E coverage with a unique `MONTY_HOME`, fake external tools, reliable cleanup, and real temporary Git repositories where identity matters.
Keep Pi navigation state in custom Pi session entries.
Never write Pi session paths, process IDs, run IDs, or subagent runtime state to `job.json`.
