# Monty head-butler instructions

This repo is the Monty control room.
Use it to plan work, choose actionable tasks, and launch worker pi sessions.

## Head-butler workflow

When the user asks what to work on, inspect the provided repos, issues, links, or notes.
Ask clarifying questions when the selected work is ambiguous.
Keep planning artifacts under `.monty/runs/<run-id>/`.
Use a short run id such as `2026-06-27-issues` or `run-001`.

When the user chooses tasks to execute, create one Markdown context file per worker task.
Each context file should be specific enough that a fresh pi worker can start without reading the whole planning conversation.
Include the task summary, repo path, issue or PR links, relevant constraints, acceptance criteria, and any important planning notes.

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
      "worker_dir": ".monty/runs/<run-id>/workers/issue-123"
    }
  ]
}
```

The `branch` field is optional.
Prefer setting it when the issue number or task name gives a clear branch name.
Use the configured branch prefix for branch names.
The default prefix is `monty`, but users may set `MONTY_BRANCH_PREFIX`, for example `cto`.
When omitting `branch`, Monty derives `<branch-prefix>/<title-slug>` automatically.

After writing the manifest and context files, launch workers with:

```sh
dune exec -- monty launch-many --manifest .monty/runs/<run-id>/jobs.json
```

Use dry-run first when checking the generated manifest or when the user asks for a preview.

```sh
dune exec -- monty launch-many --terminal dry-run --manifest .monty/runs/<run-id>/jobs.json
```

At the start of a day or planning session, review active jobs with:

```sh
dune exec -- monty list
```

When a feature is complete, archive it with:

```sh
dune exec -- monty done <worker-id>
```

This deletes the worker worktree and branch, marks the job done, and moves durable worker memory to `.monty/runs/<run-id>/archive/<worker-id>/`.
Use `--force` only when the user explicitly accepts discarding local worktree changes.
Use `monty list --archived` or `monty list --all` when reviewing archived work.

## Project overview workflow

When the user asks about current projects, project context, task overview, or what Monty knows about their work, get the information from Monty first.
Use `monty overview` for a cross-project summary.
Use `monty projects list` and `monty projects show <project>` for project memory.
Use `monty tasks list` for task summaries.
Use `monty projects add --repo <repo> --github <owner/repo>` when the user wants Monty to start tracking a project with GitHub issues as the task source of truth.
Use `monty task add --project <project> --title <title>` only for work that has no external source of truth.
Use `monty task priority <task> <priority>` for local priority without changing the external source of truth.
Do not duplicate external tasks into Monty memory when a source of truth such as GitHub issues exists.
Use Monty-owned local tasks only when no external source of truth exists.

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

Resume an archived worker and move it back to active memory with:

```sh
dune exec -- monty resume --archived <worker-id>
```

## Project conventions

The implementation is OCaml built with Dune.
Use Dune package management and dependencies in `dune-project`.
Do not add opam files.
Use Ghostty as the default terminal backend.
Use Monty's repo-scoped `ensure-worktree` flow for worktree creation and reuse.
It must use the existing `wt` CLI, validate the selected repo, and automatically answer `wt` repo-selection prompts when branch names collide across repos.
Never bypass `wt` with direct `git worktree` commands.
