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
      "title": "Fix issue 123",
      "repo": "/absolute/path/to/repo",
      "branch": "monty/issue-123",
      "context": ".monty/runs/<run-id>/issue-123.md"
    }
  ]
}
```

The `branch` field is optional.
Prefer setting it when the issue number or task name gives a clear branch name.
Use `monty/<short-task-name>` branch names.

After writing the manifest and context files, launch workers with:

```sh
dune exec -- monty launch-many --manifest .monty/runs/<run-id>/jobs.json
```

Use dry-run first when checking the generated manifest or when the user asks for a preview.

```sh
dune exec -- monty launch-many --terminal dry-run --manifest .monty/runs/<run-id>/jobs.json
```

## Worker expectations

Worker sessions are launched in worktrees created by `wt b <branch>`.
Each worker receives its context file as a pi `@file` argument.
Do not assume the worker can see the full head-butler planning conversation.
Put all essential information in the worker context file.

## Project conventions

The implementation is OCaml built with Dune.
Use Dune package management and dependencies in `dune-project`.
Do not add opam files.
Use Ghostty as the default terminal backend.
Use the existing `wt` CLI for worktree creation and reuse.
