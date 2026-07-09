# Monty development vision

Monty is the control room for AI-directed development work.
Pi and the model decide what should happen, while Monty provides deterministic lifecycle commands, durable state, repo-scoped worktree handling, and session launch plumbing.
The goal is to make AI development work parallel, resumable, inspectable, and safe.
This document records the ground truths we follow when extending Monty, including when Monty is used to extend itself.

## Monty's role

Monty is deterministic glue around an AI planning workflow.
Monty should not become a second planning brain.
The head butler and worker agents decide what work should be done.
Monty makes that work repeatable, inspectable, resumable, and safe.

Monty owns operational mechanics such as terminal launch, repo-scoped worktree selection, durable job state, and lifecycle commands.
Pi owns agent interaction and natural language execution.
The model owns planning, judgment, and implementation choices.

## Head butler and worker model

Monty has a head-butler session and worker sessions.
The head butler plans work, writes context files, launches workers, reviews active jobs, and archives completed jobs.
Workers execute focused tasks, keep durable notes, and report back through their worker memory.

A worker must be able to start from its context file and Monty instructions without reading the full planning conversation.
The head butler must be able to inspect current work without entering every worker session.

## Durable memory beats ephemeral worktrees

Worker memory is durable.
Worktrees are ephemeral.

Worker memory lives under this shape:

```text
.monty/runs/<run-id>/workers/<job-id>/
```

Archived memory lives under this shape:

```text
.monty/runs/<run-id>/archive/<job-id>/
```

Workers must write important discoveries, blockers, handoff notes, and final state into durable memory.
A task must not depend on the worktree as the only place where important knowledge exists.

## `job.json` is the source of truth after launch

`jobs.json` is launch input only.
After launch, the durable worker `job.json` is the source of truth.

Active jobs are discovered from worker `job.json` files.
Archived jobs are discovered from archive `job.json` files.
Lifecycle commands should read and write these files rather than maintaining competing state.

This avoids split-brain state.
It also lets `monty list`, `monty resume`, `monty done`, and future lifecycle commands operate from one durable record.

## Monty knows where task truth lives

Monty should know what projects exist and where task truth lives for each project.
If GitHub issues are the source of truth, Monty should fetch them live instead of copying them into local state.
If no external source of truth exists, Monty may own a local task.

Project memory belongs in Markdown under `.monty/projects/`.
Project memory should describe stable context such as purpose, conventions, architecture notes, and working commands.
It should not become a duplicate issue tracker.

## Repo plus branch identifies code work

A branch name alone is not enough.
The durable code identity for a worker is the repo plus the branch.

Monty must always know which repo a worktree belongs to.
Monty must never accidentally resume, archive, or delete the wrong repo's worktree because another repo has the same branch name.

## Always use `wt` for worktrees

Monty uses `wt` for worktree create, reuse, and delete flows.
Monty may validate `wt` results and answer `wt` selection prompts.
Monty must not bypass `wt` with direct worktree management.

This keeps Monty integrated with the user's chosen worktree manager.
Monty should make `wt` safer and more deterministic for AI workflows, not replace it.

## Lifecycle commands are first-class

Jobs have a lifecycle.
A job can be planned, launched, active, resumed, marked done, archived, and reopened.

The current lifecycle commands include:

```sh
monty launch
monty launch-many
monty list
monty resume
monty resume --archived
monty done
monty done --force
```

Lifecycle commands should be designed as stable product surfaces.
They are used by humans, the head butler, and worker sessions.

## Pi invocation is a product surface

Monty commands must be easy for pi to invoke from natural language instructions.
Generated worker `MONTY.md` files are part of the product.
They tell worker sessions what Monty commands mean and when to run them.

A user should be able to say this:

```text
ok, now we are done with this feature
```

The worker should understand that this maps to this command:

```sh
monty done
```

This is how Monty becomes AI-native without requiring a complicated API first.

## Context files are contracts

Every worker task should have a clear context file.
The context file is the contract between the head butler and the worker.

A context file should include the task summary, repo path, branch if known, links or issue IDs, constraints, acceptance criteria, and important planning notes.
The worker should not need hidden conversation state to understand the assignment.

## Destructive actions require safeguards

Monty should block dangerous actions by default.
`monty done` refuses dirty worktrees by default.
`--force` must be explicit.
Deleting worktrees and branches must be repo-scoped.
Remote actions must never happen without explicit user approval.

Safety rules must remain visible as Monty gains more automation.
This is especially important if Monty is used to extend itself.

## Dry-run, doctor, and tests are part of the design

Operational behavior should be inspectable and testable.
Dry-run modes help users and agents understand what Monty will do before it mutates anything.
`monty doctor` helps diagnose missing external dependencies.
Tests should cover state transitions, parsing behavior, lifecycle commands, and repo-disambiguation logic.

Monty controls sessions, files, worktrees, and branches.
We need confidence before automation expands.

## Configuration should be explicit and environment-friendly

Monty should support both flags and environment variables.
Important settings include `MONTY_HOME`, `MONTY_BRANCH_PREFIX`, `MONTY_WT_COMMAND`, `MONTY_PI_COMMAND`, and terminal backend options.

Monty is used from development checkouts and installed wrappers.
Its behavior must be predictable in both contexts.

## Simplicity over cleverness

Monty should prefer small CLI commands, durable files, plain JSON, and plain Markdown over hidden state or complex protocols.
The system needs to be understandable by humans and AI agents.
Simple state makes self-extension safer.

## Extension checklist

Every new Monty feature should answer these questions.

1. What durable state does this read or write?
2. Is `job.json` still the source of truth?
3. Can the head butler invoke it?
4. Can a worker invoke it from generated instructions?
5. Does it work after the worktree is deleted?
6. Does it respect repo plus branch identity?
7. Does it use `wt` for worktree operations?
8. Is there a dry-run or safe preview path?
9. What is the recovery story if it fails halfway?
10. What tests prove the behavior?

## Long-term direction

Monty should become better at managing the full lifecycle of AI-directed development work.
It should help start the day, enumerate active work, launch focused workers, resume interrupted work, archive completed features, and preserve durable knowledge.

Future extensions should make the head-butler workflow clearer and safer.
They should not make the core state model more mysterious.
