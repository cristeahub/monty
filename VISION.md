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
Pi owns agent interaction, natural language execution, and subagent runtime orchestration.
The model owns planning, judgment, and implementation choices.
Monty must not duplicate Pi run state in its durable schema.

## Head butler and worker model

Monty has a head-butler session and worker sessions.
The head butler plans work, writes context files, launches workers, reviews active jobs, and archives completed jobs.
Workers execute focused tasks, keep durable notes, and report back through their worker memory.

A worker must be able to start from its context file and Monty instructions without reading the full planning conversation.
The head butler must be able to inspect current work without entering every worker session.

Headless execution is a head-butler-only alternative to terminal workers.
Monty generates complete arguments for the harness's existing subagent tool and gives every child a Monty-owned repo-scoped worktree rather than requesting a Pi-managed worktree.
Monty does not need its own Pi extension or a second agent runtime.
Each task chain uses a fresh implementer, two mutually isolated fresh reviewers in parallel, and a fresh fixer.
Separate task chains are independent and may run concurrently.
Reviewers can write their assigned reports outside the worktree but must otherwise remain read-only.
The chain must not stage, commit, push, post remotely, manage worktrees, or complete the Monty task automatically.

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

Open jobs are discovered only from canonical `.monty/runs/<run-id>/workers/<job-id>/job.json` files.
Archived jobs are discovered only from canonical `.monty/runs/<run-id>/archive/<job-id>/job.json` files.
Physical location is authoritative when persisted status or path metadata disagrees.
Lifecycle commands should read and write these files rather than maintaining competing state.

Launch state is deliberately conservative.
`prepared` reserves an identity before external work.
`launch-requested` records intent before a terminal request that could become indeterminate.
`launch-failed` records only a definite failure before the terminal request.
A terminal command error after request intent remains `launch-requested` because a surface may already exist.
Launch-state updates must compare the expected durable source state under the home lock and must never overwrite a concurrent lifecycle transition.
No durable state claims process liveness.

Headless dispatch uses only `prepared` and `launch-requested` in its normal path.
Preparation reserves every batch identity before external worktree calls and leaves workers `prepared` until their exact subagent request is ready.
The head butler must confirm that the harness exposes the subagent tool before preparation mutates Monty state.
A pre-dispatch failure stays `prepared` and is safe to retry.
Monty atomically claims one worker as `launch-requested` and emits the complete harness arguments immediately before the head butler invokes that tool.
Any later ambiguity requires an explicit successor-chain resume and must never trigger automatic replay.
A completed headless chain remains open and `launch-requested` until the user intentionally runs the existing completion lifecycle.
Pi run IDs, backend choice, and runtime status remain ephemeral and never enter `job.json`.

This avoids split-brain state and duplicate recovery requests.
It also lets `monty list`, `monty resume`, `monty done`, and future lifecycle commands operate from one durable record.

## Monty knows where task truth lives

Monty should know what projects exist and where task truth lives for each project.
If GitHub issues provide external identity and metadata, Monty may refresh their title and URL into the local task registry.
Remote issue state must never override the local task's user-facing open or done status.
The local task registry owns task status for external and purely local work.
Worker links use exact local task keys and stable repo-plus-branch-plus-worker identities rather than ordinary title matching.

Project memory belongs in Markdown under `.monty/projects/`.
Project memory should describe stable context such as purpose, conventions, architecture notes, and working commands.
It should not become a duplicate issue tracker.

## Repo plus branch identifies code work

A branch name alone is not enough.
The durable code identity for a worker is the repo plus the branch.

Monty must always know which repo a worktree belongs to.
Monty must never accidentally resume, archive, or delete the wrong repo's worktree because another repo has the same branch name.
Resume must honor the worker's persisted worktree mode instead of current CLI defaults.

## Always use `wt` for worktrees

Monty uses `wt` for worktree create, reuse, and delete flows.
Monty may validate `wt` results and answer `wt` selection prompts.
Monty must not bypass `wt` with direct worktree management.

This keeps Monty integrated with the user's chosen worktree manager.
Monty should make `wt` safer and more deterministic for AI workflows, not replace it.

## Lifecycle commands are first-class

Jobs have a lifecycle.
A job can be planned, prepared, launch-requested, definitely launch-failed, resumed, marked done, archived, and reopened.
Completion and reopening persist operation-specific intent and can continue from either canonical physical location after interruption.
The persisted force decision remains immutable across a completion retry.

The current public lifecycle commands include:

```sh
monty launch
monty launch-many
monty list
monty resume
monty resume --archived
monty done
monty done --force
```

The `monty headless prepare-many`, `begin`, and `resume` commands form a versioned harness protocol rather than a second public lifecycle.
`begin` and `resume` emit complete arguments for the existing subagent tool, so the head butler does not reconstruct chain JSON by hand.
Ghostty remains the default behavior of the existing public commands.

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
Launch dry-run uses the same complete preflight as real launch while creating no tasks, reservations, scripts, worktrees, or terminal requests.
Complete preflight rejects any invalid or colliding batch before the first side effect.

`monty doctor` reports structured PASS, WARN, and FAIL checks with exact recovery commands.
Required dependencies are configuration-aware.
Doctor exits nonzero for FAIL and zero for PASS/WARN-only results.

Tests should cover state transitions, parsing behavior, lifecycle commands, repo disambiguation, concurrent writers, atomic-write faults, deterministic reconciliation, whole-batch validation, partial launch recovery, headless claim boundaries, generated harness arguments, isolated reviewer construction, and CLI exit contracts.
Checkout-binary E2E tests should use isolated homes, fake external tools, reliable cleanup, and real temporary Git repositories when identity matters.

Monty controls sessions, files, worktrees, and branches.
We need confidence before automation expands.

## Configuration should be explicit and environment-friendly

Monty should support both flags and environment variables.
Important settings include `MONTY_HOME`, `MONTY_BRANCH_PREFIX`, `MONTY_WT_COMMAND`, `MONTY_PI_COMMAND`, and terminal backend options.

Monty is used from development checkouts and installed wrappers.
Its behavior must be predictable in both contexts.

## Durable mutation protocol

Monty should keep one advisory lock per configured home.
Every state mutation must reload, validate, plan, and atomically write while holding that lock.
The lock must never cover network calls, worktree operations, terminal requests, pi startup, git, or other slow external commands.

JSON replacement should use a same-directory temporary file, file `fsync`, atomic rename, and parent-directory `fsync`.
Canonical path validation must reject traversal, symlink escape, ambiguous physical identity, and unsafe legacy metadata.
Unsafe legacy state requires explicit repair rather than silent migration.

Reconciliation should be deterministic and replay-safe.
Local tasks are committed before worker links so an interruption can reuse committed identity without creating duplicates.
Read-only `--no-sync` inventory must perform no reconciliation write or external fetch.

## Simplicity over cleverness

Monty should prefer small CLI commands, durable files, plain JSON, and plain Markdown over hidden state or complex protocols.
The system needs to be understandable by humans and AI agents.
Simple state makes self-extension safer.

## Feature checklist

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

Future features should make the head-butler workflow clearer and safer.
They should not make the core state model more mysterious.
