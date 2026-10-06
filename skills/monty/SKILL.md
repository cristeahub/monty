---
# Managed by Monty. Edit the source skill, not this installed copy.
name: monty
description: Operate Monty's CLI to inspect tasks and projects, plan and launch workers, resume work, handle worker handoffs, and complete explicitly selected tasks. Use for Monty operations, not ordinary coding or task-free discussion.
---

# Monty

Use the installed `monty` CLI. Run `monty guide` to find the relevant workflow,
then `monty guide TOPIC` and `monty COMMAND --help=plain` for its current syntax.
Do not use `dune exec -- monty` for operations.

The same manual is bundled in [references/workflows.md](references/workflows.md).
Read only the relevant section, especially when your role has file access but
no shell, or when running inside a container without a Monty executable.
The manual covers inventory, launch, headless, lifecycle, projects, workers,
settings, findings, handoffs, skills, web-client, and client-service.

Follow the active `AGENTS.md`, task `MONTY.md`, workspace map and user request.
This skill supplies CLI guidance; it does not assign a head-butler role or grant
permission to launch, resume, complete, discard changes, or write remotely.
Read-only reviewers remain read-only; headless children return their handoff
to Monty's supervisor and do not manage task lifecycle or worktrees.

Use native inventory and canonical task/worker identities. A numbered task
reference refers to the last list actually shown to the user. Never infer a
worker/task link from its title or branch, or completion from runtime status.
Preserve important discoveries in durable worker memory, outside worktrees.
