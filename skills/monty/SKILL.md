---
# Managed by Monty. Edit the source skill, not this installed copy.
name: monty
description: Operate Monty's CLI to inspect tasks and projects, plan and launch workers, resume work, handle worker handoffs, and complete explicitly selected tasks. Use for Monty operations, not ordinary coding or task-free discussion.
metadata:
  monty-stub: "1"
---

# Monty

Run `monty guide` and follow the topic it names for your task, then
`monty guide TOPIC` and `monty COMMAND --help=plain` for its current syntax.
The guide comes from the installed `monty`, so it describes the commands that
version has. `monty skill show` prints the whole guide at once.

If you have file tools but no shell, read the same guide in
[references/workflows.md](references/workflows.md), one section at a time.

Follow the active `AGENTS.md`, task `MONTY.md`, workspace map and user request.
This skill supplies CLI guidance; it does not assign a head-butler role or grant
permission to launch, resume, complete, discard changes, or write remotely.
Read-only reviewers remain read-only; headless children return their handoff
to Monty's supervisor and do not manage task lifecycle or worktrees.

Use native inventory and canonical task/worker identities. A numbered task
reference refers to the last list actually shown to the user. Never infer a
worker/task link from its title or branch, or completion from runtime status.
Preserve important discoveries in durable worker memory, outside worktrees.
