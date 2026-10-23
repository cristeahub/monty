# Agents

Monty ships a built-in agent skill and a session hook that let agents such as
Claude Code, Codex and Pi operate Monty efficiently: list and plan tasks, launch
and resume workers, read and acknowledge worker handoffs, and complete
explicitly selected tasks. They are built the way Rex builds its agent support,
so an agent that knows Rex finds the same shape here.

Even without the skill, an agent can discover the `monty` CLI from `--help`,
but the skill and hook save tokens and turns, and every Monty launch installs
them into the account it launches into.

## What agents can do

| You ask | The agent |
| --- | --- |
| "What is pending?" | Runs `monty handoff pending`, shows each notice, then acknowledges it. |
| "Show me the open tasks." | Runs `monty list` and presents the table with the exact rows and banner. |
| "Launch 2 and 4." | Resolves the numbered rows from the last list shown, writes standalone context files, preflights the batch dry-run and launches. |
| "What did the worker find?" | Runs `monty handoff show` and `monty handoff follow-up` without resuming implementation. |
| "Complete 3." | Runs `monty done` for that row's real identifier, refusing dirty worktrees. |

Inside a worker, the hook tells the agent it is a Monty worker, where its context
file and workspaces are, and how to publish its handoff. Inside the control room,
the hook tells the agent it is the head butler and how many handoffs are pending.

## Setup

Every `monty` launch installs the skill and hook into the account profile it
launches into (the harness's own directory or a configured service profile
home), before the harness starts. Setup by hand is only needed for sessions
started outside Monty:

```
$ monty skill setup
Claude Code (found)
  skill  created      ~/.claude/skills/monty
  hook   created      ~/.claude/settings.json

Codex (found)
  skill  created      ~/.codex/skills/monty
  hook   created      ~/.codex/hooks.json

Pi (found)
  skill  created      ~/.pi/agent/skills/monty
  hook   unsupported

Start a new agent session inside Monty to use it.
```

This installs two things:

- The **skill** teaches the agent the `monty` commands and when to use them.
  Its `SKILL.md` is a stub that sends the agent to `monty guide`, so the guide
  always comes from the installed `monty` and upgrading Monty updates what the
  agent knows without running setup again. `references/workflows.md` holds the
  same guide for agents that have file tools but no shell, such as read-only
  reviewers.
- A **hook** tells the agent, at the start of each session, whether it is a
  Monty worker or the head butler, which task and workspaces it has, and what
  handoffs are pending, so it does not spend turns finding out. Outside a Monty
  session the hook prints nothing.

Monty adds its hook to the agent's settings file and leaves the rest of the file
as it was. Running setup again reports what is already current as `unchanged`
and updates anything an older Monty installed.

Monty finds an agent by its configuration directory, such as `~/.claude`, or by
its program on `PATH`, and follows `CLAUDE_CONFIG_DIR`, `CODEX_HOME` and
`PI_CODING_AGENT_DIR`. Configured service profiles (`.monty/service-profiles.json`
or the saved `--config`) are set up too, because those are the accounts Monty
launches into. To set up an agent Monty did not find, name it with `--harness`.

| Option | Does |
| --- | --- |
| `--harness NAME` | Sets up only the named harness (`claude`, `codex` or `pi`), whether or not Monty found it. Repeat it to name several. |
| `--dry-run` | Reports what would change without changing anything. |
| `--force` | Replaces a skill named `monty` that Monty did not install. |
| `--json` | Prints the result as JSON. |
| `--home DIR` | The Monty home whose service profiles are included. |

### Approving the hook

Some agents will not run a new hook until you allow it:

- **Codex** asks before it runs a new or changed hook. Approve Monty's hook with
  `/hooks` in Codex. Until then, Codex skips it.
- **Claude Code** does not ask about each hook, but it runs no hooks at all until
  you trust the folder a session starts in.

Headless reviewer phases run with hooks disabled, so the hook never reaches a
read-only reviewer.

## Supported agents

| Agent | `--harness` | Skill | Hook |
| --- | --- | --- | --- |
| Claude Code | `claude` | `${CLAUDE_CONFIG_DIR:-~/.claude}/skills/monty` | `${CLAUDE_CONFIG_DIR:-~/.claude}/settings.json` |
| Codex | `codex` | `${CODEX_HOME:-~/.codex}/skills/monty` | `${CODEX_HOME:-~/.codex}/hooks.json` |
| Pi | `pi` | `${PI_CODING_AGENT_DIR:-~/.pi/agent}/skills/monty` | Not supported |
| Service profile | the profile's harness | `skills/monty` inside its `claude_home`, `codex_home` or `pi_home` | As above |

Any other agent that can run commands can use Monty too. `monty skill show`
prints the guide the skill gives agents, so tell the agent to run it and follow
the guide.

## How it works

The skill is a small stub:

```
---
name: monty
description: Operate Monty's CLI to inspect tasks and projects, plan and launch workers, …
---
Run `monty guide` and follow the topic it names for your task, then
`monty guide TOPIC` and `monty COMMAND --help=plain` for its current syntax.
```

`monty guide` prints the topic index of the embedded workflow manual, `monty
guide TOPIC` one topic, `monty guide all` the whole manual, and `--json` the
same as JSON. The manual is `docs/monty-workflows.md`, embedded in the binary.

The hook runs `monty __internal session-start --harness NAME` with a short
timeout. Monty decides what to say from the environment and working directory
the agent started with: the worker variables (`MONTY_JOB_ID`, `MONTY_JOB_FILE`,
`MONTY_WORKSPACE_n`, …) name a worker; a session started in `MONTY_HOME` is the
head butler; anything else is not a Monty session. It reads the pending handoff
inbox but never writes, acknowledges, reconciles or starts the service.

The skill supplies documentation, not authorization: it does not assign the
head-butler role or grant permission to launch, resume, complete, discard
changes or write remotely. Role instructions stay in `AGENTS.md` and each
worker's `MONTY.md`.

## Removing the skill

`monty skill remove` removes the skill and hook from every agent and service
profile Monty knows:

```sh
monty skill remove
```

Only what Monty installed is removed. Other skills, other hooks and the rest of
each settings file stay as they were; a skill named `monty` that Monty did not
install is reported as `kept`. `--harness`, `--dry-run` and `--json` work the
same as for setup. The next Monty launch installs both again.

Codex remembers which hooks you approved by their place in `hooks.json`. If
removing or updating Monty's hook moves the hooks after it, Codex asks you to
approve those again, and Monty says so in its output.

## Debugging

`monty skill status` reports what is installed in each agent, without changing
anything:

```
$ monty skill status
Claude Code (found)
  skill  current      ~/.claude/skills/monty
  hook   current      ~/.claude/settings.json

Codex (found)
  skill  outdated     ~/.codex/skills/monty
  hook   current      ~/.codex/hooks.json
…
Run "monty skill setup" to install or update the skill.
```

| State | Meaning |
| --- | --- |
| `current` | Installed and up to date. |
| `outdated` | Installed by an older Monty, or edited since. `monty skill setup` updates it. |
| `missing` | Not installed. |
| `foreign` | A skill named `monty` that Monty did not install. Setup leaves it alone unless you pass `--force`. |
| `unreadable` | The file could not be read safely: a symlink, invalid JSON, or an unexpected shape. Setup refuses it and says why. |
| `unsupported` | The agent cannot hold this item, such as a hook in Pi. |

A launch into an account with a `foreign` or `unreadable` item fails with the
path, because Monty never overwrites what it did not install. If an agent does
not seem to know it is in Monty, check that its session started after setup
ran, since the hook only runs when a session starts. In Codex, check that the
hook is approved with `/hooks`. In Claude Code, check that you trusted the folder.

Installed copies are marked as Monty-managed and refreshed at the next launch
after upgrading the executable. Identical copies keep their timestamps.
Installation preserves unrelated skills and settings and never follows symlinks.
Native settings that disable skills or hooks are not overridden. Existing
sessions may need their native reload command or a restart to discover a newly
installed skill.

Validation: `dune runtest` covers setup, status, remove, hook merging, service
profiles, the session hook and Pi startup with fake harnesses and isolated
native homes. The opt-in
`_build/default/test/test_skills_native.exe _build/default/bin/main.exe` checks
real Codex and Pi discovery without sending a prompt or making model calls.
