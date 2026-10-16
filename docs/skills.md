# Monty CLI skill

Monty has Pi, Codex and Claude harness selection. Claude/server ownership rollout status is tracked in [server-owned agents](server-owned-agents.md). Every Monty session launch installs or refreshes
one portable `monty` skill, including head-butler continuations, interactive
worker resumes, headless phases, and service-owned native runtimes. Installation
happens before the harness starts, in the account/profile it will actually use.

| Execution surface | Skill directory |
| --- | --- |
| Codex on the host | `${CODEX_HOME:-$HOME/.codex}/skills/monty` |
| Claude on the host | `${CLAUDE_CONFIG_DIR:-$HOME/.claude}/skills/monty` |
| Pi on the host | `${PI_CODING_AGENT_DIR:-$HOME/.pi/agent}/skills/monty` |
| Client service | `skills/monty` inside the session profile's configured `pi_home`, `codex_home` or `claude_home` |

The executable embeds `skills/monty/SKILL.md` and `docs/monty-workflows.md`.
Installation copies the manual into `references/workflows.md`; no checkout,
network download, MCP connection, extension, or additional dependency is needed.
Read-only reviewers read that reference with file tools.
The skill does not grant shell access or change the worker's role.

Edit the source files to maintain the skill; installed copies are marked as
Monty-managed and refreshed at the next launch after upgrading the executable.
Identical copies retain their timestamps. Installation preserves unrelated skills
and configuration. A conflicting unmanaged file or symlink produces an error
naming the path; move your custom skill to another name before retrying.
Native settings that disable skills are not overridden. Existing sessions may
need their native reload command or a restart to discover newly installed skills.

Read-only CLI queries and dry-run preflight do not install anything. Claude
receives Monty instructions, task context and every ordered workspace explicitly;
it is not expected to discover AGENTS.md or MONTY.md automatically.

The former documentation MCP server and setup commands have been removed.
The old `monty mcp guide` spelling remains a CLI alias so saved worker and native
session instructions can still read the manual without rewriting their context.
Current workflow and installation commands are documented under `monty guide skills`. Monty no
longer injects an MCP server into new Codex threads or headless tool lists.
Pi installation removes the previous automatically installed entry only when
its command matches the current Monty executable and its arguments are exactly
`mcp serve`. Other entries and all unrelated preferences are preserved.
Manually configured MCP servers and entries pointing at another Monty path are
left for the owner to remove. A live native session can retain old loaded tools
until restarted; migration never interrupts it automatically.

Existing worker scripts are authenticated against the old template before
explicit resume replaces them with current launch instructions. Worker/task
identity, session sidecars, selected profiles, and lifecycle are unchanged.

Sources: [Codex skills](https://learn.chatgpt.com/docs/build-skills) and
[Pi skills](https://pi.dev/docs/latest/skills). The installed Codex account-local
`CODEX_HOME/skills` discovery is also verified with an isolated `skills/list`
probe; the Pi loader reads its native agent directory's `skills` directory.

Validation: `dune runtest` uses fake harnesses and isolated native homes. The
opt-in `python3 test/test_skills_native.py _build/default/bin/main.exe` checks
real Codex and Pi discovery in both profile and shared skill directories without
sending a prompt or making model calls.
