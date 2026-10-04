# Monty MCP

Monty ships a portable, read-only documentation MCP server in its executable.
It works with local stdio MCP clients, including Codex, Pi and Claude Code.
Claude Code can consume the server even though Monty's worker launcher currently
supports only Pi and Codex. HTTP-only or remotely hosted clients need a stdio
bridge; Monty does not expose a network endpoint.

## One workflow across agents

`AGENTS.md` holds the head-butler behavior contract. `CLAUDE.md` imports that same
file for Claude Code; Pi and Codex read AGENTS.md directly. Generated worker
MONTY.md files hold role, task identity, workspace paths and durable memory
instructions. Connecting an MCP server alone does not assign a head-butler role.

`docs/monty-workflows.md` is the shared operating manual. Dune embeds it in the
binary, so it remains available outside this repository and cannot go missing
when a worktree is archived. Edit that file to update workflows. Exact command
help comes from the same Cmdliner command tree that executes the CLI.

The server exposes two tools:

| Tool | Input | Result |
| --- | --- | --- |
| `monty_help` | Optional `topic`; omitted means `index` | Topic index or one workflow |
| `monty_command_help` | Optional `command` array, e.g. `["task", "workspace", "add"]` | Exact CLI help |

These tools never execute the documented operation, inspect task/account data,
start an agent, or mutate Monty state. Agents use the installed CLI for authorized
operations. The protocol and documentation are identical across clients; clients
may prefix tool names or expose them through tool search.

Without MCP, use `monty mcp guide` and `monty mcp guide launch`. Normal
`monty task workspace add --help=plain` remains available too.

## Automatic connections

Monty adds session-local MCP configuration to host Codex start, continue,
interactive worker launch/resume and headless phase commands. It preserves
the selected CODEX_HOME/Mantle profile and other configured MCP servers.

Pi start, continue, interactive worker launch/resume and headless phases ensure
one native `monty` entry in `PI_CODING_AGENT_DIR/mcp.json`, defaulting to
`~/.pi/agent/mcp.json`. This entry persists and is also available in standalone
Pi sessions. No Pi extension is installed. The two documentation tools use direct
exposure; headless reviewers retain read-only tools. Existing entries and other
configuration keys are preserved. Matching entries without an explicit exposure
gain `"exposure": "direct"`; identical direct entries are not rewritten. Explicit
non-direct exposure or `toolExposure` values are refused: the documentation tools
must be directly available even in restricted headless sessions. Remove these
overrides or explicitly set their values to `direct` to use automatic setup.
Invalid JSON, unsafe paths, or a conflicting/disabled `monty` entry cause a clear error;
Monty never silently replaces them. To resolve a conflict, explicitly update the
entry using `monty mcp config --format json`, or rename the other entry.
Project `.pi/mcp.json` entries can override the user entry; check `/mcp` if tools
are missing. Pi must support native MCP and have its built-in MCP enabled.

Background source triage keeps its no-tools/no-MCP isolation. Container workers
do not receive a host executable path; they retain task instructions and would
need a compatible Monty binary inside the container to use MCP.

## Claude Code and other clients

After installing Monty, generate configuration using the executable that should
serve the documentation:

```sh
monty mcp config --format json > /tmp/monty-mcp.json
claude --mcp-config /tmp/monty-mcp.json
```

This connects the server for that Claude Code invocation. For a persistent
connection, Claude Code also supports:

```sh
claude mcp add --transport stdio monty -- /absolute/path/to/monty mcp serve
```

For head-butler behavior, start Claude Code in the installed Monty control room
so its CLAUDE.md imports AGENTS.md. When connecting from another project, include
the relevant Monty instructions explicitly. Operations still use the chosen
Monty home; documentation connections do not create separate task registries.

Other local clients can use this standard configuration, with an absolute path:

```json
{
  "mcpServers": {
    "monty": {
      "command": "/absolute/path/to/monty",
      "args": ["mcp", "serve"]
    }
  }
}
```

`monty mcp config --format json` prints this shape with the current binary's
absolute path. For a standalone Codex client, use the equivalent TOML emitted
by `monty mcp config --format toml`. Generating configuration does not install it
or alter any client settings. An installation at a different path requires
refreshing stored configuration.

## Protocol and validation

The server speaks newline-delimited JSON-RPC over stdio and advertises only
tools. It supports initialization, version negotiation, the initialized
notification, ping, tool discovery and tool calls. Known protocol versions are
2024-11-05, 2025-03-26, 2025-06-18 and 2025-11-25; other requested versions
negotiate 2025-11-25. Requests are bounded to 64 KiB. Invalid requests return
JSON-RPC errors; unknown workflow topics and invalid command paths return tool
errors. No banners or log output are written to protocol stdout.

The isolated test suite exercises protocol behavior, command-injection rejection,
shared manual/CLI parity, configuration merging and launch/resume wiring without
real accounts, model calls or user configuration changes.

Client references: [Codex MCP](https://learn.chatgpt.com/docs/extend/mcp),
[Pi MCP](https://pi.dev/docs/latest/mcp),
[Claude Code MCP](https://code.claude.com/docs/en/mcp).
