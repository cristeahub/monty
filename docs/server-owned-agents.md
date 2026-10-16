# Server-owned agents (implementation in progress)

The authority is `clients → Monty server → native runtimes`. This includes web/API,
CLI/head-butler conversations, Ghostty and Rex terminals, headless supervisors and the
optional tools-disabled context classifier. An unavailable server is an error;
there is no independent vendor launch fallback.

Configure the loopback service before using these entry points. All native runtime guards require Python 3.9+ on the server PATH; Claude SDK use additionally requires its configured Python 3.10+ environment. Existing profile
roots remain the workspace permission boundary. Account directories are explicit:
`pi_home`, `codex_home`, or `claude_home`. Terminal account selection uses
`PI_CODING_AGENT_DIR`, `CODEX_HOME`, or `CLAUDE_CONFIG_DIR`; generated login-shell
scripts preserve that selection. Native command overrides must match the trusted
profile command. Commands may include fixed arguments.

For example, a Claude profile is:

```json
{
  "profiles": [{
    "id": "claude-work",
    "harness": "claude",
    "command": "/absolute/path/to/claude",
    "python": "/absolute/path/to/sdk-venv/bin/python",
    "claude_home": "/absolute/path/to/claude-account",
    "roots": ["/absolute/path/to/repos", "/absolute/path/to/worktrees", "/absolute/path/to/monty-home"],
    "auth_env": {"ANTHROPIC_API_KEY": "CLAUDE_WORK_API_KEY"}
  }]
}
```

`auth_env` values name environment variables available to the server, rather than
containing credentials. Account directories, keys and private native endpoints
are not client connection requirements. Set the reference before starting the
server; changing a client environment does not rotate the server's credentials.

Claude's server bridge requires Python 3.10+ and the pinned dependency shipped in
`helpers/claude-requirements.txt`:

```sh
python3.14 -m venv /your/server/sdk-venv
/your/server/sdk-venv/bin/pip install -r helpers/claude-requirements.txt
monty service start --config /your/server/profiles.json
monty start --harness claude
```

The Python bridge uses official `ClaudeSDKClient` APIs over private server pipes.
It has no listener, database or supervisor. SDK authentication is separate from
native CLI login: the official quickstart documents API keys and third-party
providers. User-authorized live tests of Claude Code's 2.1.296 interactive CLI
and its updated 2.1.299 CLI through SDK 0.2.166 passed with a scoped Claude Team work login and no provider-key
overrides. Subscription reuse across other configurations is not assumed.

A Codex profile now creates a private app-server child and Unix socket. It never
adopts or stops a separately launched account daemon. Native terminal processes
and their socket connections are server children. Terminal clients forward bytes
and size changes through authenticated Monty commands; EOF detaches the client.
Retained output can be read with `/v1/terminals/<session>?offset=<byte-offset>`.
Native exit status, initial geometry and Codex Vim initialization are preserved.
A terminal client running inside Rex sends its `REX_*` placement as
`terminal_environment` on `start` and `terminal_open`; the server accepts only the
four Rex names with printable, prefix-checked values, applies them to the owned
native process for that attachment (Codex additionally receives them in the
thread's `shell_environment_policy.set` at start), and never writes them into the
durable session context. Reviewer phases run headless without a terminal and
therefore never receive Rex context.

A terminal and a Pi/Claude RPC adapter cannot simultaneously write a native
session. Opening its native terminal releases an idle RPC adapter. Structured
API input requires exiting that terminal and explicitly resuming the adapter;
terminal byte input continues through the same server. Native Pi 1.0.2 terminal
resume and fork identity have been verified with an isolated local model. Native
session navigation still needs fencing before rollout.

Fresh sessions receive durable conversation records; exact worker selectors use
`<harness>-session-id` sidecars, never `job.json`. The server validates worker and
ordered workspace identity before starting a terminal and writes its sidecar.
Absence is reported; explicit `--fresh` creates a distinct session. Selecting a
registered older session uses a receipt and conversation revision.

`detach` removes a client association without cancelling accepted work.
`release_runtime` explicitly releases an idle runtime's ownership. Codex worker
sessions hold the worker fence until release or native cleanup, including server
loss; their shared account adapter remains server-owned. `interrupt` requests
native interruption; `cancel_execution` targets an execution receipt and checks
its `updated_at` value. These are separate from closing a browser or terminal.

Headless commands submit a server-owned supervisor, retaining the existing worker
locks, pinned profiles, phase sequencing, artifacts and handoffs. The startup
receipt follows worker claim and prompt preparation. Claude phases use fresh
print/stream-JSON processes and require exactly one successful terminal result
with a nonempty handoff and valid native UUID; incomplete streams and permission
denials fail. Reviewers lose mutation-capable tools, hooks, MCP integrations and
delegation rather than relying on their prompt.
Completion leaves the task open and worktrees intact.

Reviewer preflight rejects fixed command arguments that enable tools,
integrations, permission bypass or extra worktrees. Claude reviewers use native
restricted/safe modes, exact read tools and a strict empty MCP configuration;
this requires Claude Code 2.1.248 or later and is tested with 2.1.299. Codex
reviewers and classifiers retain the account's model/provider connection fields
through native `config/read` while runtime integrations stay disabled. Inline
provider header/bearer credentials require environment references for these
paths; Monty reports a configuration error instead of using another provider.

Server shutdown closes owned pipes/process groups. A restart marks unfinished
receipts as `outcome_unknown` and clears connection-scoped pending requests.
It does not automatically replay a turn or recreate a writer. Inspect retained
history and artifacts before explicitly resuming. Ownership guards hold native
session locks through cleanup, including client disappearance and server EOF.
Terminal input bytes are omitted from command receipts; the request fingerprint
still prevents duplicate dispatch.

This branch is not ready for rollout. Full regressions and native Pi/Codex/Claude
checks have passed against isolated local models, including Claude reviewer
tool schemas, Codex headless account/model selection and Pi terminal fork IDs.
Native session-navigation fencing and approval routing across terminal/API
clients remain unfinished. A bounded, explicitly authorized Claude Team account
turn also passed through the server with its retained assistant response. Other
live-provider behavior remains unverified. The installed Monty binary and live
accounts have not been migrated.

References: [Claude CLI](https://code.claude.com/docs/en/cli-reference),
[SDK Python](https://code.claude.com/docs/en/agent-sdk/python),
[permissions](https://code.claude.com/docs/en/agent-sdk/permissions),
[approvals and questions](https://code.claude.com/docs/en/agent-sdk/user-input),
[authentication](https://code.claude.com/docs/en/agent-sdk/quickstart).
