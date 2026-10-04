# External context sources

Monty can collect Slack and Zulip messages in the background on a Mac and produce
an attention inbox on request. It ships **off**. No messages are sent, no remote
read flags are changed, and no coding jobs are created by collection or triage.
Email, MCP transport and debugging-system adapters are deferred.

## Onboarding

Use an installed Monty control room containing `helpers/context_source.py`.
Python 3.9+ is required. Slack uses the standard library and the official Slack
CLI with `api --json --include` capability (checked before access). Zulip uses a
separate Python environment, without Zulip Terminal:

```sh
python3 -m venv ~/.local/share/monty-zulip
~/.local/share/monty-zulip/bin/pip install -r "$MONTY_HOME/helpers/context-requirements.txt"
```

Install dependencies intentionally; collection never installs software. Keep an
owned mode-0600 credential file outside Monty state: a Slack bot/user token alone
in the file, or a Zulip `zuliprc` accepted by the official Python client. It can be
the same Zulip config used by Zulip Terminal. Monty persists only the path.
Slack developer login is insufficient. The token must grant the relevant
`channels:read`, `channels:history`, `groups:read`, `groups:history`, `im:read`,
`im:history`, `mpim:read`, `mpim:history` scopes for selected conversations.
Channel thread replies may require a user token; bot access is narrower. Test
actual history/replies access and review source health before relying on coverage.
The adapter supplies a child-only environment token, never `--token` or `--app`.
Do not put tokens in shell arguments, task context, relevance text or config JSON.

Example `slack-context.json` (replace placeholders with actual IDs and paths):

```json
{
  "id": "work-slack",
  "source": "slack",
  "account": "T01234567",
  "credential_file": "/absolute/private/slack-token",
  "slack_cli": "/absolute/bin/slack",
  "channels": ["C01234567"],
  "include_dms": true,
  "projects": ["monty"],
  "groups": ["work"],
  "relevance": "I am U01234567. Prioritize mentions, direct requests and release blockers."
}
```

Zulip uses the same shape, with `source: "zulip"`, the HTTPS realm URL as
`account`, numeric **stream IDs as strings** in `channels`, a `zuliprc` path as
`credential_file`, and the absolute virtualenv Python as `python`. Omit
`slack_cli`. DMs include the authenticated user's direct and group conversations.
An empty channel list is allowed only with explicit `include_dms: true`; an
unconfigured account never watches all accessible channels.

```sh
monty context configure slack-context.json
monty context check work-slack
monty context status
monty context on
monty context off
```

`configure` does not connect or enable anything. `check` is an explicit remote
read that checks identity/scope and reports capabilities. Both accounts must be
configured separately. Changing relevance queues retained observations again;
changing scope resets that connection's checkpoint and cache while preserving
existing findings. Account/source identity is immutable: use a new connection ID
for another account.

## Background operation and controls

`on` persists desired enabled state in settings, writes a per-home LaunchAgent to
`~/Library/LaunchAgents`, enables it and bootstraps it. `off` first persists off,
invalidates in-flight commits, disables and unloads the service, and verifies
active collection stops. Failures remain visible: retry the same command to
reconcile. Connections, checkpoints, observations and findings are preserved.
Repeated `on` preserves active collection and existing rate-limit backoff.
`status` reports desired versus actual launchd state, per-source next due times,
collection and analysis successes/errors, bounded coverage and analysis backlog.
It works while stopped. Unknown launchd state is not treated as successful control.

A calendar-minute launchd trigger checks due times, coalescing missed wakeups;
RunAtLoad catches up on login. Sources default to a 300-second cadence. A tick
uses separate nonblocking connection locks, runs network/model work outside the
Monty home lock, and only commits against the same configuration generation.
One failed account does not prevent later accounts from running. Each provider
and analysis subprocess has a time/output budget and is cancelled when disabled
or reconfigured. A child inherits its source lock, preventing overlap after an
unexpected parent death. No coding task or worktree is used.

Per-connection configurable limits:

| Field | Default | Range |
| --- | ---: | --- |
| cadence_seconds | 300 | 60–86400 |
| lookback_hours | 24 | 1–8760 |
| retention_days | 30 | 1–365 |
| max_pages | 20 | 1–100 |
| max_observations | 10000 | 100–100000 |
| max_findings | 10000 | 100–100000 |
| max_threads | 1000 | 1–10000 |
| analysis_messages | 100 | 1–500 |
| timeout_seconds | 120 | 5–600 |

Observation cache retention is bounded by age and capacity; distinct pending
message revisions are preserved through analysis outages. Finding capacity also
pauses analysis without losing backlog when full. Pending analysis is
never expired silently: at capacity collection stops without advancing its
checkpoint; fix analysis or raise capacity. Unresolved findings retain their
own evidence independently of the cache. Resolved findings and presentation
receipts also remain available. `context retriage CONNECTION` queues retained
observations after changed relevance or a desired second pass. Stop/start,
reconfiguration and retriage preserve existing due times and provider backoff.

The selected Pi/Codex harness follows normal `--harness`, `MONTY_HARNESS`, persisted
settings precedence. Command overrides use existing `--pi-command` and
`--codex-command` options for manual ticks; launchd captures their environment
counterparts at enable time. Triage accepts a bare harness executable only; coding
command arguments are rejected. Triage never inherits coding-worker yolo permissions.
Pi runs without tools/extensions/skills/context discovery. Codex requires
`--ignore-user-config` and `--ignore-rules`, disables integrations and shell tools,
and uses read-only sandboxing, no web search or MCP, in a fresh private scratch directory.
Codex auth still uses its normal credential store. Pi auth/provider configuration
still applies. Unsupported harnesses/flags fail visibly and preserve backlog.
No live model calls are needed for tests. Messages are untrusted data in prompts;
outputs must cite supplied evidence IDs and pass structural checks before commit.
Only changed observations are analyzed, in bounded batches; unresolved findings
for those conversations accompany the batch. An empty poll does not call a model.

## Read, present, acknowledge, resolve

```sh
monty findings
monty findings --new --source slack --json
monty findings --limit 20 --before 'CURSOR_FROM_PREVIOUS_PAGE' --json
monty findings acknowledge FINDING_ID@REVISION
monty findings show FINDING_ID@REVISION --question 'What was requested?'
monty findings resolve FINDING_ID@REVISION
monty findings to-task FINDING_ID@REVISION --project monty
```

Reads are side-effect free and return newest-first findings, evidence, reasons,
next steps, uncertainty, unresolved state, freshness and coverage. `--new` means
not yet presented, not “fetched since the last poll.” Acknowledge only **after**
presentation. Stale revisions fail rather than consuming a concurrent update.
Resolving is a separate explicit user action; new evidence can reopen a finding.
There is at most one attention item per conversation in this first version.
Follow-up reads expose retained context plus finding evidence without a remote
request or model call. Task conversion creates a local task with a finding
reference and a durable evidence snapshot in its notes, and does not launch work. Source filters isolate presentation receipts.
Nothing is sent through the worker run-handoff inbox or desktop notifications.

## Coverage and recovery

Slack paginates selected channel history and DM discovery, and rotates through
known thread roots, including roots that initially had no replies. New replies
to old **known** roots can create findings. Roots older than the initial lookback
that have never been observed are not covered; increase initial lookback when
configuring an account. `max_threads` bounds the index and a coverage warning
reports overflow. History reconciliation covers edits and disappeared messages
within retention; thread reconciliation covers retained known roots. Slack
rate-limit headers delay subsequent collection. The CLI also performs internal
429/503 retries before emitting headers; Monty detects its reported sleep in an
isolated temporary CLI log, stops the child, and persists that backoff. CLI config
and routine message logs are private and removed at the end of the operation.
Incomplete cursors persist. Expired Slack cursors replay their original covered
interval (or restart DM discovery), retaining prior reconciliation data and
reporting incomplete coverage until collection recovers.

Zulip polls selected streams/DMs newest-to-oldest with durable pagination, rescans
retained history for edits/deletions and catches new replies in old topics. It
does not rely on an expiring event queue. Disappeared messages are represented
as deleted/unavailable evidence; permissions or server retention can also explain
disappearance. Neither provider promises edits/deletions outside covered history.
Provider retention, permissions, rate limits and incomplete pages remain part of
the reported coverage. A failed check never means “nothing happened.”

Each collection batch, checkpoint, and health change commits in a single atomic
snapshot under Monty's home lock. A failed atomic replacement preserves the old
checkpoint for retry. Identity+revision deduplicates overlap and retries. Analysis
and presentation are separate later atomic transactions. Corrupt/unsafe state
fails closed; do not delete checkpoints to hide a failure.

Implementation references: [Slack CLI api](https://docs.slack.dev/tools/slack-cli/reference/commands/slack_api/),
[Slack replies](https://docs.slack.dev/reference/methods/conversations.replies/),
[Zulip Python client](https://zulip.com/api/configuring-python-bindings),
[Zulip messages](https://zulip.com/api/get-messages),
[launchd scheduling](https://developer.apple.com/library/archive/documentation/MacOSX/Conceptual/BPSystemStartup/Chapters/ScheduledJobs.html).
