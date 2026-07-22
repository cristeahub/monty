export const HEAD = "monty-head:v1";
export const TASK = "monty-task-link:v1";
export const RETIRED = "monty-task-retired:v1";
export const PLAN = "monty-plan:v1";
export const RPC = "subagents:rpc:v1:request";
export const REPLY = "subagents:rpc:v1:reply:";

export const REQUIRED_AGENTS = {
  "monty-headless-worker": `---
name: monty-headless-worker
description: Implementation and fixer agent for Monty headless worker chains
tools: read, grep, find, ls, bash, edit, write
model: inherit
thinking: high
systemPromptMode: replace
inheritProjectContext: true
inheritSkills: false
defaultContext: fresh
---

You are the implementation agent in a Monty headless worker chain.

Read the supplied Monty instructions and task context before inspecting the worktree.
Follow the assigned phase exactly.
For an implementation phase, make the requested changes and run focused validation.
For a fix phase, verify both review reports against the worktree, fix every valid finding, rerun affected validation, and record the final handoff in durable worker memory.

Never create, switch, or remove worktrees.
Never stage, commit, push, submit reviews, post comments, open pull requests, or run \`monty done\`.
Do not launch subagents or invoke \`/review\`.
`,
  "monty-headless-reviewer": `---
name: monty-headless-reviewer
description: Independent read-only reviewer for Monty headless worker chains
tools: read, grep, find, ls, bash
model: inherit
thinking: high
systemPromptMode: replace
inheritProjectContext: true
inheritSkills: false
defaultContext: fresh
---

You are an independent, strictly read-only code reviewer in a Monty headless worker chain.

Inspect the supplied task context and the actual worktree.
Verify requirements, implementation behavior, tests, failure handling, and repository conventions from primary evidence.
Do not trust another agent's claims without checking them.
Report only concrete findings that warrant a change.
Cite file paths and line numbers, explain impact, and propose the smallest safe correction.
State \`No findings\` plainly when no correction is warranted.

The chain captures your final response at the explicit review-report output path outside the worktree.
Never modify any file.
Never use shell commands that mutate files, Git state, dependencies, services, or remote systems.
Never stage, commit, push, submit reviews, post comments, open pull requests, or run \`monty done\`.
Do not create, switch, or remove worktrees.
Do not launch subagents.
`,
};

export function last(es, type) {
  return [...es].reverse().find(e => e.type === "custom" && e.customType === type)?.data;
}

export function find(ts, value) {
  const v = value.trim().toLowerCase();
  const stable = ts.filter(t => [t.key, t.id, t.worker?.id]
    .filter(Boolean).some(x => x.toLowerCase() === v));
  const hit = stable.length ? stable : ts.filter(t => t.title?.toLowerCase() === v);
  if (hit.length !== 1) throw new Error(hit.length ? `Multiple tasks match ${value}` : `No task matches ${value}`);
  return hit[0];
}

export function definiteClaimFailure(action, worker, message) {
  const command = action === "resume" ? "/monty-run resume" : "/monty-run";
  return `${message}. Worker ${worker} was not claimed; the same ${command} command can be retried.`;
}

export function ambiguousClaimFailure(action, worker, message) {
  return `${message}. The ${action} request may have been accepted for worker ${worker}. Do not retry automatically; use /monty-run resume only when a successor run is explicitly intentional.`;
}

export function row(t) {
  const vals = [t.id, t.project, t.status.toUpperCase(), t.title, t.branch ?? "-"];
  const widths = [24, 16, 8, 42];
  return vals.map((v, i) => widths[i] ? (i === 3 ? v.slice(0, widths[i]) : v).padEnd(widths[i]) : v).join(" ");
}

export function msgText(msg) {
  if (!msg || msg.role !== "assistant" || !Array.isArray(msg.content)) return "";
  return msg.content.filter(x => x.type === "text").map(x => x.text).join("\n");
}

export function latestPlan(es, markerType, accept = () => true) {
  let marker = -1;
  for (let i = es.length - 1; i >= 0; i--) {
    if (es[i].type === "custom" && es[i].customType === markerType && accept(es[i].data)) {
      if (!es[i].data?.enabled) return "";
      marker = i;
      break;
    }
  }
  if (marker < 0) return "";
  for (let i = es.length - 1; i > marker; i--) {
    if (es[i].type !== "message") continue;
    const text = msgText(es[i].message);
    if (/(^|\n)Plan:\s*(\n|$)/i.test(text)) return text;
  }
  return "";
}
