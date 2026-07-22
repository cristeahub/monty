export const HEAD = "monty-head:v1";
export const TASK = "monty-task-link:v1";
export const PLAN = "monty-plan:v1";
export const RPC = "subagents:rpc:v1:request";
export const REPLY = "subagents:rpc:v1:reply:";

export function last(es, type) {
  return [...es].reverse().find(e => e.type === "custom" && e.customType === type)?.data;
}

export function find(ts, value) {
  const v = value.trim().toLowerCase();
  const hit = ts.filter(t => [t.key, t.id, t.title, t.worker?.id]
    .filter(Boolean).some(x => x.toLowerCase() === v));
  if (hit.length !== 1) throw new Error(hit.length ? `Multiple tasks match ${value}` : `No task matches ${value}`);
  return hit[0];
}

export function row(t) {
  const vals = [t.id, t.project, t.status.toUpperCase(), t.title, t.branch ?? "-"];
  const widths = [24, 16, 8, 42];
  return vals.map((v, i) => widths[i] ? (i === 3 ? v.slice(0, widths[i]) : v).padEnd(widths[i]) : v).join(" ");
}

const safe = [
  /^(cat|head|tail|less|more|grep|rg|find|fd|ls|pwd|tree|wc|sort|uniq|diff|file|stat|du|df|which|type|env|printenv|date)\b/,
  /^git\s+(status|log|diff|show|branch|remote|config\s+--get|ls-)\b/,
];
const bad = /(^|[;&|]\s*)(rm|mv|cp|mkdir|touch|chmod|chown|ln|tee|git\s+(add|commit|push|pull|merge|rebase|reset|checkout|stash)|sudo|kill)\b|(^|[^<])>(?!>)/;

export function safeCmd(cmd) {
  const v = cmd.trim();
  if (bad.test(v) || /[;&`]|\$\(|\|\|/.test(v)) return false;
  return v.split("|").every(part => safe.some(r => r.test(part.trim())));
}

export function msgText(msg) {
  if (!msg || msg.role !== "assistant" || !Array.isArray(msg.content)) return "";
  return msg.content.filter(x => x.type === "text").map(x => x.text).join("\n");
}

export function latestPlan(es) {
  for (let i = es.length - 1; i >= 0; i--) {
    if (es[i].type === "message") {
      const text = msgText(es[i].message);
      if (text) return text;
    }
  }
  return "";
}
