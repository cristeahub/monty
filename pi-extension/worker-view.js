import { closeSync, existsSync, openSync, readFileSync, readdirSync, readSync, realpathSync, statSync } from "node:fs";
import { dirname, join, resolve, sep } from "node:path";
import { matchesKey, truncateToWidth, visibleWidth, wrapTextWithAnsi } from "@earendil-works/pi-tui";
import { CHAIN_BACKEND, currentAgentRecord } from "./subagent-chain.js";

const REFRESH_MS = 500;
const MAX_SESSION_BYTES = 1024 * 1024;
const MAX_TRANSCRIPT_LINES = 500;

function safeStat(path) {
  try { return statSync(path); } catch { return undefined; }
}

function safeJson(path) {
  try {
    if (statSync(path).size > MAX_SESSION_BYTES) return;
    return JSON.parse(readFileSync(path, "utf8"));
  } catch { return undefined; }
}

function stateWorkerDir(home, task) {
  if (typeof task?.memory !== "string") return;
  let stateRoot;
  let memory;
  try {
    stateRoot = realpathSync(resolve(home, ".monty")) + sep;
    memory = existsSync(task.memory) ? realpathSync(task.memory) : resolve(task.memory);
  } catch { return; }
  if (!memory.startsWith(stateRoot)) return;
  return dirname(memory);
}

function newest(paths) {
  return paths.map(path => ({ path, modified: safeStat(path)?.mtimeMs ?? 0 }))
    .sort((a, b) => b.modified - a.modified)[0];
}

export function latestWorkerSession(home, task) {
  const worker = stateWorkerDir(home, task);
  if (!worker) return;
  const attemptsRoot = join(worker, "artifacts", "headless");
  let attempts;
  try {
    attempts = readdirSync(attemptsRoot, { withFileTypes: true })
      .filter(entry => entry.isDirectory())
      .map(entry => join(attemptsRoot, entry.name));
  } catch { return; }
  const attempt = newest(attempts)?.path;
  if (!attempt) return;
  const sessionsRoot = join(attempt, "sessions");
  let sessions;
  try {
    sessions = readdirSync(sessionsRoot, { withFileTypes: true })
      .filter(entry => entry.isDirectory() && /^run-\d+$/.test(entry.name))
      .map(entry => join(sessionsRoot, entry.name, "session.jsonl"))
      .filter(existsSync);
  } catch { return; }
  return newest(sessions)?.path;
}

function containedSession(root, path) {
  if (!root || typeof path !== "string" || !existsSync(path)) return false;
  try { return realpathSync(path).startsWith(realpathSync(root) + sep); }
  catch { return false; }
}

function stepSession(status, sessionsRoot) {
  const steps = Array.isArray(status?.steps) ? status.steps : [];
  const candidates = steps.map((step, index) => ({ step, index }))
    .filter(item => containedSession(sessionsRoot, item.step?.sessionFile));
  if (!candidates.length) return {};
  const running = candidates.filter(item => item.step.status === "running");
  const selected = newest((running.length ? running : candidates).map(item => item.step.sessionFile));
  const item = candidates.find(candidate => candidate.step.sessionFile === selected?.path) || candidates.at(-1);
  return { sessionFile: item?.step.sessionFile, step: item?.step, stepIndex: item?.index };
}

function runStatus(run) {
  if (typeof run?.asyncDir !== "string") return;
  const status = safeJson(join(run.asyncDir, "status.json"));
  if (typeof run.runId === "string" && status?.runId !== run.runId) return;
  return status;
}

function tintinSnapshot(run) {
  const ids = Array.isArray(run?.activeAgentIds) ? run.activeAgentIds : [];
  const record = currentAgentRecord(ids.at(-1));
  const artifacts = run?.artifacts && typeof run.artifacts === "object"
    ? Object.values(run.artifacts).filter(path => typeof path === "string" && existsSync(path)) : [];
  const artifact = newest(artifacts)?.path;
  return {
    runId: run?.attemptId,
    state: run?.state,
    phase: run?.phase,
    label: record?.description,
    agent: record?.type,
    stepState: record?.status || run?.state,
    toolCount: record?.toolUses,
    lastUpdate: run?.updatedAt,
    messages: record?.session?.messages,
    sessionFile: record?.outputFile,
    artifact,
  };
}

export function readWorkerSnapshot(home, task, run) {
  if (run?.backend === CHAIN_BACKEND) return tintinSnapshot(run);
  const status = runStatus(run);
  const worker = stateWorkerDir(home, task);
  const selected = stepSession(status, worker && join(worker, "artifacts", "headless"));
  const fallback = latestWorkerSession(home, task);
  const selectedModified = selected.sessionFile ? safeStat(selected.sessionFile)?.mtimeMs ?? 0 : 0;
  const fallbackModified = fallback ? safeStat(fallback)?.mtimeMs ?? 0 : 0;
  const sessionFile = fallbackModified > selectedModified ? fallback : selected.sessionFile || fallback;
  const step = sessionFile === selected.sessionFile ? selected.step : undefined;
  return {
    runId: typeof status?.runId === "string" ? status.runId : run?.runId,
    state: typeof status?.state === "string" ? status.state : undefined,
    phase: step?.phase,
    label: step?.label,
    agent: step?.agent,
    stepState: step?.status,
    currentTool: status?.currentTool,
    currentPath: status?.currentPath,
    turnCount: status?.turnCount,
    toolCount: status?.toolCount,
    lastUpdate: status?.lastUpdate ?? (sessionFile ? safeStat(sessionFile)?.mtimeMs : undefined),
    sessionFile,
  };
}

function textParts(content) {
  if (typeof content === "string") return content.split(/\r?\n/);
  if (!Array.isArray(content)) return [];
  return content.filter(part => part?.type === "text" && typeof part.text === "string")
    .map(part => part.text).join("\n").split(/\r?\n/);
}

function toolSummary(part) {
  const args = part?.arguments && typeof part.arguments === "object" ? part.arguments : {};
  const value = args.path ?? args.command ?? args.pattern ?? args.query;
  if (typeof value !== "string") return "";
  return value.replace(/\s+/g, " ").trim().slice(0, 180);
}

function appendLimited(lines, values, prefix = "  ", limit = 12) {
  const visible = values.filter(value => value.trim()).slice(0, limit);
  for (const value of visible) lines.push(prefix + value.slice(0, 500));
  if (values.filter(value => value.trim()).length > limit) lines.push(`${prefix}…`);
}

function readSessionTail(path) {
  let descriptor;
  try {
    const size = statSync(path).size;
    const length = Math.min(size, MAX_SESSION_BYTES);
    const buffer = Buffer.alloc(length);
    descriptor = openSync(path, "r");
    const bytes = readSync(descriptor, buffer, 0, length, size - length);
    return { buffer: buffer.subarray(0, bytes), truncated: size > length };
  } catch { return; }
  finally { if (descriptor !== undefined) closeSync(descriptor); }
}

function transcriptMessages(messages) {
  const lines = [];
  for (const message of messages) {
    if (!message) continue;
    if (message.role === "assistant" && Array.isArray(message.content)) {
      for (const part of message.content) {
        if (part?.type === "thinking" && typeof part.thinking === "string") {
          appendLimited(lines, part.thinking.split(/\r?\n/), "  ", 8);
        } else if (part?.type === "text" && typeof part.text === "string") {
          appendLimited(lines, part.text.split(/\r?\n/), "", 30);
        } else if (part?.type === "toolCall") {
          const summary = toolSummary(part);
          lines.push(`› ${part.name || "tool"}${summary ? `  ${summary}` : ""}`);
        }
      }
    } else if (message.role === "toolResult") {
      lines.push(`${message.isError ? "✗" : "←"} ${message.toolName || "tool"}`);
      appendLimited(lines, textParts(message.content));
    }
  }
  return lines.slice(-MAX_TRANSCRIPT_LINES);
}

export function sessionTranscript(path) {
  if (!path || !existsSync(path)) return [];
  const tail = readSessionTail(path);
  if (!tail) return [];
  let text = tail.buffer.toString("utf8");
  if (tail.truncated) text = text.slice(Math.max(0, text.indexOf("\n") + 1));
  const messages = [];
  for (const line of text.split(/\r?\n/)) {
    if (!line.trim()) continue;
    try {
      const entry = JSON.parse(line);
      if (entry?.message && (entry.type === "message" || ["assistant", "user", "toolResult"].includes(entry.type)))
        messages.push(entry.message);
    } catch {}
  }
  return transcriptMessages(messages);
}

function artifactTranscript(path) {
  if (!path || !existsSync(path)) return [];
  const tail = readSessionTail(path);
  if (!tail) return [];
  return tail.buffer.toString("utf8").split(/\r?\n/).slice(-MAX_TRANSCRIPT_LINES);
}

function relativeAge(timestamp) {
  if (!Number.isFinite(timestamp)) return "";
  const seconds = Math.max(0, Math.floor((Date.now() - timestamp) / 1000));
  if (seconds < 2) return "now";
  if (seconds < 60) return `${seconds}s ago`;
  return `${Math.floor(seconds / 60)}m ago`;
}

export function workerWidgetLines(snapshot) {
  const phase = snapshot.phase || snapshot.label || snapshot.agent || "Worker";
  const state = snapshot.stepState || snapshot.state || (snapshot.sessionFile ? "transcript available" : "waiting for transcript");
  const facts = [snapshot.agent, snapshot.currentTool,
    snapshot.turnCount === undefined ? undefined : `${snapshot.turnCount} turns`,
    snapshot.toolCount === undefined ? undefined : `${snapshot.toolCount} tools`,
    relativeAge(snapshot.lastUpdate)].filter(Boolean);
  return [
    `MONTY WORKER · ${phase} · ${state}`,
    facts.length ? facts.join(" · ") : "The worker has not produced a conversation yet.",
    "Run /monty-worker or use TintinWeb FleetView for the live conversation.",
  ];
}

function fit(text, width) {
  const clipped = truncateToWidth(text, Math.max(0, width), "");
  return clipped + " ".repeat(Math.max(0, width - visibleWidth(clipped)));
}

class WorkerView {
  constructor(tui, theme, home, task, run, done) {
    this.tui = tui;
    this.theme = theme;
    this.home = home;
    this.task = task;
    this.run = run;
    this.done = done;
    this.scroll = 0;
    this.follow = true;
    this.bodyHeight = 12;
    this.snapshot = {};
    this.lines = [];
    this.transcriptVersion = "";
    this.refresh();
    this.timer = setInterval(() => {
      this.refresh();
      this.tui.requestRender();
    }, REFRESH_MS);
    this.timer.unref?.();
  }

  refresh() {
    this.snapshot = readWorkerSnapshot(this.home, this.task, this.run);
    const stat = this.snapshot.sessionFile && safeStat(this.snapshot.sessionFile);
    const artifactStat = this.snapshot.artifact && safeStat(this.snapshot.artifact);
    const messages = Array.isArray(this.snapshot.messages) ? this.snapshot.messages : undefined;
    const version = messages ? `messages:${messages.length}:${this.snapshot.stepState}`
      : stat ? `${this.snapshot.sessionFile}:${stat.size}:${stat.mtimeMs}`
        : artifactStat ? `${this.snapshot.artifact}:${artifactStat.size}:${artifactStat.mtimeMs}` : "";
    if (version === this.transcriptVersion) return;
    this.transcriptVersion = version;
    this.lines = messages ? transcriptMessages(messages)
      : stat ? sessionTranscript(this.snapshot.sessionFile) : artifactTranscript(this.snapshot.artifact);
  }

  handleInput(data) {
    if (matchesKey(data, "escape") || matchesKey(data, "ctrl+c") || data.toLowerCase() === "q") {
      this.done(undefined);
      return;
    }
    if (matchesKey(data, "pageUp") || matchesKey(data, "up") || data.toLowerCase() === "k") {
      this.follow = false;
      this.scroll = Math.max(0, this.scroll - (matchesKey(data, "pageUp") ? this.bodyHeight : 1));
    } else if (matchesKey(data, "pageDown") || matchesKey(data, "down") || data.toLowerCase() === "j") {
      this.scroll += matchesKey(data, "pageDown") ? this.bodyHeight : 1;
    } else if (matchesKey(data, "end")) {
      this.follow = true;
    } else if (data.toLowerCase() === "r") {
      this.refresh();
    }
    this.tui.requestRender();
  }

  render(width) {
    const inner = Math.max(1, width - 2);
    const rows = this.tui.terminal?.rows ?? 32;
    this.bodyHeight = Math.max(5, Math.min(32, Math.floor(rows * 0.82) - 6));
    const phase = this.snapshot.phase || this.snapshot.label || this.snapshot.agent || "Worker";
    const state = this.snapshot.stepState || this.snapshot.state
      || (this.snapshot.sessionFile ? "transcript available" : "waiting");
    const title = ` Monty worker · ${this.task.title} `;
    const detail = [phase, state, this.snapshot.currentTool, this.snapshot.currentPath].filter(Boolean).join(" · ");
    const raw = this.lines.length ? this.lines : ["Waiting for the worker conversation…"];
    const wrapped = [];
    for (const line of raw) {
      const parts = wrapTextWithAnsi(line, inner);
      wrapped.push(...(parts.length ? parts : [""]));
    }
    const maxScroll = Math.max(0, wrapped.length - this.bodyHeight);
    if (this.follow) this.scroll = maxScroll;
    else this.scroll = Math.max(0, Math.min(this.scroll, maxScroll));
    if (this.scroll >= maxScroll) this.follow = true;
    const visible = wrapped.slice(this.scroll, this.scroll + this.bodyHeight);
    const lines = [this.theme.fg("border", `╭${"─".repeat(inner)}╮`)];
    lines.push(this.theme.fg("border", "│") + fit(this.theme.fg("accent", this.theme.bold(title)), inner) + this.theme.fg("border", "│"));
    lines.push(this.theme.fg("border", "│") + fit(this.theme.fg("dim", ` ${detail}`), inner) + this.theme.fg("border", "│"));
    lines.push(this.theme.fg("border", `├${"─".repeat(inner)}┤`));
    for (let index = 0; index < this.bodyHeight; index++)
      lines.push(this.theme.fg("border", "│") + fit(visible[index] || "", inner) + this.theme.fg("border", "│"));
    lines.push(this.theme.fg("border", `├${"─".repeat(inner)}┤`));
    lines.push(this.theme.fg("border", "│") + fit(this.theme.fg("dim", " PgUp/PgDn scroll · End follow · r refresh · Esc return to head butler"), inner) + this.theme.fg("border", "│"));
    lines.push(this.theme.fg("border", `╰${"─".repeat(inner)}╯`));
    return lines.map(line => truncateToWidth(line, width, ""));
  }

  invalidate() { this.refresh(); }
  dispose() { clearInterval(this.timer); }
}

export function hasWorkerTranscript(home, task, run) {
  const snapshot = readWorkerSnapshot(home, task, run);
  return !!(run?.asyncDir || snapshot.sessionFile || snapshot.messages || snapshot.artifact
    || run?.backend === CHAIN_BACKEND);
}

export async function openWorkerView(ctx, home, task, run) {
  if (ctx.mode !== "tui") return;
  await ctx.ui.custom((tui, theme, _keybindings, done) =>
    new WorkerView(tui, theme, home, task, run, done), {
      overlay: true,
      overlayOptions: { anchor: "center", width: "96%", minWidth: 60, maxHeight: "92%", margin: 1 },
    });
}
