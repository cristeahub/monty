import { existsSync, mkdtempSync, readFileSync, realpathSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";
import { randomUUID } from "node:crypto";
import { SessionManager } from "@earendil-works/pi-coding-agent";
import { HEAD, PLAN, REPLY, RPC, TASK, find, last, latestPlan, msgText, row, safeCmd } from "../core.js";

const rawHome = process.env.MONTY_HOME?.trim();
const home = rawHome ? resolve(rawHome) : null;
const cmd = process.env.MONTY_COMMAND?.trim() || "monty";
const wt = process.env.MONTY_WT_COMMAND?.trim() || "wt";
const piCmd = process.env.MONTY_PI_COMMAND?.trim() || "pi";
const prefix = process.env.MONTY_BRANCH_PREFIX?.trim() || "monty";
const planTools = new Set(["read", "bash", "grep", "find", "ls", "questionnaire"]);

function same(a, b) {
  try { return realpathSync(a) === realpathSync(b); } catch { return resolve(a) === resolve(b); }
}

function err(r) {
  return (r.stderr || r.stdout || `Command exited ${r.code}`).trim();
}

export default function monty(pi) {
  let plan;
  let busy = false;

  async function cli(args, timeout = 120000) {
    if (!home) throw new Error("MONTY_HOME is not set");
    const r = await pi.exec(cmd, args, { cwd: home, timeout });
    if (r.code) throw new Error(err(r));
    try { return JSON.parse(r.stdout); }
    catch { throw new Error(`Monty returned invalid JSON: ${r.stdout.trim()}`); }
  }

  function rpc(method, params, timeout = 5000) {
    return new Promise((ok, no) => {
      const requestId = `monty-${randomUUID()}`;
      const event = REPLY + requestId;
      let off;
      const timer = setTimeout(() => {
        if (typeof off === "function") off();
        no(new Error(`pi-subagents ${method} RPC timed out`));
      }, timeout);
      off = pi.events.on(event, reply => {
        clearTimeout(timer);
        if (typeof off === "function") off();
        if (reply?.success) ok(reply.data);
        else no(new Error(reply?.error?.message || `pi-subagents ${method} RPC failed`));
      });
      pi.events.emit(RPC, { version: 1, requestId, method, params, source: { extension: "monty" } });
    });
  }

  function cur(ctx, type) {
    return last(ctx.sessionManager.getEntries(), type);
  }

  function loc(ctx) {
    const task = cur(ctx, TASK);
    const text = task ? `MONTY · TASK: ${task.title}`
      : plan ? `MONTY · HEAD BUTLER · Planning: ${plan.title}`
      : same(ctx.cwd, home || ctx.cwd) ? "MONTY · HEAD BUTLER" : undefined;
    ctx.ui.setStatus("monty", text ? ctx.ui.theme.fg("accent", text) : undefined);
    if (text) ctx.ui.setTitle(text);
  }

  function usePlan(ctx) {
    if (!plan) return;
    pi.setActiveTools((plan.tools || pi.getActiveTools()).filter(x => planTools.has(x)));
    loc(ctx);
  }

  function clearPlan(ctx) {
    if (!plan) return;
    pi.setActiveTools(plan.tools || pi.getActiveTools());
    plan = undefined;
    pi.appendEntry(PLAN, { enabled: false });
    loc(ctx);
  }

  async function snapshot() {
    return cli(["tasks", "list", "--json", "--no-sync", "--home", home]);
  }

  function headLink(ctx) {
    const task = cur(ctx, TASK);
    const file = ctx.sessionManager.getSessionFile();
    if (task?.head) return task.head;
    if (cur(ctx, HEAD)?.home === home && file) return file;
  }

  async function heads() {
    const out = [];
    for (const info of await SessionManager.listAll()) {
      if (info.name !== "Monty Head Butler") continue;
      try {
        const sm = SessionManager.open(info.path);
        if (last(sm.getEntries(), HEAD)?.home === home) out.push(info);
      } catch {}
    }
    return out.sort((a, b) => b.modified - a.modified);
  }

  async function getHead(ctx) {
    const direct = headLink(ctx);
    if (direct && existsSync(direct)) return direct;
    return (await heads())[0]?.path;
  }

  function link(entry, head) {
    return {
      home, head, key: entry.task.key, title: entry.task.title,
      worker: entry.worker.id, cwd: entry.cwd, instructions: entry.instructions,
      context: entry.context, memory: entry.memory,
    };
  }

  function context(entry) {
    return ["[MONTY TASK]", readFileSync(entry.instructions, "utf8"),
      readFileSync(entry.context, "utf8")].join("\n\n");
  }

  async function taskSessions(key) {
    const out = [];
    for (const info of await SessionManager.listAll()) {
      if (!info.name?.startsWith("Monty: ")) continue;
      try {
        const sm = SessionManager.open(info.path);
        const data = last(sm.getEntries(), TASK);
        if (data?.home === home && data.key === key) out.push({ info, data });
      } catch {}
    }
    return out.sort((a, b) => b.info.modified - a.info.modified);
  }

  async function pickSession(items, ctx) {
    if (items.length < 2) return items[0];
    const opts = items.map(x => `${x.info.modified.toISOString()}  ${x.info.path}`);
    const value = await ctx.ui.select("Choose task subsession", opts);
    return items[opts.indexOf(value)];
  }

  async function ensureSession(entry, ctx) {
    const head = await getHead(ctx);
    if (!head) throw new Error("The persisted Monty head-butler session could not be found");
    const old = await pickSession(await taskSessions(entry.task.key), ctx);
    let sm;
    if (!old) sm = SessionManager.create(entry.cwd, undefined, { parentSession: head });
    else if (!same(old.data.cwd, entry.cwd))
      sm = SessionManager.forkFrom(old.info.path, entry.cwd, undefined, { parentSession: head });
    else sm = SessionManager.open(old.info.path);
    sm.appendCustomEntry(TASK, link(entry, head));
    sm.appendSessionInfo(`Monty: ${entry.task.title}`);
    if (!old || !same(old.data.cwd, entry.cwd))
      sm.appendCustomMessageEntry("monty-task-context:v1", context(entry), false);
    return sm.getSessionFile();
  }

  async function enter(task, ctx) {
    if (task.action === "plan") return startPlan(task, ctx);
    if (task.action !== "open") throw new Error(`Task ${task.id} is ${task.action}`);
    const entry = await cli(["task", "enter", task.key, "--json", "--home", home, "--wt-command", wt]);
    const file = await ensureSession(entry, ctx);
    if (!file) throw new Error("Pi did not persist the task subsession");
    await ctx.waitForIdle();
    await ctx.switchSession(file);
  }

  async function choose(ctx) {
    const data = await snapshot();
    if (!data.tasks.length) return ctx.ui.notify("Monty has no open tasks", "info");
    const opts = data.tasks.map(row);
    const value = await ctx.ui.select("ID                 Project          Status   Title                                      Branch", opts);
    if (value) await enter(data.tasks[opts.indexOf(value)], ctx);
  }

  function planPrompt(task) {
    return `Plan Monty task ${task.id}: ${task.title}. Inspect the project and produce a concrete numbered Plan: section. Do not implement it yet.`;
  }

  async function startPlan(task, ctx) {
    const state = { enabled: true, key: task.key, title: task.title, tools: pi.getActiveTools() };
    const head = await getHead(ctx);
    if (!head) throw new Error("Start Monty from its head-butler session before planning a task");
    if (!same(ctx.cwd, home)) {
      const sm = SessionManager.open(head);
      sm.appendCustomEntry(PLAN, state);
      await ctx.waitForIdle();
      return ctx.switchSession(head, { withSession: next => next.sendUserMessage(planPrompt(task)) });
    }
    plan = state;
    pi.appendEntry(PLAN, state);
    usePlan(ctx);
    pi.sendUserMessage(planPrompt(task));
  }

  async function dispatch(worker, resume = false) {
    await rpc("ping");
    const action = resume ? "resume" : "begin";
    const data = await cli(["headless", action, worker, "--home", home,
      "--wt-command", wt, "--pi-command", piCmd, "--branch-prefix", prefix]);
    try { return await rpc("spawn", data.harness_call.arguments, 15000); }
    catch (e) {
      throw new Error(`${e.message}. Worker ${worker} is launch-requested; use /monty-run resume only if a successor run is intentional.`);
    }
  }

  async function startTask(ctx, text) {
    if (!plan) throw new Error("No Monty task is being planned");
    await rpc("ping");
    const dir = mkdtempSync(join(tmpdir(), "monty-plan-"));
    const file = join(dir, "plan.md");
    try {
      writeFileSync(file, text.trim() + "\n", { mode: 0o600 });
      const entry = await cli(["task", "prepare", plan.key, "--plan", file, "--json",
        "--home", home, "--wt-command", wt, "--pi-command", piCmd,
        "--branch-prefix", prefix]);
      await ensureSession(entry, ctx);
      if (entry.worker.status === "prepared") await dispatch(entry.worker.id);
      clearPlan(ctx);
      const suffix = entry.worker.status === "prepared" ? "Agents are running." : `Worker is ${entry.worker.status}.`;
      ctx.ui.notify(`Started ${entry.task.title}. ${suffix}`, "info");
    } finally { rmSync(dir, { recursive: true, force: true }); }
  }

  async function openArg(value, ctx) {
    const data = await snapshot();
    return enter(find(data.tasks, value), ctx);
  }

  pi.registerCommand("monty", { description: "Choose a Monty task", handler: async (_, ctx) => {
    try { await choose(ctx); } catch (e) { ctx.ui.notify(e.message, "error"); }
  }});
  pi.registerCommand("monty-open", { description: "Open or plan a Monty task", handler: async (args, ctx) => {
    try { args.trim() ? await openArg(args, ctx) : await choose(ctx); }
    catch (e) { ctx.ui.notify(e.message, "error"); }
  }});
  pi.registerCommand("monty-back", { description: "Return to the Monty head butler", handler: async (_, ctx) => {
    try {
      const head = await getHead(ctx);
      if (!head) throw new Error("The Monty head-butler session could not be found");
      await ctx.waitForIdle();
      await ctx.switchSession(head);
    } catch (e) { ctx.ui.notify(e.message, "error"); }
  }});
  pi.registerCommand("monty-start", { description: "Start the task currently being planned", handler: async (_, ctx) => {
    try {
      const text = latestPlan(ctx.sessionManager.getEntries());
      if (!text) throw new Error("No completed plan was found");
      await startTask(ctx, text);
    } catch (e) { ctx.ui.notify(e.message, "error"); }
  }});
  pi.registerCommand("monty-run", { description: "Run this task's asynchronous Monty chain", handler: async (args, ctx) => {
    try {
      const task = cur(ctx, TASK);
      if (!task) throw new Error("This is not a Monty task subsession");
      await dispatch(task.worker, args.trim() === "resume");
      ctx.ui.notify(`Started agents for ${task.title}`, "info");
    } catch (e) { ctx.ui.notify(e.message, "error"); }
  }});
  pi.registerCommand("monty-plan-cancel", { description: "Leave Monty plan mode", handler: async (_, ctx) => {
    clearPlan(ctx);
  }});

  pi.on("session_start", (_, ctx) => {
    if (!home) return;
    const task = cur(ctx, TASK);
    const saved = cur(ctx, PLAN);
    plan = saved?.enabled ? saved : undefined;
    if (task?.home === home) pi.setSessionName(`Monty: ${task.title}`);
    else if (same(ctx.cwd, home)) {
      if (cur(ctx, HEAD)?.home !== home) pi.appendEntry(HEAD, { home });
      pi.setSessionName("Monty Head Butler");
    }
    usePlan(ctx);
    loc(ctx);
  });

  pi.on("before_agent_start", () => plan ? {
    message: { customType: "monty-plan-context:v1", display: false,
      content: `[MONTY PLAN MODE]\nPlan task ${plan.key}: ${plan.title}. Explore read-only and return a numbered Plan: section. Do not implement or mutate local or remote state.` }
  } : undefined);

  pi.on("tool_call", event => {
    if (plan && event.toolName === "bash" && !safeCmd(event.input.command))
      return { block: true, reason: `Monty plan mode blocked a non-read-only command: ${event.input.command}` };
  });

  pi.on("agent_end", async (event, ctx) => {
    if (!plan || busy || !ctx.hasUI) return;
    const text = msgText([...event.messages].reverse().find(x => x.role === "assistant"));
    if (!/\bPlan:\s*\n/i.test(text)) return;
    busy = true;
    try {
      const choice = await ctx.ui.select(`Plan ready for ${plan.title}`, ["Start task", "Refine plan", "Stay in plan mode"]);
      if (choice === "Start task") await startTask(ctx, text);
      else if (choice === "Refine plan") {
        const note = await ctx.ui.editor("Refine the plan", "");
        if (note?.trim()) pi.sendUserMessage(note.trim());
      }
    } catch (e) { ctx.ui.notify(e.message, "error"); }
    finally { busy = false; }
  });
}
