import { existsSync, mkdtempSync, readFileSync, realpathSync, rmSync, statSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";
import { randomUUID } from "node:crypto";
import { SessionManager } from "@earendil-works/pi-coding-agent";
import { HEAD, PLAN, REPLY, REQUIRED_AGENTS, RETIRED, RPC, TASK, ambiguousClaimFailure, definiteClaimFailure, find, last, latestPlan, msgText, row } from "../core.js";

const rawHome = process.env.MONTY_HOME?.trim();
const home = rawHome ? realpathSync(resolve(rawHome)) : null;
const cmd = process.env.MONTY_COMMAND?.trim() || "monty";
const wt = process.env.MONTY_WT_COMMAND?.trim() || "wt";
const piCmd = process.env.MONTY_PI_COMMAND?.trim() || "pi";
const prefix = process.env.MONTY_BRANCH_PREFIX?.trim() || "monty";
const planTools = new Set(["read", "grep", "find", "ls", "questionnaire"]);

const requiredAgents = REQUIRED_AGENTS;

class CliRejected extends Error {}

function same(a, b) {
  try { return realpathSync(a) === realpathSync(b); } catch { return resolve(a) === resolve(b); }
}

function markerHome(data) {
  return typeof data === "string" ? data : data?.home;
}

function owned(data) {
  const value = markerHome(data);
  if (!home || typeof value !== "string") return false;
  try { return realpathSync(resolve(value)) === home; } catch { return false; }
}

function lastOwned(entries, type) {
  return [...entries].reverse()
    .find(entry => entry.type === "custom" && entry.customType === type && owned(entry.data))?.data;
}

function err(r) {
  return (r.stderr || r.stdout || `Command exited ${r.code}`).trim();
}

const emptyUsage = {
  input: 0, output: 0, cacheRead: 0, cacheWrite: 0, totalTokens: 0,
  cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, total: 0 },
};

function materialize(sm, model) {
  const file = sm.getSessionFile();
  if (!file) throw new Error("Pi did not assign a session file");
  if (!existsSync(file)) {
    if (!model) throw new Error("Pi has no selected model to materialize the session safely");
    sm.appendMessage({
      role: "assistant", content: [], api: model.api, provider: model.provider, model: model.id,
      usage: emptyUsage, stopReason: "stop", timestamp: Date.now(),
    });
  }
  if (!existsSync(file)) throw new Error("Pi did not persist the session");
  return file;
}

export default function monty(pi) {
  let plan;
  let busy = false;

  async function cli(args, timeout = 120000) {
    if (!home) throw new Error("MONTY_HOME is not set");
    const r = await pi.exec(cmd, args, { cwd: home, timeout });
    if (r.killed) throw new Error(`Monty execution was killed or timed out: ${err(r)}`);
    if (r.code) throw new CliRejected(err(r));
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
    return last(ctx.sessionManager.getBranch(), type);
  }

  function isRetired(sm, key) {
    return sm.getEntries().some(entry => entry.type === "custom"
      && entry.customType === RETIRED && owned(entry.data) && entry.data?.key === key);
  }

  function activeTask(ctx) {
    const task = cur(ctx, TASK);
    return task && typeof task === "object" && owned(task) && !task.retired
      && !isRetired(ctx.sessionManager, task.key) ? task : undefined;
  }

  function loc(ctx) {
    const task = activeTask(ctx);
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
    pi.appendEntry(PLAN, { home, enabled: false });
    loc(ctx);
  }

  async function snapshot() {
    return cli(["tasks", "list", "--json", "--no-sync", "--home", home]);
  }

  function headLink(ctx) {
    const entries = ctx.sessionManager.getEntries();
    const task = lastOwned(entries, TASK);
    const file = ctx.sessionManager.getSessionFile();
    if (task?.head) return task.head;
    if (lastOwned(entries, HEAD) && file) return file;
  }

  function sessionCwd(sm, info) {
    return sm.getHeader()?.cwd || info?.cwd || sm.getCwd();
  }

  function validHead(path) {
    try {
      const sm = SessionManager.open(path);
      return !!lastOwned(sm.getEntries(), HEAD) && same(sm.getHeader()?.cwd, home);
    } catch { return false; }
  }

  async function heads() {
    const out = [];
    for (const info of await SessionManager.listAll())
      if (validHead(info.path)) out.push(info);
    return out.sort((a, b) => b.modified - a.modified);
  }

  async function getHead(ctx) {
    const direct = headLink(ctx);
    if (direct && existsSync(direct) && validHead(direct)) return direct;
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
      try {
        const sm = SessionManager.open(info.path);
        const data = last(sm.getBranch(), TASK);
        if (data && owned(data) && !data.retired && data.key === key && !isRetired(sm, key))
          out.push({ info, data, cwd: sessionCwd(sm, info) });
      } catch {}
    }
    return out.sort((a, b) => b.info.modified - a.info.modified);
  }

  async function pickSession(items, ctx) {
    if (items.length < 2) return { item: items[0] };
    const opts = items.map(x => `${x.info.modified.toISOString()}  ${x.info.path}`);
    const value = await ctx.ui.select("Choose task subsession", opts);
    if (!value) return { cancelled: true };
    return { item: items[opts.indexOf(value)] };
  }

  async function ensureSession(entry, ctx) {
    const head = await getHead(ctx);
    if (!head) throw new Error("The persisted Monty head-butler session could not be found");
    const picked = await pickSession(await taskSessions(entry.task.key), ctx);
    if (picked.cancelled) return;
    const old = picked.item;
    const moved = old && !same(old.cwd, entry.cwd);
    let sm;
    if (!old) sm = SessionManager.create(entry.cwd, undefined, { parentSession: head });
    else if (moved) sm = SessionManager.forkFrom(old.info.path, entry.cwd, undefined, { parentSession: head });
    else sm = SessionManager.open(old.info.path);
    sm.appendCustomEntry(TASK, link(entry, head));
    if (!sm.getSessionName()) sm.appendSessionInfo(`Monty: ${entry.task.title}`);
    if (!old || moved) sm.appendCustomMessageEntry("monty-task-context:v1", context(entry), false);
    const file = materialize(sm, ctx.model);
    if (moved) SessionManager.open(old.info.path).appendCustomEntry(RETIRED, {
      home, key: old.data.key, successor: file,
    });
    return file;
  }

  async function enter(task, ctx) {
    if (task.action === "plan") return startPlan(task, ctx);
    if (task.action !== "open") throw new Error(`Task ${task.id} is ${task.action}`);
    const entry = await cli(["task", "enter", task.key, "--json", "--home", home, "--wt-command", wt]);
    const file = await ensureSession(entry, ctx);
    if (!file) return;
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

  function enablePlan(task, ctx) {
    plan = {
      home, enabled: true, key: task.key, title: task.title,
      tools: plan?.tools || pi.getActiveTools(),
    };
    pi.appendEntry(PLAN, plan);
    usePlan(ctx);
    pi.sendUserMessage(planPrompt(task));
  }

  async function startPlan(task, ctx) {
    const head = await getHead(ctx);
    if (!head) throw new Error("Start Monty from its head-butler session before planning a task");
    const current = ctx.sessionManager.getSessionFile();
    if (!current || !same(current, head)) {
      const marker = { home, enabled: true, key: task.key, title: task.title };
      SessionManager.open(head).appendCustomEntry(PLAN, marker);
      await ctx.waitForIdle();
      const result = await ctx.switchSession(head, {
        withSession: next => next.sendUserMessage(planPrompt(task)),
      });
      if (result.cancelled) SessionManager.open(head).appendCustomEntry(PLAN, { home, enabled: false });
      return;
    }
    enablePlan(task, ctx);
  }

  function validateAgent(name) {
    const path = join(home, ".pi", "agents", `${name}.md`);
    try {
      if (!statSync(path).isFile()) throw new Error("not a regular file");
      const actual = readFileSync(path, "utf8").replaceAll("\r\n", "\n").trimEnd();
      if (actual !== requiredAgents[name].trimEnd())
        throw new Error("definition does not match Monty's complete required fixed definition");
    } catch (error) {
      throw new Error(`Required project agent ${name} is invalid at ${path}: ${error.message}`);
    }
  }

  function overrideArray(value, name, field, path) {
    if (value === undefined || value === false) return;
    if (!Array.isArray(value) || value.some(item => typeof item !== "string"))
      throw new Error(`Invalid pi-subagents override ${name}.${field} at ${path}: expected strings or false`);
  }

  function parseOverride(name, value, path) {
    if (!value || typeof value !== "object" || Array.isArray(value))
      throw new Error(`Invalid pi-subagents override for ${name} at ${path}: expected an object`);
    const parsed = {};
    const oneOf = (field, values) => {
      if (!(field in value)) return;
      if (!values.includes(value[field]))
        throw new Error(`Invalid pi-subagents override ${name}.${field} at ${path}`);
      parsed[field] = value[field];
    };
    const typed = (field, type, alsoFalse = false) => {
      if (!(field in value)) return;
      if (typeof value[field] !== type && !(alsoFalse && value[field] === false))
        throw new Error(`Invalid pi-subagents override ${name}.${field} at ${path}`);
      parsed[field] = value[field];
    };
    typed("model", "string", true);
    overrideArray(value.fallbackModels, name, "fallbackModels", path);
    if (value.fallbackModels !== undefined) parsed.fallbackModels = value.fallbackModels;
    typed("thinking", "string", true);
    oneOf("systemPromptMode", ["append", "replace"]);
    typed("inheritProjectContext", "boolean");
    typed("inheritSkills", "boolean");
    oneOf("defaultContext", ["fresh", "fork", false]);
    oneOf("acceptanceRole", ["read-only", "writer", false]);
    typed("disabled", "boolean");
    typed("completionGuard", "boolean");
    if ("toolBudget" in value) {
      if (value.toolBudget !== false
          && (!value.toolBudget || typeof value.toolBudget !== "object" || Array.isArray(value.toolBudget)))
        throw new Error(`Invalid pi-subagents override ${name}.toolBudget at ${path}`);
      parsed.toolBudget = value.toolBudget;
    }
    typed("systemPrompt", "string");
    for (const field of ["skills", "tools", "subagentOnlyExtensions"]) {
      overrideArray(value[field], name, field, path);
      if (value[field] !== undefined) parsed[field] = value[field];
    }
    return Object.keys(parsed).length ? parsed : undefined;
  }

  function parseModelScope(value, path) {
    if (value === undefined) return;
    if (!value || typeof value !== "object" || Array.isArray(value))
      throw new Error(`Invalid pi-subagents modelScope at ${path}: expected an object`);
    if (value.enforce !== undefined && typeof value.enforce !== "boolean")
      throw new Error(`Invalid pi-subagents modelScope.enforce at ${path}: expected a boolean`);
    if (value.allow !== undefined
        && (!Array.isArray(value.allow) || value.allow.some(item => typeof item !== "string")
          || !value.allow.some(item => item.trim())))
      throw new Error(`Invalid pi-subagents modelScope.allow at ${path}: expected an array of strings containing at least one non-empty pattern`);
    if (value.enforce === true && value.allow === undefined)
      throw new Error(`Invalid pi-subagents modelScope at ${path}: enforce requires allow`);
  }

  function agentSettings(path) {
    if (!existsSync(path)) return { overrides: {} };
    let settings;
    try { settings = JSON.parse(readFileSync(path, "utf8")); }
    catch (error) { throw new Error(`Invalid pi-subagents settings at ${path}: ${error.message}`); }
    if (!settings || typeof settings !== "object" || Array.isArray(settings))
      throw new Error(`Invalid pi-subagents settings at ${path}: expected a JSON object`);
    const subagents = settings.subagents;
    if (!subagents || typeof subagents !== "object" || Array.isArray(subagents))
      return { overrides: {} };
    for (const field of ["disableBuiltins", "disableThinking"])
      if (subagents[field] !== undefined && typeof subagents[field] !== "boolean")
        throw new Error(`Invalid pi-subagents ${field} at ${path}: expected a boolean`);
    if (subagents.defaultModel !== undefined
        && (typeof subagents.defaultModel !== "string" || !subagents.defaultModel.trim()))
      throw new Error(`Invalid pi-subagents defaultModel at ${path}: expected a non-empty string`);
    parseModelScope(subagents.modelScope, path);
    const overrides = {};
    if (!subagents.agentOverrides || typeof subagents.agentOverrides !== "object"
        || Array.isArray(subagents.agentOverrides)) return { overrides };
    for (const [name, value] of Object.entries(subagents.agentOverrides)) {
      const parsed = parseOverride(name, value, path);
      if (parsed) overrides[name] = parsed;
    }
    return { overrides };
  }

  function validateAgentOverride(project, projectPath, name) {
    const override = project.overrides[name];
    if (!override) return;
    const fields = Object.keys(override);
    if (fields.length === 1 && fields[0] === "disabled" && override.disabled === false) return;
    if (override.disabled === true)
      throw new Error(`Required project agent ${name} is disabled by pi-subagents settings at ${projectPath}`);
    throw new Error(`Required project agent ${name} has a behavior-changing pi-subagents override at ${projectPath}`);
  }

  async function headlessPreflight() {
    const ping = await rpc("ping");
    if (ping?.version !== 1 || !Array.isArray(ping.methods)
        || !ping.methods.includes("spawn") || ping.capabilities?.asyncSpawn !== true)
      throw new Error("pi-subagents ping does not advertise the required asynchronous spawn capability");
    const projectPath = join(home, ".pi", "settings.json");
    const project = agentSettings(projectPath);
    for (const name of Object.keys(requiredAgents)) {
      validateAgent(name);
      validateAgentOverride(project, projectPath, name);
    }
  }

  async function dispatch(worker, resume = false) {
    await headlessPreflight();
    const action = resume ? "resume" : "begin";
    let data;
    try {
      data = await cli(["headless", action, worker, "--home", home,
        "--wt-command", wt, "--pi-command", piCmd, "--branch-prefix", prefix]);
    } catch (error) {
      if (error instanceof CliRejected)
        throw new Error(definiteClaimFailure(action, worker, error.message));
      throw new Error(ambiguousClaimFailure(action, worker, error.message));
    }
    try { return await rpc("spawn", data.harness_call.arguments, 15000); }
    catch (error) {
      throw new Error(ambiguousClaimFailure(action, worker, error.message));
    }
  }

  async function startTask(ctx, text) {
    if (!plan) throw new Error("No Monty task is being planned");
    await headlessPreflight();
    const dir = mkdtempSync(join(tmpdir(), "monty-plan-"));
    const file = join(dir, "plan.md");
    try {
      writeFileSync(file, text.trim() + "\n", { mode: 0o600 });
      const entry = await cli(["task", "prepare", plan.key, "--plan", file, "--json",
        "--home", home, "--wt-command", wt, "--pi-command", piCmd,
        "--branch-prefix", prefix]);
      const session = await ensureSession(entry, ctx);
      if (!session) return;
      clearPlan(ctx);
      if (entry.worker.status === "prepared") await dispatch(entry.worker.id);
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
      const text = latestPlan(ctx.sessionManager.getBranch(), PLAN, owned);
      if (!text) throw new Error("No completed plan was found after the current plan-mode marker");
      await startTask(ctx, text);
    } catch (e) { ctx.ui.notify(e.message, "error"); }
  }});
  pi.registerCommand("monty-run", { description: "Run this task's asynchronous Monty chain", handler: async (args, ctx) => {
    try {
      const task = activeTask(ctx);
      if (!task) throw new Error("This is not an active Monty task subsession for this Monty home");
      await dispatch(task.worker, args.trim() === "resume");
      ctx.ui.notify(`Started agents for ${task.title}`, "info");
    } catch (e) { ctx.ui.notify(e.message, "error"); }
  }});
  pi.registerCommand("monty-plan-cancel", { description: "Leave Monty plan mode", handler: async (_, ctx) => {
    clearPlan(ctx);
  }});

  pi.on("session_start", (_, ctx) => {
    if (!home) return;
    const task = activeTask(ctx);
    const saved = lastOwned(ctx.sessionManager.getBranch(), PLAN);
    plan = saved?.enabled ? saved : undefined;
    if (plan && !plan.tools) {
      plan = { ...plan, tools: pi.getActiveTools() };
      pi.appendEntry(PLAN, plan);
    }
    if (task) {
      if (!ctx.sessionManager.getSessionName()) pi.setSessionName(`Monty: ${task.title}`);
    } else if (same(ctx.cwd, home)) {
      if (!lastOwned(ctx.sessionManager.getEntries(), HEAD)) pi.appendEntry(HEAD, { home });
      if (!ctx.sessionManager.getSessionName()) pi.setSessionName("Monty Head Butler");
      materialize(ctx.sessionManager, ctx.model);
    }
    usePlan(ctx);
    loc(ctx);
  });

  pi.on("session_shutdown", () => {
    if (plan?.tools) pi.setActiveTools(plan.tools);
  });

  pi.on("before_agent_start", () => plan ? {
    message: { customType: "monty-plan-context:v1", display: false,
      content: `[MONTY PLAN MODE]\nPlan task ${plan.key}: ${plan.title}. Explore read-only and return a numbered Plan: section. Do not implement or mutate local or remote state.` }
  } : undefined);

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
        if (note?.trim()) {
          plan = { ...plan, enabled: true };
          pi.appendEntry(PLAN, plan);
          pi.sendUserMessage(note.trim());
        }
      }
    } catch (e) { ctx.ui.notify(e.message, "error"); }
    finally { busy = false; }
  });
}
