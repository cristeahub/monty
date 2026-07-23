import { existsSync, mkdtempSync, readFileSync, realpathSync, rmSync, statSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";
import { Type } from "@earendil-works/pi-ai";
import { SessionManager } from "@earendil-works/pi-coding-agent";
import {
  HEAD, PLAN, REQUIRED_AGENTS, RETIRED, RUN, TASK, ambiguousClaimFailure,
  definiteClaimFailure, find, last, latestPlan, msgText, row,
} from "../core.js";
import { TintinChainRunner, pingSubagents } from "../subagent-chain.js";
import { hasWorkerTranscript, openWorkerView, readWorkerSnapshot, workerWidgetLines } from "../worker-view.js";
import { showNavigationSpinner } from "../navigation-spinner.js";

const rawHome = process.env.MONTY_HOME?.trim();
const home = rawHome ? realpathSync(resolve(rawHome)) : null;
const cmd = process.env.MONTY_COMMAND?.trim() || "monty";
const wt = process.env.MONTY_WT_COMMAND?.trim() || "wt";
const piCmd = process.env.MONTY_PI_COMMAND?.trim() || "pi";
const prefix = process.env.MONTY_BRANCH_PREFIX?.trim() || "monty";
const planTools = new Set(["read", "grep", "find", "ls", "questionnaire"]);
const spinnerRenderDelayMs = 16;
const chainTool = "monty_headless_chain";
const emptyUsage = {
  input: 0, output: 0, cacheRead: 0, cacheWrite: 0, totalTokens: 0,
  cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, total: 0 },
};

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

function err(result) {
  return (result.stderr || result.stdout || `Command exited ${result.code}`).trim();
}

function materialize(session, model) {
  const file = session.getSessionFile();
  if (!file) throw new Error("Pi did not assign a session file");
  if (!existsSync(file)) {
    if (!model) throw new Error("Pi has no selected model to materialize the session safely");
    session.appendMessage({
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
  let taskWidgetTimer;
  let taskWidgetText;
  const chains = new TintinChainRunner(pi, home);

  async function cli(args, timeout = 120000) {
    if (!home) throw new Error("MONTY_HOME is not set");
    const result = await pi.exec(cmd, args, { cwd: home, timeout });
    if (result.killed) throw new Error(`Monty execution was killed or timed out: ${err(result)}`);
    if (result.code) throw new CliRejected(err(result));
    try { return JSON.parse(result.stdout); }
    catch { throw new Error(`Monty returned invalid JSON: ${result.stdout.trim()}`); }
  }

  function cur(ctx, type) {
    return last(ctx.sessionManager.getBranch(), type);
  }

  function isRetired(session, key) {
    return session.getEntries().some(entry => entry.type === "custom"
      && entry.customType === RETIRED && owned(entry.data) && entry.data?.key === key);
  }

  function headMarker(ctx) {
    return lastOwned(ctx.sessionManager.getEntries(), HEAD);
  }

  function isHead(ctx) {
    const file = ctx.sessionManager.getSessionFile();
    const marker = headMarker(ctx);
    return !!file && typeof marker?.session === "string" && same(marker.session, file)
      && same(ctx.cwd, home || ctx.cwd);
  }

  function activeTask(ctx) {
    const task = cur(ctx, TASK);
    return task && typeof task === "object" && owned(task) && task.selected !== false
      && typeof task.key === "string" && !task.retired && !isRetired(ctx.sessionManager, task.key)
      ? task : undefined;
  }

  function taskRun(ctx, task) {
    return [...ctx.sessionManager.getBranch()].reverse().find(entry =>
      entry.type === "custom" && entry.customType === RUN && owned(entry.data)
      && (entry.data?.key === task.key || (!entry.data?.key && entry.data?.worker === task.worker)))?.data;
  }

  function attemptExists(ctx, attemptId) {
    return ctx.sessionManager.getEntries().some(entry => entry.type === "custom"
      && entry.customType === RUN && owned(entry.data) && entry.data?.attemptId === attemptId);
  }

  function runPersister(task) {
    return snapshot => pi.appendEntry(RUN, {
      ...snapshot,
      key: task?.key || snapshot.key,
      title: task?.title || snapshot.title,
      worker: task?.worker || snapshot.worker,
    });
  }

  function stopTaskWidget(ctx) {
    if (taskWidgetTimer) clearInterval(taskWidgetTimer);
    taskWidgetTimer = undefined;
    taskWidgetText = undefined;
    if (ctx?.hasUI) ctx.ui.setWidget("monty-worker", undefined);
  }

  function startTaskWidget(ctx, task) {
    stopTaskWidget(ctx);
    if (!ctx.hasUI) return;
    const refresh = () => {
      const lines = workerWidgetLines(readWorkerSnapshot(home, task, taskRun(ctx, task)));
      const text = lines.join("\n");
      if (text === taskWidgetText) return;
      taskWidgetText = text;
      ctx.ui.setWidget("monty-worker", lines);
    };
    refresh();
    taskWidgetTimer = setInterval(refresh, 750);
    taskWidgetTimer.unref?.();
  }

  async function showTaskWorker(ctx, task, always = false) {
    const run = taskRun(ctx, task);
    if (!always && !hasWorkerTranscript(home, task, run)) return false;
    await openWorkerView(ctx, home, task, run);
    return true;
  }

  function loc(ctx) {
    const task = activeTask(ctx);
    const text = plan ? `MONTY · HEAD BUTLER · Planning: ${plan.title}`
      : isHead(ctx) && task ? `MONTY · HEAD BUTLER · Monitoring: ${task.title}`
        : isHead(ctx) ? "MONTY · HEAD BUTLER"
          : task ? `MONTY · LEGACY TASK: ${task.title}` : undefined;
    ctx.ui.setStatus("monty", text ? ctx.ui.theme.fg("accent", text) : undefined);
    if (text) ctx.ui.setTitle(text);
  }

  function usePlan(ctx) {
    if (!plan) return;
    pi.setActiveTools((plan.tools || pi.getActiveTools()).filter(tool => planTools.has(tool)));
    loc(ctx);
  }

  function clearPlan(ctx) {
    if (!plan) return;
    pi.setActiveTools(plan.tools || pi.getActiveTools());
    plan = undefined;
    pi.appendEntry(PLAN, { home, enabled: false });
    loc(ctx);
  }

  function clearSelection(ctx) {
    if (!activeTask(ctx)) return;
    pi.appendEntry(TASK, { home, selected: false });
    stopTaskWidget(ctx);
    loc(ctx);
  }

  async function withNavigationSpinner(ctx, message, work) {
    const spinner = showNavigationSpinner(ctx, message);
    try {
      await new Promise(resolveDelay => setTimeout(resolveDelay, spinnerRenderDelayMs));
      return await work();
    } finally { spinner.stop(); }
  }

  async function preserveForeground(ctx) {
    if (ctx.isIdle()) return;
    await withNavigationSpinner(ctx, "Waiting for current response to finish...", () => ctx.waitForIdle());
  }

  async function switchMontySession(ctx, file, options) {
    await preserveForeground(ctx);
    const spinner = showNavigationSpinner(ctx, "Switching Monty session...");
    await new Promise(resolveDelay => setTimeout(resolveDelay, spinnerRenderDelayMs));
    let rebound = false;
    try {
      const result = await ctx.switchSession(file, {
        ...options,
        withSession: async next => {
          rebound = true;
          spinner.bind(next);
          spinner.stop();
          if (options?.withSession) await options.withSession(next);
        },
      });
      if (result.cancelled) {
        spinner.stop();
        ctx.ui.notify("Monty session switch was cancelled", "info");
      }
      return result;
    } catch (error) {
      spinner.stop();
      throw error;
    } finally {
      if (!rebound) spinner.stop();
    }
  }

  async function snapshot() {
    return cli(["tasks", "list", "--json", "--no-sync", "--home", home]);
  }

  function headLink(ctx) {
    const marker = headMarker(ctx);
    const file = ctx.sessionManager.getSessionFile();
    if (file && typeof marker?.session === "string" && same(marker.session, file)) return file;
    return lastOwned(ctx.sessionManager.getEntries(), TASK)?.head;
  }

  function validHead(path) {
    try {
      const session = SessionManager.open(path);
      const marker = lastOwned(session.getEntries(), HEAD);
      return typeof marker?.session === "string" && same(marker.session, path)
        && same(session.getHeader()?.cwd, home);
    } catch { return false; }
  }

  function legacyHead(path) {
    try {
      const session = SessionManager.open(path);
      const marker = lastOwned(session.getEntries(), HEAD);
      return !!marker && marker.session === undefined && same(session.getHeader()?.cwd, home);
    } catch { return false; }
  }

  function upgradeHead(path) {
    SessionManager.open(path).appendCustomEntry(HEAD, { home, session: path });
    return path;
  }

  async function heads() {
    const result = [];
    for (const info of await SessionManager.listAll()) if (validHead(info.path)) result.push(info);
    return result.sort((left, right) => right.modified - left.modified);
  }

  async function getHead(ctx) {
    const direct = headLink(ctx);
    if (direct && existsSync(direct)) {
      if (validHead(direct)) return direct;
      if (legacyHead(direct)) return upgradeHead(direct);
    }
    return (await heads())[0]?.path;
  }

  function taskLink(entry, head) {
    return {
      home, head, selected: true, key: entry.task.key, title: entry.task.title,
      worker: entry.worker.id, cwd: entry.cwd, instructions: entry.instructions,
      context: entry.context, memory: entry.memory,
    };
  }

  function activateTask(ctx, task) {
    if (!ctx.sessionManager.getSessionName()) pi.setSessionName("Monty Head Butler");
    startTaskWidget(ctx, task);
    loc(ctx);
  }

  async function selectEntry(entry, ctx) {
    const head = await getHead(ctx);
    if (!head) throw new Error("The persisted Monty head-butler session could not be found");
    const task = taskLink(entry, head);
    const current = ctx.sessionManager.getSessionFile();
    if (current && same(current, head)) {
      pi.appendEntry(TASK, task);
      activateTask(ctx, task);
      return { ctx, task, session: head };
    }
    SessionManager.open(head).appendCustomEntry(TASK, task);
    let nextContext;
    const result = await switchMontySession(ctx, head, {
      withSession: next => {
        nextContext = next;
        activateTask(next, task);
      },
    });
    if (result.cancelled) return;
    return { ctx: nextContext, task, session: head };
  }

  async function enter(task, ctx) {
    if (task.action === "plan") return startPlan(task, ctx);
    if (task.action !== "open") throw new Error(`Task ${task.id} is ${task.action}`);
    const entry = await withNavigationSpinner(ctx, `Opening ${task.title}...`, () =>
      cli(["task", "enter", task.key, "--json", "--home", home, "--wt-command", wt]));
    const selected = await selectEntry(entry, ctx);
    if (selected) await showTaskWorker(selected.ctx, selected.task);
  }

  async function choose(ctx) {
    const data = await withNavigationSpinner(ctx, "Loading Monty tasks...", snapshot);
    if (!data.tasks.length) return ctx.ui.notify("Monty has no open tasks", "info");
    const options = data.tasks.map(row);
    const value = await ctx.ui.select(
      "ID                 Project          Status   Title                                      Branch", options);
    if (value) await enter(data.tasks[options.indexOf(value)], ctx);
  }

  function planPrompt(task) {
    return `Plan Monty task ${task.id}: ${task.title}. Inspect the project and produce a concrete numbered Plan: section. Do not implement it yet.`;
  }

  function enablePlan(task, ctx) {
    clearSelection(ctx);
    plan = {
      home, enabled: true, key: task.key, title: task.title,
      tools: plan?.tools || pi.getActiveTools(),
    };
    pi.appendEntry(PLAN, plan);
    usePlan(ctx);
    pi.sendUserMessage(planPrompt(task));
  }

  async function startPlan(task, ctx) {
    const head = await withNavigationSpinner(ctx, `Opening plan for ${task.title}...`, () => getHead(ctx));
    if (!head) throw new Error("Start Monty from its head-butler session before planning a task");
    const current = ctx.sessionManager.getSessionFile();
    if (!current || !same(current, head)) {
      const session = SessionManager.open(head);
      session.appendCustomEntry(TASK, { home, selected: false });
      session.appendCustomEntry(PLAN, { home, enabled: true, key: task.key, title: task.title });
      const result = await switchMontySession(ctx, head, {
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
      if (actual !== REQUIRED_AGENTS[name].trimEnd())
        throw new Error("definition does not match Monty's complete required fixed definition");
    } catch (error) {
      throw new Error(`Required project agent ${name} is invalid at ${path}: ${error.message}`);
    }
  }

  async function headlessPreflight() {
    await pingSubagents(pi);
    for (const name of Object.keys(REQUIRED_AGENTS)) validateAgent(name);
  }

  function chainTask(args, fallback) {
    const worker = args?.worker;
    return fallback || {
      key: worker?.task_key || `worker:${worker?.id}`,
      title: worker?.title || worker?.id,
      worker: worker?.id,
      cwd: worker?.worktree,
      memory: worker?.worker_dir ? join(worker.worker_dir, "memory.md") : undefined,
    };
  }

  async function launchChain(args, ctx, task, skipPing = false) {
    if (!isHead(ctx)) throw new Error("Monty chains must start from the permanent head-butler session");
    if (attemptExists(ctx, args?.workflow?.attempt_id))
      throw new Error(`Monty chain ${args.workflow.attempt_id} was already requested from this session`);
    materialize(ctx.sessionManager, ctx.model);
    return chains.start(args, runPersister(chainTask(args, task)), { skipPing });
  }

  async function dispatch(worker, resume, ctx, task, fromTool = false) {
    if (!isHead(ctx)) throw new Error("Monty chains must start from the exact permanent head-butler session");
    await headlessPreflight();
    const action = resume ? "resume" : "begin";
    const definite = message => fromTool
      ? `${message}. Worker ${worker} was not claimed; the same monty_headless_chain call can be retried.`
      : definiteClaimFailure(action, worker, message);
    const ambiguous = message => fromTool
      ? `${message}. The ${action} request may have been accepted for worker ${worker}. Do not retry automatically; set resume to true only when a successor run is explicitly intentional.`
      : ambiguousClaimFailure(action, worker, message);
    let data;
    try {
      data = await cli(["headless", action, worker, "--home", home,
        "--wt-command", wt, "--pi-command", piCmd, "--branch-prefix", prefix]);
    } catch (error) {
      if (error instanceof CliRejected) throw new Error(definite(error.message));
      throw new Error(ambiguous(error.message));
    }
    try {
      return await launchChain({ worker: data?.worker, workflow: data?.workflow }, ctx, task, true);
    } catch (error) {
      throw new Error(ambiguous(error.message));
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
        "--home", home, "--wt-command", wt, "--pi-command", piCmd, "--branch-prefix", prefix]);
      clearPlan(ctx);
      const selected = await selectEntry(entry, ctx);
      if (!selected) return;
      if (entry.worker.status === "prepared")
        await dispatch(entry.worker.id, false, selected.ctx, selected.task);
      startTaskWidget(selected.ctx, selected.task);
      const suffix = entry.worker.status === "prepared" ? "Agents are running." : `Worker is ${entry.worker.status}.`;
      selected.ctx.ui.notify(`Started ${entry.task.title}. ${suffix}`, "info");
    } finally { rmSync(dir, { recursive: true, force: true }); }
  }

  async function openArg(value, ctx) {
    const data = await withNavigationSpinner(ctx, "Loading Monty tasks...", snapshot);
    return enter(find(data.tasks, value), ctx);
  }

  pi.registerTool({
    name: chainTool,
    label: "Monty Headless Chain",
    description: "Atomically claim a prepared Monty worker and start its fixed TintinWeb implementer, parallel review, and fixer workflow. Set resume only for an explicitly requested successor run.",
    parameters: Type.Object({
      worker: Type.String({ description: "Prepared Monty worker ID" }),
      resume: Type.Optional(Type.Boolean({
        description: "Start an intentional successor chain for an existing launch-requested worker.",
      })),
    }),
    async execute(_toolCallId, args, _signal, _onUpdate, ctx) {
      try {
        if (!isHead(ctx)) throw new Error("Run Monty chains only from the permanent head-butler session");
        const selected = activeTask(ctx);
        const task = selected?.worker === args.worker ? selected : undefined;
        const result = await dispatch(args.worker, args.resume === true, ctx, task, true);
        return {
          content: [{ type: "text", text: `Started Monty chain ${result.attemptId}.` }],
          details: result,
        };
      } catch (error) {
        return {
          content: [{ type: "text", text: `Monty chain was not started: ${error.message}` }],
          details: { error: error.message },
          isError: true,
        };
      }
    },
  });

  pi.registerCommand("monty", { description: "Choose a Monty task", handler: async (_, ctx) => {
    try {
      await preserveForeground(ctx);
      await choose(ctx);
    } catch (error) { ctx.ui.notify(error.message, "error"); }
  }});

  pi.registerCommand("monty-open", { description: "Open or plan a Monty task", handler: async (args, ctx) => {
    try {
      await preserveForeground(ctx);
      args.trim() ? await openArg(args, ctx) : await choose(ctx);
    } catch (error) { ctx.ui.notify(error.message, "error"); }
  }});

  pi.registerCommand("monty-head-butler", { description: "Return to the Monty head-butler prompt", handler: async (_, ctx) => {
    try {
      const head = await withNavigationSpinner(ctx, "Finding the Monty head butler...", () => getHead(ctx));
      if (!head) throw new Error("The Monty head-butler session could not be found");
      const current = ctx.sessionManager.getSessionFile();
      if (current && same(current, head)) {
        clearSelection(ctx);
        clearPlan(ctx);
        return;
      }
      await switchMontySession(ctx, head);
    } catch (error) { ctx.ui.notify(error.message, "error"); }
  }});

  pi.registerCommand("monty-start", { description: "Start the task currently being planned", handler: async (_, ctx) => {
    try {
      const text = latestPlan(ctx.sessionManager.getBranch(), PLAN, owned);
      if (!text) throw new Error("No completed plan was found after the current plan-mode marker");
      await withNavigationSpinner(ctx, `Starting ${plan?.title || "Monty task"}...`, () => startTask(ctx, text));
    } catch (error) { ctx.ui.notify(error.message, "error"); }
  }});

  pi.registerCommand("monty-run", { description: "Run the selected task's asynchronous Monty chain", handler: async (args, ctx) => {
    try {
      const task = activeTask(ctx);
      if (!task) throw new Error("Select an active Monty task with /monty first");
      await withNavigationSpinner(ctx, `Starting agents for ${task.title}...`, () =>
        dispatch(task.worker, args.trim() === "resume", ctx, task));
      startTaskWidget(ctx, task);
      ctx.ui.notify(`Started agents for ${task.title}`, "info");
      await showTaskWorker(ctx, task, true);
    } catch (error) { ctx.ui.notify(error.message, "error"); }
  }});

  pi.registerCommand("monty-worker", { description: "Open the selected task's live worker transcript", handler: async (_, ctx) => {
    try {
      const task = activeTask(ctx);
      if (!task) throw new Error("Select an active Monty task with /monty first");
      await showTaskWorker(ctx, task, true);
    } catch (error) { ctx.ui.notify(error.message, "error"); }
  }});

  pi.registerCommand("monty-plan-cancel", { description: "Leave Monty plan mode", handler: async (_, ctx) => {
    clearPlan(ctx);
  }});

  pi.on("input", (event, ctx) => {
    const task = activeTask(ctx);
    if (!task || isHead(ctx) || event.source === "extension") return;
    ctx.ui.notify(
      `This legacy Monty task subsession is monitor-only. Use /monty-head-butler, then select the task again with /monty.`,
      "info",
    );
    return { action: "handled" };
  });

  pi.on("session_start", (event, ctx) => {
    if (!home) return;
    let head = isHead(ctx);
    if (!head && same(ctx.cwd, home) && event.reason === "startup") {
      const file = materialize(ctx.sessionManager, ctx.model);
      pi.appendEntry(HEAD, { home, session: file });
      head = true;
    }
    const task = activeTask(ctx);
    const saved = head ? lastOwned(ctx.sessionManager.getBranch(), PLAN) : undefined;
    plan = saved?.enabled ? saved : undefined;
    if (plan && !plan.tools) {
      plan = { ...plan, tools: pi.getActiveTools() };
      pi.appendEntry(PLAN, plan);
    }
    if (head) {
      if (!ctx.sessionManager.getSessionName()) pi.setSessionName("Monty Head Butler");
      materialize(ctx.sessionManager, ctx.model);
      if (task) startTaskWidget(ctx, task);
      else stopTaskWidget(ctx);
    } else if (task) startTaskWidget(ctx, task);
    else stopTaskWidget(ctx);
    usePlan(ctx);
    loc(ctx);
  });

  pi.on("session_before_switch", (_event, ctx) => {
    if (!chains.hasRunning()) return;
    ctx.ui.notify(
      "A Monty chain is running in this Pi session. Use /monty-head-butler to clear the task monitor without replacing the session.",
      "warning",
    );
    return { cancel: true };
  });

  pi.on("session_shutdown", (_, ctx) => {
    chains.interruptAll("Pi session ended");
    chains.dispose();
    stopTaskWidget(ctx);
    if (plan?.tools) pi.setActiveTools(plan.tools);
  });

  pi.on("before_agent_start", () => plan ? {
    message: {
      customType: "monty-plan-context:v1", display: false,
      content: `[MONTY PLAN MODE]\nPlan task ${plan.key}: ${plan.title}. Explore read-only and return a numbered Plan: section. Do not implement or mutate local or remote state.`,
    },
  } : undefined);

  pi.on("agent_end", async (event, ctx) => {
    if (!plan || busy || !ctx.hasUI) return;
    const text = msgText([...event.messages].reverse().find(message => message.role === "assistant"));
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
    } catch (error) { ctx.ui.notify(error.message, "error"); }
    finally { busy = false; }
  });
}
