import {
  existsSync, mkdirSync, readFileSync, realpathSync, renameSync, statSync, writeFileSync,
} from "node:fs";
import { dirname, isAbsolute, relative, resolve, sep } from "node:path";
import { randomUUID } from "node:crypto";

export const CHAIN_SCHEMA = "monty:tintin-chain:v1";
export const CHAIN_BACKEND = "@tintinweb/pi-subagents";
export const RPC_VERSION = 2;

const MAX_SOURCE_BYTES = 2 * 1024 * 1024;
const TERMINAL = new Set(["completed", "steered", "error", "stopped", "aborted"]);

function physical(path) {
  const absolute = resolve(path);
  let existing = absolute;
  while (!existsSync(existing) && dirname(existing) !== existing) existing = dirname(existing);
  try { return resolve(realpathSync(existing), relative(existing, absolute)); }
  catch { return absolute; }
}

function inside(root, path) {
  const base = physical(root);
  const target = physical(path);
  return target === base || target.startsWith(base + sep);
}

function object(value, label) {
  if (!value || typeof value !== "object" || Array.isArray(value))
    throw new Error(`${label} must be an object`);
  return value;
}

function string(value, label) {
  if (typeof value !== "string" || !value.trim()) throw new Error(`${label} must be a non-empty string`);
  return value;
}

function source(path) {
  const size = statSync(path).size;
  if (size > MAX_SOURCE_BYTES) throw new Error(`Monty chain source is too large: ${path}`);
  return readFileSync(path, "utf8");
}

function claimedWorker(worker) {
  const path = resolve(worker.worker_dir, "job.json");
  if (!inside(worker.worker_dir, path)) throw new Error("Claimed Monty job file escapes worker memory");
  let record;
  try { record = JSON.parse(source(path)); }
  catch (error) { throw new Error(`Cannot verify claimed Monty worker at ${path}: ${error.message}`); }
  if (record?.status !== "launch-requested")
    throw new Error(`Monty worker ${worker.id} is not launch-requested`);
  for (const field of ["id", "title", "repo", "branch"])
    if (record[field] !== worker[field]) throw new Error(`Monty worker ${field} changed after dispatch`);
  for (const field of ["worker_dir", "context", "last_known_worktree"])
    if (typeof record[field] !== "string" || physical(record[field]) !== physical(
      field === "last_known_worktree" ? worker.worktree : worker[field]))
      throw new Error(`Monty worker ${field} changed after dispatch`);
  if ((record.task_key ?? null) !== (worker.task_key ?? null))
    throw new Error("Monty worker task link changed after dispatch");
  return record;
}

function validateStep(step, label, worker, attemptRoot) {
  object(step, label);
  for (const field of ["role", "phase", "agent", "description", "prompt", "cwd", "output"])
    string(step[field], `${label}.${field}`);
  if (!isAbsolute(step.cwd) || physical(step.cwd) !== physical(worker.worktree))
    throw new Error(`${label}.cwd must be the Monty-owned worktree`);
  if (!Array.isArray(step.reads) || step.reads.some(path => typeof path !== "string" || !isAbsolute(path)))
    throw new Error(`${label}.reads must contain absolute paths`);
  if (!isAbsolute(step.output) || !inside(attemptRoot, step.output))
    throw new Error(`${label}.output must stay inside the attempt directory`);
  for (const path of step.reads) if (!existsSync(path)) throw new Error(`${label} source does not exist: ${path}`);
  return step;
}

export function validateChainDispatch(value, home) {
  const args = object(value, "Monty chain arguments");
  const worker = object(args.worker, "worker");
  const workflow = object(args.workflow, "workflow");
  if (workflow.schema !== CHAIN_SCHEMA || workflow.backend !== CHAIN_BACKEND)
    throw new Error("Unsupported Monty subagent workflow");
  string(workflow.home, "workflow.home");
  if (physical(workflow.home) !== physical(home)) throw new Error("Monty chain belongs to another home");
  for (const field of ["id", "title", "repo", "branch", "worktree", "worker_dir", "instructions", "context"])
    string(worker[field], `worker.${field}`);
  for (const field of ["attempt_id", "attempt_root"])
    string(workflow[field], `workflow.${field}`);
  if (!/^attempt-[A-Za-z0-9][A-Za-z0-9_-]{0,127}$/.test(workflow.attempt_id))
    throw new Error("workflow.attempt_id must be a generated safe identifier");
  if (!isAbsolute(worker.worktree) || !statSync(worker.worktree).isDirectory())
    throw new Error("worker.worktree must be an existing absolute directory");
  if (!isAbsolute(worker.worker_dir) || !inside(resolve(home, ".monty"), worker.worker_dir))
    throw new Error("worker.worker_dir must stay inside Monty state");
  for (const field of ["instructions", "context"])
    if (!isAbsolute(worker[field]) || !existsSync(worker[field]))
      throw new Error(`worker.${field} must be an existing absolute file`);
  if (!inside(worker.worker_dir, worker.instructions)
      || physical(worker.instructions) !== physical(resolve(worker.worker_dir, "MONTY.md")))
    throw new Error("worker.instructions must be the claimed worker's MONTY.md");
  claimedWorker(worker);
  const attemptsRoot = resolve(worker.worker_dir, "artifacts", "headless");
  if (!inside(worker.worker_dir, attemptsRoot))
    throw new Error("Monty attempt storage escapes worker memory");
  const expectedRoot = resolve(attemptsRoot, workflow.attempt_id);
  if (!isAbsolute(workflow.attempt_root) || !inside(attemptsRoot, workflow.attempt_root)
      || physical(workflow.attempt_root) !== physical(expectedRoot))
    throw new Error("workflow.attempt_root must be the generated attempt directory");
  const implementation = validateStep(workflow.implementation, "workflow.implementation", worker, workflow.attempt_root);
  if (implementation.role !== "implementation" || implementation.phase !== "Implementation"
      || implementation.agent !== "monty-headless-worker"
      || implementation.description !== `Implement ${worker.id}`)
    throw new Error("The implementation phase must use Monty's fixed worker contract");
  if (!Array.isArray(workflow.reviews) || workflow.reviews.length !== 2)
    throw new Error("The review phase must contain exactly two reviewers");
  const reviews = workflow.reviews.map((step, index) =>
    validateStep(step, `workflow.reviews[${index}]`, worker, workflow.attempt_root));
  const reviewRoles = reviews.map(step => step.role).sort();
  const reviewDescriptions = {
    correctnessReview: "Review correctness",
    qualityReview: "Review quality and tests",
  };
  if (reviews.some(step => step.phase !== "Review" || step.agent !== "monty-headless-reviewer"
      || step.description !== reviewDescriptions[step.role])
      || reviewRoles.join(",") !== "correctnessReview,qualityReview")
    throw new Error("The review phase must use Monty's two fixed reviewer contracts");
  const fixer = validateStep(workflow.fixer, "workflow.fixer", worker, workflow.attempt_root);
  if (fixer.role !== "final" || fixer.phase !== "Fix" || fixer.agent !== "monty-headless-worker"
      || fixer.description !== "Apply verified fixes")
    throw new Error("The fix phase must use Monty's fixed worker contract");
  const pathsEqual = (actual, expected) => actual.length === expected.length
    && actual.every((path, index) => physical(path) === physical(expected[index]));
  if (!pathsEqual(implementation.reads, [worker.instructions, worker.context])
      || !pathsEqual(fixer.reads, [worker.instructions, worker.context])
      || reviews.some(step => !pathsEqual(step.reads, [worker.context])))
    throw new Error("Monty chain sources do not match the fixed phase contract");
  const outputByRole = {
    implementation: resolve(expectedRoot, "implementation.md"),
    correctnessReview: resolve(expectedRoot, "reviews", "correctness.md"),
    qualityReview: resolve(expectedRoot, "reviews", "quality.md"),
    final: resolve(expectedRoot, "final.md"),
  };
  for (const step of [implementation, ...reviews, fixer])
    if (physical(step.output) !== physical(outputByRole[step.role]))
      throw new Error(`Unexpected output path for ${step.role}`);
  if (!implementation.prompt.includes("Do not invoke /review or spawn subagents")
      || reviews.some(step => !step.prompt.includes("{previous}"))
      || !fixer.prompt.includes("{outputs.correctnessReview}")
      || !fixer.prompt.includes("{outputs.qualityReview}"))
    throw new Error("Monty chain prompts do not match the fixed phase contract");
  return { worker, workflow, implementation, reviews, fixer };
}

function replaceAll(text, values) {
  let result = text;
  for (const [key, value] of Object.entries(values)) result = result.replaceAll(key, value);
  return result;
}

function suppliedFiles(step) {
  if (!step.reads.length) return "";
  const blocks = step.reads.map(path => `### ${path}\n\n${source(path).trimEnd()}`);
  return `\n\n## Supplied Monty files\n\n${blocks.join("\n\n")}`;
}

function outcomeText(outcome) {
  const lines = [`Status: ${outcome.status}`];
  if (outcome.error) lines.push(`Error: ${outcome.error}`);
  lines.push("", outcome.result?.trim() || "(no agent output)");
  return lines.join("\n");
}

function promptFor(step, outcomes) {
  const values = {
    "{previous}": outcomes.implementation ? outcomeText(outcomes.implementation) : "(implementation unavailable)",
    "{outputs.correctnessReview}": outcomes.correctnessReview
      ? outcomeText(outcomes.correctnessReview) : "(correctness review unavailable)",
    "{outputs.qualityReview}": outcomes.qualityReview
      ? outcomeText(outcomes.qualityReview) : "(quality review unavailable)",
  };
  return replaceAll(step.prompt, values) + suppliedFiles(step);
}

function atomicWrite(path, content) {
  mkdirSync(dirname(path), { recursive: true, mode: 0o700 });
  const temp = `${path}.tmp-${process.pid}-${randomUUID()}`;
  writeFileSync(temp, content.endsWith("\n") ? content : content + "\n", { mode: 0o600 });
  renameSync(temp, path);
}

function runSnapshot(run) {
  const failures = Object.entries(run.outcomes)
    .filter(([, outcome]) => outcome.event === "failed")
    .map(([role, outcome]) => ({ role, status: outcome.status, error: outcome.error }));
  return {
    home: run.home,
    key: run.worker.task_key,
    title: run.worker.title,
    worker: run.worker.id,
    backend: CHAIN_BACKEND,
    attemptId: run.workflow.attempt_id,
    attemptRoot: run.workflow.attempt_root,
    state: run.state,
    phase: run.phase,
    agents: { ...run.agents },
    activeAgentIds: [...run.active],
    artifacts: {
      implementation: run.implementation.output,
      correctnessReview: run.reviews[0].output,
      qualityReview: run.reviews[1].output,
      final: run.fixer.output,
    },
    failures,
    failure: run.failure,
    startedAt: run.startedAt,
    updatedAt: Date.now(),
  };
}

function rpc(pi, method, params = {}, timeout = 5000) {
  const channel = `subagents:rpc:${method}`;
  return new Promise((resolveReply, rejectReply) => {
    const requestId = `monty-${randomUUID()}`;
    const replyChannel = `${channel}:reply:${requestId}`;
    let off;
    const timer = setTimeout(() => {
      off?.();
      rejectReply(new Error(`${CHAIN_BACKEND} ${method} RPC timed out`));
    }, timeout);
    timer.unref?.();
    off = pi.events.on(replyChannel, reply => {
      clearTimeout(timer);
      off?.();
      if (reply?.success) resolveReply(reply.data);
      else rejectReply(new Error(typeof reply?.error === "string" ? reply.error
        : `${CHAIN_BACKEND} ${method} RPC failed`));
    });
    pi.events.emit(channel, { requestId, ...params });
  });
}

export async function pingSubagents(pi) {
  const data = await rpc(pi, "ping");
  if (data?.version !== RPC_VERSION)
    throw new Error(`${CHAIN_BACKEND} RPC protocol ${RPC_VERSION} is required`);
  return data;
}

export function currentAgentRecord(agentId) {
  if (typeof agentId !== "string") return;
  return globalThis[Symbol.for("pi-subagents:manager")]?.getRecord?.(agentId);
}

export class TintinChainRunner {
  constructor(pi, home) {
    this.pi = pi;
    this.home = home;
    this.runs = new Map();
    this.byAgent = new Map();
    this.early = new Map();
    this.offCompleted = pi.events.on("subagents:completed", event => this.receive("completed", event));
    this.offFailed = pi.events.on("subagents:failed", event => this.receive("failed", event));
  }

  async start(value, persist = () => {}, options = {}) {
    const spec = validateChainDispatch(value, this.home);
    const attempt = spec.workflow.attempt_id;
    if (this.runs.has(attempt)) throw new Error(`Monty chain ${attempt} is already active`);
    if (!options.skipPing) await pingSubagents(this.pi);
    const run = {
      ...spec, home: this.home, persist, agents: {}, active: new Set(), pending: new Map(), outcomes: {},
      state: "starting", phase: "Implementation", startedAt: Date.now(), advancing: false,
    };
    this.runs.set(attempt, run);
    this.persist(run);
    try {
      await this.spawn(run, spec.implementation);
    } catch (error) {
      this.synthetic(run, spec.implementation, "error", error);
      this.fail(run, error);
      throw error;
    }
    return runSnapshot(run);
  }

  hasRunning() {
    return [...this.runs.values()].some(run => run.state === "starting" || run.state === "running");
  }

  interruptAll(reason) {
    for (const run of this.runs.values()) {
      if (run.state !== "starting" && run.state !== "running") continue;
      run.state = "interrupted";
      run.phase = reason;
      run.failure = reason;
      this.abandon(run, "interrupted", reason, false);
      this.persist(run);
    }
  }

  dispose() {
    this.interruptAll("Runner disposed");
    this.offCompleted?.();
    this.offFailed?.();
    this.runs.clear();
    this.byAgent.clear();
    this.early.clear();
  }

  async spawn(run, step) {
    run.state = "running";
    run.phase = step.phase;
    run.pending.set(step.role, step);
    this.persist(run);
    let data;
    try {
      data = await rpc(this.pi, "spawn", {
        type: step.agent,
        prompt: promptFor(step, run.outcomes),
        options: {
          description: step.description,
          cwd: step.cwd,
        },
      }, 15000);
    } catch (error) {
      run.pending.delete(step.role);
      if (run.state !== "running") return;
      throw error;
    }
    run.pending.delete(step.role);
    const id = string(data?.id, "subagent spawn id");
    if (run.state !== "running") {
      this.early.delete(id);
      void rpc(this.pi, "stop", { agentId: id }).catch(() => {});
      return;
    }
    run.agents[step.role] = id;
    run.active.add(id);
    this.byAgent.set(id, { run, step });
    this.persist(run);
    const early = this.early.get(id);
    if (early) {
      this.early.delete(id);
      this.receive(early.kind, early.event);
    }
    return id;
  }

  receive(kind, event) {
    if (!event || !TERMINAL.has(event.status)) return;
    const linked = this.byAgent.get(event.id);
    if (!linked) {
      this.early.set(event.id, { kind, event });
      if (this.early.size > 50) this.early.delete(this.early.keys().next().value);
      return;
    }
    this.byAgent.delete(event.id);
    linked.run.active.delete(event.id);
    void this.settle(linked.run, linked.step, kind, event)
      .catch(error => this.fail(linked.run, error));
  }

  async settle(run, step, kind, event) {
    if (run.state !== "running") return;
    const outcome = {
      event: kind,
      status: event.status,
      result: typeof event.result === "string" ? event.result : "",
      error: typeof event.error === "string" ? event.error : undefined,
    };
    run.outcomes[step.role] = outcome;
    atomicWrite(step.output, outcomeText(outcome));
    this.persist(run);
    if (step.role === "implementation" && kind === "failed") {
      this.fail(run, new Error(outcome.error || `Implementation ended with ${outcome.status}`));
      return;
    }
    if (step.role === "implementation") return this.startReviews(run);
    if (step.role === "final") {
      run.state = kind === "completed" ? "completed" : "failed";
      run.phase = "Complete";
      this.persist(run);
      return;
    }
    if (run.reviews.every(review => run.outcomes[review.role])) return this.startFixer(run);
  }

  async startReviews(run) {
    if (run.advancing) return;
    run.advancing = true;
    run.phase = "Review";
    this.persist(run);
    const results = await Promise.allSettled(run.reviews.map(step => this.spawn(run, step)));
    for (let index = 0; index < results.length; index++) {
      if (results[index].status === "fulfilled") continue;
      const step = run.reviews[index];
      const error = results[index].reason instanceof Error
        ? results[index].reason.message : String(results[index].reason);
      this.synthetic(run, step, "error", error);
    }
    run.advancing = false;
    this.persist(run);
    if (run.state === "running" && run.reviews.every(review => run.outcomes[review.role]))
      await this.startFixer(run);
  }

  async startFixer(run) {
    if (run.state !== "running" || run.advancing || run.agents.final) return;
    run.advancing = true;
    run.phase = "Fix";
    this.persist(run);
    try { await this.spawn(run, run.fixer); }
    catch (error) {
      this.synthetic(run, run.fixer, "error", error);
      throw error;
    } finally { run.advancing = false; }
  }

  fail(run, error) {
    if (run.state === "completed" || run.state === "failed" || run.state === "interrupted") return;
    run.state = "failed";
    run.phase = "Failed";
    run.failure = error instanceof Error ? error.message : String(error);
    this.abandon(run, "stopped", run.failure, true);
    this.persist(run);
  }

  abandon(run, status, error, stop) {
    for (const step of run.pending.values())
      if (!run.outcomes[step.role]) this.synthetic(run, step, status, error);
    run.pending.clear();
    for (const id of [...run.active]) {
      const linked = this.byAgent.get(id);
      this.byAgent.delete(id);
      if (linked && !run.outcomes[linked.step.role])
        this.synthetic(run, linked.step, status, error);
      if (stop) void rpc(this.pi, "stop", { agentId: id }).catch(() => {});
    }
    run.active.clear();
  }

  synthetic(run, step, status, error) {
    const message = error instanceof Error ? error.message : String(error);
    const outcome = { event: "failed", status, result: "", error: message };
    run.outcomes[step.role] = outcome;
    try { atomicWrite(step.output, outcomeText(outcome)); } catch {}
  }

  persist(run) {
    try { run.persist(runSnapshot(run)); } catch {}
  }
}
