import test from "node:test";
import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { accessSync, constants, existsSync, mkdirSync, mkdtempSync, readFileSync, realpathSync, rmSync, symlinkSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { delimiter, dirname, join } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { ambiguousClaimFailure, definiteClaimFailure, find, HEAD, last, latestPlan, PLAN, REQUIRED_AGENTS, RETIRED, row } from "../pi-extension/core.js";

const builtRoot = dirname(dirname(fileURLToPath(import.meta.url)));
const sourceRoot = existsSync(join(builtRoot, ".pi")) ? builtRoot
  : execFileSync("git", ["rev-parse", "--show-toplevel"], { cwd: builtRoot, encoding: "utf8" }).trim();
const repoFile = path => pathToFileURL(join(sourceRoot, path));
const shippedAgentPaths = Object.fromEntries(Object.keys(REQUIRED_AGENTS).map(name => [
  name, repoFile(`.pi/agents/${name}.md`),
]));
const shippedAgents = Object.fromEntries(Object.entries(shippedAgentPaths)
  .map(([name, path]) => [name, readFileSync(path, "utf8")]));

const tasks = [
  { key: "local:local-001", id: "local:local-001", project: "monty", status: "open", title: "Native Pi", branch: null, worker: null },
  { key: "local:local-002", id: "github:owner/repo#2", project: "app", status: "open", title: "Fix app", branch: "cto/fix", worker: { id: "fix-app" } },
];

function findPi() {
  const names = process.platform === "win32" ? ["pi.cmd", "pi.exe", "pi"] : ["pi"];
  for (const dir of (process.env.PATH || "").split(delimiter)) for (const name of names) {
    const path = join(dir, name);
    try { accessSync(path, constants.X_OK); return realpathSync(path); } catch {}
  }
  throw new Error("pi must be available on PATH");
}

test("task lookup prioritizes stable identities before titles", () => {
  assert.equal(find(tasks, "local:local-001").title, "Native Pi");
  assert.equal(find(tasks, "fix-app").key, "local:local-002");
  const collision = [...tasks, {
    key: "local:local-003", id: "external:3", title: "local:local-001", worker: null,
  }];
  assert.equal(find(collision, "local:local-001").title, "Native Pi");
  assert.throws(() => find(tasks, "missing"), /No task/);
});

test("headless claim diagnostics distinguish definite and ambiguous failures", () => {
  assert.match(definiteClaimFailure("begin", "worker-1", "rejected"),
    /was not claimed; the same \/monty-run command can be retried/);
  assert.match(definiteClaimFailure("resume", "worker-1", "rejected"),
    /same \/monty-run resume command can be retried/);
  assert.match(ambiguousClaimFailure("begin", "worker-1", "timed out"),
    /may have been accepted.*Do not retry automatically/);
});

test("task rows contain only the five Monty columns", () => {
  const value = row(tasks[1]);
  for (const part of ["github:owner/repo#2", "app", "OPEN", "Fix app", "cto/fix"])
    assert.match(value, new RegExp(part.replace("#", "\\#")));
});

test("session state and plan extraction honor the current enabled marker", () => {
  const entries = [
    { type: "message", message: { role: "assistant", content: [{ type: "text", text: "Plan:\n1. Stale" }] } },
    { type: "custom", customType: "state", data: 1 },
    { type: "custom", customType: "plan", data: { enabled: true } },
    { type: "message", message: { role: "assistant", content: [{ type: "text", text: "Not finished" }] } },
    { type: "message", message: { role: "assistant", content: [{ type: "text", text: "Plan:\n1. Current" }] } },
    { type: "custom", customType: "state", data: 2 },
  ];
  assert.equal(last(entries, "state"), 2);
  assert.match(latestPlan(entries, "plan"), /Current/);
  assert.equal(latestPlan(entries.slice(0, 4), "plan"), "");
  assert.equal(latestPlan([...entries, { type: "custom", customType: "plan", data: { enabled: false } }], "plan"), "");
  const withForeign = [...entries, {
    type: "custom", customType: "plan", data: { home: "/foreign", enabled: false },
  }];
  assert.match(latestPlan(withForeign, "plan", data => data.home !== "/foreign"), /Current/);
});

test("shipped fixed agents and Pi package are complete and exact", () => {
  for (const [name, expected] of Object.entries(REQUIRED_AGENTS)) {
    assert.equal(shippedAgents[name].replaceAll("\r\n", "\n").trimEnd(), expected.trimEnd(),
      `${name} drifted from the fixed definition`);
    assert.match(shippedAgents[name], /^model: inherit$/m, `${name} must ignore global default-model settings`);
  }
  const packageJson = JSON.parse(readFileSync(new URL("../pi-extension/package.json", import.meta.url), "utf8"));
  for (const entry of packageJson.pi.extensions)
    assert.ok(existsSync(new URL(`../pi-extension/${entry}`, import.meta.url)), `missing packaged extension ${entry}`);
  const installer = readFileSync(repoFile("install.sh"), "utf8");
  assert.doesNotMatch(installer, /--exclude ['"]\.\/\.pi(?:['"/]|$)/,
    "the control-room installer must include project agent definitions");
});

test("the Pi extension has valid JavaScript syntax", () => {
  execFileSync(process.execPath, ["--check", new URL("../pi-extension/core.js", import.meta.url).pathname]);
  execFileSync(process.execPath, ["--check", new URL("../pi-extension/extensions/monty.js", import.meta.url).pathname]);
});

test("plan switching and refinement create fresh durable markers", () => {
  const source = readFileSync(new URL("../pi-extension/extensions/monty.js", import.meta.url), "utf8");
  const startPlan = source.slice(source.indexOf("async function startPlan"), source.indexOf("function validateAgent"));
  assert.match(startPlan, /SessionManager\.open\(head\)\.appendCustomEntry\(PLAN, marker\)/);
  assert.match(startPlan, /withSession: next => next\.sendUserMessage\(planPrompt\(task\)\)/);
  assert.doesNotMatch(startPlan.slice(startPlan.indexOf("ctx.switchSession")), /\bpi\./);
  const refine = source.slice(source.indexOf('choice === "Refine plan"'));
  assert.match(refine, /pi\.appendEntry\(PLAN, plan\);\s*pi\.sendUserMessage\(note\.trim\(\)\)/,
    "refinement must invalidate the old plan before requesting the next response");
});

test("offline Pi RPC loads Monty and preserves persisted navigation", { timeout: 30000 }, async () => {
  const root = mkdtempSync(join(tmpdir(), "monty-pi-integration-"));
  const home = join(root, "home");
  const homeAlias = join(root, "home-alias");
  const taskCwd = join(root, "task");
  const nextTaskCwd = join(root, "task-next");
  const agentDir = join(root, "agent");
  const instructions = join(root, "instructions.md");
  const context = join(root, "context.md");
  const snapshot = join(root, "snapshot.json");
  const entry = join(root, "entry.json");
  const fakeMonty = join(root, "monty");
  const bridge = join(root, "subagents-bridge.js");
  const rpcLog = join(root, "rpc.jsonl");
  const cliLog = join(root, "cli.log");
  const toolLog = join(root, "tools.jsonl");
  const ping = join(root, "ping.json");
  const reviewer = join(home, ".pi", "agents", "monty-headless-reviewer.md");
  const implementer = join(home, ".pi", "agents", "monty-headless-worker.md");
  const harness = { agent: "chain", cwd: taskCwd, tasks: [{ agent: "implementer", task: "exact" }] };
  const oldAgentDir = process.env.PI_CODING_AGENT_DIR;
  let client;
  try {
    for (const dir of [home, taskCwd, nextTaskCwd, agentDir, dirname(reviewer)]) mkdirSync(dir, { recursive: true });
    symlinkSync(home, homeAlias);
    const reviewerDefinition = shippedAgents["monty-headless-reviewer"];
    const implementerDefinition = shippedAgents["monty-headless-worker"];
    writeFileSync(reviewer, reviewerDefinition);
    writeFileSync(implementer, implementerDefinition);
    writeFileSync(ping, JSON.stringify({
      version: 1, methods: ["ping", "spawn"], capabilities: { asyncSpawn: true },
    }));
    writeFileSync(instructions, "Task instructions\n");
    writeFileSync(context, "Task context\n");
    writeFileSync(snapshot, JSON.stringify({ tasks: [{
      key: "local:local-001", id: "local:local-001", project: "monty", status: "open",
      title: "Native Pi", branch: "monty/native-pi", action: "open", worker: { id: "native-pi" },
    }, {
      key: "local:local-002", id: "local:local-002", project: "monty", status: "open",
      title: "Plan from task", branch: null, action: "plan", worker: null,
    }, {
      key: "local:local-003", id: "local:local-003", project: "monty", status: "open",
      title: "Replacement plan", branch: null, action: "plan", worker: null,
    }] }));
    writeFileSync(entry, JSON.stringify({
      task: { key: "local:local-001", title: "Native Pi" },
      worker: { id: "native-pi" }, cwd: taskCwd, instructions, context, memory: join(root, "memory.md"),
    }));
    writeFileSync(fakeMonty, `#!/bin/sh\nprintf '%s\\n' "$*" >> "$MONTY_TEST_CLI_LOG"\ncase "$*" in\n  *"tasks list"*) cat "$MONTY_TEST_SNAPSHOT" ;;\n  *"task enter"*) cat "$MONTY_TEST_ENTRY" ;;\n  *"headless begin"*) printf '%s\\n' "$MONTY_TEST_HARNESS" ;;\n  *) echo "unexpected fake monty command: $*" >&2; exit 2 ;;\nesac\n`, { mode: 0o755 });
    writeFileSync(bridge, `import { appendFileSync, readFileSync } from "node:fs";
export default function (pi) {
  pi.events.on("subagents:rpc:v1:request", request => {
    appendFileSync(process.env.MONTY_TEST_RPC_LOG, JSON.stringify(request) + "\\n");
    const data = request.method === "ping"
      ? JSON.parse(readFileSync(process.env.MONTY_TEST_PING, "utf8")) : { ok: true };
    pi.events.emit("subagents:rpc:v1:reply:" + request.requestId, { success: true, data });
  });
  pi.registerCommand("test-tools", { description: "Record active tools", handler: () => {
    appendFileSync(process.env.MONTY_TEST_TOOL_LOG, JSON.stringify(pi.getActiveTools()) + "\\n");
  }});
}
`);

    const piCli = findPi();
    const piPackage = await import(pathToFileURL(join(dirname(piCli), "index.js")));
    const { RpcClient, SessionManager } = piPackage;
    process.env.PI_CODING_AGENT_DIR = agentDir;
    const extension = new URL("../pi-extension/extensions/monty.js", import.meta.url).pathname;
    client = new RpcClient({
      cliPath: piCli, cwd: home,
      env: {
        MONTY_HOME: homeAlias, MONTY_COMMAND: fakeMonty, MONTY_TEST_SNAPSHOT: snapshot,
        MONTY_TEST_ENTRY: entry, MONTY_TEST_RPC_LOG: rpcLog,
        MONTY_TEST_CLI_LOG: cliLog, MONTY_TEST_TOOL_LOG: toolLog, MONTY_TEST_PING: ping,
        MONTY_TEST_HARNESS: JSON.stringify({ harness_call: { arguments: harness } }),
        PI_CODING_AGENT_DIR: agentDir, PI_OFFLINE: "1",
      },
      args: ["--offline", "--no-extensions", "--no-context-files", "--extension", bridge, "--extension", extension],
    });
    await client.start();

    const commands = await client.getCommands();
    for (const name of ["monty", "monty-open", "monty-back", "monty-start", "monty-run", "monty-plan-cancel"])
      assert.ok(commands.some(command => command.name === name), `missing /${name}`);
    const head = await client.getState();
    assert.ok(head.model, "offline Pi selected a model for session materialization");
    assert.ok(head.sessionFile && existsSync(head.sessionFile), "head session was not materialized");
    const headEntries = await client.getEntries();
    const headMarker = headEntries.entries.find(e => e.type === "custom" && e.customType === HEAD);
    assert.equal(headMarker?.data?.home, realpathSync(home), "head markers use the physical Monty home");
    SessionManager.open(head.sessionFile).appendCustomEntry(HEAD, homeAlias);
    await client.setSessionName("Renamed head");

    await client.prompt("/monty-open local:local-001");
    const opened = await client.getState();
    const taskFile = opened.sessionFile;
    assert.ok(taskFile && taskFile !== head.sessionFile && existsSync(taskFile), "task session was materialized");
    assert.deepEqual([opened.model?.provider, opened.model?.id], [head.model.provider, head.model.id],
      "materializing the task preserved the selected model");
    const task = SessionManager.open(taskFile);
    assert.equal(realpathSync(task.getHeader().cwd), realpathSync(taskCwd), "task header has the active worktree cwd");
    assert.ok(task.getBranch().some(e => e.type === "custom_message" && e.customType === "monty-task-context:v1"),
      "task context was persisted on the active branch");
    const bootstrap = task.getBranch().find(e => e.type === "message" && e.message.role === "assistant");
    assert.deepEqual([bootstrap.message.api, bootstrap.message.provider, bootstrap.message.model],
      [head.model.api, head.model.provider, head.model.id], "the synthetic assistant records the selected model");

    const rpcCalls = () => readFileSync(rpcLog, "utf8").trim().split("\n").filter(Boolean).map(JSON.parse);
    const beginCalls = () => (readFileSync(cliLog, "utf8").match(/headless begin/g) || []).length;
    await client.prompt("/monty-run");
    assert.deepEqual(rpcCalls().map(call => call.method), ["ping", "spawn"], "/monty-run pings then spawns");
    assert.deepEqual(rpcCalls()[1].params, harness, "Monty forwards harness arguments unchanged");

    const callsBeforeForeignLink = rpcCalls().length;
    SessionManager.open(taskFile).appendCustomEntry("monty-task-link:v1", {
      home: join(root, "other-home"), head: head.sessionFile, key: "local:local-001",
      title: "Foreign task", worker: "foreign", cwd: taskCwd,
    });
    await client.switchSession(taskFile);
    await client.prompt("/monty-run");
    assert.equal(rpcCalls().length, callsBeforeForeignLink,
      "/monty-run rejected a cross-home task link before RPC preflight");
    SessionManager.open(taskFile).appendCustomEntry("monty-task-link:v1", {
      home, head: head.sessionFile, key: "local:local-001", title: "Native Pi",
      worker: "native-pi", cwd: taskCwd,
    });
    await client.switchSession(taskFile);

    const beginsBeforeBadPreflight = beginCalls();
    writeFileSync(ping, JSON.stringify({ version: 1, methods: ["ping"], capabilities: { asyncSpawn: false } }));
    await client.prompt("/monty-run");
    assert.equal(beginCalls(), beginsBeforeBadPreflight,
      "missing ping capabilities were rejected before headless begin");
    writeFileSync(ping, JSON.stringify({
      version: 1, methods: ["ping", "spawn"], capabilities: { asyncSpawn: true },
    }));
    writeFileSync(reviewer, reviewerDefinition.replace("tools: read, grep, find, ls, bash", "tools: read, grep, find, ls, bash, edit"));
    await client.prompt("/monty-run");
    assert.equal(beginCalls(), beginsBeforeBadPreflight,
      "malformed project reviewer definition was rejected before headless begin");
    writeFileSync(reviewer, reviewerDefinition);
    const projectSettings = join(home, ".pi", "settings.json");
    const userSettings = join(agentDir, "settings.json");
    const rejectedSettings = [
      [{ subagents: { agentOverrides: { unrelated: { disabled: "yes" } } } },
        "malformed unrelated override"],
      [{ subagents: { agentOverrides: { "monty-headless-reviewer": { model: "other/model" } } } },
        "target behavior override"],
      [{ subagents: { modelScope: { enforce: true } } }, "malformed modelScope"],
      [{ subagents: { defaultModel: false } }, "malformed top-level subagent field"],
      [{ subagents: { agentOverrides: { "monty-headless-reviewer": { disabled: true } } } },
        "project disable override"],
    ];
    for (const [settings, label] of rejectedSettings) {
      writeFileSync(projectSettings, JSON.stringify(settings));
      await client.prompt("/monty-run");
      assert.equal(beginCalls(), beginsBeforeBadPreflight, `${label} was rejected before headless begin`);
    }
    rmSync(projectSettings);
    writeFileSync(userSettings, "{ malformed user settings");
    await client.prompt("/monty-run");
    assert.equal(beginCalls(), beginsBeforeBadPreflight + 1,
      "malformed user settings were ignored for project-scoped dispatch");
    writeFileSync(userSettings, JSON.stringify({
      subagents: { agentOverrides: { "monty-headless-worker": { disabled: true } } },
    }));
    await client.prompt("/monty-run");
    assert.equal(beginCalls(), beginsBeforeBadPreflight + 2,
      "user agent overrides were ignored for project-scoped dispatch");
    writeFileSync(projectSettings, JSON.stringify({ subagents: {
      defaultModel: "global/default", disableBuiltins: false, disableThinking: false,
      modelScope: { enforce: true, allow: ["", "*"] },
      agentOverrides: {
        unrelated: { tools: ["read"] },
        "monty-headless-worker": { disabled: false },
        "monty-headless-reviewer": { disabled: false },
      },
    } }));
    await client.prompt("/monty-run");
    assert.equal(beginCalls(), beginsBeforeBadPreflight + 3,
      "valid project settings passed independently of the user override");
    rmSync(projectSettings);
    rmSync(userSettings);

    await client.setSessionName("Renamed task");
    await client.prompt("/monty-back");
    assert.equal((await client.getState()).sessionFile, head.sessionFile, "returned to the exact persisted head");

    const branchedTask = SessionManager.create(taskCwd, undefined, { parentSession: head.sessionFile });
    const beforeTaskLink = branchedTask.appendCustomEntry("test-before-task-link", {});
    branchedTask.appendCustomEntry("monty-task-link:v1", {
      home, head: head.sessionFile, key: "local:local-001", title: "Native Pi",
      worker: "native-pi", cwd: taskCwd,
    });
    branchedTask.appendMessage({ ...bootstrap.message, timestamp: Date.now() });
    const branchedTaskFile = branchedTask.getSessionFile();
    branchedTask.branch(beforeTaskLink);
    branchedTask.appendSessionInfo("Monty: branched before task link");
    await new Promise(resolveDelay => setTimeout(resolveDelay, 20));
    const newerDecoyHead = SessionManager.create(home);
    newerDecoyHead.appendCustomEntry(HEAD, { home: realpathSync(home) });
    newerDecoyHead.appendMessage({ ...bootstrap.message, timestamp: Date.now() });
    newerDecoyHead.appendSessionInfo("Newer decoy head");
    const newerDecoyHeadFile = newerDecoyHead.getSessionFile();
    assert.ok(existsSync(newerDecoyHeadFile), "the newer decoy head remained available");
    const navigationSessions = await SessionManager.listAll();
    const linkedHeadInfo = navigationSessions.find(info => realpathSync(info.path) === realpathSync(head.sessionFile));
    const decoyHeadInfo = navigationSessions.find(info => realpathSync(info.path) === realpathSync(newerDecoyHeadFile));
    assert.ok(decoyHeadInfo?.modified > linkedHeadInfo?.modified,
      "the global fallback would prefer the newer decoy head");
    await client.switchSession(branchedTaskFile);
    await client.prompt("/monty-back");
    assert.equal((await client.getState()).sessionFile, head.sessionFile,
      "/monty-back recovered the exact session-wide task head after branching before its marker");
    assert.ok(existsSync(newerDecoyHeadFile), "the newer global decoy was not removed during navigation");
    rmSync(newerDecoyHeadFile);

    const branchedHead = SessionManager.create(home);
    const beforeHead = branchedHead.appendCustomEntry("test-before-head", {});
    branchedHead.appendCustomEntry(HEAD, { home: realpathSync(home) });
    branchedHead.appendMessage({ ...bootstrap.message, timestamp: Date.now() });
    const branchedHeadFile = branchedHead.getSessionFile();
    branchedHead.branch(beforeHead);
    branchedHead.appendSessionInfo("Renamed branched head");
    SessionManager.open(taskFile).appendCustomEntry("monty-task-link:v1", {
      home, key: "local:local-001", title: "Native Pi", worker: "native-pi", cwd: join(root, "stale-cwd"),
    });
    await client.switchSession(taskFile);
    await client.prompt("/monty-back");
    assert.equal((await client.getState()).sessionFile, branchedHeadFile,
      "/monty-back discovered a renamed head after branching before its HEAD marker");
    await client.switchSession(taskFile);
    rmSync(branchedHeadFile);
    SessionManager.open(taskFile).appendCustomEntry("monty-task-link:v1", {
      home, head: head.sessionFile, key: "local:local-001", title: "Native Pi",
      worker: "native-pi", cwd: taskCwd,
    });
    await client.switchSession(taskFile);

    const decoy = SessionManager.create(taskCwd);
    const rootEntry = decoy.appendCustomEntry("test-root", {});
    decoy.appendCustomEntry("monty-task-link:v1", {
      home, key: "local:local-001", title: "Inactive branch", worker: "decoy", cwd: taskCwd,
    });
    decoy.branch(rootEntry);
    decoy.appendMessage({ ...bootstrap.message, timestamp: Date.now() });
    decoy.appendSessionInfo("Monty: inactive branch");

    await client.prompt("/monty-open local:local-001");
    assert.equal((await client.getState()).sessionFile, taskFile,
      "renamed task session was reused using header cwd instead of stale custom cwd");
    const allSessions = await SessionManager.listAll();
    const linked = [];
    for (const info of allSessions) {
      const sm = SessionManager.open(info.path);
      if (last(sm.getBranch(), "monty-task-link:v1")?.key === "local:local-001") linked.push(info.path);
    }
    assert.deepEqual(linked, [taskFile], "opening the renamed task did not create a duplicate session");

    writeFileSync(entry, JSON.stringify({
      task: { key: "local:local-001", title: "Native Pi" },
      worker: { id: "native-pi" }, cwd: nextTaskCwd, instructions, context,
      memory: join(root, "memory.md"),
    }));
    await client.prompt("/monty-open local:local-001");
    const successorFile = (await client.getState()).sessionFile;
    assert.notEqual(successorFile, taskFile, "a moved worktree forked a successor task session");
    assert.equal(last(SessionManager.open(taskFile).getEntries(), RETIRED)?.successor, successorFile,
      "the superseded task session received a session-wide tombstone");
    await new Promise(resolveDelay => setTimeout(resolveDelay, 20));
    const retiredDecoyHead = SessionManager.create(home);
    retiredDecoyHead.appendCustomEntry(HEAD, { home: realpathSync(home) });
    retiredDecoyHead.appendMessage({ ...bootstrap.message, timestamp: Date.now() });
    retiredDecoyHead.appendSessionInfo("Newer retired-navigation decoy head");
    const retiredDecoyHeadFile = retiredDecoyHead.getSessionFile();
    const retiredNavigationSessions = await SessionManager.listAll();
    const retiredLinkedHeadInfo = retiredNavigationSessions.find(info => realpathSync(info.path) === realpathSync(head.sessionFile));
    const retiredDecoyHeadInfo = retiredNavigationSessions.find(info => realpathSync(info.path) === realpathSync(retiredDecoyHeadFile));
    assert.ok(retiredDecoyHeadInfo?.modified > retiredLinkedHeadInfo?.modified,
      "retired-predecessor fallback would prefer the newer valid decoy head");
    const predecessor = SessionManager.open(taskFile);
    predecessor.branch(bootstrap.id);
    const callsBeforeRetiredBranch = rpcCalls().length;
    await client.switchSession(taskFile);
    await client.prompt("/monty-run");
    assert.equal(rpcCalls().length, callsBeforeRetiredBranch,
      "branching before the old task marker did not reactivate the retired session");
    await client.prompt("/monty-back");
    assert.equal((await client.getState()).sessionFile, head.sessionFile,
      "/monty-back honored the retired predecessor's exact persisted head");
    assert.ok(existsSync(retiredDecoyHeadFile),
      "retired-predecessor navigation did not remove the newer valid decoy head");
    rmSync(retiredDecoyHeadFile);
    await client.prompt("/monty-open local:local-001");
    assert.equal((await client.getState()).sessionFile, successorFile,
      "a retired predecessor did not trigger another successor fork");
    const activeLinked = [];
    for (const info of await SessionManager.listAll()) {
      const session = SessionManager.open(info.path);
      const data = last(session.getBranch(), "monty-task-link:v1");
      const retired = session.getEntries().some(e => e.type === "custom"
        && e.customType === RETIRED && e.data?.key === "local:local-001");
      if (data?.home === realpathSync(home) && !retired && data.key === "local:local-001") activeLinked.push(info.path);
    }
    assert.deepEqual(activeLinked, [successorFile], "only the successor retained an active task link");

    await client.prompt("/monty-open local:local-002");
    assert.equal((await client.getState()).sessionFile, head.sessionFile, "planning from a task switched to the head");
    const planBranch = SessionManager.open(head.sessionFile).getBranch();
    let marker = last(planBranch, PLAN);
    assert.equal(marker?.key, "local:local-002", "plan state was activated on the head branch");
    assert.equal(marker?.home, realpathSync(home), "plan markers use the physical Monty home");
    assert.ok(marker?.tools?.includes("bash"), "the head instance persisted its unrestricted tools before plan mode");
    const originalTools = marker.tools;
    const lastTools = () => JSON.parse(readFileSync(toolLog, "utf8").trim().split("\n").at(-1));
    await client.prompt("/test-tools");
    assert.ok(!lastTools().includes("bash"), "plan mode removed write-capable tools on the head");
    await client.prompt("/monty-open local:local-003");
    marker = last(SessionManager.open(head.sessionFile).getBranch(), PLAN);
    assert.equal(marker?.key, "local:local-003", "same-head re-planning replaced the active plan");
    assert.deepEqual(marker?.tools, originalTools,
      "same-head re-planning retained the original unrestricted tools");
    SessionManager.open(head.sessionFile).appendCustomEntry(PLAN, {
      home: join(root, "foreign-home"), enabled: false,
    });
    await client.prompt("/monty-open local:local-001");
    await client.prompt("/test-tools");
    assert.ok(lastTools().includes("bash"), "session shutdown restored tools before replacing the head");
    const ownedPlans = SessionManager.open(head.sessionFile).getBranch()
      .filter(e => e.type === "custom" && e.customType === PLAN && e.data?.home === realpathSync(home));
    assert.equal(ownedPlans.at(-1)?.data?.enabled, true,
      "switching away left plan state durable and ignored a foreign plan marker");
    await client.prompt("/monty-back");
    await client.prompt("/test-tools");
    assert.ok(!lastTools().includes("bash"), "returning to the planning head restored read-only mode");
    await client.abort();
    await client.prompt("/monty-plan-cancel");
    assert.equal(last(SessionManager.open(head.sessionFile).getBranch(), "monty-plan:v1")?.enabled, false);
  } finally {
    if (client) await client.stop().catch(() => {});
    if (oldAgentDir === undefined) delete process.env.PI_CODING_AGENT_DIR;
    else process.env.PI_CODING_AGENT_DIR = oldAgentDir;
    rmSync(root, { recursive: true, force: true });
  }
});
