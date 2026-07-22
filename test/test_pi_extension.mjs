import test from "node:test";
import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { find, last, latestPlan, row, safeCmd } from "../pi-extension/core.js";

const tasks = [
  { key: "local:local-001", id: "local:local-001", project: "monty", status: "open", title: "Native Pi", branch: null, worker: null },
  { key: "local:local-002", id: "github:owner/repo#2", project: "app", status: "open", title: "Fix app", branch: "cto/fix", worker: { id: "fix-app" } },
];

test("task lookup uses stable visible identities", () => {
  assert.equal(find(tasks, "local:local-001").title, "Native Pi");
  assert.equal(find(tasks, "fix-app").key, "local:local-002");
  assert.throws(() => find(tasks, "missing"), /No task/);
});

test("task rows contain only the five Monty columns", () => {
  const value = row(tasks[1]);
  for (const part of ["github:owner/repo#2", "app", "OPEN", "Fix app", "cto/fix"])
    assert.match(value, new RegExp(part.replace("#", "\\#")));
});

test("plan mode accepts only read-only shell pipelines", () => {
  assert.equal(safeCmd("git status"), true);
  assert.equal(safeCmd("rg task lib | head -20"), true);
  for (const cmd of ["git commit -am x", "cat a > b", "cat a | sh", "ls; rm -rf x", "python mutate.py"])
    assert.equal(safeCmd(cmd), false, cmd);
});

test("session state and plan extraction use the newest entries", () => {
  const entries = [
    { type: "custom", customType: "state", data: 1 },
    { type: "message", message: { role: "assistant", content: [{ type: "text", text: "Plan:\n1. First" }] } },
    { type: "custom", customType: "state", data: 2 },
  ];
  assert.equal(last(entries, "state"), 2);
  assert.match(latestPlan(entries), /First/);
});

test("the Pi extension has valid JavaScript syntax", () => {
  execFileSync(process.execPath, ["--check", new URL("../pi-extension/extensions/monty.js", import.meta.url).pathname]);
});
