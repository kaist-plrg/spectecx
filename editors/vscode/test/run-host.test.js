const assert = require("node:assert/strict");
const { EventEmitter } = require("node:events");
const { test } = require("node:test");
const { load } = require("./load");

for (const outcome of [
  "success",
  "failed tests",
  "failed exit",
  "signal",
  "missing result",
  "launch failure",
  "timeout",
]) {
  test(`host runner handles ${outcome}`, async () => {
    let result;
    let removed;
    let killed = false;
    let launched = false;
    const child = new EventEmitter();
    child.exitCode = null;
    child.signalCode = null;
    child.kill = () => {
      killed = true;
    };
    const fs = {
      constants: { X_OK: 1 },
      accessSync() {},
      mkdirSync() {},
      writeFileSync() {},
      copyFileSync() {},
      cpSync() {},
      mkdtempSync: () => "/test/isolated-profile",
      existsSync: () => result !== undefined,
      readFileSync: () => JSON.stringify(result),
      rmSync: (directory) => {
        removed = directory;
      },
    };
    const spawn = (editor, args) => {
      assert.ok(args.includes("--wait"));
      assert.ok(args.includes("--user-data-dir"));
      return child;
    };
    const timer = (action) =>
      setImmediate(() => {
        if (!launched) {
          launched = true;
          if (outcome === "launch failure")
            child.emit("error", new Error("Could not launch editor"));
          else if (outcome !== "timeout") {
            if (["success", "failed tests", "failed exit"].includes(outcome)) {
              result = { passed: outcome !== "failed tests", message: "Test failed" };
            }
            child.exitCode = outcome === "failed exit" ? 1 : outcome === "signal" ? null : 0;
            child.signalCode = outcome === "signal" ? "SIGTERM" : null;
            child.emit("exit", child.exitCode, child.signalCode);
          }
        }
        action();
      });
    const runner = load(
      "test/run-host.js",
      { "node:fs": fs, "node:child_process": { spawn } },
      {
        process: { env: {} },
        setTimeout: timer,
        console: { log() {} },
      },
    );
    if (outcome === "success") {
      await runner.main();
      assert.equal(removed, "/test/isolated-profile");
    } else {
      await assert.rejects(
        runner.main(),
        /Test failed|VS Code exited|Could not launch editor|No test result/,
      );
      assert.equal(removed, undefined);
    }
    assert.equal(killed, outcome === "timeout");
  });
}
