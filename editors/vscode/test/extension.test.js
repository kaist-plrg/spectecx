const assert = require("node:assert/strict");
const { EventEmitter } = require("node:events");
const path = require("node:path");
const { test } = require("node:test");
const { load, deferred, flush } = require("./load");

function setup({
  platform = process.platform,
  files,
  startup,
  spawnProcess,
  globals,
  executable = true,
} = {}) {
  const commands = {};
  const clients = [];
  const children = [];
  const warnings = [];
  const errors = [];
  const messages = [];
  const settings = { "languageServer.path": "/test/server" };
  const disposable = { dispose() {} };
  let onConfiguration;
  const State = { Stopped: 1, Starting: 2, Running: 3 };
  const CloseAction = { DoNotRestart: 1, Restart: 2 };
  class Client {
    constructor(id, name, server, options) {
      this.server = server;
      this.options = options;
      clients.push(this);
    }
    createDefaultErrorHandler() {
      return { error() {}, closed: () => ({ action: CloseAction.Restart }) };
    }
    onDidChangeState(action) {
      this.onState = action;
      return disposable;
    }
    isRunning() {
      return this.running;
    }
    async start() {
      if (this.started) return this.started;
      const ready = deferred();
      this.started = ready.promise;
      this.onState({ newState: State.Starting });
      try {
        this.child = await this.server();
        const exited = new Promise((resolve, reject) => {
          this.child.once("exit", () => reject(new Error("Server exited")));
        });
        await Promise.race([exited, startup?.(this) ?? Promise.resolve()]);
        this.running = true;
        this.onState({ newState: State.Running });
        ready.resolve();
      } catch (error) {
        this.running = false;
        this.onState({ newState: State.Stopped });
        ready.reject(error);
        this.options.initializationFailedHandler(error);
      }
    }
    async stop() {
      this.stopped = true;
      this.running = false;
      this.onState({ newState: State.Stopped });
    }
    async dispose() {
      this.disposed = true;
    }
  }
  const vscode = {
    workspace: {
      isTrusted: true,
      workspaceFolders: [{ uri: { fsPath: "/test" } }],
      getConfiguration: () => ({ get: (key, fallback) => settings[key] ?? fallback }),
      onDidChangeConfiguration: (action) => {
        onConfiguration = action;
        return disposable;
      },
    },
    commands: {
      registerCommand: (name, action) => {
        commands[name] = action;
        return disposable;
      },
    },
    window: {
      createOutputChannel: () => ({
        appendLine: (message) => messages.push(message),
        dispose() {},
      }),
      showWarningMessage: async (message) => {
        warnings.push(message);
      },
      showErrorMessage: async (message) => {
        errors.push(message);
      },
    },
  };
  const spawn =
    spawnProcess ??
    ((command, args) => {
      const child = new EventEmitter();
      Object.assign(child, { command, args, exitCode: null, signalCode: null });
      child.kill = (signal) => {
        child.signalCode = signal;
        setImmediate(() => child.emit("exit", null, signal));
        return true;
      };
      children.push(child);
      setImmediate(() => child.emit("spawn"));
      return child;
    });
  const extension = load(
    "extension.js",
    {
      vscode,
      "node:fs": {
        statSync: (file) => ({ isFile: () => !files || files.includes(file.toLowerCase()) }),
        accessSync() {
          if (!executable) throw new Error("Permission denied");
        },
        constants: { X_OK: 1 },
      },
      "node:path": platform === "win32" ? path.win32 : path.posix,
      "node:child_process": { spawn },
      "vscode-languageclient/node": { LanguageClient: Client, CloseAction, State },
    },
    {
      process: { platform, env: { PATH: platform === "win32" ? "C:\\tools" : "/tools" } },
      ...globals,
    },
  );
  const changed = (affects = true) => onConfiguration({ affectsConfiguration: () => affects });
  const activate = async () => {
    extension.activate({ subscriptions: [] });
    await flush();
    await flush();
  };
  return {
    extension,
    activate,
    settings,
    vscode,
    commands,
    clients,
    children,
    warnings,
    errors,
    messages,
    changed,
  };
}

test("keeps the extension identity and file association", () => {
  const pkg = require("../package.json");
  assert.equal(pkg.name, "spectecx-syntax");
  assert.equal(pkg.publisher, "kaist-plrg");
  assert.deepEqual(
    pkg.contributes.languages.map((lang) => lang.id),
    ["spectec"],
  );
  assert.deepEqual(pkg.contributes.languages[0].extensions, [".spectec"]);
  assert.equal(pkg.capabilities.untrustedWorkspaces.supported, false);
  assert.deepEqual(Object.keys(pkg.contributes.configuration.properties).sort(), [
    "spectec.languageServer.arguments",
    "spectec.languageServer.enable",
    "spectec.languageServer.path",
    "spectec.trace.server",
  ]);
  assert.deepEqual(
    pkg.contributes.commands.map((command) => command.command),
    ["spectec.restartLanguageServer"],
  );
});

for (const mode of [
  "untrusted",
  "disabled",
  "missing",
  "not-executable",
  "configured",
  "workspace",
]) {
  test(`handles ${mode} server startup`, async () => {
    const context = setup({
      files: mode === "missing" ? [] : undefined,
      executable: mode !== "not-executable",
    });
    if (mode === "untrusted") context.vscode.workspace.isTrusted = false;
    if (mode === "disabled") context.settings["languageServer.enable"] = false;
    context.settings["languageServer.path"] =
      mode === "workspace" ? "" : "${workspaceFolder}/server";
    await context.activate();
    if (["untrusted", "disabled", "missing", "not-executable"].includes(mode)) {
      assert.equal(context.clients.length, 0);
      assert.equal(context.warnings.length, ["missing", "not-executable"].includes(mode) ? 1 : 0);
    } else {
      assert.equal(
        context.children[0].command,
        mode === "workspace" ? "/test/spectecx-lsp" : "/test/server",
      );
      assert.equal(context.clients[0].options.documentSelector[0].scheme, "file");
    }
    await context.extension.deactivate();
    if (context.clients.length) assert.equal(context.clients[0].stopped, true);
  });
}

test("reports startup failure and allows a retry", async () => {
  let attempts = 0;
  const context = setup({
    startup: () => {
      if (attempts++ === 0) return Promise.reject(new Error("start failed"));
    },
  });
  await context.activate();
  assert.equal(context.errors.length, 1);
  assert.equal(context.children[0].signalCode, "SIGKILL");
  assert.equal(context.clients[0].disposed, true);
  await context.commands["spectec.restartLanguageServer"]();
  assert.equal(context.clients.length, 2);
  await context.extension.deactivate();
});

test("reports a process launch failure", async () => {
  const context = setup({
    spawnProcess: () => {
      const child = new EventEmitter();
      setImmediate(() => child.emit("error", new Error("Executable disappeared")));
      return child;
    },
  });
  await context.activate();
  assert.equal(context.errors.length, 1);
  assert.equal(context.clients[0].disposed, true);
  assert.ok(context.messages.some((message) => message.includes("Executable disappeared")));
  await context.extension.deactivate();
});

test("cleans up initialization failure after the client restarts itself", async () => {
  let attempts = 0;
  const context = setup({
    startup: () => {
      if (++attempts === 2) return Promise.reject(new Error("restart failed"));
    },
  });
  try {
    await context.activate();
    const client = context.clients[0];
    context.children[0].kill("SIGKILL");
    client.running = false;
    client.started = undefined;
    await assert.rejects(client.start(), /restart failed/);
    await flush();
    assert.equal(context.children[1].signalCode, "SIGKILL");
    assert.equal(client.disposed, true);
    assert.equal(context.errors.length, 1);
    await context.commands["spectec.restartLanguageServer"]();
    assert.equal(context.clients[1].running, true);
  } finally {
    await context.extension.deactivate();
  }
});

test("serializes restarts and applies changed arguments", async () => {
  const context = setup();
  await context.activate();
  context.changed(false);
  await flush();
  assert.equal(context.clients.length, 1);
  const restart = context.commands["spectec.restartLanguageServer"];
  await Promise.all([restart(), restart()]);
  assert.equal(context.clients.length, 3);
  assert.equal(context.clients[0].stopped, true);
  assert.equal(context.clients[1].stopped, true);
  context.settings["languageServer.arguments"] = ["--test"];
  context.changed();
  await flush();
  await flush();
  assert.deepEqual(context.children[3].args, ["--test"]);
  await context.extension.deactivate();
  await restart();
  assert.equal(context.clients.length, 4);
});

for (const action of ["restart", "disable", "deactivate"]) {
  test(`${action} cancels a pending startup`, async () => {
    let attempts = 0;
    const context = setup({ startup: () => (attempts++ === 0 ? deferred().promise : undefined) });
    await context.activate();
    if (action === "restart") await context.commands["spectec.restartLanguageServer"]();
    else if (action === "disable") {
      context.settings["languageServer.enable"] = false;
      context.changed();
      await flush();
      await flush();
    } else await context.extension.deactivate();
    assert.equal(context.children[0].signalCode, "SIGKILL");
    assert.equal(context.clients[0].disposed, true);
    assert.equal(context.clients[0].options.errorHandler.closed().action, 1);
    assert.equal(context.errors.length, 0);
    assert.equal(context.clients.length, action === "restart" ? 2 : 1);
    await context.extension.deactivate();
  });
}

test("startup timeout kills a real unresponsive process", async () => {
  const { spawn } = require("node:child_process");
  let child;
  const context = setup({
    spawnProcess: () => {
      child = spawn(process.execPath, ["-e", "setInterval(() => {}, 1000)"]);
      return child;
    },
    startup: () => deferred().promise,
    globals: { setTimeout: (action) => setTimeout(action, 30) },
  });
  try {
    await context.activate();
    await new Promise((resolve) => child.once("exit", resolve));
    await flush();
    assert.equal(child.signalCode, "SIGKILL");
    assert.equal(context.errors.length, 1);
    assert.ok(context.messages.some((message) => message.includes("timed out")));
    await context.extension.deactivate();
  } finally {
    if (child?.exitCode === null && child.signalCode === null) child.kill("SIGKILL");
  }
});

for (const [configured, expected] of [
  ["spectecx-lsp", "C:\\tools\\spectecx-lsp.exe"],
  ["spectecx-lsp.exe", "C:\\tools\\spectecx-lsp.exe"],
  ["C:/tools/spectecx-lsp.exe", "C:/tools/spectecx-lsp.exe"],
  ["${workspaceFolder}\\spectecx-lsp.exe", "C:\\workspace\\spectecx-lsp.exe"],
  ["", "C:\\workspace\\spectecx-lsp.exe"],
]) {
  test(`resolves Windows executable ${configured || "in workspace"}`, async () => {
    const context = setup({ platform: "win32", files: [expected.toLowerCase()] });
    context.settings["languageServer.path"] = configured;
    context.vscode.workspace.workspaceFolders = [{ uri: { fsPath: "C:\\workspace" } }];
    await context.activate();
    assert.equal(context.warnings.length, 0);
    assert.equal(context.children[0].command, expected);
    await context.extension.deactivate();
  });
}

for (const failure of ["warning", "settings", "startup error"]) {
  test(`logs a rejected ${failure} notification`, async () => {
    const context = setup({
      files: failure === "startup error" ? undefined : [],
      startup: () => Promise.reject(new Error("failed")),
    });
    const reject = async () => {
      throw new Error("UI unavailable");
    };
    if (failure === "warning") context.vscode.window.showWarningMessage = reject;
    else if (failure === "settings") {
      context.vscode.window.showWarningMessage = async () => "Open Settings";
      context.vscode.commands.executeCommand = reject;
    } else context.vscode.window.showErrorMessage = reject;
    await context.activate();
    assert.ok(context.messages.some((message) => message.includes("UI unavailable")));
    await context.extension.deactivate();
  });
}
