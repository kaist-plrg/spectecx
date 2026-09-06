const fs = require("node:fs");
const os = require("node:os");
const path = require("node:path");
const { spawn } = require("node:child_process");
const { once } = require("node:events");
const vscode = require("vscode");
const { LanguageClient, CloseAction, State } = require("vscode-languageclient/node");

const SERVER_NAME = "spectecx-lsp";
const CONFIG_SECTION = "spectec";
const START_TIMEOUT_MS = 10000;

let stopClient;
let cancelStart;
/** @type {vscode.OutputChannel | undefined} */
let log;
let lifecycle = Promise.resolve();
let deactivating = false;

function enqueue(action) {
  const next = lifecycle.then(action);
  lifecycle = next.catch((error) => log?.appendLine(`server lifecycle failed: ${error}`));
  return next;
}

function config() {
  return vscode.workspace.getConfiguration(CONFIG_SECTION);
}

function expand(p) {
  let out = p.trim();
  if (out === "~" || out.startsWith(`~${path.sep}`) || out.startsWith("~/")) {
    out = path.join(os.homedir(), out.slice(1));
  }
  const folder = (vscode.workspace.workspaceFolders || [])[0];
  if (folder) {
    out = out.replace(/\$\{workspaceFolder\}/g, folder.uri.fsPath);
  }
  return out;
}

function isExecutableFile(p) {
  try {
    if (!fs.statSync(p).isFile()) return false;
    fs.accessSync(p, fs.constants.X_OK);
    return true;
  } catch {
    return false;
  }
}

function findExecutable(name, directories) {
  const names = [name];
  if (process.platform === "win32" && !path.extname(name)) {
    names.push(`${name}.exe`);
  }
  for (const directory of directories) {
    if (!directory) continue;
    for (const executable of names) {
      const candidate = path.join(directory, executable);
      if (isExecutableFile(candidate)) return candidate;
    }
  }
  return undefined;
}

function findOnPath(name) {
  return findExecutable(name, (process.env.PATH || "").split(path.delimiter));
}

function findInWorkspace() {
  const folders = vscode.workspace.workspaceFolders || [];
  return findExecutable(
    SERVER_NAME,
    folders.map((folder) => folder.uri.fsPath),
  );
}

/** @returns {{command: string} | {error: string}} */
function resolveServer() {
  const configured = config().get("languageServer.path", "").trim();
  if (configured) {
    const p = expand(configured);
    if (path.isAbsolute(p) || p.includes(path.sep) || p.includes("/")) {
      return isExecutableFile(p)
        ? { command: p }
        : {
            error: `\`${CONFIG_SECTION}.languageServer.path\` is not an executable file: ${p}`,
          };
    }
    const found = findOnPath(p);
    return found
      ? { command: found }
      : {
          error: `\`${CONFIG_SECTION}.languageServer.path\` is set to \`${p}\`, which is not on PATH`,
        };
  }
  const found = findInWorkspace() || findOnPath(SERVER_NAME);
  return found
    ? { command: found }
    : { error: `\`${SERVER_NAME}\` is not in this workspace or on PATH` };
}

async function reportResolutionError(message) {
  log?.appendLine(`server not started: ${message}`);
  const choice = await vscode.window.showWarningMessage(
    `SpecTecX: ${message}. Build the server with \`make lsp\` and put the executable on PATH, ` +
      `or point \`${CONFIG_SECTION}.languageServer.path\` at the server executable. Syntax highlighting still works.`,
    "Open Settings",
    "Show Log",
  );
  if (choice === "Open Settings") {
    await vscode.commands.executeCommand(
      "workbench.action.openSettings",
      `${CONFIG_SECTION}.languageServer.path`,
    );
  } else if (choice === "Show Log") {
    log?.show();
  }
}

async function start() {
  if (deactivating || !vscode.workspace.isTrusted) return;
  if (!config().get("languageServer.enable", true)) {
    log?.appendLine(`disabled by ${CONFIG_SECTION}.languageServer.enable`);
    return;
  }
  const resolved = resolveServer();
  if ("error" in resolved) {
    void reportResolutionError(resolved.error).catch((error) =>
      log?.appendLine(`could not show server settings: ${error}`),
    );
    return;
  }
  log?.appendLine(`starting ${resolved.command}`);

  let serverProcess;
  let cancelled = false;
  let timedOut = false;
  let timer;
  const terminate = () => {
    cancelled = true;
    clearTimeout(timer);
    if (serverProcess?.exitCode === null && serverProcess.signalCode === null) {
      serverProcess.kill("SIGKILL");
    }
  };
  const args = config().get("languageServer.arguments", []);
  const serverOptions = async () => {
    if (cancelled) throw new Error("Server startup cancelled");
    serverProcess = spawn(resolved.command, args, {
      cwd: vscode.workspace.workspaceFolders?.[0]?.uri.fsPath,
      windowsHide: true,
    });
    serverProcess.on("error", (error) => log?.appendLine(`server process failed: ${error}`));
    cancelStart = terminate;
    timer = setTimeout(() => {
      timedOut = true;
      log?.appendLine("server initialization timed out after 10 seconds");
      terminate();
    }, START_TIMEOUT_MS);
    await once(serverProcess, "spawn");
    return serverProcess;
  };
  const clientOptions = {
    documentSelector: [{ scheme: "file", language: "spectec" }],
    outputChannel: log,
    // The extension owns startup failure cleanup.
    initializationFailedHandler: (error) => {
      throw error;
    },
    errorHandler: {
      error: (...args) => defaultErrors.error(...args),
      closed: () =>
        cancelled ? { action: CloseAction.DoNotRestart, handled: true } : defaultErrors.closed(),
    },
  };

  const starting = new LanguageClient(
    CONFIG_SECTION,
    "SpecTecX Language Server",
    serverOptions,
    clientOptions,
  );
  const defaultErrors = starting.createDefaultErrorHandler();
  let shutdownPromise;
  const shutdown = () => {
    shutdownPromise ??= (async () => {
      cancelled = true;
      clearTimeout(timer);
      if (cancelStart === terminate) cancelStart = undefined;
      try {
        if (starting.isRunning()) await starting.stop();
      } finally {
        terminate();
        stateChange.dispose();
        await starting
          .dispose()
          .catch((error) => log?.appendLine(`client disposal failed: ${error}`));
      }
    })();
    return shutdownPromise;
  };
  let failurePromise;
  const handleFailure = (error) => {
    failurePromise ??= (async () => {
      const reportFailure = !cancelled || timedOut;
      await shutdown();
      log?.appendLine(`failed to start: ${error}`);
      if (reportFailure) {
        void Promise.resolve(
          vscode.window.showErrorMessage(
            `SpecTecX: could not start ${resolved.command}. See the SpecTecX output channel.`,
          ),
        ).catch((error) => log?.appendLine(`could not show startup error: ${error}`));
      }
    })();
    return failurePromise;
  };
  const stateChange = starting.onDidChangeState(({ newState }) => {
    if (newState === State.Starting) {
      // LanguageClient 9.0.1 can discard its shared startup promise on disconnect.
      void starting
        .start()
        .catch(handleFailure)
        .catch((error) => log?.appendLine(`startup cleanup failed: ${error}`));
    } else {
      clearTimeout(timer);
      if (cancelStart === terminate) cancelStart = undefined;
    }
  });
  stopClient = shutdown;
  cancelStart = terminate;
  try {
    await starting.start();
  } catch (error) {
    await handleFailure(error);
  } finally {
    clearTimeout(timer);
    if (cancelStart === terminate) cancelStart = undefined;
  }
}

async function stop() {
  const shutdown = stopClient;
  stopClient = undefined;
  await shutdown?.();
}

async function restart() {
  cancelStart?.();
  return enqueue(async () => {
    await stop();
    await start();
  });
}

/** @param {vscode.ExtensionContext} context */
function activate(context) {
  deactivating = false;
  log = vscode.window.createOutputChannel("SpecTecX");
  context.subscriptions.push(
    log,
    vscode.commands.registerCommand(`${CONFIG_SECTION}.restartLanguageServer`, restart),
    vscode.workspace.onDidChangeConfiguration((event) => {
      if (event.affectsConfiguration(`${CONFIG_SECTION}.languageServer`)) {
        void restart().catch((error) => log?.appendLine(`restart failed: ${error}`));
      }
    }),
  );
  enqueue(start);
}

async function deactivate() {
  deactivating = true;
  cancelStart?.();
  await enqueue(stop);
}

module.exports = { activate, deactivate };
