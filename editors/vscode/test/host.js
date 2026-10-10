const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vscode = require("vscode");

async function eventually(action, accept, label, attempts = 100) {
  for (let attempt = 0; attempt < attempts; attempt += 1) {
    const value = await action();
    if (accept(value)) return value;
    await new Promise((resolve) => setTimeout(resolve, 100));
  }
  throw new Error(`Timed out: ${label}`);
}

async function run() {
  const root = vscode.workspace.workspaceFolders[0].uri.fsPath;
  const uri = vscode.Uri.file(path.join(root, "impty/spec.spectec"));
  const document = await vscode.workspace.openTextDocument(uri);
  assert.equal(document.languageId, "spectec");
  await vscode.window.showTextDocument(document);
  await vscode.extensions.getExtension("kaist-plrg.spectecx-syntax").activate();

  async function replace(doc, before, after) {
    const start = doc.getText().indexOf(before);
    assert.ok(start >= 0, `Missing text: ${before}`);
    const range = new vscode.Range(doc.positionAt(start), doc.positionAt(start + before.length));
    const edit = new vscode.WorkspaceEdit();
    edit.replace(doc.uri, range, after);
    assert.ok(await vscode.workspace.applyEdit(edit));
    assert.ok(doc.isDirty);
  }

  const hasError = (diagnostics) =>
    diagnostics.some((d) => d.severity === vscode.DiagnosticSeverity.Error);
  const isClear = (diagnostics) => diagnostics.length === 0;
  const diagnostics = (doc, accept, label) =>
    eventually(() => vscode.languages.getDiagnostics(doc.uri), accept, label);
  const assignment = "env |- (x '=' e) -| (x -> v)::env";
  const invalidAssignment = "env |- (x '=' e) -| (x -> e)::env";
  await replace(document, assignment, invalidAssignment);
  await diagnostics(document, hasError, "invalid assignment");
  await replace(document, invalidAssignment, assignment);
  await diagnostics(document, isClear, "assignment recovery");

  const definitions = await vscode.workspace.openTextDocument(
    vscode.Uri.file(path.join(root, "multifile/1-def.spectec")),
  );
  const use = await vscode.workspace.openTextDocument(
    vscode.Uri.file(path.join(root, "multifile/2-use.spectec")),
  );
  await replace(use, "syntax t = foo", "syntax t = fresh");
  await diagnostics(use, hasError, "missing sibling definition");
  await replace(definitions, "syntax foo", "syntax fresh");
  await diagnostics(use, isClear, "unsaved sibling definition");
  await replace(definitions, "syntax fresh", "syntax foo");
  await diagnostics(use, hasError, "changed sibling definition");
  await replace(use, "syntax t = fresh", "syntax t = foo");
  await diagnostics(use, isClear, "sibling recovery");

  await vscode.commands.executeCommand("spectec.restartLanguageServer");
  await replace(document, assignment, invalidAssignment);
  await diagnostics(document, hasError, "diagnostics after restart");
  await replace(document, invalidAssignment, assignment);
  await diagnostics(document, isClear, "recovery after restart");
  const configuration = vscode.workspace.getConfiguration("spectec");
  const server = configuration.get("languageServer.path");
  const pidFile = path.join(root, "silent-server.pid");
  const readPid = () =>
    fs.existsSync(pidFile) ? Number(fs.readFileSync(pidFile, "utf8")) : undefined;
  const isStopped = (pid) => {
    try {
      process.kill(pid, 0);
      return false;
    } catch (error) {
      if (error.code !== "ESRCH") throw error;
      return true;
    }
  };
  const target = vscode.ConfigurationTarget.Workspace;
  try {
    await configuration.update("languageServer.enable", false, target);
    await configuration.update("languageServer.path", process.env.SPECTEC_TEST_NODE, target);
    await configuration.update(
      "languageServer.arguments",
      [path.join(__dirname, "silent-server.js"), pidFile],
      target,
    );
    await configuration.update("languageServer.enable", true, target);
    const firstPid = await eventually(readPid, Number.isInteger, "silent server startup");
    const restarting = vscode.commands.executeCommand("spectec.restartLanguageServer");
    const secondPid = await eventually(
      readPid,
      (pid) => Number.isInteger(pid) && pid !== firstPid,
      "cancelled startup restart",
    );
    await eventually(() => isStopped(firstPid), Boolean, "cancelled process exit");
    await configuration.update("languageServer.enable", false, target);
    await restarting;
    await eventually(() => isStopped(secondPid), Boolean, "disabled process exit");
    await configuration.update("languageServer.enable", true, target);
    const timedOutPid = await eventually(
      readPid,
      (pid) => Number.isInteger(pid) && pid !== secondPid,
      "timeout server startup",
    );
    await eventually(() => isStopped(timedOutPid), Boolean, "initialization timeout", 150);

    await configuration.update("languageServer.enable", false, target);
    const attemptsFile = path.join(root, "restart-attempts.json");
    const attempts = () =>
      fs.existsSync(attemptsFile) ? JSON.parse(fs.readFileSync(attemptsFile, "utf8")) : [];
    await configuration.update(
      "languageServer.arguments",
      [path.join(__dirname, "restart-server.js"), attemptsFile],
      target,
    );
    await configuration.update("languageServer.enable", true, target);
    const running = await eventually(attempts, (runs) => runs[0]?.ready, "server before crash");
    process.kill(running[0].pid, "SIGKILL");
    const failed = await eventually(
      attempts,
      (runs) => runs[1]?.rejected,
      "automatic restart initialization failure",
    );
    await eventually(() => isStopped(failed[1].pid), Boolean, "failed restart process cleanup");
    await vscode.commands.executeCommand("spectec.restartLanguageServer");
    await eventually(
      attempts,
      (runs) => runs[2]?.ready,
      "manual restart after initialization failure",
    );
  } finally {
    await configuration.update("languageServer.enable", false, target);
    await configuration.update("languageServer.path", server, target);
    await configuration.update("languageServer.arguments", [], target);
    await configuration.update("languageServer.enable", true, target);
  }
  await replace(document, assignment, invalidAssignment);
  await diagnostics(document, hasError, "diagnostics after cancelled startup");
  await replace(document, invalidAssignment, assignment);
  await diagnostics(document, isClear, "recovery after cancelled startup");
  console.log(
    "SpecTecX extension host: multi-file diagnostics, unsaved editing recovery and restart passed",
  );
}

module.exports = {
  run: async () => {
    const unhandled = [];
    const onRejection = (error) => unhandled.push(error);
    process.on("unhandledRejection", onRejection);
    const resultFile = path.join(
      path.dirname(vscode.workspace.workspaceFolders[0].uri.fsPath),
      "result.json",
    );
    try {
      await run();
      assert.deepEqual(unhandled, [], "Unhandled promise rejections");
      fs.writeFileSync(resultFile, JSON.stringify({ passed: true }));
    } catch (error) {
      fs.writeFileSync(resultFile, JSON.stringify({ passed: false, message: error.stack }));
      throw error;
    } finally {
      process.off("unhandledRejection", onRejection);
    }
  },
};
