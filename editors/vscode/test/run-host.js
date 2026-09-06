const fs = require("node:fs");
const os = require("node:os");
const path = require("node:path");
const { spawn } = require("node:child_process");

async function main() {
  const root = path.resolve(__dirname, "../../..");
  const server =
    process.env.SPECTEC_TEST_SERVER || path.join(root, "_build/default/spectec/bin/lsp_main.exe");
  fs.accessSync(server, fs.constants.X_OK);
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), "spectec-editor-test-"));
  const workspace = path.join(directory, "workspace");
  fs.mkdirSync(workspace);
  fs.mkdirSync(path.join(workspace, ".vscode"));
  fs.writeFileSync(
    path.join(workspace, ".vscode/settings.json"),
    JSON.stringify({
      "spectec.languageServer.path": server,
      "spectec.trace.server": "verbose",
      "telemetry.telemetryLevel": "off",
      "update.mode": "none",
    }),
  );
  const imp = path.join(workspace, "impty");
  fs.mkdirSync(imp);
  for (const file of ["spec.spectec", "base.spec"]) {
    fs.copyFileSync(path.join(root, "spectec/specs/impty/base", file), path.join(imp, file));
  }
  fs.cpSync(path.join(root, "spectec/test/lsp/multifile"), path.join(workspace, "multifile"), {
    recursive: true,
  });
  const resultFile = path.join(directory, "result.json");
  const editor = process.env.VSCODE_EXECUTABLE || "code";
  const extension = process.env.SPECTEC_TEST_EXTENSION || path.resolve(__dirname, "..");
  const child = spawn(
    editor,
    [
      "--new-window",
      "--wait",
      "--no-sandbox",
      "--skip-welcome",
      "--skip-release-notes",
      "--disable-updates",
      "--disable-extensions",
      "--disable-workspace-trust",
      "--user-data-dir",
      path.join(directory, "profile"),
      "--extensions-dir",
      path.join(directory, "extensions"),
      `--extensionDevelopmentPath=${extension}`,
      `--extensionTestsPath=${path.join(__dirname, "host.js")}`,
      workspace,
    ],
    { stdio: "inherit", env: { ...process.env, SPECTEC_TEST_NODE: process.execPath } },
  );
  let launchError;
  let exited = false;
  child.on("error", (error) => {
    launchError = error;
  });
  child.on("exit", () => {
    exited = true;
  });
  let passed = false;
  console.log(`Isolated VS Code test: ${directory}`);
  try {
    for (let attempt = 0; attempt < 300; attempt += 1) {
      if (launchError) throw launchError;
      if (exited && child.exitCode !== 0) {
        throw new Error(
          `VS Code exited with ${child.signalCode || child.exitCode}. Logs: ${directory}`,
        );
      }
      if (fs.existsSync(resultFile)) {
        const result = JSON.parse(fs.readFileSync(resultFile, "utf8"));
        if (!result.passed) throw new Error(result.message);
        if (exited) {
          passed = true;
          console.log("Extension-host tests passed.");
          return;
        }
      } else if (exited) {
        throw new Error(`VS Code exited without a test result. Logs: ${directory}`);
      }
      await new Promise((resolve) => setTimeout(resolve, 200));
    }
    throw new Error(`No test result within 60 seconds. Logs: ${directory}`);
  } finally {
    if (!exited && !launchError) child.kill();
    if (passed) fs.rmSync(directory, { recursive: true, force: true });
  }
}

if (require.main === module) {
  main().catch((error) => {
    console.error(error);
    process.exitCode = 1;
  });
}

module.exports = { main };
