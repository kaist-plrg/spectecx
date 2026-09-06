# SpecTecX for VS Code

Syntax highlighting and parse and elaboration diagnostics for SpecTecX specification files (`.spectec`).

## Install

Follow [Build from source](#build-from-source) to create `spectecx.vsix`, then run this command from the repository root:

```bash
code --install-extension editors/vscode/spectecx.vsix
```

For VSCodium, use `codium --install-extension editors/vscode/spectecx.vsix`.

## Build from source

Set up the OCaml toolchain following the root [README's Building the Project section](https://github.com/kaist-plrg/spectecx/blob/main/README.md#building-the-project), then run the following from the repository root with Node.js 22 or later installed:

```bash
make lsp
make vsix
```

The package is written to `editors/vscode/spectecx.vsix`. Packaging installs the locked JavaScript dependencies, including the language client. The server executable is built separately.

## Language server

In a trusted workspace, the extension looks for `spectecx-lsp` at an open workspace folder's root, then on `PATH`. Set `spectec.languageServer.path` to an executable path or command name to override discovery. Paths support `~` and `${workspaceFolder}`, using the first workspace folder for that substitution. An explicit path must resolve successfully.

Use **SpecTecX: Restart Language Server** after rebuilding the server. Set `spectec.languageServer.enable` to `false` to disable the server while retaining syntax highlighting. `spectec.languageServer.arguments` supplies command-line arguments, and `spectec.trace.server` controls protocol logging in the SpecTecX output channel.

The nearest ancestor directory containing a `*.spec` marker defines a spec, including its nested `.spectec` files. Without a marker, the server checks the file and its immediate `.spectec` siblings. It uses unsaved contents for every open file in that spec. Diagnostics are reported in the file that contains the error.

The server checks the whole spec synchronously after each edit. Large specs can delay diagnostic updates.

## Tests

`make test-lsp` tests the server and its editor protocol. From this directory, `npm test` checks extension startup, configuration and shutdown. `npm run test:host` checks multi-file diagnostics, editing recovery, server restart and startup cancellation in an isolated VS Code profile. It requires a built server and a VS Code executable on `PATH`, or the `VSCODE_EXECUTABLE` environment variable. It does not modify the normal editor profile. Successful runs remove their temporary profile, and failed runs retain it for inspection.
