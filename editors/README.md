# Editor support for SpecTecX

Integrations for `.spectec` files: syntax highlighting and language services.

## Syntax highlighting

Each subdirectory provides a highlighter. See its README to install.

- `vscode/` — a VS Code extension (TextMate grammar).
- `emacs/` — a tree-sitter major mode (`spectec-ts-mode`).
- `vim/` — a Vim/Neovim syntax file with filetype detection.

## Language server

`spectecx-lsp` checks open specs on each edit and reports parse and elaboration diagnostics. Build it from the repository root and put it on your `PATH`:

```bash
make lsp                                               # produces ./spectecx-lsp
ln -sf "$PWD/spectecx-lsp" ~/.local/bin/spectecx-lsp   # if ~/.local/bin is on PATH
```

The plugins below launch `spectecx-lsp` from `PATH`. VS Code also checks workspace roots and supports an explicit executable path.

### Neovim (0.11+)

The `vim/` plugin carries both filetype detection and the language-server config
(`vim/lsp/spectecx.lua`). Add `vim/` to your runtimepath (e.g. via your plugin
manager), then enable the server in your config:

```lua
vim.lsp.enable("spectecx")
```

Open a `.spectec` file and errors are underlined. (Classic Vim ignores the Lua
config and just uses the syntax file.)

### Emacs (eglot)

`spectec-ts-mode` registers the server with eglot, so no separate LSP config is
needed:

```elisp
(add-to-list 'load-path "/path/to/editors/emacs")
(require 'spectec-ts-mode)
```

Open a `.spectec` file and run `M-x eglot`.

### VS Code

Run `make vsix` to package the extension, then install `editors/vscode/spectecx.vsix`. The extension starts the language server in trusted workspaces. See [the VS Code README](vscode/README.md) for configuration and tests.

## Trying it without changing your config

```bash
printf 'syntax t = foo\n' > /tmp/t.spectec

# Neovim
nvim --cmd "set rtp^=$PWD/editors/vim" -c "lua vim.lsp.enable('spectecx')" /tmp/t.spectec

# Emacs (then M-x eglot)
emacs -Q -l editors/emacs/spectec-ts-mode.el /tmp/t.spectec
```

Either should underline `foo` as an undefined type.
