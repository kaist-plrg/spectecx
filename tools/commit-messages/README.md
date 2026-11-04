# Commit-message tools

This directory holds the commit-message checker used by CI, the formatter, the optional `commit-msg` hook and installer, and their tests.

The optional `commit-msg` hook runs [format_commit_message.py](format_commit_message.py) on each commit message before Git records it. The formatter wraps body prose at 72 characters without splitting URLs or identifiers. For an overlong `Fixes: HASH ("subject")` record, it puts the complete quoted subject on a second line, indented by one space. Subjects, code, quotes, and other source and credit records stay unchanged.

Run the commands below from the repository root. Python 3 is required to run the hook. Installation and removal affect all worktrees sharing this repository and leave Git configuration unchanged.

## Install

```bash
python3 tools/commit-messages/install_commit_message_hook.py
```

The installer copies `commit-msg` and `commit-message-formatter.py` into the hooks directory. It refuses to replace different existing files or to use a custom hooks directory (`core.hooksPath`).

## Install manually

Manual installation also refuses custom hooks directories and existing destination files.

```bash
(
    set -euC
    git rev-parse --show-toplevel >/dev/null
    if git config --get core.hooksPath; then
        echo 'core.hooksPath is configured. Integrate with that hook setup manually.' >&2
        exit 1
    else
        test "$?" -eq 1
    fi

    hook_dir=$(git rev-parse --git-path hooks)
    for name in commit-msg commit-message-formatter.py; do
        if [ -e "$hook_dir/$name" ] || [ -L "$hook_dir/$name" ]; then
            echo "Existing file left unchanged: $hook_dir/$name" >&2
            exit 1
        fi
    done

    mkdir -p "$hook_dir"
    cat tools/commit-messages/format_commit_message.py > "$hook_dir/commit-message-formatter.py"
    chmod 644 "$hook_dir/commit-message-formatter.py"
    cat tools/commit-messages/hooks/commit-msg > "$hook_dir/commit-msg"
    chmod 755 "$hook_dir/commit-msg"
    echo "Installed hook and formatter at $hook_dir"
)
```

## Remove or update

These commands remove the installed files only if both match this checkout.

```bash
(
    set -eu
    hook_dir=$(git rev-parse --git-path hooks)
    test ! -L "$hook_dir/commit-msg"
    test ! -L "$hook_dir/commit-message-formatter.py"
    cmp "$hook_dir/commit-msg" tools/commit-messages/hooks/commit-msg
    cmp "$hook_dir/commit-message-formatter.py" tools/commit-messages/format_commit_message.py
    rm "$hook_dir/commit-msg" "$hook_dir/commit-message-formatter.py"
)
```

If a file differs, inspect it before removing or moving it aside. To update, remove or move aside the installed files, then reinstall from the desired checkout.

## Format and check without the hook

```bash
python3 tools/commit-messages/format_commit_message.py --write MESSAGE_FILE
python3 tools/commit-messages/format_commit_message.py --check MESSAGE_FILE
python3 tools/commit-messages/check_auto.py --messages-only origin/main..HEAD
```

`--write` updates the message file. `--check` reports wrapping errors without editing it. The last command checks commit messages since `origin/main`.
