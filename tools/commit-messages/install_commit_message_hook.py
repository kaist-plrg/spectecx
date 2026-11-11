#!/usr/bin/env python3
"""Install the commit-message hook for all worktrees sharing this repository."""

import argparse
import os
from pathlib import Path
import subprocess
import sys


def git(*args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(["git", *args], capture_output=True, text=True)


def main() -> int:
    argparse.ArgumentParser(
        description="Install the commit-message hook for all worktrees sharing this repository."
    ).parse_args()
    worktree = git("rev-parse", "--show-toplevel")
    if worktree.returncode:
        print("Run this installer inside a Git worktree.", file=sys.stderr)
        return 1

    configured = git("config", "--get", "core.hooksPath")
    if configured.returncode != 1:
        if configured.returncode:
            print(configured.stderr.strip(), file=sys.stderr)
        else:
            print(
                "core.hooksPath is configured. Integrate the commit-msg hook "
                "with that hook setup explicitly. This installer does not change "
                "or write to configured hook directories.",
                file=sys.stderr,
            )
        return 1

    location = git("rev-parse", "--git-path", "hooks/commit-msg")
    if location.returncode:
        print(location.stderr.strip(), file=sys.stderr)
        return 1
    destination = Path(os.path.abspath(location.stdout.strip()))
    scripts = Path(__file__).resolve().parent
    helper = destination.parent / "commit-message-formatter.py"
    files = (
        (destination, (scripts / "hooks" / "commit-msg").read_bytes(), 0o755),
        (helper, (scripts / "format_commit_message.py").read_bytes(), 0o644),
    )
    missing = []
    for path, content, mode in files:
        if path.is_symlink() or path.exists():
            if path.is_file() and not path.is_symlink() and path.read_bytes() == content:
                continue
            print(
                f"Existing hook or formatter left unchanged: {path}\n"
                "Integrate the formatter manually, or move the installed hook "
                "and formatter aside before reinstalling. The installer never "
                "replaces different files.",
                file=sys.stderr,
            )
            return 1
        missing.append((path, content, mode))

    destination.parent.mkdir(parents=True, exist_ok=True)
    for path, content, mode in reversed(missing):
        try:
            with path.open("xb") as installed:
                installed.write(content)
        except FileExistsError:
            print(f"File appeared during installation. Left unchanged: {path}", file=sys.stderr)
            return 1
        path.chmod(mode)
    destination.chmod(destination.stat().st_mode | 0o111)
    status = "Installed" if missing else "Commit-message hook already installed"
    print(f"{status} at {destination}")
    print("The installed hook and formatter apply to all worktrees sharing this repository.")
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except OSError as error:
        print(f"Cannot install commit-message hook: {error}", file=sys.stderr)
        sys.exit(1)
