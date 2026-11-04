"""Test the optional hook in isolated Git repositories."""

import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]
MESSAGE = (
    "docs: explain the workflow\n\n"
    "This paragraph explains why a contributor needs a consistent way to record "
    "changes before final integration and how formatting preserves the complete "
    "message without editing the meaning of its words.\n\n"
    "Original-commit: https://github.com/example/project/commit/" + "a" * 40 + "\n"
)


class CommitMessageHookTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="commit hook tests ")
        self.addCleanup(self.temporary.cleanup)
        self.directory = Path(self.temporary.name)
        self.repository = self.directory / "repository with spaces"
        self.repository.mkdir()
        self.environment = {
            **os.environ,
            "GIT_CONFIG_NOSYSTEM": "1",
            "GIT_CONFIG_GLOBAL": os.devnull,
            "GIT_AUTHOR_NAME": "Test Contributor",
            "GIT_AUTHOR_EMAIL": "test@example.invalid",
            "GIT_COMMITTER_NAME": "Test Contributor",
            "GIT_COMMITTER_EMAIL": "test@example.invalid",
        }
        self.git("init", "--initial-branch=main")
        self.copy_scripts(self.repository)
        (self.repository / "tracked.txt").write_text("unchanged content\n")
        self.git("add", "tracked.txt")
        self.git("commit", "-m", "test: initial commit")

    def run_command(self, arguments, *, cwd=None, check=True):
        result = subprocess.run(
            arguments,
            cwd=cwd or self.repository,
            env=self.environment,
            capture_output=True,
            text=True,
        )
        if check:
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        return result

    def git(self, *arguments, cwd=None, check=True):
        return self.run_command(["git", *arguments], cwd=cwd, check=check)

    def copy_scripts(self, destination):
        for relative in (
            "tools/commit-messages/format_commit_message.py",
            "tools/commit-messages/install_commit_message_hook.py",
            "tools/commit-messages/hooks/commit-msg",
        ):
            target = destination / relative
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(ROOT / relative, target)

    def install(self, *, cwd=None, check=True):
        repository = cwd or self.repository
        return self.run_command(
            [sys.executable, str(repository / "tools/commit-messages/install_commit_message_hook.py")],
            cwd=repository,
            check=check,
        )

    def hook_path(self, repository=None):
        repository = repository or self.repository
        path = Path(self.git("rev-parse", "--git-path", "hooks/commit-msg", cwd=repository).stdout.strip())
        return path if path.is_absolute() else repository / path

    def installed_formatter(self):
        return self.hook_path().parent / "commit-message-formatter.py"

    def assert_formatted(self, repository=None):
        message = self.git("log", "-1", "--format=%B", cwd=repository).stdout
        self.assertEqual(message.splitlines()[0], "docs: explain the workflow")
        trailer = MESSAGE.splitlines()[-1]
        self.assertIn(trailer, message.splitlines())
        for line in message.splitlines()[1:]:
            if line != trailer:
                self.assertLessEqual(len(line), 72)
        self.assertEqual(MESSAGE.split(), message.split())

    def test_message_argument_is_formatted_without_other_changes(self):
        before_tree = self.git("rev-parse", "HEAD^{tree}").stdout
        before_status = self.git("status", "--porcelain").stdout
        before_configuration = self.git("config", "--local", "--list").stdout
        self.install()
        self.git("commit", "--allow-empty", "-m", MESSAGE)
        self.assert_formatted()
        self.assertEqual(self.git("rev-list", "--count", "HEAD").stdout.strip(), "2")
        self.assertEqual(before_tree, self.git("rev-parse", "HEAD^{tree}").stdout)
        self.assertEqual(before_status, self.git("status", "--porcelain").stdout)
        self.assertEqual(before_configuration, self.git("config", "--local", "--list").stdout)

    def test_message_file_is_preserved(self):
        self.install()
        message_file = self.directory / "message with spaces.txt"
        message_file.write_text(MESSAGE)
        self.git("commit", "--allow-empty", "-F", str(message_file))
        self.assert_formatted()
        self.assertEqual(message_file.read_text(), MESSAGE)

    def test_installation_is_idempotent(self):
        self.install()
        hook = self.hook_path()
        first = hook.read_bytes()
        self.assertIn("already installed", self.install().stdout)
        self.assertEqual(hook.read_bytes(), first)
        self.assertTrue(os.access(hook, os.X_OK))

    def test_existing_hook_is_not_overwritten(self):
        hook = self.hook_path()
        hook.parent.mkdir(parents=True, exist_ok=True)
        original = b"#!/bin/sh\necho existing hook\n"
        hook.write_bytes(original)
        self.assertNotEqual(self.install(check=False).returncode, 0)
        self.assertEqual(hook.read_bytes(), original)
        self.assertFalse(self.installed_formatter().exists())

    def test_existing_formatter_is_not_overwritten(self):
        helper = self.installed_formatter()
        helper.parent.mkdir(parents=True, exist_ok=True)
        helper.write_text("existing formatter\n")
        self.assertNotEqual(self.install(check=False).returncode, 0)
        self.assertEqual(helper.read_text(), "existing formatter\n")
        self.assertFalse(self.hook_path().exists())

    def test_configured_hook_directory_is_not_changed(self):
        configured = self.directory / "custom hooks"
        self.git("config", "core.hooksPath", str(configured))
        result = self.install(check=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("core.hooksPath", result.stderr)
        self.assertFalse(configured.exists())
        self.assertEqual(self.git("config", "--get", "core.hooksPath").stdout.strip(), str(configured))

    def test_symlink_hook_is_not_replaced(self):
        target = self.directory / "existing hook"
        original = (ROOT / "tools/commit-messages/hooks/commit-msg").read_bytes()
        target.write_bytes(original)
        hook = self.hook_path()
        hook.parent.mkdir(parents=True, exist_ok=True)
        hook.symlink_to(target)
        self.assertNotEqual(self.install(check=False).returncode, 0)
        self.assertTrue(hook.is_symlink())
        self.assertEqual(target.read_bytes(), original)
        self.assertFalse(self.installed_formatter().exists())

    def test_installation_from_linked_worktree_serves_checkouts_without_scripts(self):
        linked = self.directory / "linked checkout with spaces"
        self.git("worktree", "add", "-b", "linked", str(linked))
        self.copy_scripts(linked)
        self.install(cwd=linked)
        self.assertEqual(self.hook_path(linked).resolve(), self.hook_path().resolve())
        self.git("commit", "--allow-empty", "-m", MESSAGE, cwd=linked)
        self.assert_formatted(linked)
        self.assertEqual(self.git("rev-list", "--count", "HEAD").stdout.strip(), "1")

        shutil.rmtree(self.repository / "tools")
        self.git("commit", "--allow-empty", "-m", MESSAGE)
        self.assert_formatted()

    def test_bare_repository_worktree(self):
        bare = self.directory / "shared bare repository"
        linked = self.directory / "bare linked checkout"
        self.git("clone", "--bare", str(self.repository), str(bare))
        self.git("--git-dir", str(bare), "worktree", "add", str(linked), "main")
        self.copy_scripts(linked)
        self.install(cwd=linked)
        self.assertEqual(self.hook_path(linked).resolve(), (bare / "hooks/commit-msg").resolve())
        self.git("commit", "--allow-empty", "-m", MESSAGE, cwd=linked)
        self.assert_formatted(linked)

    def test_merge_message_is_formatted(self):
        self.git("switch", "-c", "feature")
        self.git("commit", "--allow-empty", "-m", "test: feature commit")
        self.git("switch", "main")
        self.install()
        self.git("merge", "--no-ff", "feature", "-m", MESSAGE)
        self.assert_formatted()
        self.assertEqual(len(self.git("rev-list", "--parents", "-1", "HEAD").stdout.split()), 3)

    def test_missing_formatter_fails_without_creating_commit(self):
        self.install()
        self.installed_formatter().unlink()
        result = self.git("commit", "--allow-empty", "-m", MESSAGE, check=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("formatter not found", result.stderr)
        self.assertEqual(self.git("rev-list", "--count", "HEAD").stdout.strip(), "1")

    def test_failed_check_prevents_commit(self):
        self.install()
        self.installed_formatter().write_text(
            "import sys\n"
            "raise SystemExit(1 if '--check' in sys.argv else 0)\n"
        )
        result = self.git("commit", "--allow-empty", "-m", MESSAGE, check=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(self.git("rev-list", "--count", "HEAD").stdout.strip(), "1")


if __name__ == "__main__":
    unittest.main()
