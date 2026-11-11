"""Tests for check_auto.py, run in a temporary Git repository."""

import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


SCRIPT = Path(__file__).resolve().with_name("check_auto.py")


class CheckAutoTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.repo = Path(self.tmp.name)
        self.git("init", "-q", "-b", "main")
        self.git("config", "user.email", "test@example.com")
        self.git("config", "user.name", "Test")
        self.commit("docs: add a readme", {"README.md": "# Readme\n"})
        self.base = self.git("rev-parse", "HEAD").strip()

    def tearDown(self):
        self.tmp.cleanup()

    def git(self, *args):
        return subprocess.run(["git", *args], cwd=self.repo, check=True, capture_output=True, text=True).stdout

    def commit(self, message, files=None):
        for name, text in (files or {"note.txt": message}).items():
            path = self.repo / name
            path.write_text(path.read_text() + text if path.exists() else text)
            self.git("add", name)
        self.git("commit", "-q", "-m", message)
        return self.git("rev-parse", "HEAD").strip()

    def check(self, *args):
        result = subprocess.run([sys.executable, str(SCRIPT), *args], cwd=self.repo, capture_output=True, text=True)
        self.assertIn(result.returncode, (0, 1), result.stderr)
        return result.stdout

    def rules(self, *args):
        return {line.split(": ")[1] for line in self.check(*args).splitlines()}

    def test_clean_commit_has_no_findings(self):
        self.commit("feat(cli): add a flag\n\nAdds `--quiet` to `run`.")
        self.assertEqual(self.check(f"{self.base}..HEAD"), "")

    def test_subject_needs_a_listed_type(self):
        self.commit("spec(deneb): change a rule")
        self.assertIn("R.commit-format.subject", self.rules(self.base))

    def test_long_subject_and_body_lines(self):
        self.commit("docs: " + "word " * 15 + "\n\n" + "word " * 20)
        output = self.check(self.base)
        self.assertIn("subject has", output)
        self.assertIn("can be wrapped", output)

    def test_non_ascii_message(self):
        self.commit("docs: add an em dash\n\nUses — here.")
        self.assertIn("R.commit-display.characters", self.rules(self.base))

    def test_record_order(self):
        target = self.git("rev-parse", "--short=12", "HEAD").strip()
        self.commit(f'fix: repair it\n\nRepairs it.\n\nCo-authored-by: A <a@example.com>\nFixes: {target} ("docs: add a readme")')
        self.assertIn("R.sources.order", self.rules(self.base))

    def test_fixes_record_must_match_its_commit(self):
        target = self.git("rev-parse", "HEAD").strip()
        self.commit(f'fix: repair it\n\nRepairs it.\n\nFixes: {target[:8]} ("docs: add a readme file")')
        self.assertEqual({"R.sources.hashes", "R.sources.regressions"}, self.rules(self.base))

    def test_markdown_hard_wrap_and_punctuation(self):
        self.commit("docs: add a guide", {"guide.md": "A first line\nthat continues; badly.\n\n- A list item\n- `a; b` in code\n"})
        output = self.check(self.base)
        self.assertIn("guide.md:2: R.markdown-wrapping", output)
        self.assertIn("guide.md:2: R.prose.punctuation", output)
        self.assertNotIn("guide.md:4", output)
        self.assertNotIn("guide.md:5", output)

    def test_ocaml_helpers_and_comments(self):
        source = "(* Sums the list; then stops. *)\nlet rec aux acc = function [] -> acc | x :: xs -> aux (acc + x) xs\n(* Builds [a; b]. *)\n"
        self.commit("feat: add sum", {"sum.ml": source})
        output = self.check(self.base)
        self.assertIn("sum.ml:1: R.prose.punctuation", output)
        self.assertIn("sum.ml:2: R.names.helpers", output)
        self.assertNotIn("sum.ml:3", output)

    def event(self, payload):
        path = self.repo / "event.json"
        path.write_text(json.dumps(payload))
        return str(path)

    def test_pull_request_event(self):
        head = self.commit("update the readme")
        event = self.event({"pull_request": {"base": {"sha": self.base}, "head": {"sha": head}}})
        self.assertIn("R.commit-format.subject", self.rules("--event-file", event))

    def test_push_event_for_a_new_branch_checks_the_tip_only(self):
        self.commit("update the readme")
        tip = self.commit("docs: add a note")
        event = self.event({"before": "0" * 40, "after": tip})
        self.assertEqual(self.check("--event-file", event), "")

    def test_push_event_that_deletes_a_branch(self):
        event = self.event({"before": self.base, "after": "0" * 40})
        self.assertEqual(self.check("--event-file", event), "")

    def test_messages_only_skips_added_lines(self):
        self.commit("docs: add a guide", {"guide.md": "A first line\nthat continues.\n"})
        self.assertEqual(self.check("--messages-only", self.base), "")
        self.assertIn("R.markdown-wrapping", self.rules(self.base))

    def test_staged_lines_only(self):
        (self.repo / "README.md").write_text("# Readme\n\nA wrapped\nparagraph.\n")
        self.git("add", "README.md")
        self.assertIn("README.md:4: R.markdown-wrapping", self.check("--staged"))


if __name__ == "__main__":
    unittest.main()
