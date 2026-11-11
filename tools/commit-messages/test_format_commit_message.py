"""Test body wrapping, preserved text, and formatter commands."""

import importlib.util
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


SCRIPT = Path(__file__).resolve().with_name("format_commit_message.py")
spec = importlib.util.spec_from_file_location("formatter", SCRIPT)
formatter = importlib.util.module_from_spec(spec)
spec.loader.exec_module(formatter)
PROSE = (
    "A contributor needs a consistent way to describe a change so that reviewers "
    "can understand its motivation without reconstructing the entire discussion "
    "and its implementation history."
)
URL = "https://example.com/" + "a" * 100


class FormatMessageTests(unittest.TestCase):
    def assert_preserved_and_wrapped(self, text):
        result = formatter.format_message(text)
        self.assertEqual(text.split(), result.split())
        self.assertEqual(formatter.check_message(result), [])
        self.assertEqual(formatter.format_message(result), result)
        return result

    def test_paragraphs_and_blank_boundaries(self):
        text = "docs: clarify guidance\n\n" + PROSE + "\n\n\n" + PROSE + "\n"
        result = self.assert_preserved_and_wrapped(text)
        self.assertIn("\n\n\n", result)
        self.assertTrue(all(len(line) <= 72 for line in result.splitlines()))

    def test_short_manually_wrapped_lines_pass(self):
        text = "docs: title\n\nOne short line\nfollowed by another short line.\n"
        self.assertEqual(formatter.check_message(text), [])
        self.assertEqual(formatter.format_message(text), "docs: title\n\nOne short line followed by another short line.\n")

    def test_checker_accepts_72_characters_and_flags_73(self):
        body = "word " * 14 + "ok"
        self.assertEqual(len(body), 72)
        self.assertEqual(formatter.check_message("docs: title\n\n" + body), [])
        self.assertEqual(formatter.check_message("docs: title\n\n" + body + "."), [(3, body + ".")])

    def test_subject_is_not_checked_or_wrapped(self):
        text = "docs: " + PROSE + "\n\nShort body.\n"
        self.assertEqual(self.assert_preserved_and_wrapped(text), text)

    def test_bullets_have_hanging_indents_and_remain_independent(self):
        text = "Merge: changes\n\nChanges:\n- docs: " + PROSE + "\n- ci: " + PROSE + "\n\nMinor Changes:\n* " + PROSE
        result = self.assert_preserved_and_wrapped(text)
        self.assertEqual(result.count("\n- "), 2)
        self.assertEqual(result.count("\n* "), 1)
        for line in result.splitlines():
            self.assertLessEqual(len(line), 72)
        self.assertIn("\n  ", result)

    def test_nested_bullets_are_checked_and_wrapped(self):
        text = "docs: title\n\n- Main item.\n    - " + PROSE
        self.assertEqual(formatter.check_message(text)[0][0], 4)
        result = self.assert_preserved_and_wrapped(text)
        self.assertIn("\n    - ", result)
        self.assertIn("\n      ", result)
        self.assertTrue(all(len(line) <= 72 for line in result.splitlines()))

    def test_empty_bullet_marker_is_preserved(self):
        text = "docs: title\n\n- \n1. \n"
        self.assertEqual(self.assert_preserved_and_wrapped(text), text)

    def test_numbered_lists(self):
        text = "docs: title\n\n1. " + PROSE + "\n10) " + PROSE
        result = self.assert_preserved_and_wrapped(text)
        self.assertIn("\n   ", result)
        self.assertIn("\n    ", result)
        self.assertIn("\n10) ", result)

    def test_long_url_alone_is_allowed(self):
        for prefix in ("", "- ", "1. ", "  "):
            with self.subTest(prefix=prefix):
                text = "docs: title\n\n" + prefix + URL
                self.assertEqual(self.assert_preserved_and_wrapped(text), text)

    def test_url_does_not_exempt_surrounding_prose(self):
        text = "docs: title\n\nThe upstream source is " + URL + " and its implementation is useful.\n"
        self.assertEqual(formatter.check_message(text)[0][0], 3)
        result = self.assert_preserved_and_wrapped(text)
        self.assertIn("\n" + URL + "\n", result)

    def test_long_identifier_is_not_split(self):
        identifier = "some_" + "identifier_" * 12
        text = "docs: title\n\n" + PROSE + " " + identifier + " is the entry point."
        result = self.assert_preserved_and_wrapped(text)
        self.assertIn("\n" + identifier + "\n", result)

    def test_source_and_credit_records_are_preserved(self):
        for name in ("Original-commit", "Copied-from", "Reference", "Cherry-picked-from",
                     "Co-authored-by", "Signed-off-by", "Reviewed-by", "Tested-by", "Acked-by", "Change-Id"):
            with self.subTest(name=name):
                text = "docs: title\n\n" + name + ": " + URL + "\n " + PROSE
                self.assertEqual(self.assert_preserved_and_wrapped(text), text)

    def test_fix_record_preserves_exact_source_subject(self):
        subject = 'feat: preserve "quoted (names)" and  repeated spaces ' + PROSE
        record = 'Fixes: abcdef123456 ("' + subject + '")'
        text = "fix: regression\n\n" + record + "\n"
        self.assertEqual(formatter.check_message(text), [(3, record)])
        result = self.assert_preserved_and_wrapped(text)
        self.assertEqual(result, 'fix: regression\n\nFixes: abcdef123456\n ("' + subject + '")\n')

    def test_fix_record_splits_only_above_72_characters(self):
        for width in (72, 73):
            with self.subTest(width=width):
                subject = "x" * (width - len('Fixes: abcdef123456 ("")'))
                record = 'Fixes: abcdef123456 ("' + subject + '")'
                self.assertEqual(len(record), width)
                text = "fix: regression\n\n" + record + "\n"
                if width == 72:
                    self.assertEqual(self.assert_preserved_and_wrapped(text), text)
                else:
                    self.assertEqual(formatter.check_message(text), [(3, record)])
                    expected = 'fix: regression\n\nFixes: abcdef123456\n ("' + subject + '")\n'
                    self.assertEqual(self.assert_preserved_and_wrapped(text), expected)

    def test_two_line_fix_records_are_preserved(self):
        for indent in (" ", "  "):
            with self.subTest(indent=indent):
                text = 'fix: regression\n\nFixes: abcdef123456\n' + indent + '("' + PROSE + '")\n'
                self.assertEqual(self.assert_preserved_and_wrapped(text), text)

    def test_nonstandard_fix_records_are_preserved(self):
        for record in ("Fixes: " + URL, "Fixes: abcdef123456 " + PROSE):
            with self.subTest(record=record):
                text = "fix: regression\n\n" + record + "\n"
                self.assertEqual(self.assert_preserved_and_wrapped(text), text)

    def test_fix_record_text_in_preserved_regions_is_unchanged(self):
        record = 'Fixes: abcdef123456 ("' + PROSE + '")'
        messages = (
            record + "\n\nShort body.\n",
            "docs: title\n\n```\n" + record + "\n```\n",
            "docs: title\n\n    " + record + "\n",
            "docs: title\n\n# ------------------------ >8 ------------------------\n" + record + "\n",
        )
        for text in messages:
            with self.subTest(text=text):
                self.assertEqual(self.assert_preserved_and_wrapped(text), text)

    def test_ordinary_colon_prose_is_checked(self):
        text = "docs: title\n\nThe reason: " + PROSE
        self.assertTrue(formatter.check_message(text))
        self.assert_preserved_and_wrapped(text)

    def test_code_and_quotes_are_preserved(self):
        for body in ("```ocaml\n" + PROSE + "\n```", "~~~\n" + PROSE + "\n~~~",
                     "    " + PROSE, "\t" + PROSE, "> " + PROSE):
            with self.subTest(body=body):
                text = "docs: title\n\n" + body + "\n"
                self.assertEqual(self.assert_preserved_and_wrapped(text), text)

    def test_unclosed_code_block_is_preserved(self):
        text = "docs: title\n\n```\n" + PROSE
        self.assertEqual(self.assert_preserved_and_wrapped(text), text)

    def test_comments_do_not_take_subject_position(self):
        text = "# " + PROSE + "\n\nsubject " + PROSE + "\n\n" + PROSE
        result = self.assert_preserved_and_wrapped(text)
        self.assertEqual(result.splitlines()[2], text.splitlines()[2])
        self.assertEqual(formatter.check_message(text)[0][0], 5)

    def test_git_cut_marker_and_following_diff_are_preserved(self):
        tail = "# ------------------------ >8 ------------------------\n" + PROSE + "\ndiff --git a/file b/file\n+" + PROSE
        text = "docs: title\n\n" + PROSE + "\n\n" + tail
        result = self.assert_preserved_and_wrapped(text)
        self.assertTrue(result.endswith(tail))

    def test_newline_and_empty_messages(self):
        for text in ("", "docs: subject", "docs: subject\n", "docs: subject\n\n", "docs: subject\r\n\r\nShort body.\r\n"):
            with self.subTest(text=text):
                self.assertEqual(self.assert_preserved_and_wrapped(text), text)

    def test_diagnostics_keep_original_line_numbers(self):
        text = "docs: title\n\nShort.\n\n" + PROSE + "\n\n- " + PROSE
        self.assertEqual([n for n, _ in formatter.check_message(text)], [5, 7])


class FormatMessageCliTests(unittest.TestCase):
    def run_cli(self, *args, input=None):
        return subprocess.run([sys.executable, str(SCRIPT), *args], input=input,
                              text=True, capture_output=True)

    def test_stdout_and_check_stdin(self):
        text = "docs: title\n\n" + PROSE + "\n"
        result = self.run_cli("-", input=text)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout.split(), text.split())
        self.assertTrue(all(len(line) <= 72 for line in result.stdout.splitlines()))
        checked = self.run_cli("--check", "-", input=text)
        self.assertEqual(checked.returncode, 1)
        self.assertIn("-:3:", checked.stderr)
        self.assertIn(PROSE, checked.stderr)
        self.assertEqual(self.run_cli("--check", "-", input=result.stdout).returncode, 0)

    def test_write_only_changes_explicit_file(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "message with spaces.txt"
            text = "docs: title\n\n" + PROSE + "\n"
            path.write_text(text)
            output = self.run_cli(str(path))
            self.assertEqual(output.returncode, 0, output.stderr)
            self.assertEqual(path.read_text(), text)
            result = self.run_cli("--write", str(path))
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(result.stdout, "")
            self.assertEqual(path.read_text(), output.stdout)
            self.assertEqual(path.read_text().split(), text.split())
            self.assertTrue(all(len(line) <= 72 for line in path.read_text().splitlines()))
            self.assertEqual(self.run_cli("--check", str(path)).returncode, 0)

    def test_invalid_write_target_and_missing_file(self):
        for args in (("--write", "-"), ("does-not-exist",)):
            with self.subTest(args=args):
                self.assertEqual(self.run_cli(*args).returncode, 2)


if __name__ == "__main__":
    unittest.main()
