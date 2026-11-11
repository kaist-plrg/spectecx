#!/usr/bin/env python3
"""Check the contribution guides' rules marked [auto] within a review's scope.

A range checks each commit message in it and the lines it adds. --staged
checks the lines the staged diff adds. --event-file takes the range from a
GitHub pull_request or push event. Each finding names the rule it breaks.
The exit status is 1 when any finding is reported, and 2 when Git or the
event file cannot be read.
"""

import argparse
import json
from pathlib import Path
import re
import subprocess
import sys

sys.path.insert(0, str(Path(__file__).resolve().parent))
import format_commit_message as formatter  # noqa: E402


TYPES = {"feat", "fix", "refactor", "reorg", "test", "docs", "ci", "chore", "perf", "style"}
SUBJECT = re.compile(r"^(?P<type>[a-z]+)(?:\([^()\s]+\))?!?: \S")
RECORD = re.compile(r"^(?P<key>Original-commit|Copied-from|Reference|Cherry-picked-from|Fixes|Co-authored-by):")
RECORD_RANK = {"Original-commit": 0, "Copied-from": 0, "Reference": 0, "Cherry-picked-from": 0,
               "Fixes": 1, "Co-authored-by": 2}
FIXES = re.compile(r'^Fixes: (?P<hash>[0-9a-fA-F]+) \("(?P<subject>.*)"\)$')
COMMIT_URL = re.compile(r"/commit/(?P<hash>[0-9a-fA-F]+)")
HELPER = re.compile(r"\blet\s+rec\s+(aux|go|loop)\b")
FENCE = re.compile(r"^ {0,3}(`{3,}|~{3,})")
LIST_ITEM = re.compile(r"^\s*([-*+]|\d+[.)])\s")
OTHER_BLOCK = re.compile(r"^\s*(#|\||>|<|\[[^\]]+\]:)")
CODE_SPAN = re.compile(r"`[^`]*`")
LINK_TARGET = re.compile(r"\]\([^)]*\)")
HTML_TAG = re.compile(r"<[^>]*>")
ODOC_CODE = re.compile(r"\[[^\]]*\]")
ZERO = "0" * 40
EMPTY_TREE = "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

findings = []


def report(where, rule, message):
    findings.append(f"{where}: {rule}: {message}")


def git(*args, check=True):
    return subprocess.run(["git", *args], check=check, capture_output=True, text=True).stdout


def punctuation(text):
    """Return the semicolons and em dashes in prose text, ignoring code and markup."""
    for pattern in (CODE_SPAN, LINK_TARGET, HTML_TAG):
        text = pattern.sub("", text)
    return sorted({mark for mark in (";", "—") if mark in text})


# Commit messages ---------------------------------------------------------

def check_commit(sha):
    where = f"commit {sha[:12]}"
    message = git("show", "-s", "--format=%B", sha)
    lines = message.rstrip("\n").split("\n")
    subject = lines[0]
    if len(git("rev-list", "--parents", "-n", "1", sha).split()) <= 2:
        match = SUBJECT.match(subject)
        if not match or match.group("type") not in TYPES:
            report(where, "R.commit-format.subject", f"subject is not type(scope): summary with a listed type: {subject}")
    if len(subject) > formatter.WIDTH:
        report(where, "R.commit-wrapping", f"subject has {len(subject)} characters")
    for number, line in formatter.check_message(message):
        report(f"{where} line {number}", "R.commit-wrapping", f"line has {len(line)} characters and can be wrapped")
    for number, line in enumerate(lines, 1):
        if not line.isascii():
            report(f"{where} line {number}", "R.commit-display.characters", f"non-ASCII text: {line.strip()}")
    check_records(where, lines[1:])


def paragraphs(lines):
    block = []
    for line in lines + [""]:
        if line.strip():
            block.append(line)
        elif block:
            yield block
            block = []


def check_records(where, body):
    blocks = list(paragraphs(body))
    for block in blocks[:-1]:
        for line in block:
            if RECORD.match(line):
                report(where, "R.sources.order", f"record outside the final block: {line}")
    if not blocks:
        return
    records = []
    for line in blocks[-1]:
        if RECORD.match(line):
            records.append(line)
        elif line.startswith(" ") and records:
            records[-1] += line
        elif any(RECORD.match(other) for other in blocks[-1]):
            report(where, "R.sources.order", f"prose shares the record block: {line}")
    ranks = [RECORD_RANK[RECORD.match(record).group("key")] for record in records]
    if ranks != sorted(ranks):
        report(where, "R.sources.order", "records are not in the order source records, Fixes:, Co-authored-by:")
    for record in records:
        if record.startswith("Fixes:"):
            check_fixes(where, record)
        for url in COMMIT_URL.finditer(record):
            if len(url.group("hash")) != 12:
                report(where, "R.sources.hashes", f"commit URL uses {len(url.group('hash'))} hash characters, not 12")


def check_fixes(where, record):
    match = FIXES.match(record)
    if not match:
        report(where, "R.sources.regressions", f'record is not Fixes: <hash> ("<subject>"): {record}')
        return
    given, subject = match.group("hash"), match.group("subject")
    full = git("rev-parse", "--verify", "--quiet", f"{given}^{{commit}}", check=False).strip()
    if not full:
        report(where, "R.sources.regressions", f"{given} does not name exactly one commit in this repository")
        return
    short = git("rev-parse", "--short=12", full).strip()
    if len(given) != len(short):
        report(where, "R.sources.hashes", f"Fixes: uses {len(given)} hash characters, not {len(short)}")
    actual = git("log", "-1", "--format=%s", full).rstrip("\n")
    if actual != subject:
        report(where, "R.sources.regressions", f'quoted subject differs from {short}: "{actual}"')


# Added lines -------------------------------------------------------------

def added_lines(diff_args):
    """Map each added or changed file to the line numbers the diff adds."""
    files, path = {}, None
    for line in git("diff", "--no-color", "--unified=0", "--diff-filter=AMR", *diff_args).split("\n"):
        if line.startswith("+++ "):
            path = line[6:] if line.startswith("+++ b/") else None
            if path:
                files[path] = set()
        elif line.startswith("@@") and path:
            match = re.match(r"@@ -\S+ \+(\d+)(?:,(\d+))? @@", line)
            start, count = int(match.group(1)), int(match.group(2) or 1)
            files[path].update(range(start, start + count))
    return files


def check_markdown(path, text, added):
    inside = False
    frontmatter = text.startswith("---\n")
    previous_prose = False
    for number, line in enumerate(text.split("\n"), 1):
        if frontmatter:
            frontmatter = not (number > 1 and line == "---")
            continue
        if FENCE.match(line):
            inside, previous_prose = not inside, False
            continue
        if inside or not line.strip():
            previous_prose = False
            continue
        if LIST_ITEM.match(line):
            starts_block, previous_prose_next = True, True
        elif OTHER_BLOCK.match(line):
            starts_block, previous_prose_next = True, False
        elif line.startswith("    ") and not previous_prose:
            starts_block, previous_prose_next = True, False
        else:
            starts_block, previous_prose_next = False, True
        if number in added:
            if previous_prose and not starts_block:
                report(f"{path}:{number}", "R.markdown-wrapping", "line continues the previous line's paragraph")
            for mark in punctuation(line):
                report(f"{path}:{number}", "R.prose.punctuation", f"prose contains {'a semicolon' if mark == ';' else 'an em dash'}")
        previous_prose = previous_prose_next


def comment_lines(text):
    """Map line numbers to the OCaml comment text on each line, outside odoc code blocks."""
    comments, depth, number, i, in_string, in_code = {}, 0, 1, 0, False, False
    while i < len(text):
        pair = text[i:i + 2]
        if text[i] == "\n":
            number += 1
        if depth == 0:
            if in_string:
                if text[i] == "\\":
                    i += 1
                elif text[i] == '"':
                    in_string = False
            elif text[i] == '"':
                in_string = True
            elif pair == "(*":
                depth, i = 1, i + 2
                continue
        else:
            if pair == "(*":
                depth, i = depth + 1, i + 2
                continue
            if pair == "*)":
                depth, i = depth - 1, i + 2
                continue
            if pair == "{[":
                in_code, i = True, i + 2
                continue
            if pair == "]}":
                in_code, i = False, i + 2
                continue
            if not in_code and text[i] != "\n":
                comments[number] = comments.get(number, "") + text[i]
        i += 1
    return comments


def check_ocaml(path, text, added):
    comments = comment_lines(text)
    for number, line in enumerate(text.split("\n"), 1):
        if number not in added:
            continue
        if HELPER.search(line):
            report(f"{path}:{number}", "R.names.helpers", f"recursive helper is named {HELPER.search(line).group(1)}")
        for mark in punctuation(ODOC_CODE.sub("", comments.get(number, ""))):
            report(f"{path}:{number}", "R.prose.punctuation", f"comment contains {'a semicolon' if mark == ';' else 'an em dash'}")


def check_added(diff_args, read):
    for path, added in sorted(added_lines(diff_args).items()):
        if not added:
            continue
        if path.endswith(".md"):
            check_markdown(path, read(path), added)
        elif path.endswith((".ml", ".mli")):
            check_ocaml(path, read(path), added)


def event_range(path):
    """Return the (base, tip) an event introduces, with base None for a new branch's tip alone."""
    with open(path, encoding="utf-8") as source:
        event = json.load(source)
    if "pull_request" in event:
        base, tip = event["pull_request"]["base"]["sha"], event["pull_request"]["head"]["sha"]
    else:
        base, tip = event["before"], event["after"]
    if not all(re.fullmatch(r"[0-9a-fA-F]{40}", sha) for sha in (base, tip)):
        raise ValueError("event commit IDs must be 40 hexadecimal characters")
    if tip == ZERO:
        return None
    return (None if base == ZERO else base), tip


def check_range(base, tip, messages_only):
    if base is None:
        commits = [git("rev-parse", tip).strip()]
        parents = git("rev-list", "--parents", "-n", "1", tip).split()[1:]
        diff_args = [parents[0] if parents else EMPTY_TREE, tip]
    else:
        commits = git("rev-list", "--reverse", f"{base}..{tip}").split()
        diff_args = [f"{base}...{tip}"]
    for sha in commits:
        check_commit(sha)
    if not messages_only:
        check_added(diff_args, lambda path: git("show", f"{tip}:{path}"))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("range", nargs="?", help="<base>..<tip>, or a base ref checked up to HEAD")
    parser.add_argument("--staged", action="store_true", help="check the lines the staged diff adds")
    parser.add_argument("--event-file", help="check the range a GitHub pull_request or push event introduces")
    parser.add_argument("--messages-only", action="store_true", help="check commit messages and skip added lines")
    args = parser.parse_args()
    if sum((args.staged, bool(args.range), bool(args.event_file))) != 1:
        parser.error("give exactly one of a range, --staged, or --event-file")
    if args.staged and args.messages_only:
        parser.error("--messages-only needs commits, which --staged does not have")
    try:
        if args.staged:
            check_added(["--cached"], lambda path: git("show", f":{path}"))
        elif args.event_file:
            scope = event_range(args.event_file)
            if scope is None:
                print("The event deletes a branch. Nothing to check.", file=sys.stderr)
                return 0
            check_range(*scope, args.messages_only)
        else:
            base, _, tip = args.range.partition("..")
            check_range(base, tip or "HEAD", args.messages_only)
    except (OSError, ValueError, KeyError) as exc:
        print(f"Cannot read the event: {exc}", file=sys.stderr)
        return 2
    except subprocess.CalledProcessError as exc:
        print(f"git {' '.join(exc.cmd[1:])} failed: {exc.stderr.strip()}", file=sys.stderr)
        return 2
    for finding in findings:
        print(finding)
    print(f"{len(findings)} finding(s).", file=sys.stderr)
    return int(bool(findings))


if __name__ == "__main__":
    sys.exit(main())
