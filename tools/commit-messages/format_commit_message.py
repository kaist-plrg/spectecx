#!/usr/bin/env python3
"""Wrap commit-message body prose at 72 characters without changing its words.

Overlong Fixes records put the complete quoted subject on a second line,
indented by one space. That subject may exceed the limit.
Commit subjects, other source and credit records, code, quotes, and Git
comments stay unchanged. Git cut-off markers and the text below them also
stay unchanged.
Single words, URLs, and identifiers may exceed the limit.
Only wrapping and Fixes layout are checked. Subject length and message
content are not checked.
"""

import argparse
import re
import sys
import textwrap
from typing import NamedTuple


WIDTH = 72
BULLET = re.compile(r"^( *)([-*+]\s+|\d+[.)]\s+)(.*)$")
SOURCE = re.compile(
    r"^(?:Original-commit|Copied-from|Reference|Fixes|Cherry-picked-from|"
    r"Co-authored-by|Signed-off-by|Reviewed-by|Tested-by|Acked-by|Change-Id):",
    re.IGNORECASE,
)
FIX_RECORD = re.compile(r'(Fixes: [0-9a-fA-F]{12,40}) (\(".*"\))', re.IGNORECASE)
FENCE = re.compile(r"^ {0,3}(`{3,}|~{3,})")
HEADER = re.compile(r"^[A-Za-z][A-Za-z ]{0,40}:$")
GIT_CUT_MARKER = re.compile(r"^# -+ >8 -+\s*$")


class Block(NamedTuple):
    start: int
    lines: list
    prefix: str = ""
    continuation: str = ""
    protected: bool = False
    source_record: bool = False


def _blocks(lines):
    """Group prose and source records with their original line positions."""
    index = 0
    subject_seen = False
    while index < len(lines):
        line = lines[index]
        if GIT_CUT_MARKER.match(line):
            yield Block(index, lines[index:], protected=True)
            return
        if not line.strip() or line.startswith("#"):
            yield Block(index, [line], protected=True)
            index += 1
            continue
        if not subject_seen:
            yield Block(index, [line], protected=True)
            subject_seen = True
            index += 1
            continue
        fence = FENCE.match(line)
        if fence:
            end = index + 1
            marker = fence.group(1)
            closing = re.compile(r"^ {0,3}" + re.escape(marker[0]) + "{" + str(len(marker)) + r",}\s*$")
            while end < len(lines):
                end += 1
                if closing.match(lines[end - 1]):
                    break
            yield Block(index, lines[index:end], protected=True)
            index = end
            continue
        if SOURCE.match(line):
            end = index + 1
            while end < len(lines) and lines[end].strip() and lines[end][0].isspace():
                end += 1
            yield Block(index, lines[index:end], protected=True, source_record=True)
            index = end
            continue
        bullet = BULLET.match(line)
        if ((line.startswith(("    ", "\t")) and not bullet)
                or line.lstrip().startswith(">") or HEADER.fullmatch(line)):
            yield Block(index, [line], protected=True)
            index += 1
            continue
        if bullet:
            prefix = bullet.group(1) + bullet.group(2)
            continuation = " " * len(prefix)
        else:
            prefix = line[:len(line) - len(line.lstrip(" "))]
            continuation = prefix
        end = index + 1
        while end < len(lines):
            following = lines[end]
            if (not following.strip() or following.startswith("#")
                    or FENCE.match(following) or SOURCE.match(following)
                    or HEADER.fullmatch(following) or BULLET.match(following)
                    or following.lstrip().startswith(">")):
                break
            indent = following[:len(following) - len(following.lstrip(" "))]
            if following.startswith("\t") or indent != continuation:
                break
            end += 1
        yield Block(index, lines[index:end], prefix, continuation)
        index = end


def _payload(block, offset):
    prefix = block.prefix if offset == 0 else block.continuation
    return block.lines[offset][len(prefix):]


def _split_fix_record(block):
    if not block.source_record or len(block.lines[0]) <= WIDTH:
        return None
    record = FIX_RECORD.fullmatch(block.lines[0])
    if record:
        return [record.group(1), " " + record.group(2), *block.lines[1:]]
    return None


def format_message(text):
    """Wrap body prose and split long Fixes records without changing their words."""
    lines = text.splitlines()
    output = []
    for block in _blocks(lines):
        if block.protected:
            split_record = _split_fix_record(block)
            output.extend(split_record if split_record else block.lines)
            continue
        words = " ".join(_payload(block, i) for i in range(len(block.lines))).split()
        if not words:
            output.extend(block.lines)
            continue
        output.extend(textwrap.wrap(
            " ".join(words), width=WIDTH,
            initial_indent=block.prefix, subsequent_indent=block.continuation,
            break_long_words=False, break_on_hyphens=False,
        ))
    newline = "\r\n" if "\r\n" in text and "\n" not in text.replace("\r\n", "") else "\n"
    return newline.join(output) + (newline if text.endswith(("\n", "\r")) else "")


def check_message(text):
    """Return a list of (1-based line number, original text) pairs for overlong body lines.

    Include long Fixes records that can be split before their quoted subjects.
    Exclude prose lines that cannot be shortened without splitting a word.
    """
    violations = []
    for block in _blocks(text.splitlines()):
        if block.protected:
            if _split_fix_record(block):
                violations.append((block.start + 1, block.lines[0]))
            continue
        for offset, line in enumerate(block.lines):
            if len(line) > WIDTH and len(_payload(block, offset).split()) > 1:
                violations.append((block.start + offset + 1, line))
    return violations


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument("--write", action="store_true", help="format the file in place")
    mode.add_argument("--check", action="store_true", help="report body lines needing wrapping")
    parser.add_argument("file", help="message file, or '-' for stdin")
    args = parser.parse_args()
    if args.write and args.file == "-":
        parser.error("--write requires a file, not stdin")
    try:
        if args.file == "-":
            text = sys.stdin.read()
        else:
            with open(args.file, encoding="utf-8", newline="") as source:
                text = source.read()
        if args.check:
            violations = check_message(text)
            for number, line in violations:
                print(f"{args.file}:{number}: body line has {len(line)} characters (limit {WIDTH}): {line}", file=sys.stderr)
            if violations:
                print("Run scripts/format_commit_message.py --write MESSAGE_FILE to format the message.", file=sys.stderr)
            return int(bool(violations))
        formatted = format_message(text)
        if args.write:
            if formatted != text:
                with open(args.file, "w", encoding="utf-8", newline="") as destination:
                    destination.write(formatted)
        else:
            sys.stdout.write(formatted)
        return 0
    except (OSError, UnicodeError) as exc:
        print(f"Cannot format commit message: {exc}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main())
