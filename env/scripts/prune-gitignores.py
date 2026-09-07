#!/usr/bin/env python3
"""Remove ignore rules that do not match any existing path."""

import argparse
import os
from pathlib import Path
import subprocess
import tempfile


HOME = Path.home()
GIT = [
    "git",
    f"--git-dir={HOME / '.home.git'}",
    f"--work-tree={HOME}",
]
IGNORE_FILES = [
    HOME / ".gitignore",
    HOME / "env/.gitignore",
    HOME / "env-private/home.gitignore",
]
PRESERVED_RULES = {"*~", "-*~"}


def git(*arguments, input_bytes=None):
    return subprocess.run(
        [*GIT, *arguments],
        cwd=HOME,
        input=input_bytes,
        stdout=subprocess.PIPE,
        check=True,
    ).stdout


def ignored_paths():
    output = git(
        "status",
        "--ignored=matching",
        "--porcelain=v1",
        "--untracked-files=all",
        "-z",
    )
    return [entry[3:] for entry in output.split(b"\0") if entry.startswith(b"!! ")]


def used_rules(paths):
    if not paths:
        return set()
    output = git(
        "check-ignore",
        "--no-index",
        "--verbose",
        "-z",
        "--stdin",
        input_bytes=b"\0".join(paths) + b"\0",
    )
    fields = output.split(b"\0")
    if fields[-1] == b"":
        fields.pop()
    if len(fields) % 4:
        raise ValueError("unexpected git check-ignore output")

    result = set()
    for offset in range(0, len(fields), 4):
        source, line_number, _, _ = fields[offset:offset + 4]
        source_path = Path(os.fsdecode(source))
        if not source_path.is_absolute():
            source_path = HOME / source_path
        result.add((source_path.resolve(), int(line_number)))
    return result


def is_rule(line):
    stripped = line.strip()
    return (
        bool(stripped)
        and stripped not in PRESERVED_RULES
        and not stripped.startswith(("#", "!"))
    )


def write_lines(path, lines):
    mode = path.stat().st_mode
    with tempfile.NamedTemporaryFile("w", dir=path.parent, delete=False) as output:
        output.writelines(lines)
        temporary = Path(output.name)
    temporary.chmod(mode)
    temporary.replace(path)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--apply",
        action="store_true",
        help="rewrite the ignore files; the default only reports",
    )
    arguments = parser.parse_args()

    missing = [path for path in IGNORE_FILES if not path.is_file()]
    if missing:
        raise ValueError("missing ignore files: " + ", ".join(map(str, missing)))

    used = used_rules(ignored_paths())
    total = 0
    for path in IGNORE_FILES:
        lines = path.read_text().splitlines(keepends=True)
        unused = [
            (line_number, line.rstrip("\n"))
            for line_number, line in enumerate(lines, 1)
            if is_rule(line) and (path.resolve(), line_number) not in used
        ]
        total += len(unused)
        print(f"{path}: {len(unused)} unused rules")
        for line_number, line in unused:
            print(f"  {line_number}: {line}")
        if arguments.apply and unused:
            unused_lines = {line_number for line_number, _ in unused}
            write_lines(
                path,
                [line for line_number, line in enumerate(lines, 1)
                 if line_number not in unused_lines],
            )

    action = "removed" if arguments.apply else "would remove"
    print(f"{action} {total} rules")


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        raise SystemExit(f"prune-gitignores: {error}")
