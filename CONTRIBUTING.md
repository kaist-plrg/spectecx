# Contributing

We add shared rules when recurring contribution problems justify them. Suggestions for improving this guide are welcome.

See [README.md](README.md) for setup and project orientation, and [Makefile](Makefile) for build and test commands. Format OCaml changes with `make fmt`. The formatter version is pinned in [spectec/.ocamlformat](spectec/.ocamlformat), and CI checks formatting with that version. [editors/vscode/README.md](editors/vscode/README.md) describes the checks for the VS Code extension.

## Review and merging

Review first addresses what the change does. Once that is settled, review addresses how the code is organized, including names and module boundaries.

Once review is resolved, the core maintainer prepares the branch immediately before merging. The maintainer folds commits that fix review findings into the commits they belong to. The maintainer also reorders or combines commits so every commit builds, and fixes small wording problems in comments and commit messages without changing their meaning.

This preparation preserves intent, source references, and author credit. If preparation changes behavior or meaning, the maintainer sends those changes back for review before merging. Rewriting a branch that other people use requires their agreement.

Write comments and commit messages for the final history. Good comments and messages usually survive the maintainer's edits with few changes. You may push review fixes as new commits with short messages. Do not rewrite branch history before the merge. The core maintainer does that.

## Contributor expectations

These expectations are the project's requirements. Agree any exception with the core maintainer before proceeding.

- Submit changes through a PR by default.
- Keep each PR focused on one topic. Identify incidental changes separately.
- Explain the motivation, result, and scope. Include enough context for a reviewer who has not followed your work.
- Follow the surrounding code's naming and style. Keep comments and documentation accurate to the current system. Explain changes in commit messages and PR descriptions.
- Identify copied or adapted work, such as a port from P4-SpecTec, cite its sources, and preserve author credit. Explain local adaptations.
- Get affected contributors' agreement before rewriting a branch they use or publishing a rewrite of their branch.
- Take responsibility for the work you submit, whatever tools helped produce it. Understand your changes well enough to explain them and answer review questions. Routine AI use needs no disclosure. Describe tool use only when a research method, result, or limitation depends on it.
