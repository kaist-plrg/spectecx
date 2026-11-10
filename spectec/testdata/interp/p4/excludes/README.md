`upstream/static/` is a direct mirror of the current `p4-spectec/excludes/static/`.

`upstream/dynamic/` is a direct mirror of the current `p4-spectec/excludes/dynamic/`. Its files list the STF tests that the P4 evaluation suites skip.

`local/unsupported-features/` is for exclusions caused by features that exist
upstream but are not implemented in `spectecx` yet.

`local/legacy/` contains the pre-existing local exclusions from this repository.
They are to be reconciled with `upstream/static` incrementally.
