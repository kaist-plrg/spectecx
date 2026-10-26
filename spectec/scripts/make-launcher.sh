#!/bin/sh
set -eu

switch=$1
program=$2

# Run under `dune exec`, which sets the Dune variables checked below.
: "${DUNE_DIR_LOCATIONS:?Dune site locations are required}"
: "${OCAMLPATH:?Dune library paths are required}"
: "${DUNE_OCAML_STDLIB:?Dune standard-library path is required}"
: "${DUNE_OCAML_HARDCODED:?Dune runtime library paths are required}"

quote() {
  printf "'%s'" "$(printf '%s' "$1" | sed "s/'/'\\\\''/g")"
}

# `env` reads a program path that contains `=` as an assignment, so `sh` runs
# the program.
printf '#!/bin/sh\nexec '
for arg in opam exec "--switch=$switch" -- env \
  "DUNE_DIR_LOCATIONS=$DUNE_DIR_LOCATIONS" \
  "DUNE_OCAML_STDLIB=$DUNE_OCAML_STDLIB" \
  "DUNE_OCAML_HARDCODED=$DUNE_OCAML_HARDCODED" \
  "DUNE_SOURCEROOT=${DUNE_SOURCEROOT:-}" \
  "OCAMLPATH=$OCAMLPATH" \
  "CAML_LD_LIBRARY_PATH=${CAML_LD_LIBRARY_PATH:-}" \
  /bin/sh -c 'exec "$0" "$@"' "$program"; do
  quote "$arg"
  printf ' '
done
printf '"$@"\n'
