#!/bin/sh
set -eu
cd "$(dirname "$0")"

lake build
lake env lean Audit.lean | diff -u Audit.expected -
printf '%s\n' 'Proofs and axiom audit passed.'
