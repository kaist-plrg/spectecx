#!/bin/sh
set -eu
cd "$(dirname "$0")"

for spec in tfae trfae; do
  fingerprint=$(awk -f strip-comments.awk "../../specs/itpl/$spec/spec.spectec" | git hash-object --stdin)
  printf '%s %s\n' "$fingerprint" "$spec"
done | diff -u Specs.expected -
lake build
lake env lean Audit.lean | diff -u Audit.expected -
printf '%s\n' 'Spec fingerprints, proofs and axiom audit passed.'
