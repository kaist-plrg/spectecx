set -eu

test_switch=$(opam switch show)

run_spectec() {
  opam exec --switch="$test_switch" -- spectec "$@"
}

assert_missing() {
  command=$1
  if run_spectec --help | grep -Eq "^  ${command}[[:space:]]"; then
    printf 'unexpected target command: %s\n' "$command" >&2
    exit 1
  fi
}

check_target() {
  package=$1
  command=$2
  shift 2

  opam install --yes --with-test "./${package}.opam"
  run_spectec --help | grep -Eq "^  ${command}[[:space:]]"
  run_spectec "$@"
  opam remove --yes "$package"
  assert_missing "$command"
}

opam install --yes --with-test ./spectec.opam
assert_missing impty
assert_missing miniml
assert_missing p4
assert_missing ae
assert_missing vae
assert_missing f1vae
assert_missing fvae
assert_missing fae
assert_missing rfae
assert_missing bfae
assert_missing mfae
assert_missing lfae
assert_missing kfae
assert_missing tfae
assert_missing trfae
assert_missing tvfae

check_target spectec-target-p4 p4 p4 parse -p spectec/testdata/interp/p4/p4c/p4_16_samples/issue2342.p4 --color never
check_target spectec-target-miniml miniml miniml parse -p spectec/testdata/interp/miniml/add.ml --color never
check_target spectec-target-impty impty impty parse -p spectec/testdata/interp/impty/base/hello.imp --color never

repo_root=$(pwd -P)
itpl_test_dir=$(mktemp -d)
trap 'rm -rf "$itpl_test_dir"' EXIT

check_itpl() {
  target=$1
  expected=$2
  input=$3
  task=${4:-eval}
  actual=$(cd "$itpl_test_dir" && run_spectec "$target" "$task" -p "$input" --color never)
  if [ "$actual" != "$expected" ]; then
    printf '%s: expected %s, got %s\n' "$target" "$expected" "$actual" >&2
    exit 1
  fi
}

opam install --yes --with-test ./spectec-target-itpl.opam
check_itpl ae 4 "${repo_root}/spectec/testdata/interp/itpl/ae/arithmetic.ae"
check_itpl vae 21 "${repo_root}/spectec/testdata/interp/itpl/vae/shadowing.vae"
check_itpl f1vae 16 "${repo_root}/spectec/testdata/interp/itpl/f1vae/twice.f1vae"
check_itpl fvae 11 "${repo_root}/spectec/testdata/interp/itpl/fvae/capture.fvae"
check_itpl fae 6 "${repo_root}/spectec/testdata/interp/itpl/fae/sugar.fae"
check_itpl rfae 55 "${repo_root}/spectec/testdata/interp/itpl/rfae/sum.rfae"
check_itpl bfae 7 "${repo_root}/spectec/testdata/interp/itpl/bfae/alias.bfae"
check_itpl mfae 2 "${repo_root}/spectec/testdata/interp/itpl/mfae/counter.mfae"
check_itpl lfae 7 "${repo_root}/spectec/testdata/interp/itpl/lfae/unused.lfae"
check_itpl kfae 3 "${repo_root}/spectec/testdata/interp/itpl/kfae/escape.kfae"
check_itpl tfae 42 "${repo_root}/spectec/testdata/interp/itpl/tfae/twice.tfae"
check_itpl trfae 55 "${repo_root}/spectec/testdata/interp/itpl/trfae/sum.trfae"
check_itpl tvfae 8 "${repo_root}/spectec/testdata/interp/itpl/tvfae/variant.tvfae"
check_itpl tfae num "${repo_root}/spectec/testdata/interp/itpl/tfae/twice.tfae" typecheck
check_itpl trfae num "${repo_root}/spectec/testdata/interp/itpl/trfae/sum.trfae" typecheck
check_itpl tvfae num "${repo_root}/spectec/testdata/interp/itpl/tvfae/variant.tvfae" typecheck
opam remove --yes spectec-target-itpl
assert_missing ae
assert_missing vae
assert_missing f1vae
assert_missing fvae
assert_missing fae
assert_missing rfae
assert_missing bfae
assert_missing mfae
assert_missing lfae
assert_missing kfae
assert_missing tfae
assert_missing trfae
assert_missing tvfae
