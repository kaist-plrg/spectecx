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
  actual=$(cd "$itpl_test_dir" && run_spectec "$target" eval -p "$input" --color never)
  if [ "$actual" != "$expected" ]; then
    printf '%s: expected %s, got %s\n' "$target" "$expected" "$actual" >&2
    exit 1
  fi
}

opam install --yes --with-test ./spectec-target-itpl.opam
check_itpl ae 4 "${repo_root}/spectec/testdata/interp/itpl/ae/arithmetic.ae"
check_itpl vae 21 "${repo_root}/spectec/testdata/interp/itpl/vae/shadowing.vae"
opam remove --yes spectec-target-itpl
assert_missing ae
assert_missing vae
