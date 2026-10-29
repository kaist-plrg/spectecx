set -eu

launcher=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
directory=$(mktemp -d "${TMPDIR:-/tmp}/spectec launcher.XXXXXX")
trap 'rm -rf "$directory"' EXIT
cd "$directory"

fail() {
  cat "$1" >&2
  printf '%s\n' "$2" >&2
  exit 1
}

for run in 1 2 3 4 5 6 7 8; do
  (
    status=0
    "$launcher" impty --help > "help.$run" 2>&1 || status=$?
    echo "$status" > "status.$run"
  ) &
done
wait
for run in 1 2 3 4 5 6 7 8; do
  if [ "$(cat "status.$run")" != 0 ] || ! grep -q 'impty commands' "help.$run"; then
    fail "help.$run" "concurrent run $run of $launcher failed"
  fi
done

printf 'impty.spec_dir = missing-spec\n' > spectecx.config
status=0
"$launcher" impty batch --color never > batch.out 2>&1 || status=$?
if [ "$status" != 1 ] ||
  ! grep -q 'spec directory missing-spec does not exist' batch.out; then
  fail batch.out "$launcher ignored spectecx.config in the working directory"
fi

printf 'Launcher concurrency and working-directory configuration passed\n'
