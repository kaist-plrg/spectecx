AE arithmetic and VAE binding follow the same semantics in IL, SL, and PL.

  $ check() {
  >   target=$1
  >   expected=$2
  >   expression=$3
  >   printf '%s\n' "$expression" > "input.$target"
  >   for mode in '' --sl --pl; do
  >     actual=$(spectec "$target" eval $mode -p "input.$target" --color never) || return 1
  >     if [ "$actual" != "$expected" ]; then
  >       printf '%s %s: expected %s, got %s\n' "$target" "$mode" "$expected" "$actual"
  >       return 1
  >     fi
  >   done
  > }

Signed literals, arbitrary-size integers, grouping, and left-associative arithmetic:

  $ check ae 0 '0'
  $ check ae -10 '-10'
  $ check ae 40 '1+-3+42'
  $ check ae 3 '8-3-2'
  $ check ae 7 '8-(3-2)'
  $ check ae 3 '1--2'
  $ check ae 1000000000000000000000000000000 '999999999999999999999999999999+1'

VAE inherits arithmetic and resolves identifiers through the current environment.

  $ check vae -3 '(2 - 8) + 3'
  $ check vae 2 'val x = 1 in x + x'
  $ check vae 12 'val x = 10 in val y = 2 in x + y'
  $ check vae 2 'val x = 1 in val x = 2 in x'
  $ check vae 11 'val x = 10 in val x = x + 1 in x'
  $ check vae 21 'val x = 10 in (val x = x + 1 in x) + x'
  $ check vae 3 'val x = (val y = 3 in y) in x'

Free identifiers, including those outside a binding's scope, fail during evaluation.

  $ rejects() {
  >   printf '%s\n' "$1" > input.vae
  >   spectec vae parse -p input.vae --color never > /dev/null || return 1
  >   for mode in '' --sl --pl; do
  >     if spectec vae eval $mode -p input.vae --color never > error 2>&1; then
  >       printf 'unexpected success: %s\n' "$1"
  >       return 1
  >     fi
  >     grep -Eq 'source: (il|sl|pl)-interp' error || { cat error; return 1; }
  >   done
  > }
  $ rejects 'x'
  $ rejects 'val x = x in x'
  $ rejects '(val x = 1 in x) + x'
  $ rejects 'val x = (val y = 1 in y) in y'
  $ rejects 'val x = y in 0'
