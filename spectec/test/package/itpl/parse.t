AE rejects VAE syntax and both parsers require complete expressions.

  $ rejects() {
  >   target=$1
  >   printf '%s\n' "$2" > "input.$target"
  >   if spectec "$target" parse -p "input.$target" --color never > error 2>&1; then
  >     printf 'unexpected success: %s\n' "$2"
  >     return 1
  >   fi
  >   grep -q 'syntax error' error || { cat error; return 1; }
  > }
  $ rejects ae 'x'
  $ rejects ae 'val x = 1 in x'
  $ rejects ae '1 2'
  $ rejects ae '1 +'
  $ rejects ae '(1 + 2'
  $ rejects ae '-(1 + 2)'
  $ rejects vae 'val x = 1'
  $ rejects vae 'val x = 1 in'
  $ rejects vae 'val in = 1 in in'

Each language accepts only its own function syntax, and F1VAE definitions require one named parameter and a final expression.

  $ rejects vae 'f(1)'
  $ rejects vae 'fun x -> x'
  $ rejects f1vae '(fun x -> x) 1'
  $ rejects f1vae 'def f(x) = x; f(1)(2)'
  $ rejects f1vae 'def f() = 1; f(0)'
  $ rejects f1vae 'def f(x) = x'
  $ rejects f1vae 'def f(x) = x;'
  $ rejects f1vae 'def f(x) = x f(1)'
  $ rejects f1vae 'def f(x) = x; (f)(1)'
  $ rejects fvae 'def f(x) = x; f(1)'
  $ rejects fvae 'fun -> 1'
  $ rejects fvae 'fun x ->'
  $ rejects fvae 'val fun = 1 in fun'

Lexical errors report the line and column of the invalid character.

  $ printf '1 +\n  @\n' > input.ae
  $ spectec ae parse -p input.ae --color never
  error: unexpected character '@'
    --> input.ae:2:3
    |
  2 |   @
    |   ^
    |
    | source: task-parse
  [1]

Keywords remain identifiers in languages that do not reserve them.

  $ accepts() {
  >   target=$1
  >   printf '%s\n' "$2" > "input.$target"
  >   spectec "$target" parse -r -p "input.$target" --color never > /dev/null
  > }

Keywords are reserved only in the languages that use them.

  $ accepts vae 'val fun = 1 in val def = 2 in fun + def'
  $ accepts f1vae 'def fun(x) = x; fun(3)'
  $ accepts fvae 'val def = fun x -> x in def 3'

Parsing and printing preserve the expression structure.

  $ spectec ae parse -r -p ../../../testdata/interp/itpl/ae/arithmetic.ae --color never
  ((3 - 1) + 2)
  $ spectec vae parse -r -p ../../../testdata/interp/itpl/vae/shadowing.vae --color never
  (val x = 10 in ((val x = (x + 1) in x) + x))
  $ spectec f1vae parse -r -p ../../../testdata/interp/itpl/f1vae/twice.f1vae --color never
  def twice(x) = (x + x); (twice(3) + twice(5))
  $ spectec fvae parse -r -p ../../../testdata/interp/itpl/fvae/capture.fvae --color never
  (val x = 10 in (val f = (fun y -> (x + y)) in (val x = 100 in (f (1)))))
