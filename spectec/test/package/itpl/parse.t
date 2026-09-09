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

FAE and RFAE require local definitions, and RFAE conditionals require all three expressions with then and else delimiters.

  $ rejects fae 'def f(x) = x; f(1)'
  $ rejects fae 'def f() = 1 in f 0'
  $ rejects fae 'def f(x) = x in'
  $ rejects fae 'fun x ->'
  $ rejects rfae 'def f(x) = x; f(1)'
  $ rejects rfae 'def f(x) = x'
  $ rejects rfae 'def f() = 1 in f 0'
  $ rejects rfae 'if0 0 then 1'
  $ rejects rfae 'if0 0 1 2'
  $ rejects rfae 'if0 0 then else 2'
  $ rejects rfae 'if0 0 then 1 else'
  $ rejects rfae 'val if0 = 1 in if0'

Mutation syntax is limited to BFAE and MFAE. Only BFAE accepts a general expression as an assignment target and provides box operations.

  $ rejects fae '1; 2'
  $ rejects rfae 'x := 2'
  $ rejects bfae 'box'
  $ rejects bfae '!'
  $ rejects bfae 'val box = 1 in box'
  $ rejects bfae '1 :='
  $ rejects bfae '1;'
  $ rejects bfae 'def f(x) = x; f(1)'
  $ rejects mfae '1 := 2'
  $ rejects mfae '(x + 1) := 2'
  $ rejects mfae '(f x) := 2'
  $ rejects mfae 'x :='
  $ rejects mfae '1;'
  $ rejects mfae '!x'

KFAE requires a name and body for vcc. LFAE uses FAE syntax, and both languages require complete function expressions.

  $ rejects kfae 'vcc'
  $ rejects kfae 'vcc x'
  $ rejects kfae 'vcc x in'
  $ rejects kfae 'vcc in 1'
  $ rejects kfae 'vcc vcc in 1'
  $ rejects kfae 'val vcc = 1 in vcc'
  $ rejects kfae 'fun x ->'
  $ rejects kfae 'def f(x) = x; f(1)'
  $ rejects kfae '1; 2'
  $ rejects lfae 'fun x ->'
  $ rejects lfae 'def f(x) = x; f(1)'
  $ rejects lfae 'vcc x in x'
  $ rejects lfae '1; 2'
  $ rejects lfae 'x := 1'

Typed functions require parameter annotations, and TRFAE recursive definitions also require return annotations. Type names and arrow operands must be complete.

  $ rejects tfae 'fun x -> x'
  $ rejects tfae 'fun (x) -> x'
  $ rejects tfae 'fun (x : bool) -> x'
  $ rejects tfae 'fun (x : num ->) -> x'
  $ rejects tfae 'fun (x : -> num) -> x'
  $ rejects tfae 'fun (x : (num -> num) -> x'
  $ rejects tfae 'def f(x : num) : num = x in f 1'
  $ rejects tfae 'val x : num = 1 in x'
  $ rejects tfae '1; 2'
  $ rejects trfae 'fun x -> x'
  $ rejects trfae 'def f(x : num) = x in f 1'
  $ rejects trfae 'def f(x) : num = x in f 1'
  $ rejects trfae 'def f(x : num) : num = x'
  $ rejects trfae 'def f(x : num) : num -> = x in f 1'
  $ rejects trfae 'if0 0 then 1'
  $ rejects trfae 'if0 0 then 1 else'
  $ rejects trfae 'x := 1'
  $ rejects fae 'fun (x : num) -> x'
  $ rejects rfae 'def f(x : num) : num = x in f 1'

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

Conditional keywords remain identifiers in the earlier languages.

  $ accepts vae 'val if0 = 1 in val then = 2 in val else = 3 in if0 + then + else'
  $ accepts f1vae 'def if0(then) = then + 1; val else = 2 in if0(else)'
  $ accepts fvae 'val if0 = fun then -> then + 1 in val else = 2 in if0 else'
  $ accepts fae 'def if0(then) = then + 1 in val else = 2 in if0 else'

Only BFAE reserves box as a keyword.

  $ accepts vae 'val box = 2 in box'
  $ accepts f1vae 'def box(x) = x + 1; box(2)'
  $ accepts fvae '(fun box -> box + 1) 2'
  $ accepts fae 'def box(x) = x + 1 in box 2'
  $ accepts rfae 'def box(x) = if0 x then 0 else 1 + box (x - 1) in box 3'
  $ accepts mfae 'val box = 2 in box := 3'
  $ accepts bfae 'def if0(then) = then + 1 in val else = 2 in if0 else'

Only KFAE reserves vcc as a keyword.

  $ accepts vae 'val vcc = 2 in vcc'
  $ accepts f1vae 'def vcc(x) = x + 1; vcc(2)'
  $ accepts fvae '(fun vcc -> vcc + 1) 2'
  $ accepts fae 'def vcc(x) = x + 1 in vcc 2'
  $ accepts rfae 'def vcc(x) = if0 x then 0 else 1 + vcc (x - 1) in vcc 3'
  $ accepts bfae 'val vcc = box 2 in vcc := 3; !vcc'
  $ accepts mfae 'val vcc = 2 in vcc := 3'
  $ accepts lfae 'val vcc = 3 in vcc'
  $ accepts kfae 'def if0(then) = then + 1 in val else = 2 in if0 else'

Parsing and printing preserve the expression structure.

  $ spectec ae parse -r -p ../../../testdata/interp/itpl/ae/arithmetic.ae --color never
  ((3 - 1) + 2)
  $ spectec vae parse -r -p ../../../testdata/interp/itpl/vae/shadowing.vae --color never
  (val x = 10 in ((val x = (x + 1) in x) + x))
  $ spectec f1vae parse -r -p ../../../testdata/interp/itpl/f1vae/twice.f1vae --color never
  def twice(x) = (x + x); (twice(3) + twice(5))
  $ spectec fvae parse -r -p ../../../testdata/interp/itpl/fvae/capture.fvae --color never
  (val x = 10 in (val f = (fun y -> (x + y)) in (val x = 100 in (f (1)))))
  $ spectec fae parse -r -p ../../../testdata/interp/itpl/fae/sugar.fae --color never
  ((fun twice -> (twice (3))) ((fun x -> (x + x))))
  $ spectec rfae parse -r -p ../../../testdata/interp/itpl/rfae/sum.rfae --color never
  (def sum(x) = (if0 x then 0 else (x + (sum ((x - 1))))) in (sum (10)))
  $ spectec lfae parse -r -p ../../../testdata/interp/itpl/lfae/unused.lfae --color never
  ((fun x -> 7) (missing))
  $ spectec kfae parse -r -p ../../../testdata/interp/itpl/kfae/escape.kfae --color never
  (1 + (vcc k in ((k (2)) + 3)))

Printing FAE exposes the translation of val and def into functions and applications.

  $ printf '%s\n' 'val x = 1 in x + 2' > input.fae
  $ spectec fae parse -r -p input.fae --color never
  ((fun x -> (x + 2)) (1))
  $ printf '%s\n' 'def f(x) = x + 1 in f 2' > input.fae
  $ spectec fae parse -r -p input.fae --color never
  ((fun f -> (f (2))) ((fun x -> (x + 1))))

Mutation syntax and sequencing sugar round-trip without fixing the generated parameter name.

  $ accepts bfae 'val b = box 1 in b := 2; !b'
  $ accepts mfae '0; __seq'

The parser accepts num as an identifier in languages with term variables.

  $ for target in vae f1vae fvae fae rfae bfae mfae lfae kfae tfae trfae; do
  >   accepts "$target" 'val num = 3 in num' || exit 1
  > done
