spectec query: looks up names and definitions in the spec that contains the
given file or directory. Answers show paths relative to the working directory,
so the specs are copied there first:

  $ cp -RL ../../specs/impty/base imp
  $ cp -RL ../../specs/p4 p4

uses lists the uses of a name by the definitions that contain them. Clause
heads define a function and do not count as uses of it:

  $ spectec query uses imp '$lookup'
  imp/spec.spectec
    def $lookup: 13
    rule Check_expr/id: 76
    rule Check_command/assign: 115
    rule Eval_expr/id: 169
  4 uses in 4 definitions across 1 file

uses gives the clauses of one function one line and counts them as one
definition:

  $ spectec query uses imp K_query
  imp/spec.spectec
    def $lookup: 9, 10, 11, 12, 13
  5 uses in 1 definition across 1 file

A relation's uses are the premises that apply it, not its own rule headers:

  $ spectec query uses imp/spec.spectec Check_command
  imp/spec.spectec
    rule Check_command/ite: 120, 121
    rule Check_command/while: 126
    rule Check_command/seq: 130, 131
    rule Check_prog: 142
  6 uses in 4 definitions across 1 file

A query for a metavariable also finds its suffixed forms, such as c_1 and c_2:

  $ spectec query uses imp c | tail -3
    rule Eval_command/while-true: 227, 229, 230
    rule Eval_command/seq: 233, 234, 235
  24 uses in 8 definitions across 1 file

A syntax case declares its atoms, so only the rules use INT here:

  $ spectec query uses imp INT
  imp/spec.spectec
    rule Check_expr/num: 69
    rule Check_expr/add: 83, 84, 85
    rule Check_expr/leq: 89, 90
  6 uses in 3 definitions across 1 file

The type parameters in a definition's header and the atoms of a relation's
notation are declarations too:

  $ mkdir generic && cat > generic/a.spectec <<'EOF'
  > syntax T = nat
  > dec $id<T>(T) : T
  > def $id<T>(x) = x
  > relation Sub: T SUB T
  > rule Sub/refl: t SUB t
  > EOF
  $ spectec query uses generic T
  generic/a.spectec
    dec $id: 2
    relation Sub: 4
  4 uses in 2 definitions across 1 file
  $ spectec query uses generic SUB
  generic/a.spectec
    rule Sub/refl: 5
  1 use in 1 definition across 1 file

A file named through a symbolic link belongs to the spec that contains the
file the link points to:

  $ mkdir -p via-link/real via-link/view
  $ printf 'syntax real = nat\nsyntax pair = real\n' > via-link/real/a.spectec
  $ echo 'syntax view = real' > via-link/view/b.spectec
  $ ln -s ../real/a.spectec via-link/view/a.spectec
  $ spectec query uses via-link/view/a.spectec real
  via-link/real/a.spectec
    syntax pair: 2
  1 use in 1 definition across 1 file

A name without uses still gets a count:

  $ spectec query uses imp nothere
  0 uses in 0 definitions across 0 files

For a directory inside a marked spec, the query reads the whole spec and lists
uses by file:

  $ spectec query uses p4/5-typing '$empty_typingContext'
  p4/5-typing/5.06.2-typing-expression-eval.spectec
    rule Expr_eval_lctk/static_assert: 308
  p4/5-typing/5.11-typing-declaration.spectec
    rule Program_ok: 1160
  2 uses in 2 definitions across 2 files

When the parser rejects the spec, the query reports the diagnostics and fails:

  $ mkdir broken && echo 'syntax t =' > broken/a.spectec
  $ spectec query uses broken t --color never
  error[parse/syntax-empty-body]: empty type
    --> broken/a.spectec:1:11
    |
  1 | syntax t =
    |           ^
  [1]

When a path cannot be read, the query fails the same way:

  $ spectec query uses missing x --color never
  error: cannot read missing: No such file or directory
    --> missing
    |
    | source: io
  [1]

The query also fails when it cannot list a directory of the spec:

  $ mkdir -p locked/inner/sub && touch locked/locked.spec locked/a.spectec
  $ chmod 000 locked/inner/sub
  $ spectec query uses locked x --color never
  error: cannot list locked/inner/sub: Permission denied
    --> locked
    |
    | source: io
  [1]

Run from inside the spec, the message names the directory relative to the
working directory:

  $ (cd locked/inner && spectec query uses . x --color never)
  error: cannot list sub: Permission denied
    --> .
    |
    | source: io
  [1]
  $ chmod 755 locked/inner/sub

The query answers even for a spec with elaboration errors. A function
parameter's name in a declaration is not a use, but a clause head that binds $h
and a call of $h are:

  $ mkdir untyped && cat > untyped/a.spectec <<'EOF'
  > syntax point hint(show X) = { X nat, Y nat }
  > dec $f(nat) : undefinedtype
  > def $f(n) = n
  > dec $apply(def $h(nat) : nat, nat) : nat
  > def $apply(def $h, n) = $h(n)
  > dec $x(point) : nat
  > def $x(p) = p.X
  > EOF
  $ spectec query uses untyped undefinedtype
  untyped/a.spectec
    dec $f: 2
  1 use in 1 definition across 1 file
  $ spectec query uses untyped '$h'
  untyped/a.spectec
    def $apply: 5
  2 uses in 1 definition across 1 file

A struct type's field names and its hints are not uses, but a field access is:

  $ spectec query uses untyped X
  untyped/a.spectec
    def $x: 7
  1 use in 1 definition across 1 file
