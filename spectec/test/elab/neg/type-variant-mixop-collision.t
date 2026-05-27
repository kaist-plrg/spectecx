  $ ./main.exe type-variant-mixop-collision.spectec 2>&1
  error[elab/variant-mixop-collision]: variant cases are ambiguous: `FOO %`
    --> type-variant-mixop-collision.spectec:5:1
    |
  5 |   | FOO nat
    | ^^^^^^^^^^^
    | ...
    |
    | source: elab
    | note: Variant cases must differ in their literal tokens or argument positions; differences in argument types do not register. The cases shown here share the same sequence, so the elaborator cannot pick between them.
    | related: case with shape `FOO %`
    |   --> type-variant-mixop-collision.spectec:5:5
    |   |
    | 5 |   | FOO nat
    |   |     ^^^
    | related: case with shape `FOO %`
    |   --> type-variant-mixop-collision.spectec:6:5
    |   |
    | 6 |   | FOO int
    |   |     ^^^
  [1]
