# Introduction to Programming Languages (ITPL)

The `spectec-target-itpl` package provides independently registered targets for *Introduction to Programming Languages* by Jaemin Hong and Sukyoung Ryu (August 10, 2023), the textbook for KAIST CS320. Each specification is self-contained.

| Target | Textbook section | Coverage |
| --- | --- | --- |
| [AE](ae/spec.spectec) | 6.4 | Integers and arithmetic |
| [VAE](vae/spec.spectec) | 7.3 | Variable bindings |
| [F1VAE](f1vae/spec.spectec) | 8.2 | Named first-order functions |
| [FVAE](fvae/spec.spectec) | 9.2 | Anonymous functions and closures |
| [FAE](fae/spec.spectec) | 9.4 | Bindings as application sugar |
| [RFAE](rfae/spec.spectec) | 10.2 | Conditionals and recursive functions |
| [BFAE](bfae/spec.spectec) | 11.2 | Mutable boxes over FAE |
| [MFAE](mfae/spec.spectec) | 12.2 | Mutable variables over FAE, call-by-value |
| [LFAE](lfae/spec.spectec) | 14.1 | Call-by-name evaluation |
| [KFAE](kfae/spec.spectec) | 16.1, 17.2 | First-class continuations, explicit big-step rules |

## Use

```sh
./spectecx ae eval -p spectec/testdata/interp/itpl/ae/arithmetic.ae
./spectecx vae parse -r -p spectec/testdata/interp/itpl/vae/shadowing.vae
```

All targets provide `eval`, `parse`, and `batch`. Evaluation supports IL (the default), `--sl`, and `--pl`. `parse -r` checks printing and reparsing after desugaring. Input extensions match target names. Batch filenames containing `_error` indicate expected failures.

Concrete inputs accept signed decimal integers, parentheses, and left-associative `+` and `-` at equal precedence. General unary negation is absent. Identifiers start with a letter or underscore and may contain letters, digits, underscores, or apostrophes. Binding and function bodies extend as far right as possible, so use parentheses when placing them inside other expressions.

## Notation and maps

Integer arithmetic uses mathematical integers without fixed-width overflow. `_NUM` and `_ID` distinguish literals and identifiers internally. The ordinary evaluation arrow is `==>`.

Evaluation judgments are relations. Map operations are explicit functions over cons-lists. `$domain`, `$lookup`, and `$update` represent domain membership, map application, and overriding update. Lookup is partial. Evaluation from empty maps preserves one entry per key.

The specs use abstract lambda and closure forms. The parser accepts `fun x -> e` and left-associative application `e e`. Application binds tighter than arithmetic. Write a negative argument as `f (-1)`, since `f - 1` is subtraction.

In BFAE, `box` and `!` bind tighter than application. Assignment binds below arithmetic and above sequencing. Assignment and sequencing associate to the right. MFAE assignment targets must be identifiers.

## Adaptations and selected variants

| Language | Representation or scope choice |
| --- | --- |
| F1VAE | A program wrapper supplies the external function environment from `def f(x) = e;` declarations and a final expression. Duplicate function names are rejected. |
| FAE and descendants | The parser translates nonrecursive `val` bindings and local `def` bindings into lambda applications. RFAE has a core recursive `def` form. |
| RFAE | Finite recursive bindings encode the cyclic environment equation in Rec. A binding captures the environment before installing itself. Lookup reconstructs an ordinary closure with the self-binding, so App uses ordinary closures. Concrete `if0 e then e else e` delimits the three abstract operands. |
| BFAE and MFAE | `_ADDR nat` distinguishes addresses from integers. `$fresh` selects one above the largest allocated address, starting at 1, while the textbook permits any fresh address. Address labels are representation choices, and textbook comparisons must account for address renaming. Both targets extend FAE without RFAE conditionals or recursive definitions. |
| MFAE | Implements call-by-value (§12.2), excluding call-by-reference (§12.4). Sequencing is application sugar. Its unused parameter is absent from all identifier occurrences, including assignment targets, a stronger condition than absence from free identifiers. |
| LFAE | Implements call-by-name (§14.1), excluding the call-by-need cache (§14.3). `Strict` forces delayed values in arithmetic, function position, and the final result. |
| KFAE | Uses §17.2's bracketed continuation contexts with `HOLE` for the square hole. Subtraction extends the displayed addition rules symmetrically. Capture uses `vcc x in e` from §16.1's abstract grammar, which §17.2 writes as `vcc x; e`. |

## Limits

These big-step relations describe terminating evaluation. Absence of a derivation does not distinguish stuckness from divergence. The tests check selected program results across IL, SL, and PL. They do not prove textbook equivalence or type soundness. BFAE and MFAE evaluation judgments return a final store, but the CLI prints only the result value. Each input starts with an empty store, and box addresses print as their natural-number labels.
