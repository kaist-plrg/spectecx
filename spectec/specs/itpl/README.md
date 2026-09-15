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
| [TFAE](tfae/spec.spectec) | 19.5, 19.6.1 | Annotated functions and nonrecursive bindings |
| [TRFAE](trfae/spec.spectec) | 20.1-20.5 | Numeric conditions and annotated recursion |
| [TVFAE](tvfae/spec.spectec) | 21.1-21.5 | Named algebraic data types |
| [PTFAE](ptfae/spec.spectec) | 22.1-22.3 | Explicit polymorphism |
| [STFAE](stfae/spec.spectec) | 23, exercises 23.3-23.4 | Records and structural subtyping |

## Use

```sh
./spectecx ae eval -p spectec/testdata/interp/itpl/ae/arithmetic.ae
./spectecx vae parse -r -p spectec/testdata/interp/itpl/vae/shadowing.vae
```

All targets provide `eval`, `parse`, and `batch`. Evaluation supports IL (the default), `--sl`, and `--pl`. `parse -r` checks printing and reparsing after desugaring. Typed targets also provide `typecheck`, which returns the program type. Their `eval` command checks before evaluating, and `batch` runs both tasks. Input extensions match target names. Batch filenames containing `_error` indicate expected failures.

Concrete inputs accept signed decimal integers, parentheses, and left-associative `+` and `-` at equal precedence. General unary negation is absent. Identifiers start with a letter or underscore and may contain letters, digits, underscores, or apostrophes. Binding and function bodies extend as far right as possible, so use parentheses when placing them inside other expressions.

## Notation and maps

Integer arithmetic uses mathematical integers without fixed-width overflow. `_NUM` and `_ID` distinguish literals and identifiers internally. The ordinary evaluation arrow is `==>`.

Evaluation judgments are relations. Map operations are explicit functions over cons-lists. `$domain`, `$lookup`, and `$update` represent domain membership, map application, and overriding update. Lookup is partial. Evaluation from empty maps preserves one entry per key.

The specs use abstract lambda and closure forms. `LAMBDA` denotes λ, and PTFAE's `TLAMBDA` denotes Λ. The parser accepts `fun x -> e` and left-associative application `e e`. Application binds tighter than arithmetic. Write a negative argument as `f (-1)`, since `f - 1` is subtraction.

In BFAE, `box` and `!` bind tighter than application. Assignment binds below arithmetic and above sequencing. Assignment and sequencing associate to the right. MFAE assignment targets must be identifiers.

## Adaptations and selected variants

| Language | Representation or scope choice |
| --- | --- |
| F1VAE | A program wrapper supplies the external function environment from `def f(x) = e;` declarations and a final expression. Duplicate function names are rejected. |
| FAE and its untyped descendants | The parser translates nonrecursive `val` bindings and local `def` bindings into lambda applications. RFAE has a core recursive `def` form. |
| RFAE | Finite recursive bindings encode the cyclic environment equation in Rec. A binding captures the environment before installing itself. Lookup reconstructs an ordinary closure with the self-binding, so App uses ordinary closures. Concrete `if0 e then e else e` delimits the three abstract operands. |
| BFAE and MFAE | `_ADDR nat` distinguishes addresses from integers. `$fresh` selects one above the largest allocated address, starting at 1, while the textbook permits any fresh address. Address labels are representation choices, and textbook comparisons must account for address renaming. Both targets extend FAE without RFAE conditionals or recursive definitions. |
| MFAE | Implements call-by-value (§12.2), excluding call-by-reference (§12.4). Sequencing is application sugar. Its unused parameter is absent from all identifier occurrences, including assignment targets, a stronger condition than absence from free identifiers. |
| LFAE | Implements call-by-name (§14.1), excluding the call-by-need cache (§14.3). `Strict` forces delayed values in arithmetic, function position, and the final result. |
| KFAE | Uses §17.2's bracketed continuation contexts with `HOLE` for the square hole. Subtraction extends the displayed addition rules symmetrically. Capture uses `vcc x in e` from §16.1's abstract grammar, which §17.2 writes as `vcc x; e`. |
| TFAE, TRFAE, TVFAE, PTFAE and STFAE | Functions use `fun (x : t) -> e`, with `num` and right-associative function types `t1 -> t2`. The parser accepts `num` as a term identifier. The specifications define primitive, unannotated, eager `val` bindings (§19.6.1). Type environments use overriding maps, with `:` separating identifiers from types. |
| TFAE | The parser translates nonrecursive local `def` bindings into `val` bindings of annotated functions. The pair extension (§19.6.2) is absent. |
| TRFAE | Recursive functions use `def f(x : t1) : t2 = e1 in e2`, requiring both annotations. The checker follows Typ-If0, requiring a numeric condition and equal branch types. Recursive environment entries and ordinary closures follow RFAE. Lookup reconstructs the self-binding, and application binds the parameter, which takes precedence if the parameter and function names coincide. |
| TVFAE | The parser accepts `type T = C1@t1 + C2@t2 in e` and `e match { C1(x) -> e1; C2(y) -> e2 }`. The checker follows the main Typ-TypeDef rule, including the type-name reuse counterexample in §21.6. When both constructor declarations use the same name, the checker uses the argument type from the second declaration, as in the Scala checker (§21.5). The evaluator accepts repeated match cases and selects the first matching case, as the Scala interpreter does (§21.3). The displayed Match-L and Match-R rules (§21.2) both apply when case names coincide. The specification follows Scala to resolve this apparent omission. A match with two cases for one constructor fails during evaluation if the matched value uses the other constructor. `_runtime_error` examples pass checking and fail evaluation. |
| PTFAE | The parser accepts `tfun A -> e`, `e[t]`, and `forall A. t`. The specification defines capture-avoiding substitution but retains the named typing rules and structural type equality from §22.3, including the type-name reuse counterexample. The de Bruijn and type-renaming alternatives are absent. `_runtime_error` examples pass checking and fail evaluation. |
| STFAE | The parser accepts records `{a = e}`, record types `{a : t}`, projection `e.a`, `top`, and `bottom`. `Check_expr` computes a minimal type following exercises 23.3-23.4. `Check_type` checks against a supplied supertype. The specification defines a subtyping algorithm instead of searching the declarative rules. |

## Limits

These big-step relations describe terminating evaluation. Absence of a derivation does not distinguish stuckness from divergence. The tests check selected program results across IL, SL, and PL. They do not prove textbook equivalence or type soundness. BFAE and MFAE evaluation judgments return a final store, but the CLI prints only the result value. Each input starts with an empty store, and box addresses print as their natural-number labels.
