# Introduction to Programming Languages (ITPL)

The `spectec-target-itpl` package provides independently registered targets for *Introduction to Programming Languages* by Jaemin Hong and Sukyoung Ryu (August 10, 2023), the textbook for KAIST CS320. Each specification is self-contained.

| Target | Textbook section | Coverage |
| --- | --- | --- |
| [AE](ae/spec.spectec) | 6.4 | Integers and arithmetic |
| [VAE](vae/spec.spectec) | 7.3 | Variable bindings |

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

## Limits

These big-step relations describe terminating evaluation. Absence of a derivation does not distinguish stuckness from divergence. The tests check selected program results across IL, SL, and PL. They do not prove textbook equivalence or type soundness.
