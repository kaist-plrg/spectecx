# ITPL type safety proofs

These Lean 4 proofs show that well-typed TFAE and TRFAE programs cannot encounter a runtime error. The model interprets runtime errors in two ways. The undefined behavior (UB) interpretation permits every behavior after an error. The no behavior (NB) interpretation adds none for that error. UB-to-NB refinement means that every UB behavior is also an NB behavior. The proofs establish this property for handwritten Lean definitions of the typing and evaluation rules.

## Run

With [Lean's toolchain manager](https://github.com/leanprover/elan) installed, run from this directory:

```sh
./check.sh
```

The `lean-toolchain` file selects Lean 4.35.0. An installation of that release with its `bin` directory on `PATH` also works. The script builds the proofs, collects every axiom that a declaration in the `Itpl` modules depends on, and compares that list against `Audit.expected`. The accepted axioms are Lean's standard `propext`, `Quot.sound`, and `Classical.choice`. An admitted proof would add `sorryAx` to the list, and a project-specific axiom would add its own name.

Before building, `check.sh` compares the TFAE and TRFAE spec fingerprints against `Specs.expected`. A mismatch means a spec changed after the last comparison with the Lean definitions.

Comments and blank lines do not change a spec's fingerprint. After a spec change, compare the changed rules with `Definitions/Spec` and the `error` and `outOfFuel` cases of `eval`. Then copy the new fingerprint from the `diff` output of `check.sh` into `Specs.expected`.

## UB-to-NB refinement

Both interpretations use the same evaluator and differ only in the treatment of runtime errors.

```text
CheckExpr tenv expr type
CheckEnv env tenv
───────────────────────────────────────────────────────
HasBehavior ub env expr ⊆ HasBehavior nb env expr
```

`typed_refinement` proves this statement when `CheckEnv env tenv` relates the runtime environment to the type environment. `trfae_refinement` specializes it to programs in empty environments. TFAE programs are TRFAE programs without conditionals and recursive definitions, so `trfae_refinement` covers them as well.

`refinement_iff_safe` establishes that UB-to-NB refinement holds exactly when `eval` cannot return `error`, its result for a runtime error. `typed_return` separately proves that any returned value has the expression's assigned type.

## Evaluation and behavior

`eval fuel env expr` returns `value v`, `error`, or `outOfFuel`. Fuel bounds recursive evaluation depth. Each recursive call to `eval` receives one less fuel. With zero fuel, `eval` returns `outOfFuel`. The function returns a result for every input, including ill-typed programs.

The evaluator returns `error` for missing identifiers, nonnumeric arithmetic operands, and applying a nonfunction. An unsuccessful attempt to match one rule is not itself a runtime error. The `Eval_expr` rules describe only successful evaluation. The Lean `eval` definition supplies the error cases and specifies which operand it checks first.

`eval_monotone` proves that increasing fuel preserves `value` and `error` results. `loop_outOfFuel` proves that the recursive loop returns `outOfFuel` for every fuel amount. UB-to-NB refinement therefore covers unbounded execution without asserting that well-typed programs terminate.

The value clause of `HasBehavior` uses any fuel amount at which `eval` returns a value, and `eval_monotone` prevents a larger amount from producing an error or a different value.

The behavior model for these lecture languages is inspired by the UB/NB distinction in [Conditional Contextual Refinement, section 3.2](https://sf.snu.ac.kr/publications/ccr.pdf). `eval_typed` follows the approach of bounding recursive evaluation with fuel, as described in [Type Soundness Proofs with Definitional Interpreters](https://namin.seas.harvard.edu/files/big-step.pdf).

## What the proofs rely on

Lean checks each proof against the definitions used in that proof. A reader must check whether those definitions match the specs and express the intended meaning of safety.

| Location | Contents | Checked by |
|---|---|---|
| [Definitions/Spec](Itpl/Definitions/Spec/) | `Syntax.lean` defines the spec's syntax, `Environment.lean` defines the spec's environment functions, and `CheckExpr.lean` and `EvalExpr.lean` define the spec's relations. | A reader, against the spec |
| [Definitions/Model/Eval.lean](Itpl/Definitions/Model/Eval.lean) | `eval` implements evaluation with `value`, `error` and `outOfFuel` results. `EvalResult.OutOfFuelOrEq` appears in the statement of `eval_monotone`. | Lean for `value` results, through `exists_eval_iff_evalExpr`. A reader for the `error` and `outOfFuel` cases and `EvalResult.OutOfFuelOrEq` |
| [Definitions/Safety](Itpl/Definitions/Safety/) | `CheckValue.lean` defines typing for runtime values, bindings and environments. `Refinement.lean` defines UB and NB behaviors, `Refines` and `Safe`. | A reader |

The definitions in `Definitions/Safety` determine what the safety theorems mean.

The table names the claimed theorems in each module:

| Module | Claimed theorems |
|---|---|
| [TypedProgramsAreSafe.lean](Itpl/Proofs/TypedProgramsAreSafe.lean) | `typed_refinement`, `trfae_refinement`, `typed_return`, `refinement_iff_safe` |
| [LookupAgreesWithDomain.lean](Itpl/Proofs/LookupAgreesWithDomain.lean) | `mem_domain_iff`, `mem_domainType_iff` |
| [MoreFuelKeepsValuesAndErrors.lean](Itpl/Proofs/MoreFuelKeepsValuesAndErrors.lean) | `eval_monotone` |
| [EvalAgreesWithEvalExpr.lean](Itpl/Proofs/EvalAgreesWithEvalExpr.lean) | `exists_eval_iff_evalExpr` |

The theorem `exists_eval_iff_evalExpr` relates `eval` to `EvalExpr`. The theorems `mem_domain_iff` and `mem_domainType_iff` relate `lookup` and `lookupType` to `domain` and `domainType`, respectively. These theorems do not check the copied definitions against the spec.

## Correspondence with SpecTecX

The reference specs are [TFAE](../../specs/itpl/tfae/spec.spectec) and [TRFAE](../../specs/itpl/trfae/spec.spectec). The [ITPL specs README](../../specs/itpl/README.md) identifies their textbook sources. The `exists_eval_iff_evalExpr` theorem proves:

```text
EvalExpr env expr value ↔ there exists fuel such that eval fuel env expr = .value value
```

| SpecTecX | Lean |
|---|---|
| `_NUM`, `NUM`, and function types | `Expr.num`, `Ty.num`, and `Ty.arrow` |
| `VAL`, identifiers, `LAMBDA`, application | `Expr.val`, `Expr.id`, `Expr.lambda`, `Expr.app` |
| `IF0` and recursive `DEF` | `Expr.if0` and `Expr.letrec` |
| `` `< LAMBDA x '.' e ',' env `> `` and list environments | `Value.closure` and `Env` |
| Bindings `x -> v` and `f -> RECDEF d` | `Binding.value` and `Binding.recDef` |
| `$binding_name`, `$domain`, `$update_binding`, `$update`, `$bind_recursive`, `$lookup` | `Binding.name`, `domain`, `updateBinding`, `update`, `bindRecursive`, `lookup` |
| `typebinding`, `$domain_type`, `$update_type`, `$lookup_type` | Pairs in `TypeEnv`, `domainType`, `updateType`, `lookupType` |
| `Check_expr` | `CheckExpr` |
| `Eval_expr` rules | `EvalExpr` constructors |
| `Check_prog` and `Eval_prog` | Empty type and runtime environments |

The specs define `$lookup` and `$lookup_type` only on the domain of an environment. The `Eval_expr/id` rule checks `x <- $domain(env)` first, and `Check_expr/id` checks `x <- $domain_type(tenv)`. The Lean `lookup` and `lookupType` return `none` outside the domain, and `mem_domain_iff` and `mem_domainType_iff` prove that a name is in the domain exactly when its lookup returns a value. A Lean premise `lookup env x = some v` therefore stands for both the domain premise and the lookup.

The shared model uses TRFAE's environment functions. TFAE's `$update` works directly on value bindings and computes the same environment as `update` here. TFAE programs never create a `RECDEF` entry.

The proofs do not cover the SpecTecX parser, elaborator or interpreters.

## Examples

`Examples.lean` defines the five programs below and proves the listed properties.

| Program | Checked property |
|---|---|
| `capture` | Captured `x = 10` survives a caller binding of `x = 100`, producing `11` |
| `sum` | A typed recursive function produces `55` |
| `loop` | A typed recursive program diverges and satisfies UB-to-NB refinement |
| `badApplication` | Applying `1` to `2` produces `error` and fails UB-to-NB refinement |
| `badAddition` | Adding an integer and a closure produces `error` and fails UB-to-NB refinement |
