import Itpl.Definitions.Model.Eval

namespace Itpl

theorem EvalResult.OutOfFuelOrEq.bind {r s : EvalResult} {k l : Value → EvalResult}
    (h : EvalResult.OutOfFuelOrEq r s) (hk : ∀ v, EvalResult.OutOfFuelOrEq (k v) (l v)) :
    EvalResult.OutOfFuelOrEq (r.bind k) (s.bind l) := by
  rcases h with h | h
  · subst r
    exact Or.inl rfl
  · subst s
    cases r with
    | outOfFuel => exact Or.inl rfl
    | error => exact Or.inr rfl
    | value v => exact hk v

theorem eval_monotone {fuel more : Nat} {env : Env} {expr : Expr} (hle : fuel ≤ more) :
    EvalResult.OutOfFuelOrEq (eval fuel env expr) (eval more env expr) := by
  induction fuel generalizing more env expr with
  | zero => exact Or.inl rfl
  | succ fuel ih =>
    cases more with
    | zero => omega
    | succ more =>
      have hle' : fuel ≤ more := by omega
      cases expr with
      | num => exact Or.inr rfl
      | id => exact Or.inr rfl
      | lambda => exact Or.inr rfl
      | add left right | sub left right =>
        apply EvalResult.OutOfFuelOrEq.bind (ih hle')
        intro v
        cases v with
        | num n =>
          exact EvalResult.OutOfFuelOrEq.bind (ih hle') (fun _ => Or.inr rfl)
        | closure => exact Or.inr rfl
      | val x init next =>
        exact EvalResult.OutOfFuelOrEq.bind (ih hle') (fun _ => ih hle')
      | app fn arg =>
        apply EvalResult.OutOfFuelOrEq.bind (ih hle')
        intro v
        cases v with
        | num => exact Or.inr rfl
        | closure => exact EvalResult.OutOfFuelOrEq.bind (ih hle') (fun _ => ih hle')
      | if0 condition yes no =>
        apply EvalResult.OutOfFuelOrEq.bind (ih hle')
        intro v
        split
        · exact ih hle'
        · exact ih hle'
      | letrec => exact ih hle'

theorem eval_value_of_le {fuel more : Nat} {env : Env} {expr : Expr} {v : Value}
    (h : eval fuel env expr = .value v) (hle : fuel ≤ more) :
    eval more env expr = .value v := by
  rcases h ▸ eval_monotone (env := env) (expr := expr) hle with ht | he
  · cases ht
  · exact he.symm

end Itpl
