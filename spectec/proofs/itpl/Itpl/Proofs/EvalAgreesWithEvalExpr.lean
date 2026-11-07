import Itpl.Definitions.Spec.EvalExpr
import Itpl.Proofs.MoreFuelKeepsValuesAndErrors

namespace Itpl

theorem EvalResult.bind_eq_value {r : EvalResult} {k : Value → EvalResult} {v : Value}
    (h : r.bind k = .value v) : ∃ w, r = .value w ∧ k w = .value v := by
  cases r with
  | outOfFuel => cases h
  | error => cases h
  | value w => exact ⟨w, rfl, h⟩

theorem evalExpr_of_eval {fuel : Nat} {env : Env} {expr : Expr} {v : Value}
    (h : eval fuel env expr = .value v) : EvalExpr env expr v := by
  induction fuel generalizing env expr v with
  | zero => cases h
  | succ fuel ih =>
    cases expr with
    | num => cases h; exact .num
    | id =>
      simp only [eval] at h
      split at h
      next he => cases h; exact .id he
      next => cases h
    | lambda => cases h; exact .lambda
    | add left right | sub left right =>
      obtain ⟨l, hl, hk⟩ := EvalResult.bind_eq_value h
      cases l with
      | num n =>
        obtain ⟨r, hr, hout⟩ := EvalResult.bind_eq_value hk
        cases r with
        | num m => cases hout; first | exact .add (ih hl) (ih hr) | exact .sub (ih hl) (ih hr)
        | closure => cases hout
      | closure => cases hk
    | val x init next =>
      obtain ⟨w, hw, hb⟩ := EvalResult.bind_eq_value h
      exact .val (ih hw) (ih hb)
    | app fn arg =>
      obtain ⟨fval, hf, hk⟩ := EvalResult.bind_eq_value h
      cases fval with
      | num => cases hk
      | closure =>
        obtain ⟨w, hw, hb⟩ := EvalResult.bind_eq_value hk
        exact .app (ih hf) (ih hw) (ih hb)
    | if0 condition yes no =>
      obtain ⟨w, hw, hb⟩ := EvalResult.bind_eq_value h
      split at hb
      next => exact .if0Zero (ih hw) (ih hb)
      next hne => exact .if0Nonzero (ih hw) hne (ih hb)
    | letrec => exact .letrec rfl (ih h)

theorem exists_eval_of_evalExpr {env : Env} {expr : Expr} {v : Value} (h : EvalExpr env expr v) :
    ∃ fuel, eval fuel env expr = .value v := by
  induction h with
  | num => exact ⟨1, rfl⟩
  | id hlookup => exact ⟨1, by simp [eval, hlookup]⟩
  | lambda => exact ⟨1, rfl⟩
  | add _ _ ihl ihr | sub _ _ ihl ihr | val _ _ ihl ihr | if0Zero _ _ ihl ihr =>
    obtain ⟨n, hn⟩ := ihl
    obtain ⟨m, hm⟩ := ihr
    exact ⟨max n m + 1, by simp [eval, EvalResult.bind,
      eval_value_of_le hn (Nat.le_max_left n m), eval_value_of_le hm (Nat.le_max_right n m)]⟩
  | app _ _ _ ihf iha ihb =>
    obtain ⟨n, hn⟩ := ihf
    obtain ⟨m, hm⟩ := iha
    obtain ⟨k, hk⟩ := ihb
    let fuel := max n (max m k)
    have hnf : n ≤ fuel := by omega
    have hmf : m ≤ fuel := by omega
    have hkf : k ≤ fuel := by omega
    exact ⟨fuel + 1, by simp [eval, EvalResult.bind,
      eval_value_of_le hn hnf, eval_value_of_le hm hmf, eval_value_of_le hk hkf]⟩
  | if0Nonzero _ hne _ ihc ihn =>
    obtain ⟨n, hn⟩ := ihc
    obtain ⟨m, hm⟩ := ihn
    refine ⟨max n m + 1, ?_⟩
    simp only [eval, eval_value_of_le hn (Nat.le_max_left n m), EvalResult.bind]
    exact eval_value_of_le hm (Nat.le_max_right n m)
  | letrec henv_rec _ ih =>
    subst henv_rec
    obtain ⟨n, hn⟩ := ih
    exact ⟨n + 1, hn⟩

theorem exists_eval_iff_evalExpr {env : Env} {expr : Expr} {v : Value} :
    (∃ fuel, eval fuel env expr = .value v) ↔ EvalExpr env expr v :=
  ⟨fun ⟨_, h⟩ => evalExpr_of_eval h, exists_eval_of_evalExpr⟩

end Itpl
