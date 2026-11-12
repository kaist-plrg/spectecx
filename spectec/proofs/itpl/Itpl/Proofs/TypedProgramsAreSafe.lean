import Itpl.Definitions.Model.Eval
import Itpl.Definitions.Safety.CheckValue
import Itpl.Definitions.Safety.Refinement

namespace Itpl

theorem lookup_cons_of_ne {env : Env} {binding : Binding} {x : String} (h : x ≠ binding.name) :
    lookup (binding :: env) x = lookup env x := by
  cases binding <;> simp only [Binding.name] at h <;> simp [lookup, h]

inductive CheckResult : EvalResult → Ty → Prop where
  | outOfFuel {a : Ty} :
      CheckResult .outOfFuel a
  | value {v : Value} {a : Ty} :
      CheckValue v a →
      CheckResult (.value v) a

theorem updateBinding_typed {env : Env} {tenv : TypeEnv} {binding : Binding} {a : Ty}
    (henv : CheckEnv env tenv) (hbinding : CheckBinding binding a) :
    CheckEnv (updateBinding env binding) (updateType tenv binding.name a) := by
  induction env generalizing tenv with
  | nil =>
    cases henv
    exact .cons hbinding .nil
  | cons old rest ih =>
    cases henv with
    | cons hold hrest =>
      simp only [updateBinding, updateType]
      split
      next => exact .cons hbinding hrest
      next => exact .cons hold (ih hrest)

theorem update_typed {env : Env} {tenv : TypeEnv} {x : String} {v : Value} {a : Ty}
    (henv : CheckEnv env tenv) (hv : CheckValue v a) :
    CheckEnv (update env x v) (updateType tenv x a) :=
  updateBinding_typed henv (.value hv)

theorem bindRecursive_typed {env : Env} {tenv : TypeEnv} {f x : String} {a b : Ty} {body : Expr}
    (henv : CheckEnv env tenv)
    (hbody : CheckExpr (updateType (updateType tenv f (.arrow a b)) x a) body b) :
    CheckEnv (bindRecursive env f x body) (updateType tenv f (.arrow a b)) :=
  updateBinding_typed henv (.recDef henv hbody)

theorem lookup_typed {env : Env} {tenv : TypeEnv} {x : String} {a : Ty}
    (henv : CheckEnv env tenv) (h : lookupType tenv x = some a) :
    ∃ v, lookup env x = some v ∧ CheckValue v a := by
  induction env generalizing tenv with
  | nil => cases henv; simp [lookupType] at h
  | cons binding rest ih =>
    cases henv with
    | cons hbinding hrest =>
      by_cases heq : x = binding.name
      · cases hbinding with
        | value hv =>
          simp only [Binding.name] at heq
          simp only [lookup, lookupType, Binding.name, heq, ↓reduceIte] at h ⊢
          cases h
          exact ⟨_, rfl, hv⟩
        | recDef hcaptured hbody =>
          simp only [Binding.name] at heq
          simp only [lookup, lookupType, Binding.name, heq, ↓reduceIte] at h ⊢
          cases h
          exact ⟨_, rfl, .closure (bindRecursive_typed hcaptured hbody) hbody⟩
      · simp only [lookupType, heq, ↓reduceIte] at h
        rw [lookup_cons_of_ne heq]
        exact ih hrest h

theorem CheckResult.bind {r : EvalResult} {k : Value → EvalResult} {a b : Ty}
    (h : CheckResult r a) (hk : ∀ v, CheckValue v a → CheckResult (k v) b) :
    CheckResult (r.bind k) b := by
  cases h with
  | outOfFuel => exact .outOfFuel
  | value hv => exact hk _ hv

theorem eval_typed {tenv : TypeEnv} {expr : Expr} {a : Ty} {env : Env}
    (ht : CheckExpr tenv expr a) (henv : CheckEnv env tenv) (fuel : Nat) :
    CheckResult (eval fuel env expr) a := by
  induction fuel generalizing tenv expr a env with
  | zero => exact .outOfFuel
  | succ fuel ih =>
    cases ht with
    | num => exact .value .num
    | id h =>
      obtain ⟨v, hv, htype⟩ := lookup_typed henv h
      simp only [eval, hv]
      exact .value htype
    | lambda hbody => exact .value (.closure henv hbody)
    | add hl hr | sub hl hr =>
      apply CheckResult.bind (ih hl henv)
      intro v hv
      cases hv with
      | num =>
        apply CheckResult.bind (ih hr henv)
        intro w hw
        cases hw
        exact .value .num
    | val hinit hnext =>
      exact CheckResult.bind (ih hinit henv) (fun _ hv => ih hnext (update_typed henv hv))
    | app hfn harg =>
      apply CheckResult.bind (ih hfn henv)
      intro v hv
      cases hv with
      | closure hcaptured hbody =>
        exact CheckResult.bind (ih harg henv) (fun _ hw => ih hbody (update_typed hcaptured hw))
    | if0 hcond hyes hno =>
      apply CheckResult.bind (ih hcond henv)
      intro v hv
      cases hv with
      | num =>
        split
        · exact ih hyes henv
        · exact ih hno henv
    | letrec hbody hnext => exact ih hnext (bindRecursive_typed henv hbody)

theorem eval_ne_error {tenv : TypeEnv} {expr : Expr} {a : Ty} {env : Env}
    (ht : CheckExpr tenv expr a) (henv : CheckEnv env tenv) (fuel : Nat) :
    eval fuel env expr ≠ .error := by
  intro h
  have safe := eval_typed ht henv fuel
  rw [h] at safe
  cases safe

theorem eval_value_typed {tenv : TypeEnv} {expr : Expr} {a : Ty} {env : Env} {fuel : Nat}
    {v : Value} (ht : CheckExpr tenv expr a) (henv : CheckEnv env tenv)
    (h : eval fuel env expr = .value v) : CheckValue v a := by
  have safe := eval_typed ht henv fuel
  rw [h] at safe
  cases safe with
  | value hv => exact hv

theorem nb_no_error {env : Env} {expr : Expr} : ¬ HasBehavior .nb env expr .error := by
  simp [HasBehavior]

theorem ub_error_iff_eval_error {env : Env} {expr : Expr} :
    HasBehavior .ub env expr .error ↔ ∃ fuel, eval fuel env expr = .error := by
  simp [HasBehavior]

theorem refinement_iff_safe {env : Env} {expr : Expr} :
    Refines .ub .nb env expr ↔ Safe env expr := by
  constructor
  · intro href fuel herror
    exact nb_no_error (href .error (ub_error_iff_eval_error.mpr ⟨fuel, herror⟩))
  · intro hsafe behavior hb
    rcases hb with hp | hv | hd | ⟨_, fuel, hs⟩
    · exact Or.inl hp
    · exact Or.inr (Or.inl hv)
    · exact Or.inr (Or.inr (Or.inl hd))
    · exact False.elim (hsafe fuel hs)

theorem typed_refinement {tenv : TypeEnv} {expr : Expr} {a : Ty} {env : Env}
    (ht : CheckExpr tenv expr a) (henv : CheckEnv env tenv) :
    Refines .ub .nb env expr :=
  refinement_iff_safe.mpr (eval_ne_error ht henv)

theorem trfae_refinement {expr : Expr} {a : Ty} (ht : CheckExpr [] expr a) :
    Refines .ub .nb [] expr :=
  typed_refinement ht .nil

theorem typed_return {tenv : TypeEnv} {expr : Expr} {a : Ty} {env : Env} {v : Value}
    (ht : CheckExpr tenv expr a) (henv : CheckEnv env tenv)
    (hb : HasBehavior .ub env expr (.value v)) : CheckValue v a := by
  rcases hb with hp | ⟨fuel, w, heval, hw⟩ | ⟨hd, _⟩ | ⟨_, fuel, hs⟩
  · cases hp
  · cases hw
    exact eval_value_typed ht henv heval
  · cases hd
  · exact False.elim (eval_ne_error ht henv fuel hs)

end Itpl
