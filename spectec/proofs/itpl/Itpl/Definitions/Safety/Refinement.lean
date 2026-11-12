import Itpl.Definitions.Model.Eval

namespace Itpl

inductive ErrorInterpretation where
  | ub
  | nb

inductive Behavior where
  | value (v : Value)
  | divergence
  | error
  | partialTrace

def HasBehavior (interpretation : ErrorInterpretation) (env : Env) (expr : Expr)
    (behavior : Behavior) : Prop :=
  behavior = .partialTrace ∨
  (∃ fuel v, eval fuel env expr = .value v ∧ behavior = .value v) ∨
  (behavior = .divergence ∧ ∀ fuel, eval fuel env expr = .outOfFuel) ∨
  (interpretation = .ub ∧ ∃ fuel, eval fuel env expr = .error)

def Refines (left right : ErrorInterpretation) (env : Env) (expr : Expr) : Prop :=
  ∀ behavior, HasBehavior left env expr behavior → HasBehavior right env expr behavior

def Safe (env : Env) (expr : Expr) : Prop :=
  ∀ fuel, eval fuel env expr ≠ .error

end Itpl
