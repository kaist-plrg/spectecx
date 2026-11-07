import Itpl.Proofs.TypedProgramsAreSafe
import Itpl.Proofs.EvalAgreesWithEvalExpr

namespace Itpl.Examples

def capture : Expr :=
  .val "x" (.num 10)
    (.val "f" (.lambda "y" .num (.add (.id "x") (.id "y")))
      (.val "x" (.num 100) (.app (.id "f") (.num 1))))

theorem capture_typed : CheckExpr [] capture .num := by
  unfold capture
  apply CheckExpr.val CheckExpr.num
  apply CheckExpr.val
  · apply CheckExpr.lambda
    exact CheckExpr.add (.id rfl) (.id rfl)
  · apply CheckExpr.val CheckExpr.num
    exact CheckExpr.app (.id rfl) .num

theorem capture_result : eval 8 [] capture = .value (.num 11) := rfl

theorem capture_refines : Refines .ub .nb [] capture := trfae_refinement capture_typed

def sum : Expr :=
  .letrec "sum" "x" .num .num
    (.if0 (.id "x") (.num 0) (.add (.id "x") (.app (.id "sum") (.sub (.id "x") (.num 1)))))
    (.app (.id "sum") (.num 10))

theorem sum_typed : CheckExpr [] sum .num := by
  unfold sum
  apply CheckExpr.letrec
  · apply CheckExpr.if0 (.id rfl) .num
    exact .add (.id rfl) (.app (.id rfl) (.sub (.id rfl) .num))
  · exact .app (.id rfl) .num

theorem sum_result : eval 64 [] sum = .value (.num 55) := rfl

theorem sum_refines : Refines .ub .nb [] sum := trfae_refinement sum_typed

def badApplication : Expr := .app (.num 1) (.num 2)

theorem badApplication_error : eval 2 [] badApplication = .error := rfl

theorem badApplication_not_typed {a : Ty} : ¬ CheckExpr [] badApplication a := by
  intro h
  exact eval_ne_error h .nil 2 badApplication_error

theorem badApplication_not_refines : ¬ Refines .ub .nb [] badApplication := by
  intro h
  exact refinement_iff_safe.mp h 2 badApplication_error

def badAddition : Expr := .add (.num 1) (.lambda "x" .num (.id "x"))

theorem badAddition_error : eval 2 [] badAddition = .error := rfl

theorem badAddition_not_refines : ¬ Refines .ub .nb [] badAddition := by
  intro h
  exact refinement_iff_safe.mp h 2 badAddition_error

def loopBody : Expr := .app (.id "loop") (.id "x")
def loopEnv (v : Value) : Env := [.recDef "loop" "x" loopBody [], .value "x" v]
def loop : Expr := .letrec "loop" "x" .num .num loopBody (.app (.id "loop") (.num 0))

theorem loop_typed : CheckExpr [] loop .num := by
  unfold loop loopBody
  exact .letrec (.app (.id rfl) (.id rfl)) (.app (.id rfl) .num)

theorem loopBody_outOfFuel {v : Value} (fuel : Nat) :
    eval fuel (loopEnv v) loopBody = .outOfFuel := by
  induction fuel with
  | zero => rfl
  | succ fuel ih =>
    cases fuel with
    | zero => rfl
    | succ fuel =>
      simpa [eval, loopBody, loopEnv, lookup, update, bindRecursive, updateBinding, Binding.name,
        EvalResult.bind] using ih

theorem loop_outOfFuel : ∀ fuel, eval fuel [] loop = .outOfFuel
  | 0 | 1 | 2 => rfl
  | fuel + 3 => loopBody_outOfFuel (fuel + 1)

theorem loop_diverges : HasBehavior .nb [] loop .divergence :=
  Or.inr (Or.inr (Or.inl ⟨rfl, loop_outOfFuel⟩))

theorem loop_refines : Refines .ub .nb [] loop := trfae_refinement loop_typed

theorem loop_has_no_value {v : Value} : ¬ EvalExpr [] loop v := by
  intro h
  obtain ⟨fuel, hfuel⟩ := exists_eval_of_evalExpr h
  rw [loop_outOfFuel] at hfuel
  cases hfuel

end Itpl.Examples
