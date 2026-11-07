import Itpl.Definitions.Spec.Environment

namespace Itpl

inductive CheckExpr : TypeEnv → Expr → Ty → Prop where
  | num {tenv : TypeEnv} {n : Int} :
      CheckExpr tenv (.num n) .num
  | add {tenv : TypeEnv} {e_1 e_2 : Expr} :
      CheckExpr tenv e_1 .num →
      CheckExpr tenv e_2 .num →
      CheckExpr tenv (.add e_1 e_2) .num
  | sub {tenv : TypeEnv} {e_1 e_2 : Expr} :
      CheckExpr tenv e_1 .num →
      CheckExpr tenv e_2 .num →
      CheckExpr tenv (.sub e_1 e_2) .num
  | val {tenv : TypeEnv} {x : String} {t_1 t_2 : Ty} {e_1 e_2 : Expr} :
      CheckExpr tenv e_1 t_1 →
      CheckExpr (updateType tenv x t_1) e_2 t_2 →
      CheckExpr tenv (.val x e_1 e_2) t_2
  | id {tenv : TypeEnv} {x : String} {t : Ty} :
      lookupType tenv x = some t →
      CheckExpr tenv (.id x) t
  | lambda {tenv : TypeEnv} {x : String} {t_1 t_2 : Ty} {e : Expr} :
      CheckExpr (updateType tenv x t_1) e t_2 →
      CheckExpr tenv (.lambda x t_1 e) (.arrow t_1 t_2)
  | app {tenv : TypeEnv} {t_1 t_2 : Ty} {e_1 e_2 : Expr} :
      CheckExpr tenv e_1 (.arrow t_1 t_2) →
      CheckExpr tenv e_2 t_1 →
      CheckExpr tenv (.app e_1 e_2) t_2
  | if0 {tenv : TypeEnv} {t : Ty} {e_1 e_2 e_3 : Expr} :
      CheckExpr tenv e_1 .num →
      CheckExpr tenv e_2 t →
      CheckExpr tenv e_3 t →
      CheckExpr tenv (.if0 e_1 e_2 e_3) t
  | letrec {tenv : TypeEnv} {f x : String} {t t_1 t_2 : Ty} {e_1 e_2 : Expr} :
      CheckExpr (updateType (updateType tenv f (.arrow t_1 t_2)) x t_1) e_1 t_2 →
      CheckExpr (updateType tenv f (.arrow t_1 t_2)) e_2 t →
      CheckExpr tenv (.letrec f x t_1 t_2 e_1 e_2) t

end Itpl
