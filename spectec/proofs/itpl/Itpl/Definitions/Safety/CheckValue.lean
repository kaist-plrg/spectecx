import Itpl.Definitions.Spec.CheckExpr

namespace Itpl

mutual
  inductive CheckValue : Value → Ty → Prop where
    | num {n : Int} :
        CheckValue (.num n) .num
    | closure {env : Env} {tenv : TypeEnv} {x : String} {t_1 t_2 : Ty} {e : Expr} :
        CheckEnv env tenv →
        CheckExpr (updateType tenv x t_1) e t_2 →
        CheckValue (.closure x e env) (.arrow t_1 t_2)

  inductive CheckBinding : Binding → Ty → Prop where
    | value {x : String} {t : Ty} {v : Value} :
        CheckValue v t →
        CheckBinding (.value x v) t
    | recDef {env : Env} {tenv : TypeEnv} {f x : String} {t_1 t_2 : Ty} {e : Expr} :
        CheckEnv env tenv →
        CheckExpr (updateType (updateType tenv f (.arrow t_1 t_2)) x t_1) e t_2 →
        CheckBinding (.recDef f x e env) (.arrow t_1 t_2)

  inductive CheckEnv : Env → TypeEnv → Prop where
    | nil :
        CheckEnv [] []
    | cons {b : Binding} {env : Env} {tenv : TypeEnv} {t : Ty} :
        CheckBinding b t →
        CheckEnv env tenv →
        CheckEnv (b :: env) ((b.name, t) :: tenv)
end

end Itpl
