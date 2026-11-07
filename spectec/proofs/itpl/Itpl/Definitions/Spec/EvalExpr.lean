import Itpl.Definitions.Spec.Environment

namespace Itpl

inductive EvalExpr : Env → Expr → Value → Prop where
  | num {env : Env} {n : Int} :
      EvalExpr env (.num n) (.num n)
  | id {env : Env} {x : String} {v : Value} :
      lookup env x = some v →
      EvalExpr env (.id x) v
  | lambda {env : Env} {x : String} {t : Ty} {e : Expr} :
      EvalExpr env (.lambda x t e) (.closure x e env)
  | add {env : Env} {e_1 e_2 : Expr} {n_1 n_2 : Int} :
      EvalExpr env e_1 (.num n_1) →
      EvalExpr env e_2 (.num n_2) →
      EvalExpr env (.add e_1 e_2) (.num (n_1 + n_2))
  | sub {env : Env} {e_1 e_2 : Expr} {n_1 n_2 : Int} :
      EvalExpr env e_1 (.num n_1) →
      EvalExpr env e_2 (.num n_2) →
      EvalExpr env (.sub e_1 e_2) (.num (n_1 - n_2))
  | val {env : Env} {x : String} {e_1 e_2 : Expr} {v_1 v_2 : Value} :
      EvalExpr env e_1 v_1 →
      EvalExpr (update env x v_1) e_2 v_2 →
      EvalExpr env (.val x e_1 e_2) v_2
  | app {env env_clo : Env} {x : String} {e_1 e_2 e_body : Expr} {v v_arg : Value} :
      EvalExpr env e_1 (.closure x e_body env_clo) →
      EvalExpr env e_2 v_arg →
      EvalExpr (update env_clo x v_arg) e_body v →
      EvalExpr env (.app e_1 e_2) v
  | if0Zero {env : Env} {e_1 e_2 e_3 : Expr} {v : Value} :
      EvalExpr env e_1 (.num 0) →
      EvalExpr env e_2 v →
      EvalExpr env (.if0 e_1 e_2 e_3) v
  | if0Nonzero {env : Env} {e_1 e_2 e_3 : Expr} {v v_cond : Value} :
      EvalExpr env e_1 v_cond →
      v_cond ≠ .num 0 →
      EvalExpr env e_3 v →
      EvalExpr env (.if0 e_1 e_2 e_3) v
  | letrec {env env_rec : Env} {f x : String} {t_1 t_2 : Ty} {e_1 e_2 : Expr} {v : Value} :
      env_rec = bindRecursive env f x e_1 →
      EvalExpr env_rec e_2 v →
      EvalExpr env (.letrec f x t_1 t_2 e_1 e_2) v

end Itpl
