import Itpl.Definitions.Spec.Syntax

namespace Itpl

def Binding.name : Binding → String
  | .value x _ => x
  | .recDef f _ _ _ => f

def domain : Env → List String
  | [] => []
  | b :: env => b.name :: domain env

def updateBinding : Env → Binding → Env
  | [], b => [b]
  | b_1 :: env, b =>
    if b.name = b_1.name then b :: env else b_1 :: updateBinding env b

def update (env : Env) (x : String) (v : Value) : Env :=
  updateBinding env (.value x v)

def bindRecursive (env : Env) (f x : String) (e : Expr) : Env :=
  updateBinding env (.recDef f x e env)

def lookup : Env → String → Option Value
  | [], _ => none
  | .value x_1 v :: env, x => if x = x_1 then some v else lookup env x
  | .recDef f x e env_def :: env, f_1 =>
    if f_1 = f then some (.closure x e (bindRecursive env_def f x e))
    else lookup env f_1

def domainType : TypeEnv → List String
  | [] => []
  | (x, _) :: tenv => x :: domainType tenv

def updateType : TypeEnv → String → Ty → TypeEnv
  | [], x, t => [(x, t)]
  | (x_1, t_1) :: tenv, x, t =>
    if x = x_1 then (x, t) :: tenv else (x_1, t_1) :: updateType tenv x t

def lookupType : TypeEnv → String → Option Ty
  | [], _ => none
  | (x_1, t) :: tenv, x => if x = x_1 then some t else lookupType tenv x

end Itpl
