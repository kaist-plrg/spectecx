import Itpl.Definitions.Spec.Environment

namespace Itpl

inductive EvalResult where
  | outOfFuel
  | error
  | value (v : Value)

def EvalResult.bind (r : EvalResult) (k : Value → EvalResult) : EvalResult :=
  match r with
  | .outOfFuel => .outOfFuel
  | .error => .error
  | .value v => k v

def eval : Nat → Env → Expr → EvalResult
  | 0, _, _ => .outOfFuel
  | fuel + 1, env, expr =>
    match expr with
    | .num n => .value (.num n)
    | .id x => match lookup env x with
      | some v => .value v
      | none => .error
    | .lambda x _ e => .value (.closure x e env)
    | .add e_1 e_2 => (eval fuel env e_1).bind fun v_1 =>
      match v_1 with
      | .num n_1 => (eval fuel env e_2).bind fun v_2 =>
        match v_2 with
        | .num n_2 => .value (.num (n_1 + n_2))
        | _ => .error
      | _ => .error
    | .sub e_1 e_2 => (eval fuel env e_1).bind fun v_1 =>
      match v_1 with
      | .num n_1 => (eval fuel env e_2).bind fun v_2 =>
        match v_2 with
        | .num n_2 => .value (.num (n_1 - n_2))
        | _ => .error
      | _ => .error
    | .val x e_1 e_2 => (eval fuel env e_1).bind fun v_1 => eval fuel (update env x v_1) e_2
    | .app e_1 e_2 => (eval fuel env e_1).bind fun v_1 =>
      match v_1 with
      | .closure x e_body env_clo =>
          (eval fuel env e_2).bind fun v_arg => eval fuel (update env_clo x v_arg) e_body
      | _ => .error
    | .if0 e_1 e_2 e_3 => (eval fuel env e_1).bind fun v_cond =>
      match v_cond with
      | .num 0 => eval fuel env e_2
      | _ => eval fuel env e_3
    | .letrec f x _ _ e_1 e_2 => eval fuel (bindRecursive env f x e_1) e_2

def EvalResult.OutOfFuelOrEq (r s : EvalResult) : Prop := r = .outOfFuel ∨ r = s

end Itpl
