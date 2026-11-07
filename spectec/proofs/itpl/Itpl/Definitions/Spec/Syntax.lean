namespace Itpl

inductive Ty where
  | num
  | arrow (arg result : Ty)

inductive Expr where
  | num (n : Int)
  | add (left right : Expr)
  | sub (left right : Expr)
  | val (name : String) (init next : Expr)
  | id (name : String)
  | lambda (param : String) (argType : Ty) (body : Expr)
  | app (fn arg : Expr)
  | if0 (condition onZero onNonzero : Expr)
  | letrec (name param : String) (argType resultType : Ty) (body next : Expr)

mutual
  inductive Value where
    | num (n : Int)
    | closure (param : String) (body : Expr) (env : List Binding)

  inductive Binding where
    | value (name : String) (v : Value)
    | recDef (name param : String) (body : Expr) (env : List Binding)
end

abbrev Env := List Binding

abbrev TypeEnv := List (String × Ty)

end Itpl
