module Location = Common.Location

type error = Diag.t list

exception AlgoError of Diag.t

type code =
  | Free_variable_in_output
  | Bind_both_sides_of_equality
  | Multibind_dimension_mismatch
  | Bind_in_non_invertible
  | Empty_iter_expression
  | Empty_iter_premise
  | Iter_binding_only
  | Iter_dimension_mismatch

let string_of_code = function
  | Free_variable_in_output -> "free-variable-in-output"
  | Bind_both_sides_of_equality -> "bind-both-sides-of-equality"
  | Multibind_dimension_mismatch -> "multibind-dimension-mismatch"
  | Bind_in_non_invertible -> "bind-in-non-invertible"
  | Empty_iter_expression -> "empty-iter-expression"
  | Empty_iter_premise -> "empty-iter-premise"
  | Iter_binding_only -> "iter-binding-only"
  | Iter_dimension_mismatch -> "iter-dimension-mismatch"

let render_code (c : code) : string = "algo/" ^ string_of_code c

let error ?code ?detail (at : Location.t) (msg : string) =
  let code = Option.map render_code code in
  raise (AlgoError (Diag.error ?code ?detail ~source:"algo" at msg))
