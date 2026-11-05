module Location = Common.Location

type error = Diag.t list

exception AlgoError of Diag.t

type code =
  | Dataflow_free_variable_in_output
  | Dataflow_bind_both_sides_of_equality
  | Dataflow_multibind_dimension_mismatch
  | Dataflow_bind_in_non_invertible
  | Dataflow_empty_iter_expression
  | Dataflow_empty_iter_premise
  | Dataflow_iter_binding_only
  | Dataflow_iter_dimension_mismatch

let string_of_code = function
  | Dataflow_free_variable_in_output -> "dataflow-free-variable-in-output"
  | Dataflow_bind_both_sides_of_equality ->
      "dataflow-bind-both-sides-of-equality"
  | Dataflow_multibind_dimension_mismatch ->
      "dataflow-multibind-dimension-mismatch"
  | Dataflow_bind_in_non_invertible -> "dataflow-bind-in-non-invertible"
  | Dataflow_empty_iter_expression -> "dataflow-empty-iter-expression"
  | Dataflow_empty_iter_premise -> "dataflow-empty-iter-premise"
  | Dataflow_iter_binding_only -> "dataflow-iter-binding-only"
  | Dataflow_iter_dimension_mismatch -> "dataflow-iter-dimension-mismatch"

let render_code (c : code) : string = "elab/" ^ string_of_code c

(* [Pass.elaborate] reports dataflow analysis errors together with elaboration
   errors, so dataflow errors carry the `elab` source and the `elab/` code
   prefix. *)
let error ?code ?detail (at : Location.t) (msg : string) =
  let code = Option.map render_code code in
  raise (AlgoError (Diag.error ?code ?detail ~source:"elab" at msg))
