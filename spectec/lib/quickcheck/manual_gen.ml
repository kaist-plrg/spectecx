open Lang.Il
open Common.Source

(* Manual generator for --manual mode.
   Fill in [gen_inputs] to provide custom generators for your types.

   The function receives the full list of free variables (ir_var list) declared
   in the .quickcheck block, and should return [Some gen] where [gen] produces
   a binding list [(var_name, value); ...] for all of them at once.
   Return [None] to fall through to an error.

   Example — one variable "prog" of type prog:

   let gen_prog (_spec : spec) : Value.t Gen.t =
     let open Gen in
     sized (fun _size ->
       (* Build a Value.t by hand using CaseV, TupleV, etc.
          See il_gen.ml for reference on Value.make_val usage. *)
       return (Value.make_val (VarT (("prog" $ no_region), [])) (CaseV [])))

   Then in gen_inputs:
   | [{ Qc_ir.iv_id = id; iv_typ = { it = VarT ({ it = "prog"; _ }, _); _ } }] ->
     Some (Gen.map (fun v -> [(id, v)]) (gen_prog _spec))
*)

let gen_inputs (_spec : spec) (i : int) :
    (string * value) list Gen.t option =
  ignore no_region;
  match i with
  (* Add cases here. Match on the block index, e.g.:

     | 0 ->
       Some (Gen.map (fun v -> [("prog", v)]) (gen_prog _spec))

     | 1 ->
       Some (
         let open Gen in
         let* v1 = ... and* v2 = ... in
         return [("x", v1); ("y", v2)]
       )
  *)
  | _ -> None
