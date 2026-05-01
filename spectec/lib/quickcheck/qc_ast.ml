open Lang.El

(* A single typed input variable declaration: (id : typ) *)
type ast_param = {
  p_id  : id;
  p_typ : plaintyp;
}

(* A single quickcheck/prop or quickcheck/gen block *)
type ast_block =
  | AB_Prop of {
      params : ast_param list;
      goal   : prem;         (* EL-level goal premise *)
      prems  : prem list;    (* EL-level filter premises *)
    }
  | AB_Gen of {
      params : ast_param list;
      prems  : prem list;
    }

type ast_file = ast_block list
