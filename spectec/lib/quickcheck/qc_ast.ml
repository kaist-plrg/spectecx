open Lang.El

(* A single typed input variable declaration: (id : typ) *)
type ast_param = { p_id : id; p_typ : plaintyp }

(* A hint annotation: (hint generator NAME) *)
type ast_hint = GeneratorHint of string

(* A single quickcheck/<name> block.
   goal = None  → generation mode
   goal = Some  → property mode *)
type ast_block = {
  name : string;
  params : ast_param list;
  hint : ast_hint option;
  goal : prem option;
  prems : prem list;
}

type ast_file = ast_block list
