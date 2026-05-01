open Lang.El

type ast_param = {
  p_id  : id;
  p_typ : plaintyp;
}

type ast_block =
  | AB_Prop of {
      params : ast_param list;
      goal   : prem;
      prems  : prem list;
    }
  | AB_Gen of {
      params : ast_param list;
      prems  : prem list;
    }

type ast_file = ast_block list
