open Lang.Il

type binding_origin =
  | Free
    (* Generated freely by il_gen; primary test inputs. *)
  | BoundByRule of string * int
    (* Bound as output (position int) of a rule premise. *)
  | BoundByLet of exp
    (* Bound by a LetPr rhs expression. *)

type ir_var = {
  iv_id     : string;
  iv_typ    : typ;          (* Il.typ — used directly by il_gen *)
  iv_origin : binding_origin;
}

type qc_command =
  | QcProp of {
      free_vars : ir_var list;
        (* All variables in scope: Free (generated) and Bound (computed from prems).
           Ordered so Free vars come first, then Bound vars in dependency order. *)
      goal      : prem;
        (* IL premise to check. Failure → test failure (not discard). *)
      prems     : prem list;
        (* IL filter premises, evaluated before the goal.
           Failure → test case discarded (QuickCheck ==>). *)
    }
  | QcGen of {
      free_vars : ir_var list;
      prems     : prem list;
    }

type t = qc_command list
