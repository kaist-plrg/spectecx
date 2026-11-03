module Location = Common.Location
module Il = Lang.Il
module Sl = Lang.Sl

type t =
  | Test_start of { test_case_id : string }
  | Test_end of { test_case_id : string }
  | Rel_enter of { id : string; at : Location.t; inputs : Il.Value.t list }
  | Rel_exit of {
      id : string;
      at : Location.t;
      success : bool;
      conclusion : (Il.Value.t, Il.Value.t option) Il.Mode.t;
    }
  | Rule_enter of { id : string; rule_id : string; at : Location.t }
  | Rule_exit of {
      id : string;
      rule_id : string;
      at : Location.t;
      success : bool;
    }
  | Func_enter of { id : string; at : Location.t; inputs : Il.Value.t list }
  | Func_exit of { id : string; at : Location.t; output : Il.Value.t option }
  | Clause_enter of { id : string; clause_idx : int; at : Location.t }
  | Clause_exit of {
      id : string;
      clause_idx : int;
      at : Location.t;
      success : bool;
    }
  | Iter_prem_enter of { prem : Il.prem; at : Location.t }
  | Iter_prem_exit of { at : Location.t }
  | Prem_enter of { prem : Il.prem; at : Location.t }
  | Prem_exit of {
      prem : Il.prem;
      at : Location.t;
      success : bool;
      bindings : (Il.id * Il.Value.t) list;
    }
  | Instr of { instr : Sl.instr; at : Location.t }
