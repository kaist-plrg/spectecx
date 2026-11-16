module Location = Common.Location
module Il = Lang.Il
module Al = Lang.Al
module Sl = Lang.Sl
module Pl = Lang.Pl

(* The instruction's language matches the mode of the emitting interpreter. *)
type instr = AlInstr of Al.instr | SlInstr of Sl.instr | PlInstr of Pl.instr

(* Each enter has a matching exit. If evaluation raises, the interpreter
   emits pending exits as failures, innermost first. *)
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
  (* The interpreter emits these events for repetitions of an instruction.
     [index] starts at zero. [count] includes repetitions that may be skipped
     after a failure. *)
  | Repetition_enter of { index : int; count : int; at : Location.t }
  | Repetition_exit of {
      index : int;
      count : int;
      at : Location.t;
      success : bool;
    }
  (* Each pair covers one evaluation of the instruction before its block runs.
     For an iterated instruction the pair falls inside the repetition events.
     AL [IterI] uses the events of its body. [bindings] holds the resulting
     values of variables named by the instruction and bound with no iterators. *)
  | Instr_enter of { instr : instr; at : Location.t }
  | Instr_exit of {
      instr : instr;
      at : Location.t;
      success : bool;
      bindings : (Il.id * Il.Value.t) list;
    }
