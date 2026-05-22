(** Per-node hint annotation, denormalized from the EL spec by the annotate pass
    alongside pass-derived rulegroup I/O. *)

type hints = {
  prose : Hints.Alter.t option;
  prose_in : Hints.Alter.t option;
  prose_out : Hints.Alter.t option;
  prose_true : Hints.Alter.t option;
  prose_false : Hints.Alter.t option;
  prose_fields : Hints.Fields.t option;
  prose_input_exps : Sl.exp list option;
  prose_output_exps : Sl.exp list option;
}

let empty : hints =
  {
    prose = None;
    prose_in = None;
    prose_out = None;
    prose_true = None;
    prose_false = None;
    prose_fields = None;
    prose_input_exps = None;
    prose_output_exps = None;
  }

type 'a t = { node : 'a; hints : hints }

let bare (node : 'a) : 'a t = { node; hints = empty }
