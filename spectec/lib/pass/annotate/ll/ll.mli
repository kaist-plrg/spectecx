(** Linearized form. Sits between SL and PL: bindings carry no nested block, and
    runs of consecutive branching instructions are grouped under [TryI]. *)

include module type of Types
