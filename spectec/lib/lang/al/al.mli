(** Data types and pure data functions for AL, the algorithmic language. AL
    makes variable bindings and guards explicit for execution. *)

include module type of Types

(** The IL premise that [instr] was written as, or [None] when the algo pass
    synthesized [instr]. *)
val premise_of : instr -> Il.prem option

(** Whether the algo pass synthesized [instr], so that it has no source in the
    spec: a check on a rule's or clause's inputs, or on a premise's results. *)
val is_synthesized : instr -> bool

module Free : module type of Free
module Print : module type of Print
