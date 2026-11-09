(** Data types and pure data functions for AL, the algorithmic language. AL
    makes variable bindings and guards explicit for execution. *)

include module type of Types
module Free : module type of Free
module Print : module type of Print
