(** Data types and pure data functions for EL. EL is the surface language of
    SpecTec. *)

include module type of Types
module Free : module type of Free
module Print : module type of Print
