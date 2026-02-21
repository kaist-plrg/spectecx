(** Target - Defines a target's configuration.

    A target specifies:
    - name: Target identifier (e.g., "p4", "ethereum")
    - spec_dir: Directory containing the target's spec files
    - test_dir: Directory containing test inputs
    - builtins: OCaml implementations for library functions
    - handler: Wrapper that provides global mutable state to runtime *)

module type S = sig
  val name : string
  val spec_dir : string
  val test_dir : string
  val builtins : (string * Interp.Builtins.Define.t) list
  val handler : (unit -> 'a) -> 'a
end
