(** Target - Defines a target's configuration.

    A target specifies:
    - name: Target identifier (e.g., "p4", "ethereum")
    - spec_dir: Directory containing the target's spec files
    - test_dir: Directory containing test inputs *)

(** Test expectation: does the test expect success or failure? *)
type expectation = Positive | Negative

module type S = sig
  val name : string
  val spec_dir : string
  val test_dir : string
end
