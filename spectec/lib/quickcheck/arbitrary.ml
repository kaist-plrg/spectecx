(* Arbitrary / Coarbitrary — Haskell type classes encoded as OCaml module types *)

module type ARBITRARY = sig
  type t
  val arbitrary : t Gen.t
end

module type COARBITRARY = sig
  type t
  val coarbitrary : t -> 'b Gen.t -> 'b Gen.t
end

(* Bool
   arbitrary  = elements [True, False]
   coarbitrary b = variant (if b then 0 else 1) *)
module Bool = struct
  type t = bool
  let arbitrary = Gen.elements [ true; false ]
  let coarbitrary b gen = Gen.variant (if b then 0 else 1) gen
end

(* Nat: non-negative integers, bounded by size parameter *)
module Nat = struct
  type t = int
  let arbitrary = Gen.sized (fun n -> Gen.choose_int (0, n))

  (* coarbitrary n: bitwise recursive perturbation — variant of Haskell Int coarbitrary *)
  let rec coarbitrary n gen =
    if n = 0 then Gen.variant 0 gen
    else Gen.variant 1 (coarbitrary (n / 2) gen)
end

(* Int: signed integers
   arbitrary = sized (\n -> choose (-n,n))
   coarbitrary: direct translation of Haskell spec *)
module Int = struct
  type t = int
  let arbitrary = Gen.sized (fun n -> Gen.choose_int (-n, n))

  let rec coarbitrary n gen =
    if n = 0 then Gen.variant 0 gen
    else if n < 0 then Gen.variant 2 (coarbitrary (abs n) gen)
    else Gen.variant 1 (coarbitrary (n / 2) gen)
end

(* Char *)
module Char = struct
  type t = char
  let arbitrary = Gen.of_fun (fun _n r ->
    Stdlib.Char.chr (Stdlib.Char.code 'a' + Random.int ~lo:0 ~hi:25 r))
  let coarbitrary c gen = Gen.variant (Stdlib.Char.code c) gen
end

(* Text: string of arbitrary length using lowercase alphabetical characters *)
module Text = struct
  type t = string

  let arbitrary =
    let open Gen in
    let gen_char = Char.arbitrary in
    let* len = Gen.elements [1] in
    let* chars = Gen.sequence (List.init len (fun _ -> gen_char)) in
    return (String.init (List.length chars) (List.nth chars))

  (* perturbs recursively using each character's code *)
  let coarbitrary s gen =
    String.fold_right (fun c g -> Gen.variant (Stdlib.Char.code c) g) s gen
end

(* --- Derived Functors --- *)

module Make_list (A : ARBITRARY) = struct
  type t = A.t list
  let arbitrary = Gen.list_of A.arbitrary
end

module Make_option (A : ARBITRARY) = struct
  type t = A.t option
  let arbitrary = Gen.option_of A.arbitrary
end

module Make_pair (A : ARBITRARY) (B : ARBITRARY) = struct
  type t = A.t * B.t
  let arbitrary = Gen.pair A.arbitrary B.arbitrary
end

(* promote (`coarbitrary` arbitrary) — direct translation of Haskell arbitrary(a->b) *)
module Make_fun (A : COARBITRARY) (B : ARBITRARY) = struct
  type t = A.t -> B.t
  let arbitrary = Gen.promote (fun a -> A.coarbitrary a B.arbitrary)
end

(* --- First-class module helper --- *)

type 'a arbitrary = (module ARBITRARY with type t = 'a)

let gen_of (type a) (module M : ARBITRARY with type t = a) = M.arbitrary
