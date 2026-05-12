(* Arbitrary — Haskell type class encoded as OCaml module type *)

module type ARBITRARY = sig
  type t
  val arbitrary : t Gen.t
end

(* Bool
   arbitrary  = elements [True, False] *)
module Bool = struct
  type t = bool
  let arbitrary = Gen.elements [ true; false ]
end

(* Text: string of arbitrary length using lowercase alphabetical characters *)
module Text = struct
  type t = string

  let arbitrary =
    let open Gen in
    let gen_char = Gen.of_fun (fun _n r ->
      Stdlib.Char.chr (Stdlib.Char.code 'a' + Random.int ~lo:0 ~hi:25 r)) in
    let* len = Gen.elements [1] in
    let* chars = Gen.sequence (List.init len (fun _ -> gen_char)) in
    return (String.init (List.length chars) (List.nth chars))
end

(* --- First-class module helper --- *)

type 'a arbitrary = (module ARBITRARY with type t = 'a)

let gen_of (type a) (module M : ARBITRARY with type t = a) = M.arbitrary
