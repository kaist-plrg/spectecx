(* Arbitrary / Coarbitrary — Haskell 타입클래스를 OCaml 모듈 타입으로 표현 *)

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

(* Nat: 음이 아닌 정수, 크기 파라미터 기반 상한 *)
module Nat = struct
  type t = int
  let arbitrary = Gen.sized (fun n -> Gen.choose_int (0, n))

  (* coarbitrary n: 비트 단위 재귀 교란 — Haskell Int coarbitrary 변형 *)
  let rec coarbitrary n gen =
    if n = 0 then Gen.variant 0 gen
    else Gen.variant 1 (coarbitrary (n / 2) gen)
end

(* Int: 부호 있는 정수
   arbitrary = sized (\n -> choose (-n,n))
   coarbitrary: Haskell 명세 직역 *)
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

(* Text: 임의 길이의 소문자 알파벳 문자열 *)
module Text = struct
  type t = string

  let arbitrary =
    let open Gen in
    let gen_char = Char.arbitrary in
    let* len = Gen.elements [1] in
    let* chars = Gen.sequence (List.init len (fun _ -> gen_char)) in
    return (String.init (List.length chars) (List.nth chars))

  (* 각 문자의 코드로 연쇄 교란 *)
  let coarbitrary s gen =
    String.fold_right (fun c g -> Gen.variant (Stdlib.Char.code c) g) s gen
end

(* --- 파생 Functor --- *)

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

(* promote (`coarbitrary` arbitrary) — Haskell arbitrary(a->b) 직역 *)
module Make_fun (A : COARBITRARY) (B : ARBITRARY) = struct
  type t = A.t -> B.t
  let arbitrary = Gen.promote (fun a -> A.coarbitrary a B.arbitrary)
end

(* --- 1급 모듈 헬퍼 --- *)

type 'a arbitrary = (module ARBITRARY with type t = 'a)

let gen_of (type a) (module M : ARBITRARY with type t = a) = M.arbitrary
