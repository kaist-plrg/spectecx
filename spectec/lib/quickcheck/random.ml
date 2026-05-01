(* XorShift 기반 분리 가능 PRNG.
   두 int 씨드(s1, s2)로 상태를 표현하고 split은 순수 함수로 구현된다.
   Haskell split :: StdGen -> (StdGen, StdGen) 시맨틱을 따른다. *)

type t = { s1 : int; s2 : int }

let make seed =
  { s1 = seed lxor 0x9e3779b9; s2 = seed lxor 0x6c62272e }

let make_self_init () =
  let s = Stdlib.Random.State.make_self_init () in
  { s1 = Stdlib.Random.State.bits s; s2 = Stdlib.Random.State.bits s }

(* XorShift mix: 두 씨드를 혼합하여 비트열을 생성 *)
let bits { s1; s2 } =
  let x = s1 lxor s2 in
  let x = x lxor (x lsr 13) in
  let x = x lxor (x lsl 7) in
  let x = x lxor (x lsr 17) in
  x land max_int

(* 두 독립 자식 스트림을 반환 — Haskell split 시맨틱
   63비트 OCaml int 범위에 맞는 LCG 상수를 사용한다. *)
let split { s1; s2 } =
  let a = (s1 * 1664525 + 1013904223) land max_int in
  let b = (s2 * 22695477 + 1) land max_int in
  { s1 = a; s2 = s2 lxor b },
  { s1 = s1 lxor a; s2 = b }

let bool r = bits r land 1 = 0

let int ~lo ~hi r =
  if lo >= hi then lo
  else
    let range = hi - lo + 1 in
    lo + (bits r) mod range

let float ~lo ~hi r =
  lo +. (hi -. lo) *. (float_of_int (bits r) /. float_of_int max_int)

let char r = Char.chr (32 + (bits r) mod 95)
