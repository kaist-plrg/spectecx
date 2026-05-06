module Xl = Lang.Xl
open Lang.Il
open Builtins

(* Conversion between meta-numerics and OCaml numerics *)

let bigint_of_value (value : value) : Bigint.t =
  value |> Value.get_num |> Xl.Num.to_int

(* dec $sum_nat(nat* ) : nat *)

let sum ~at (nums : Bigint.t list) : Value.t result =
  at |> ignore;
  let sum = List.fold_left Bigint.( + ) Bigint.zero nums in
  Ok (Value.nat sum)

(* dec $max_nat(nat* ) : nat *)

(* Returns zero if list is empty *)
let max ~at (nums : Bigint.t list) : Value.t result =
  at |> ignore;
  let max_value = List.fold_left Bigint.max Bigint.zero nums in
  Ok (Value.nat max_value)

(* dec $min_nat(nat* ) : nat *)

(* Returns zero if list is empty *)
let min ~at (nums : Bigint.t list) : Value.t result =
  at |> ignore;
  let min = List.fold_left Bigint.min Bigint.zero nums in
  Ok (Value.nat min)

let builtins =
  [
    ("sum_nat", Define.T0.a1 (Arg.list_of Arg.nat) sum);
    ("max_nat", Define.T0.a1 (Arg.list_of Arg.nat) max);
    ("min_nat", Define.T0.a1 (Arg.list_of Arg.nat) min);
  ]
