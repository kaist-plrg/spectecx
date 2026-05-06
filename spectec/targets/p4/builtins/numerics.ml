open Lang.Il
open Builtins

(* Built-in implementations *)

(* dec $shl(int, int) : int *)

let rec shl' (v : Bigint.t) (o : Bigint.t) : Bigint.t =
  if Bigint.(o > zero) then shl' Bigint.(v * (one + one)) Bigint.(o - one)
  else v

let shl ~at (base : Bigint.t) (offset : Bigint.t) : Value.t result =
  at |> ignore;
  Ok (Value.int (shl' base offset))

(* dec $shr(int, int) : int *)

let rec shr' (v : Bigint.t) (o : Bigint.t) : Bigint.t =
  if Bigint.(o > zero) then
    let v_shifted = Bigint.(v / (one + one)) in
    shr' v_shifted Bigint.(o - one)
  else v

let shr ~at (base : Bigint.t) (offset : Bigint.t) : Value.t result =
  at |> ignore;
  Ok (Value.int (shr' base offset))

(* dec $shr_arith(int, int, int) : int *)

let shr_arith ~at (base : Bigint.t) (offset : Bigint.t) (modulus : Bigint.t) :
    Value.t result =
  at |> ignore;
  let rec shr_arith' (v : Bigint.t) (o : Bigint.t) (m : Bigint.t) : Bigint.t =
    if Bigint.(o > zero) then
      let v_shifted = Bigint.((v / (one + one)) + m) in
      shr_arith' v_shifted Bigint.(o - one) m
    else v
  in
  Ok (Value.int (shr_arith' base offset modulus))

(* dec $pow2(nat) : int *)

let pow2' (w : Bigint.t) : Bigint.t = shl' Bigint.one w

let pow2 ~at (width : Bigint.t) : Value.t result =
  at |> ignore;
  Ok (Value.int (pow2' width))

(* dec $bitstr_to_int(int, int) : int *)

let rec bitstr_to_int' (w : Bigint.t) (n : Bigint.t) : Bigint.t =
  let two = Bigint.(one + one) in
  let w' = pow2' w in
  if Bigint.(n >= w' / two) then bitstr_to_int' w Bigint.(n - w')
  else if Bigint.(n < -(w' / two)) then bitstr_to_int' w Bigint.(n + w')
  else n

let bitstr_to_int ~at (width : Bigint.t) (bitstr : Bigint.t) : Value.t result =
  at |> ignore;
  Ok (Value.int (bitstr_to_int' width bitstr))

(* dec $int_to_bitstr(int, int) : int *)

let rec int_to_bitstr' (w : Bigint.t) (n : Bigint.t) : Bigint.t =
  let w' = pow2' w in
  if Bigint.(n >= w') then Bigint.(n % w')
  else if Bigint.(n < zero) then int_to_bitstr' w Bigint.(n + w')
  else n

let int_to_bitstr ~at (width : Bigint.t) (rawint : Bigint.t) : Value.t result =
  at |> ignore;
  Ok (Value.int (int_to_bitstr' width rawint))

(* dec $bneg(int) : int *)

let bneg ~at (n : Bigint.t) : Value.t result =
  at |> ignore;
  Ok (Value.int (Bigint.bit_not n))

(* dec $band(int, int) : int *)

let band ~at (l : Bigint.t) (r : Bigint.t) : Value.t result =
  at |> ignore;
  Ok (Value.int (Bigint.bit_and l r))

(* dec $bxor(int, int) : int *)

let bxor ~at (l : Bigint.t) (r : Bigint.t) : Value.t result =
  at |> ignore;
  Ok (Value.int (Bigint.bit_xor l r))

(* dec $bor(int, int) : int *)

let bor ~at (l : Bigint.t) (r : Bigint.t) : Value.t result =
  at |> ignore;
  Ok (Value.int (Bigint.bit_or l r))

(* dec $bitacc(int, int, int) : int *)

let bitacc ~at (n : Bigint.t) (m : Bigint.t) (l : Bigint.t) : Value.t result =
  try
    if Bigint.(l < zero) then
      Error (Error.runtime at "bitacc: slice x[y:z] must have z >= 0")
    else if Bigint.(m < l) then
      Error (Error.runtime at "bitacc: slice x[y:z] must have y >= z")
    else
      let slice_width = Bigint.(m + one - l) in
      let l_int = Bigint.to_int_exn l in
      let shifted = Bigint.(n asr l_int) in
      let mask = Bigint.(pow2' slice_width - one) in
      let result = Bigint.bit_and shifted mask in
      Ok (Value.int result)
  with
  | Failure msg ->
      (* Catches 'to_int_exn' *)
      Error
        (Error.runtime at
           (Printf.sprintf "bitacc: slice index is too large (%s)" msg))
  | _ -> Error (Error.runtime at "bitacc: unexpected error during calculation")

let builtins : (string * Define.t) list =
  [
    ("shl", Define.T0.a2 Arg.int Arg.int shl);
    ("shr", Define.T0.a2 Arg.int Arg.int shr);
    ("shr_arith", Define.T0.a3 Arg.int Arg.int Arg.int shr_arith);
    ("pow2", Define.T0.a1 Arg.nat pow2);
    ("bitstr_to_int", Define.T0.a2 Arg.int Arg.int bitstr_to_int);
    ("int_to_bitstr", Define.T0.a2 Arg.int Arg.int int_to_bitstr);
    ("bneg", Define.T0.a1 Arg.int bneg);
    ("band", Define.T0.a2 Arg.int Arg.int band);
    ("bxor", Define.T0.a2 Arg.int Arg.int bxor);
    ("bor", Define.T0.a2 Arg.int Arg.int bor);
    ("bitacc", Define.T0.a3 Arg.int Arg.int Arg.int bitacc);
  ]
