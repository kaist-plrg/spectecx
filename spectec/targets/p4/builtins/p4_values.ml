open Lang.Il
open Common.Annotated
open Builtins
open Mixfix

let ( let* ) = Result.bind

(* Readers of P4 values *)

let bit32_of_value at value =
  match value.it with
  | CaseV [ Arg width; Atom atom; Arg bits ]
    when Lang.Xl.Atom.eq atom.it (Lang.Xl.Atom.keyword "W") ->
      let* width = Arg.nat at width in
      let* bits = Arg.num at bits in
      let bits = Lang.Xl.Num.to_int bits in
      if Bigint.(width = of_int 32 && bits >= zero && bits < one lsl 32) then
        Ok bits
      else Error (Error.type_err at "bit<32> value" value)
  | _ -> Error (Error.type_err at "bit<32> value" value)

let bool_of_value at value =
  match value.it with
  | CaseV [ Atom atom; Arg value ]
    when Lang.Xl.Atom.eq atom.it (Lang.Xl.Atom.tag "B") ->
      Arg.bool at value
  | _ -> Error (Error.type_err at "P4 boolean value" value)
