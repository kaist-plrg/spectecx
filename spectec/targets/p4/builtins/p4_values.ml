open Lang.Il
open Common.Annotated
module Location = Common.Location
open Builtins
open Mixfix

let ( let* ) = Result.bind

(* Values of the P4 specification's syntax *)

let global = Case.case_v ~var:"cursor" [ Case.kw "GLOBAL" ]
let local = Case.case_v ~var:"cursor" [ Case.kw "LOCAL" ]

let bare_name (name : string) : Value.t =
  Case.case_v ~var:"prefixedNameIR"
    [ Case.tag "BARE"; Case.arg (Value.text name) ]

let typ_bit = Typ.var "bit" [] $ Location.Unknown

let value_of_bits (bits : bool list) : Value.t =
  Value.list typ_bit (List.map Value.bool bits)

let return_empty : Value.t =
  let typ_value = Typ.var "value" [] $ Location.Unknown in
  Case.case_v ~var:"returnResult"
    [ Case.kw "RETURN"; Case.arg (Value.opt typ_value None) ]

let reject (error : Value.t) : Value.t =
  Case.case_v ~var:"rejectTransitionResult" [ Case.kw "REJECT"; Case.arg error ]

let is_reject (value : Value.t) : bool =
  match value.it with
  | CaseV (Atom atom :: _) ->
      Lang.Xl.Atom.eq atom.it (Lang.Xl.Atom.keyword "REJECT")
  | _ -> false

let error_value (name : string) : Value.t =
  Case.case_v ~var:"errorValue"
    [ Case.kw "ERROR"; Case.op "."; Case.arg (Value.text name) ]

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
