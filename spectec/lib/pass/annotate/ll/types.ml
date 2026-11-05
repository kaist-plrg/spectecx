open Common.Annotated
module Sl = Lang.Sl

[@@@ocamlformat "disable"]

type id = Sl.id
type atom = Sl.atom
type mixop = Sl.mixop
type exp = Sl.exp
type notexp = Sl.notexp
type relcall = Sl.relcall
type iterexp = Sl.iterexp
type iterinstr = Sl.iterinstr
type pattern = Sl.pattern
type tparam = Sl.tparam
type typ = Sl.typ
type cmpop = Sl.cmpop
type optyp = Sl.optyp
type arg = Sl.arg
type deftyp = Sl.deftyp
type phantom = Sl.phantom

(* Case analysis *)

type case = guard * block

and guard =
  | BoolG of bool
  | CmpG of cmpop * optyp * exp
  | SubG of typ
  | MatchG of pattern
  | MemG of exp

(* Instructions *)

and instr = instr' located
and instr' =
  | RelI of { call : relcall; iterinstrs : iterinstr list }
  | RelAssertI of { call : relcall; expect : bool; iterexps : iterexp list; block : block; phantom : phantom option }
  | IfI of exp * iterexp list * block * phantom option
  | CaseI of exp * case list * phantom option
  | OtherwiseI of block
  | TryI of block list
  | LetI of exp * exp * iterinstr list
  | ResultI of exp list
  | ReturnI of exp
  | DebugI of exp

and block = instr list
and elseblock = instr list

(* Definitions *)

type def = def' located
and def' =
  | TypD of id * tparam list * deftyp
  | RelD of id * (mixop * int list) * exp list * block * elseblock option
  | BuiltinDecD of id * tparam list * arg list * typ
  | DecD of id * tparam list * arg list * block * elseblock option

type spec = def list
