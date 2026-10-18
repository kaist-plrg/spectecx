open Common.Source

[@@@ocamlformat "disable"]

(* Leaf type aliases from SL *)

type num = Sl.num
type text = Sl.text

type id = Sl.id
type id' = Sl.id'

type atom = Sl.atom
type atom' = Sl.atom'

type mixop = Sl.mixop

type iter = Sl.iter

type var = Sl.var

type value = Sl.value
type value' = Sl.value'

type typ = Sl.typ
type typ' = Sl.typ'

type nottyp = Sl.nottyp
type nottyp' = Sl.nottyp'

type deftyp = Sl.deftyp
type deftyp' = Sl.deftyp'

type typfield = Sl.typfield
type typcase = Sl.typcase

type unop = Sl.unop
type binop = Sl.binop
type cmpop = Sl.cmpop
type optyp = Sl.optyp

type pattern = Sl.pattern
type tparam = Sl.tparam
type targ = Sl.targ
type iterexp = Sl.iterexp

type pid = Sl.pid
type phantom = Sl.phantom
type pathcond = Sl.pathcond

(* Expressions *)

type exp = Annot.hints Il.Expression.t
type exp' = Annot.hints Il.Expression.node
type notexp = Annot.hints Il.Expression.notation
type path = Annot.hints Il.Expression.path
type path' = Annot.hints Il.Expression.path_node
type arg = Annot.hints Il.Expression.arg
type arg' = Annot.hints Il.Expression.arg_node

type relcall = { relid : id; notexp : notexp }

(* Parameters *)

type param = param' phrase
and param' =
  | ExpP of typ * exp
  | DefP of id

(* Case analysis *)

and case = guard * instr list

and guard =
  | BoolG of bool
  | CmpG of cmpop * optyp * exp
  | SubG of typ
  | MatchG of pattern
  | MemG of exp
  (* Shorthand guards — only emitted by the shorten pass *)
  | CheckLetSubG of typ * exp        (* scrut <: typ, bind scrut as exp *)
  | CheckLetMatchG of pattern * exp  (* scrut matches pattern, bind scrut as exp *)

(* Instructions *)

and instr = (instr' phrase) Annot.t
and instr' =
  | RelI of { call : relcall; iterexps : iterexp list }
  | RelAssertI of { call : relcall; expect : bool; iterexps : iterexp list; block : instr list; phantom : phantom option }
  | IfI of exp * iterexp list * instr list * phantom option
  | CaseI of exp * case list * phantom option
  | OtherwiseI of instr
  | TryI of block list
  | LetI of exp * exp * iterexp list
  | ResultI of exp list
  | ReturnI of exp
  | DebugI of exp
  (* Shorthands — only emitted by the shorten pass *)
  (* DestructI: bind each [exp] from [exp_source]'s positional CaseV args;
     [string option] is the prose field name (None = underscore, hidden). *)
  | DestructI of (string option * exp) list * exp
  (* CheckLetI: bind [exp_target] to [exp_source] after a subtype-or-match
     check, then run [block]. *)
  | CheckLetI of exp * exp * instr list
  (* OptionGetI: bind [exp_target] to the inner value of [exp_source] when it is
     [Some _] and then run [block]; otherwise skip [block]. *)
  | OptionGetI of exp * exp * block

and block = instr list
and elseblock = instr list

(* Definitions *)

type def = (def' phrase) Annot.t
and def' =
  | TypD of id * tparam list * deftyp
  | RelD of id * (mixop * int list) * exp list * block * elseblock option
  | BuiltinDecD of id * tparam list * arg list * typ
  | DecD of id * tparam list * arg list * block * elseblock option

type spec = def list
