open Common.Source

[@@@ocamlformat "disable"]

(* Numbers *)

type num = Il.num

(* Texts *)

type text = Il.text

(* Identifiers *)

type id = Il.id
type id' = Il.id'

(* Atoms *)

type atom = Il.atom
type atom' = Il.atom'

(* Mixfix operators *)

type mixop = Il.mixop

(* Iterators *)

type iter = Il.iter

(* Variables *)

type var = Il.var

(* Types *)

type typ = Il.typ
type typ' = Il.typ'

type nottyp = Il.nottyp
type nottyp' = Il.nottyp'

type deftyp = Il.deftyp
type deftyp' = Il.deftyp'

type typfield = Il.typfield
type typcase = Il.typcase

(* Values *)

type vid = Il.vid
type vnote = Il.vnote

type value = Il.value
type value' = Il.value'

type valuefield = atom * value
type valuecase = Il.valuecase

(* Operators *)

type numop = Il.numop
type unop = Il.unop
type binop = Il.binop
type cmpop = Il.cmpop
type optyp = Il.optyp

(* Expressions *)

type exp = Il.exp
type exp' = Il.exp'

type notexp = Il.notexp
type iterexp = Il.iterexp

(* Patterns *)

type pattern = Il.pattern

(* Path *)

type path = Il.path
type path' = Il.path'

(* Parameters *)

type param = Il.param
type param' = Il.param'

(* Type parameters *)

type tparam = Il.tparam
type tparam' = Il.tparam'

(* Arguments *)

type arg = Il.arg
type arg' = Il.arg'

(* Type arguments *)

type targ = Il.targ
type targ' = Il.targ'

(* Path conditions *)

and pid = int

and phantom = pid * pathcond list

and pathcond =
  | ForallC of pathcond * iterexp list
  | ExistsC of pathcond * iterexp list
  | PlainC of exp
  | HoldC of id * notexp
  | NotHoldC of id * notexp

(* Case analysis *)

and case = guard * instr list

and guard =
  | BoolG of bool
  | CmpG of cmpop * optyp * exp
  | SubG of typ
  | MatchG of pattern
  | MemG of exp

(* Instructions *)

and instr = instr' phrase
and instr' =
  | IfI of exp * iterexp list * instr list * phantom option
  | IfHoldI of id * notexp * iterexp list * instr list * phantom option
  | IfNotHoldI of id * notexp * iterexp list * instr list * phantom option
  | CaseI of exp * case list * phantom option
  | OtherwiseI of instr
  | LetI of exp * exp * iterexp list * instr list
  | RuleI of id * notexp * iterexp list * instr list
  | ResultI of exp list
  | ReturnI of exp
  | DebugI of exp * instr

and block = instr list
and elseblock = instr list

(* Hints *)

type hint = { hintid : id; hintexp : El.exp }

(* Definitions *)

type def = def' phrase
and def' =
  (* `syntax` id `<` list(tparam, `,`) `>` `=` deftyp *)
  | TypD of id * tparam list * deftyp
  (* `relation` id `:` mixop `hint(input` `%`int* `)` list(exp, `,`) `:` instr* *)
  | RelD of id * (mixop * int list) * exp list * block * elseblock option
  (* `builtin` `dec` id `<` list(tparam, `,`) `>` list(arg, `,`) *)
  | BuiltinDecD of id * tparam list * arg list
  (* `dec` id `<` list(tparam, `,`) `>` list(param, `,`) `:` typ instr* *)
  | DecD of id * tparam list * arg list * block * elseblock option

(* Spec *)

type spec = def list
