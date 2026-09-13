(** OCaml image of the surface grammar of [spec/ch02-syntax.tex] Figs. 2.1-2.3.

    One constructor per production of [GRAMMAR.md], in the same order. A few
    constructors marked [UNRESOLVED] stand for a name whose *sort* the grammar
    fixes but the token stream does not (is [Foo] a struct, a type parameter or
    a transparent alias?). {!Resolve} eliminates every one of them against the
    program's own declarations plus the prelude of Fig. 2.7, so a value handed to
    {!Value} never contains one. *)

type vis = VPriv | VPub
type locality = Local | Foreign
type edition = E2021 | E2024
type crateid = { cname : string; cloc : locality; ced : edition }

(* Program-writable regions (Fig. 2.2). Fig. 2.4's RPLACE/RVAR/ROBJ are not
   surface syntax and have no constructor here. *)
type region = RName of string | RStatic | RAnon

type typ =
  | Unit
  | BoolT
  | I32
  | U8
  | Usize
  | Str
  | TParam of string
  | Ref of region * typ
  | RefMut of region * typ
  | FnPtr of string list * typ list * typ
  | Adt of string * typ list * region list
  | Tup of typ list
  | Arr of typ * Bigint.t
  | DynW of bound list * region option
  | Proj of typ * traitref * string
  | ImplT of bound list * cap
  | Hole
  | AliasU of string * typ list * region list
  | TShort of typ * string  (* sugar: T::A, <ty>::A *)
  | TName of string * typ list * region list  (* UNRESOLVED *)

and traitref = TR of string * typ list * region list

and bound =
  | BTrait of string * typ list * region list * constr list
  | BRegion of region
  | BRelax
  | BForall of string list * bound

and constr = CEq of string * typ | CBnd of string * bound list
and cap = CapNone | CapUse of string list * string list

type pat =
  | QVar of string
  | QWild
  | QVariant of string * string * pat list
  | QTupStruct of string * pat list

type lit =
  | LNum of Bigint.t
  | LI32 of Bigint.t
  | LU8 of Bigint.t
  | LUsize of Bigint.t
  | LTrue
  | LFalse
  | LUnit

type path =
  | PFn of string
  | PStruct of string
  | PVariant of string * string
  | PConst of string
  | PQFn of typ * traitref * string
  | PQConst of typ * traitref * string
  | PBVariant of string  (* sugar: a bare prelude variant *)
  | PShort of typ * string  (* sugar: T::X, <ty>::X *)
  | PDShort of string * string  (* sugar: D::f *)
  | PName of string  (* UNRESOLVED: a bare name *)
  | PName2 of string * string  (* UNRESOLVED: A::B *)
  | PQual of typ * traitref * string  (* UNRESOLVED: <ty as D<..>>::X *)

type targs = TANone | TASome of typ list * region list
type mv = MvNone | MvMove
type rett = RtNone | RtSome of typ
type cparam = CaPat of pat | CaPatT of pat * typ

type term =
  | EVar of string
  | EPath of path * targs
  | ELit of lit
  | ECall of term * term list
  | ETup of term list
  | EStruct of string * targs * (string * term) list
  | EArray of term list
  | ERepeat of term * Bigint.t
  | ERef of term
  | ERefMut of term
  | EDeref of term
  | EField of term * string
  | EAssign of lv * term
  | ECast of term * typ
  | ETry of term
  | EClosure of mv * cparam list * rett * term
  | EAsync of mv * term
  | EAwait of term
  | EMatch of term * (pat * term) list
  | ELoop of term
  | EReturn of term
  | ELet of pat * typ * term * term
  | ESeq of term * term
  | EIdent of string  (* UNRESOLVED: a bare lowercase name *)

and lv = LvVar of string | LvDeref of term | LvField of term * string

type aq = AqNone | AqAsync
type gparam = GTy of string | GTyB of string * bound list | GRg of string | GRgB of string * string list

type fparam = APat of pat * typ | ASelf of typ

type bassert =
  | BATy of typ * bound list
  | BARg of region * region list
  | BAForall of string list * bassert

type sfield = SF of string * typ
type variant = VAR of string * typ list

type titem =
  | TIType of string * bound list * bassert list
  | TIConst of string * typ
  | TIFn of string * gparam list * fparam list * typ * bassert list

type iitem =
  | IIType of string * typ * bassert list
  | IIConst of string * typ * term
  | IIFn of string * gparam list * fparam list * typ * bassert list * term

type item =
  | IStruct of vis * string * gparam list * bassert list * sfield list
  | ITupStruct of vis * string * gparam list * bassert list * typ list
  | IEnum of vis * string * gparam list * bassert list * variant list
  | ITrait of vis * string * gparam list * bound list * bassert list * titem list
  | IImpl of vis * gparam list * traitref * typ * bassert list * iitem list
  | IFn of vis * aq * string * gparam list * fparam list * typ * bassert list * term
  | IConst of vis * string * typ * term
  | IAlias of vis * string * gparam list * typ

type crate = CRATE of crateid * item list
type pgm = PGM of crate list
