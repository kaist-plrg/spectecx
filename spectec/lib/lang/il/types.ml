open Xl
open Common.Source

[@@@ocamlformat "disable"]
[@@@warning "-30"]  (* Several AST records intentionally share semantic field names. *)

(* Numbers *)

type num = Num.t

(* Texts *)

type text = string

(* Identifiers *)

type id = id' phrase
and id' = string

(* Atoms *)

type atom = atom' phrase
and atom' = Atom.t

(* Mixfix operators *)

type mixop = Mixfix.mixop

(* Iterators *)

type iter =
  | Opt       (* `?` *)
  | List      (* `*` *)

(* Hints *)

type hint = { hintid : id; hintexp : El.exp }

(* Types *)

type typ = typ' phrase
and typ' =
  | BoolT                   (* `bool` *)
  | NumT of Num.typ         (* numtyp *)
  | TextT                   (* `text` *)
  | VarT of { synid : id; targs : targ list }  (* id (`<` list(targ, `,`) `>`)? *)
  | TupleT of typ list      (* `(` list(typ, `,`) `)` *)
  | IterT of { typ : typ; iter : iter }  (* typ iter *)
  | FuncT                   (* `func` *)

(* Type arguments *)

and targ = targ' phrase
and targ' = typ'

(* Variables *)

type var = { varid : id; typ : typ; iters : iter list }

type nottyp = nottyp' phrase
and nottyp' = typ Mixfix.t

and reltyp = reltyp' phrase
and reltyp' = (typ, typ) Mode.t

and deftyp = deftyp' phrase
and deftyp' =
  | OpaqueT
  | PlainT of typ
  | StructT of typfield list
  | VariantT of typcase list

and typfield = atom * typ
and typorigin = typorigin' Common.Source.phrase
and typorigin' = { synid : id; targs : targ list }
and typcase = { notation : nottyp; origin : typorigin; hints : hint list }

(* Values *)

type vid = int
and vnote = { vid : vid; vhash : int; typ : typ' }

and value = (value', vnote) note
and value' =
  | BoolV of bool
  | NumV of Num.t
  | TextV of string
  | StructV of valuefield list
  | CaseV of valuecase
  | TupleV of value list
  | OptV of value option
  | ListV of value list
  | FuncV of id
  | OpaqueV of Opaque.value

and valuefield = atom * value
and valuecase = value Mixfix.t

(* Operators *)

type numop = [ `DecOp | `HexOp ]
and unop = [ Bool.unop | Num.unop ]
and binop = [ Bool.binop | Num.binop ]
and cmpop = [ Bool.cmpop | Num.cmpop ]
and optyp = [ Bool.typ | Num.typ ]

(* Expressions *)

type iterexp = iter * var list

type pattern =
  | CaseP of mixop
  | ListP of [ `Cons | `Fixed of int | `Nil ]
  | OptP of [ `Some | `None ]

module Expression = struct
  type 'ann note = { typ : typ'; annot : 'ann }

  type 'ann t = ('ann node, 'ann note) note_phrase
  and 'ann node =
    | BoolE of bool                            (* bool *)
    | NumE of num                              (* num *)
    | TextE of text                            (* text *)
    | VarE of id                               (* varid *)
    | UnE of unop * optyp * 'ann t             (* unop exp *)
    | BinE of binop * optyp * 'ann t * 'ann t  (* exp binop exp *)
    | CmpE of cmpop * optyp * 'ann t * 'ann t  (* exp cmpop exp *)
    | UpCastE of typ * 'ann t                  (* exp as typ *)
    | DownCastE of typ * 'ann t                (* exp as typ *)
    | SubE of 'ann t * typ                     (* exp `<:` typ *)
    | MatchE of 'ann t * pattern               (* exp `matches` pattern *)
    | TupleE of 'ann t list                    (* `(` exp* `)` *)
    | CaseE of 'ann notation                   (* notexp *)
    | StrE of (atom * 'ann t) list             (* { expfield* } *)
    | OptE of 'ann t option                    (* exp? *)
    | ListE of 'ann t list                     (* `[` exp* `]` *)
    | ConsE of 'ann t * 'ann t                 (* exp `::` exp *)
    | CatE of 'ann t * 'ann t                  (* exp `++` exp *)
    | MemE of 'ann t * 'ann t                  (* exp `<-` exp *)
    | LenE of 'ann t                           (* `|` exp `|` *)
    | DotE of 'ann t * atom                    (* exp.atom *)
    | IdxE of 'ann t * 'ann t                  (* exp `[` exp `]` *)
    | SliceE of 'ann t * 'ann t * 'ann t       (* exp `[` exp `:` exp `]` *)
    | UpdE of 'ann t * 'ann path * 'ann t      (* exp `[` path `=` exp `]` *)
    | CallE of id * targ list * 'ann arg list  (* $id`<` targ* `>``(` arg* `)` *)
    | IterE of 'ann t * iterexp                (* exp iterexp *)

  and 'ann notation = 'ann t Mixfix.t
  and 'ann path = ('ann path_node, typ') note_phrase
  and 'ann path_node =
    | RootP                                    (*  *)
    | IdxP of 'ann path * 'ann t               (* path `[` exp `]` *)
    | SliceP of 'ann path * 'ann t * 'ann t    (* path `[` exp `:` exp `]` *)
    | DotP of 'ann path * atom                 (* path `.` atom *)

  and 'ann arg = 'ann arg_node phrase
  and 'ann arg_node =
    | ExpA of 'ann t                           (* exp *)
    | DefA of id                               (* `$`id *)

  (** Replace annotations throughout an expression, including expressions in paths
      and arguments. The callback receives each original expression. Syntax,
      types, source regions, and provenance are preserved. Callback order among
      sibling expressions is unspecified. *)
  let map_annotations annotate exp =
    let rec map_exp (exp : _ t) =
      let it =
        match exp.it with
        | BoolE b -> BoolE b
        | NumE n -> NumE n
        | TextE text -> TextE text
        | VarE id -> VarE id
        | UnE (op, typ, exp) -> UnE (op, typ, map_exp exp)
        | BinE (op, typ, exp_l, exp_r) ->
            BinE (op, typ, map_exp exp_l, map_exp exp_r)
        | CmpE (op, typ, exp_l, exp_r) ->
            CmpE (op, typ, map_exp exp_l, map_exp exp_r)
        | UpCastE (typ, exp) -> UpCastE (typ, map_exp exp)
        | DownCastE (typ, exp) -> DownCastE (typ, map_exp exp)
        | SubE (exp, typ) -> SubE (map_exp exp, typ)
        | MatchE (exp, pattern) -> MatchE (map_exp exp, pattern)
        | TupleE exps -> TupleE (List.map map_exp exps)
        | CaseE notation -> CaseE (Mixfix.map map_exp notation)
        | StrE fields ->
            StrE (List.map (fun (atom, exp) -> (atom, map_exp exp)) fields)
        | OptE exp -> OptE (Option.map map_exp exp)
        | ListE exps -> ListE (List.map map_exp exps)
        | ConsE (exp_h, exp_t) -> ConsE (map_exp exp_h, map_exp exp_t)
        | CatE (exp_l, exp_r) -> CatE (map_exp exp_l, map_exp exp_r)
        | MemE (exp_e, exp_s) -> MemE (map_exp exp_e, map_exp exp_s)
        | LenE exp -> LenE (map_exp exp)
        | DotE (exp, atom) -> DotE (map_exp exp, atom)
        | IdxE (exp_b, exp_i) -> IdxE (map_exp exp_b, map_exp exp_i)
        | SliceE (exp_b, exp_l, exp_h) ->
            SliceE (map_exp exp_b, map_exp exp_l, map_exp exp_h)
        | UpdE (exp_b, path, exp_f) ->
            UpdE (map_exp exp_b, map_path path, map_exp exp_f)
        | CallE (id, targs, args) -> CallE (id, targs, List.map map_arg args)
        | IterE (exp, iterexp) -> IterE (map_exp exp, iterexp)
      in
      { exp with it; note = { exp.note with annot = annotate exp } }
    and map_path (path : _ path) =
      let it =
        match path.it with
        | RootP -> RootP
        | IdxP (path, exp) -> IdxP (map_path path, map_exp exp)
        | SliceP (path, exp_l, exp_h) ->
            SliceP (map_path path, map_exp exp_l, map_exp exp_h)
        | DotP (path, atom) -> DotP (map_path path, atom)
      in
      { path with it }
    and map_arg (arg : _ arg) =
      let it =
        match arg.it with
        | ExpA exp -> ExpA (map_exp exp)
        | DefA id -> DefA id
      in
      { arg with it }
    in
    map_exp exp
end

type exp = unit Expression.t
type exp' = unit Expression.node
type notexp = unit Expression.notation
type path = unit Expression.path
type path' = unit Expression.path_node
type arg = unit Expression.arg
type arg' = unit Expression.arg_node

type param = param' phrase
and param' =
  (* typ *)
  | ExpP of typ
  (* `def` `$`id ` (`<` list(tparam, `,`) `>`)? (`(` list(param, `,`) `)`)? `:` typ *)
  | DefP of { defid : id; tparams : tparam list; params : param list; typ : typ }

(* Type parameters *)

and tparam = tparam' phrase
and tparam' = id'

(* Rules *)

and rule = rule' phrase
and rule' = { ruleid : id; concl : notexp; prems : prem list }

(* Clauses *)

and clause = clause' phrase
and clause' = { args : arg list; body : exp; prems : prem list }

(* Premises *)

and relcall = { relid : id; notexp : notexp }

and prem = (prem', prem_prov) phrase_prov
and prem_prov =
  | Source of El.prem  (* lowered from this EL premise *)
  | Synthesized        (* introduced by elaboration; no EL source *)
and prem' =
  | RelPr of relcall               (* id `:` notexp *)
  | RelAssertPr of { call : relcall; expect : bool }
                                   (* `if` id `:` notexp `holds` when expect, else `does not hold` *)
  | IfPr of { cond : exp; role : if_role }  (* `if` exp *)
  | ElsePr                         (* `otherwise` *)
  | LetPr of exp * exp             (* `let` exp `=` exp *)
  | IterPr of prem * iterexp       (* prem iterexp *)
  | DebugPr of exp                 (* `debug` exp *)

and if_role =
  | Condition
  | Guard  (* failing it means the rule does not apply *)

(* Definitions *)

type def = def' phrase
and def' =
  (* `syntax` id `<` list(tparam, `,`) `>` `=` deftyp *)
  | TypD of { synid : id; tparams : tparam list; deftyp : deftyp }
  (* `relation` id `:` reltyp rule* *)
  | RelD of { relid : id; reltyp : reltyp; rules : rule list }
  (* `dec` id `<` list(tparam, `,`) `>` list(param, `,`) `:` typ clause* *)
  | DecD of { defid : id; tparams : tparam list; params : param list; typ : typ; clauses : clause list }
  (* `builtin` `dec` id `<` list(tparam, `,`) `>` list(param, `,`) `:` typ hint* *)
  | BuiltinDecD of { defid : id; tparams : tparam list; params : param list; typ : typ; hints : hint list }

(* Spec *)

type spec = def list
