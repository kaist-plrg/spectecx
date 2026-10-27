open Xl
open Types.Expression
open Types
open Common.Print
open Common.Source

(* Numbers *)

let string_of_num = function
  | `Int i when i >= Bigint.zero -> Bigint.to_string i
  | num -> Num.string_of_num num

(* Texts *)

let string_of_text text = text

(* Identifiers *)

let string_of_varid varid = varid.it
let string_of_typid typid = typid.it
let string_of_relid relid = relid.it
let string_of_ruleid ruleid = ruleid.it
let string_of_defid defid = "$" ^ defid.it

(* Atoms *)

let string_of_atom atom =
  match atom.it with
  | Atom.Tag _ -> ""
  | _ -> Atom.to_string atom.it |> String.lowercase_ascii

let string_of_atoms atoms =
  match atoms with
  | [] -> ""
  | _ -> atoms |> List.map string_of_atom |> String.concat ""

(* Mixfix operators *)

let string_of_mixop mixop = Mixfix.to_string mixop

(* Iterators *)

let string_of_iter iter = match iter with Opt -> "?" | List -> "*"

(* Variables *)

let string_of_var { varid; iters; _ } =
  string_of_varid varid ^ String.concat "" (List.map string_of_iter iters)

(* Types *)

let rec string_of_typ typ =
  match typ.it with
  | BoolT -> "bool"
  | NumT numtyp -> Num.string_of_typ numtyp
  | TextT -> "text"
  | VarT { synid; targs } -> string_of_typid synid ^ string_of_targs targs
  | TupleT typs -> "(" ^ string_of_typs ", " typs ^ ")"
  | IterT { typ; iter } -> string_of_typ typ ^ string_of_iter iter
  | FuncT -> "func"

and string_of_typs sep typs = String.concat sep (List.map string_of_typ typs)

and string_of_nottyp nottyp =
  Mixfix.render ~pad_brackets:true ~string_of_atom ~string_of_arg:string_of_typ
    nottyp.it

and string_of_reltyp reltyp =
  Mode.render ~pad_brackets:true ~string_of_atom ~string_of_in:string_of_typ
    ~string_of_out:string_of_typ reltyp.it

and string_of_deftyp deftyp =
  match deftyp.it with
  | OpaqueT -> "opaque"
  | PlainT typ -> string_of_typ typ
  | StructT typfields -> "{" ^ string_of_typfields ", " typfields ^ "}"
  | VariantT typcases -> "\n   | " ^ string_of_typcases "\n   | " typcases

and string_of_typfield typfield =
  let atom, typ = typfield in
  string_of_atom atom ^ " " ^ string_of_typ typ

and string_of_typfields sep typfields =
  String.concat sep (List.map string_of_typfield typfields)

and string_of_typorigin typorigin =
  let { synid; targs } = typorigin.it in
  "(from " ^ string_of_typid synid ^ string_of_targs targs ^ ")"

and string_of_typcase typcase =
  let { notation; origin; hints } = typcase in
  string_of_nottyp notation ^ " " ^ string_of_typorigin origin
  ^ string_of_hints hints

and string_of_typcases sep typcases =
  String.concat sep (List.map string_of_typcase typcases)

(* Values *)

and string_of_value ?(short = false) ?(level = 0) value =
  match value.it with
  | BoolV b -> string_of_bool b
  | NumV n -> string_of_num n
  | TextV s -> String.escaped s
  | StructV [] -> "{}"
  | StructV valuefields when short ->
      Format.asprintf "{ .../%d }" (List.length valuefields)
  | StructV valuefields ->
      Format.sprintf "{\n%s\n%s}"
        (String.concat ";\n"
           (List.map
              (fun (atom, value) ->
                let indent = indent (level + 1) in
                Format.asprintf "%s%s %s" indent (string_of_atom atom)
                  (string_of_value ~short ~level:(level + 1) value))
              valuefields))
        (indent level)
  | CaseV vc when short -> string_of_mixop (Mixfix.to_mixop vc)
  | CaseV vc -> string_of_notval ~level vc
  | TupleV values ->
      Format.asprintf "(%s)"
        (String.concat ", "
           (List.map (string_of_value ~short ~level:(level + 1)) values))
  | OptV (Some value) ->
      Format.asprintf "Some(%s)"
        (string_of_value ~short ~level:(level + 1) value)
  | OptV None -> "None"
  | ListV [] -> "[]"
  | ListV values when short -> Format.asprintf "[ .../%d ]" (List.length values)
  | ListV values ->
      Format.asprintf "[\n%s\n%s]"
        (String.concat ",\n"
           (List.map
              (fun value ->
                let indent = indent (level + 1) in
                indent ^ string_of_value ~short ~level:(level + 1) value)
              values))
        (indent level)
  | FuncV id -> string_of_defid id
  | OpaqueV value -> "<opaque " ^ Opaque.name value ^ ">"

and string_of_notval ?(level = 0) notval =
  Mixfix.render ~pad_brackets:true ~string_of_atom
    ~string_of_arg:(string_of_value ~level:(level + 1))
    notval

(* Operators *)

and string_of_unop = function
  | #Bool.unop as op -> Bool.string_of_unop op
  | #Num.unop as op -> Num.string_of_unop op

and string_of_binop = function
  | #Bool.binop as op -> Bool.string_of_binop op
  | #Num.binop as op -> Num.string_of_binop op

and string_of_cmpop = function
  | #Bool.cmpop as op -> Bool.string_of_cmpop op
  | #Num.cmpop as op -> Num.string_of_cmpop op

(* Expressions *)

and string_of_exp exp =
  match exp.it with
  | BoolE b -> string_of_bool b
  | NumE n -> string_of_num n
  | TextE text -> "\"" ^ String.escaped text ^ "\""
  | VarE varid -> string_of_varid varid
  | UnE (unop, _, exp) -> string_of_unop unop ^ string_of_exp exp
  | BinE (binop, _, exp_l, exp_r) ->
      "(" ^ string_of_exp exp_l ^ " " ^ string_of_binop binop ^ " "
      ^ string_of_exp exp_r ^ ")"
  | CmpE (cmpop, _, exp_l, exp_r) ->
      "(" ^ string_of_exp exp_l ^ " " ^ string_of_cmpop cmpop ^ " "
      ^ string_of_exp exp_r ^ ")"
  | UpCastE (typ, exp) -> string_of_exp exp ^ " as " ^ string_of_typ typ
  | DownCastE (typ, exp) -> string_of_exp exp ^ " as " ^ string_of_typ typ
  | SubE (exp, typ) -> string_of_exp exp ^ " <: " ^ string_of_typ typ
  | MatchE (exp, pattern) ->
      string_of_exp exp ^ " matches " ^ string_of_pattern pattern
  | TupleE es -> "(" ^ string_of_exps ", " es ^ ")"
  | CaseE notexp -> string_of_notexp notexp
  | StrE expfields ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (atom, exp) -> string_of_atom atom ^ " " ^ string_of_exp exp)
             expfields)
      ^ "}"
  | OptE exp_opt -> "?(" ^ string_of_exps "" (Option.to_list exp_opt) ^ ")"
  | ListE exps -> "[" ^ string_of_exps ", " exps ^ "]"
  | ConsE (exp_h, exp_t) -> string_of_exp exp_h ^ " :: " ^ string_of_exp exp_t
  | CatE (exp_l, exp_r) -> string_of_exp exp_l ^ " ++ " ^ string_of_exp exp_r
  | MemE (exp_e, exp_s) -> string_of_exp exp_e ^ " <- " ^ string_of_exp exp_s
  | LenE exp -> "|" ^ string_of_exp exp ^ "|"
  | DotE (exp_b, atom) -> string_of_exp exp_b ^ "." ^ string_of_atom atom
  | IdxE (exp_b, exp_i) -> string_of_exp exp_b ^ "[" ^ string_of_exp exp_i ^ "]"
  | SliceE (exp_b, exp_l, exp_h) ->
      string_of_exp exp_b ^ "[" ^ string_of_exp exp_l ^ " : "
      ^ string_of_exp exp_h ^ "]"
  | UpdE (exp_b, path, exp_f) ->
      string_of_exp exp_b ^ "[" ^ string_of_path path ^ " = "
      ^ string_of_exp exp_f ^ "]"
  | CallE (defid, targs, args) ->
      string_of_defid defid ^ string_of_targs targs ^ string_of_args args
  | IterE (exp, iterexp) -> string_of_exp exp ^ string_of_iterexp iterexp

and string_of_exps sep exps = String.concat sep (List.map string_of_exp exps)

and string_of_notexp notexp =
  Mixfix.render ~pad_brackets:true ~string_of_atom ~string_of_arg:string_of_exp
    notexp

and string_of_iterexp iterexp =
  let iter, vars = iterexp in
  string_of_iter iter ^ "{"
  ^ String.concat ", "
      (List.map
         (fun var ->
           string_of_var var ^ " <- "
           ^ string_of_var { var with iters = var.iters @ [ iter ] })
         vars)
  ^ "}"

and string_of_iterexps iterexps =
  iterexps |> List.map string_of_iterexp |> String.concat ""

(* Patterns *)

and string_of_pattern pattern =
  match pattern with
  | CaseP mixop -> string_of_mixop mixop
  | ListP `Cons -> "_ :: _"
  | ListP (`Fixed len) -> Format.asprintf "[ _/%d ]" len
  | ListP `Nil -> "[]"
  | OptP `Some -> "(_)"
  | OptP `None -> "()"

(* Paths *)

and string_of_path path =
  match path.it with
  | RootP -> ""
  | IdxP (path, exp) -> string_of_path path ^ "[" ^ string_of_exp exp ^ "]"
  | SliceP (path, exp_l, exp_h) ->
      string_of_path path ^ "[" ^ string_of_exp exp_l ^ " : "
      ^ string_of_exp exp_h ^ "]"
  | DotP ({ it = RootP; _ }, atom) -> string_of_atom atom
  | DotP (path, atom) -> string_of_path path ^ "." ^ string_of_atom atom

(* Parameters *)

and string_of_param param =
  match param.it with
  | ExpP typ -> string_of_typ typ
  | DefP { defid; tparams; params; typ } ->
      string_of_defid defid ^ string_of_tparams tparams
      ^ string_of_params params ^ " : " ^ string_of_typ typ

and string_of_params params =
  match params with
  | [] -> ""
  | params -> "(" ^ String.concat ", " (List.map string_of_param params) ^ ")"

(* Type parameters *)

and string_of_tparam tparam = tparam.it

and string_of_tparams tparams =
  match tparams with
  | [] -> ""
  | tparams ->
      "<" ^ String.concat ", " (List.map string_of_tparam tparams) ^ ">"

(* Arguments *)

and string_of_arg arg =
  match arg.it with
  | ExpA exp -> string_of_exp exp
  | DefA defid -> string_of_defid defid

and string_of_args args =
  match args with
  | [] -> ""
  | args -> "(" ^ String.concat ", " (List.map string_of_arg args) ^ ")"

(* Type arguments *)

and string_of_targ targ = string_of_typ targ

and string_of_targs targs =
  match targs with
  | [] -> ""
  | targs -> "<" ^ String.concat ", " (List.map string_of_targ targs) ^ ">"

(* Rules *)

and string_of_rule rule =
  let { ruleid; concl; prems } = rule.it in
  ";; " ^ string_of_region rule.at ^ "\n   rule " ^ string_of_ruleid ruleid
  ^ ": " ^ string_of_notexp concl ^ string_of_prems prems

and string_of_rules rules =
  String.concat ""
    (List.map (fun rule -> "\n\n   " ^ string_of_rule rule) rules)

(* Clause *)

and string_of_clause idx clause =
  let { args; body; prems } = clause.it in
  ";; " ^ string_of_region clause.at ^ "\n   clause " ^ string_of_int idx
  ^ string_of_args args ^ " = " ^ string_of_exp body ^ string_of_prems prems

and string_of_clauses clauses =
  String.concat ""
    (List.mapi
       (fun idx clause -> "\n\n   " ^ string_of_clause idx clause)
       clauses)

(* Premises *)

and string_of_relcall { relid; notexp } =
  string_of_relid relid ^ ": " ^ string_of_notexp notexp

and string_of_prem prem =
  match prem.it with
  | RelPr call -> "rel " ^ string_of_relcall call
  | RelAssertPr { call; expect } ->
      "if " ^ string_of_relcall call
      ^ if expect then " holds" else " does not hold"
  | IfPr { cond; _ } -> "if " ^ string_of_exp cond
  | ElsePr -> "otherwise"
  | LetPr (exp_l, exp_r) ->
      "let " ^ string_of_exp exp_l ^ " = " ^ string_of_exp exp_r
  | IterPr (({ it = IterPr _; _ } as prem), iterexp) ->
      string_of_prem prem ^ string_of_iterexp iterexp
  | IterPr (prem, iterexp) ->
      "(" ^ string_of_prem prem ^ ")" ^ string_of_iterexp iterexp
  | DebugPr exp -> "debug " ^ string_of_exp exp

and string_of_prems prems =
  String.concat ""
    (List.map (fun prem -> "\n      -- " ^ string_of_prem prem) prems)

(* Hints *)

and string_of_hint hint =
  " hint(" ^ hint.hintid.it ^ " " ^ El.Print.string_of_exp hint.hintexp ^ ")"

and string_of_hints hints = String.concat "" (List.map string_of_hint hints)

(* Definitions *)

let rec string_of_def def =
  ";; " ^ string_of_region def.at ^ "\n"
  ^
  match def.it with
  | TypD { synid = id; deftyp = { it = OpaqueT; _ }; _ } ->
      "opaque syntax " ^ string_of_typid id
  | TypD { synid; tparams; deftyp } ->
      "syntax " ^ string_of_typid synid ^ string_of_tparams tparams ^ " = "
      ^ string_of_deftyp deftyp
  | RelD { relid; reltyp; rules } ->
      "relation " ^ string_of_relid relid ^ ": " ^ string_of_reltyp reltyp
      ^ string_of_rules rules
  | BuiltinDecD { defid; tparams; params; typ; _ } ->
      "builtin dec " ^ string_of_defid defid ^ string_of_tparams tparams
      ^ string_of_params params ^ " : " ^ string_of_typ typ
  | DecD { defid; tparams; params; typ; clauses } ->
      "def " ^ string_of_defid defid ^ string_of_tparams tparams
      ^ string_of_params params ^ " : " ^ string_of_typ typ ^ " ="
      ^ string_of_clauses clauses

and string_of_defs defs = String.concat "\n\n" (List.map string_of_def defs)

(* Spec *)

let string_of_spec spec = string_of_defs spec

(* Values as an indented tree *)

type tree_part = Type | Mixop | Atom | Leaf | Region | No_region

let string_of_value_tree ?(style = fun _ s -> s) value =
  let buf = Buffer.create 1024 in
  let unquote s =
    let len = String.length s in
    if len >= 2 && s.[0] = '`' && s.[len - 1] = '`' then String.sub s 1 (len - 2)
    else s
  in
  let string_of_at at =
    if at = no_region then style No_region "(no region)"
    else style Region (string_of_range at.left at.right)
  in
  let add_line prefix label at =
    Buffer.add_string buf (prefix ^ label ^ "  " ^ string_of_at at ^ "\n")
  in
  let values_of values = List.map (fun value -> Mixfix.Arg value) values in
  let rec add_node prefix child_prefix value =
    let label, children =
      match value.it with
      | CaseV valuecase ->
          let mixop_str =
            unquote (string_of_mixop (Mixfix.to_mixop valuecase))
          in
          let typ_name =
            match value.note.typ with VarT { synid; _ } -> synid.it | _ -> ""
          in
          let label =
            if mixop_str = "" then style Type typ_name
            else style Type typ_name ^ " " ^ style Mixop mixop_str
          in
          (label, valuecase)
      | TupleV values -> (style Type "tuple", values_of values)
      | ListV values -> (style Type "list", values_of values)
      | OptV (Some value) -> (style Type "some", [ Mixfix.Arg value ])
      | StructV valuefields ->
          (style Type "struct", values_of (List.map snd valuefields))
      | _ -> (style Leaf (string_of_value value), [])
    in
    add_line prefix label value.at;
    let arity = List.length children in
    List.iteri
      (fun idx child ->
        let is_last = idx = arity - 1 in
        let connector = if is_last then "└─" else "├─" in
        let continuation = if is_last then "  " else "│ " in
        match child with
        | Mixfix.Arg value ->
            add_node (child_prefix ^ connector)
              (child_prefix ^ continuation)
              value
        | Mixfix.Atom atom ->
            add_line (child_prefix ^ connector)
              (style Type "atom" ^ " "
              ^ style Atom (unquote (Xl.Atom.to_string atom.it)))
              atom.at)
      children
  in
  add_node "" "" value;
  Buffer.contents buf
