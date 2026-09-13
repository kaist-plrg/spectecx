(** IL value of [pgm] -> Rust source.

    The output is re-parsed by [spectecx rust parse -r], so every printed form
    must be one the parser maps back to the same value.  Where Fig. 2.3 has a
    single production for several surface spellings (an elided region and `'_`,
    an omitted return type and `-> ()`, `struct S;` and `struct S {}`), the
    printer emits the explicit one. *)

open Ast

let concat sep f l = String.concat sep (List.map f l)

(* `{ a, b }`, and `{ }` rather than `{  }` when there is nothing to brace. *)
let braced f l = if l = [] then "{ }" else "{ " ^ concat ", " f l ^ " }"

(* ------------------------------------------------------------------ *)
(* Types                                                               *)
(* ------------------------------------------------------------------ *)

let string_of_region = function
  | RName r -> "'" ^ r
  | RStatic -> "'static"
  | RAnon -> "'_"

let rec string_of_typ (t : typ) : string =
  match t with
  | Unit -> "()"
  | BoolT -> "bool"
  | I32 -> "i32"
  | U8 -> "u8"
  | Usize -> "usize"
  | Str -> "str"
  | TParam x -> x
  | Ref (r, t) -> "&" ^ string_of_region r ^ " " ^ string_of_typ t
  | RefMut (r, t) -> "&" ^ string_of_region r ^ " mut " ^ string_of_typ t
  | FnPtr (rs, ts, t) ->
      (if rs = [] then "" else "for<" ^ concat ", " (fun r -> "'" ^ r) rs ^ "> ")
      ^ "fn(" ^ concat ", " string_of_typ ts ^ ") -> " ^ string_of_typ t
  | Adt (n, ts, rs) -> n ^ string_of_args ts rs
  | AliasU (n, ts, rs) -> n ^ string_of_args ts rs
  | Tup ts -> "(" ^ concat ", " string_of_typ ts ^ ")"
  | Arr (t, n) -> "[" ^ string_of_typ t ^ "; " ^ Bigint.to_string n ^ "]"
  | DynW (bs, r) ->
      "dyn " ^ concat " + " string_of_bound bs
      ^ (match r with None -> "" | Some r -> " + " ^ string_of_region r)
  | Proj (t, tr, a) ->
      "<" ^ string_of_typ t ^ " as " ^ string_of_traitref tr ^ ">::" ^ a
  | ImplT (bs, cap) ->
      "impl " ^ concat " + " string_of_bound bs ^ string_of_cap cap
  | Hole -> "_"
  | TShort (t, a) -> "<" ^ string_of_typ t ^ ">::" ^ a
  | TName (n, ts, rs) -> n ^ string_of_args ts rs

and string_of_args ts rs =
  if ts = [] && rs = [] then ""
  else
    "<"
    ^ String.concat ", "
        (List.map string_of_typ ts @ List.map string_of_region rs)
    ^ ">"

and string_of_traitref (TR (d, ts, rs)) = d ^ string_of_args ts rs

and string_of_bound = function
  | BTrait (d, ts, rs, cs) ->
      let args =
        List.map string_of_typ ts
        @ List.map string_of_region rs
        @ List.map string_of_constr cs
      in
      d ^ (if args = [] then "" else "<" ^ String.concat ", " args ^ ">")
  | BRegion r -> string_of_region r
  | BRelax -> "?Sized"
  | BForall (rs, b) ->
      "for<" ^ concat ", " (fun r -> "'" ^ r) rs ^ "> " ^ string_of_bound b

and string_of_constr = function
  | CEq (a, t) -> a ^ " = " ^ string_of_typ t
  | CBnd (a, bs) -> a ^ ": " ^ concat " + " string_of_bound bs

and string_of_cap = function
  | CapNone -> ""
  | CapUse (ts, rs) ->
      " + use<"
      ^ String.concat ", " (ts @ List.map (fun r -> "'" ^ r) rs)
      ^ ">"

(* ------------------------------------------------------------------ *)
(* Patterns and terms                                                  *)
(* ------------------------------------------------------------------ *)

let rec string_of_pat = function
  | QVar x -> x
  | QWild -> "_"
  | QVariant (e, v, qs) ->
      e ^ "::" ^ v ^ "(" ^ concat ", " string_of_pat qs ^ ")"
  | QTupStruct (s, qs) -> s ^ "(" ^ concat ", " string_of_pat qs ^ ")"

let string_of_lit = function
  | LNum n -> Bigint.to_string n
  | LI32 n -> Bigint.to_string n ^ "i32"
  | LU8 n -> Bigint.to_string n ^ "u8"
  | LUsize n -> Bigint.to_string n ^ "usize"
  | LTrue -> "true"
  | LFalse -> "false"
  | LUnit -> "()"

let string_of_targs = function
  | TANone -> ""
  | TASome (ts, rs) ->
      "::<"
      ^ String.concat ", "
          (List.map string_of_typ ts @ List.map string_of_region rs)
      ^ ">"

let string_of_path = function
  | PFn f -> f
  | PStruct s -> s
  | PVariant (e, v) -> e ^ "::" ^ v
  | PConst n -> n
  | PQFn (t, tr, f) ->
      "<" ^ string_of_typ t ^ " as " ^ string_of_traitref tr ^ ">::" ^ f
  | PQConst (t, tr, n) ->
      "<" ^ string_of_typ t ^ " as " ^ string_of_traitref tr ^ ">::" ^ n
  | PBVariant v -> v
  | PShort (t, x) -> "<" ^ string_of_typ t ^ ">::" ^ x
  | PDShort (d, f) -> d ^ "::" ^ f
  | PName n -> n
  | PName2 (a, b) -> a ^ "::" ^ b
  | PQual (t, tr, x) ->
      "<" ^ string_of_typ t ^ " as " ^ string_of_traitref tr ^ ">::" ^ x

(* Precedence: 0 assignment/closure/return, 1 `as`, 2 prefix `&`/`*`,
   3 postfix call/field/`?`, 4 primary.  Mirrors Parse. *)
let rec string_of_term ~prec (e : term) : string =
  let paren p s = if prec > p then "(" ^ s ^ ")" else s in
  match e with
  | EAssign (l, e) ->
      paren 0 (string_of_lv l ^ " = " ^ string_of_term ~prec:0 e)
  | EReturn e -> paren 0 ("return " ^ string_of_term ~prec:0 e)
  | EClosure (m, ps, r, body) ->
      paren 0
        ((match m with MvNone -> "" | MvMove -> "move ")
        ^ (if ps = [] then "||"
           else "|" ^ concat ", " string_of_cparam ps ^ "|")
        ^ (match r with RtNone -> " " | RtSome t -> " -> " ^ string_of_typ t ^ " ")
        ^ string_of_block body)
  | ECast (e, t) ->
      paren 1 (string_of_term ~prec:2 e ^ " as " ^ string_of_typ t)
  | ERef e -> paren 2 ("&" ^ string_of_term ~prec:2 e)
  | ERefMut e -> paren 2 ("&mut " ^ string_of_term ~prec:2 e)
  | EDeref e -> paren 2 ("*" ^ string_of_term ~prec:2 e)
  | ECall (f, args) ->
      paren 3
        (string_of_term ~prec:3 f ^ "("
        ^ concat ", " (string_of_term ~prec:0) args
        ^ ")")
  | EField (e, x) -> paren 3 (string_of_term ~prec:3 e ^ "." ^ x)
  | EAwait e -> paren 3 (string_of_term ~prec:3 e ^ ".await")
  | ETry e -> paren 3 (string_of_term ~prec:3 e ^ "?")
  | EVar x -> x
  | EIdent x -> x
  | EPath (p, ta) -> string_of_path p ^ string_of_targs ta
  | ELit l -> string_of_lit l
  | ETup es -> "(" ^ concat ", " (string_of_term ~prec:0) es ^ ")"
  | EStruct (s, ta, fs) ->
      s ^ string_of_targs ta ^ " "
      ^ braced (fun (x, e) -> x ^ ": " ^ string_of_term ~prec:0 e) fs
  | EArray es -> "[" ^ concat ", " (string_of_term ~prec:0) es ^ "]"
  | ERepeat (e, n) ->
      "[" ^ string_of_term ~prec:0 e ^ "; " ^ Bigint.to_string n ^ "]"
  | EAsync (m, e) ->
      "async " ^ (match m with MvNone -> "" | MvMove -> "move ")
      ^ string_of_block e
  | EMatch (e, arms) ->
      (* the scrutinee is parenthesised so that a path scrutinee is not read
         back as the head of a struct literal *)
      "match (" ^ string_of_term ~prec:0 e ^ ") { "
      ^ concat ", "
          (fun (q, e) -> string_of_pat q ^ " => " ^ string_of_term ~prec:0 e)
          arms
      ^ " }"
  | ELoop e -> "loop " ^ string_of_block e
  | (ELet _ | ESeq _) as e -> string_of_block e

and string_of_cparam = function
  | CaPat q -> string_of_pat q
  | CaPatT (q, t) -> string_of_pat q ^ ": " ^ string_of_typ t

and string_of_lv = function
  | LvVar x -> x
  | LvDeref e -> "*" ^ string_of_term ~prec:2 e
  | LvField (e, x) -> string_of_term ~prec:3 e ^ "." ^ x

and string_of_block (e : term) : string = "{ " ^ string_of_stmts e ^ " }"

and string_of_stmts (e : term) : string =
  match e with
  | ELet (q, t, e1, rest) ->
      "let " ^ string_of_pat q ^ ": " ^ string_of_typ t ^ " = "
      ^ string_of_term ~prec:0 e1 ^ "; " ^ string_of_stmts rest
  | ESeq (e1, e2) ->
      string_of_term ~prec:0 e1 ^ "; " ^ string_of_stmts e2
  | e -> string_of_term ~prec:0 e

(* ------------------------------------------------------------------ *)
(* Items                                                               *)
(* ------------------------------------------------------------------ *)

let string_of_vis = function VPriv -> "" | VPub -> "pub "
let string_of_aq = function AqNone -> "" | AqAsync -> "async "

let string_of_gparam = function
  | GTy t -> t
  | GTyB (t, bs) -> t ^ ": " ^ concat " + " string_of_bound bs
  | GRg r -> "'" ^ r
  | GRgB (r, rs) -> "'" ^ r ^ ": " ^ concat " + " (fun s -> "'" ^ s) rs

let string_of_gparams gs =
  if gs = [] then "" else "<" ^ concat ", " string_of_gparam gs ^ ">"

let rec string_of_bassert = function
  | BATy (t, bs) -> string_of_typ t ^ ": " ^ concat " + " string_of_bound bs
  | BARg (r, rs) ->
      string_of_region r ^ ": " ^ concat " + " string_of_region rs
  | BAForall (rs, b) ->
      "for<" ^ concat ", " (fun r -> "'" ^ r) rs ^ "> " ^ string_of_bassert b

let string_of_where w =
  if w = [] then " " else " where " ^ concat ", " string_of_bassert w ^ " "

let string_of_fparam = function
  | APat (q, t) -> string_of_pat q ^ ": " ^ string_of_typ t
  | ASelf t -> "self: " ^ string_of_typ t

let string_of_fparams ps = "(" ^ concat ", " string_of_fparam ps ^ ")"

let string_of_titem = function
  | TIType (a, bs, w) ->
      "  type " ^ a
      ^ (if bs = [] then "" else ": " ^ concat " + " string_of_bound bs)
      ^ string_of_where w ^ ";\n"
  | TIConst (n, t) -> "  const " ^ n ^ ": " ^ string_of_typ t ^ ";\n"
  | TIFn (f, gs, ps, ret, w) ->
      "  fn " ^ f ^ string_of_gparams gs ^ string_of_fparams ps ^ " -> "
      ^ string_of_typ ret ^ string_of_where w ^ ";\n"

let string_of_iitem = function
  | IIType (a, t, w) ->
      "  type " ^ a ^ " = " ^ string_of_typ t ^ string_of_where w ^ ";\n"
  | IIConst (n, t, e) ->
      "  const " ^ n ^ ": " ^ string_of_typ t ^ " = "
      ^ string_of_term ~prec:0 e ^ ";\n"
  | IIFn (f, gs, ps, ret, w, e) ->
      "  fn " ^ f ^ string_of_gparams gs ^ string_of_fparams ps ^ " -> "
      ^ string_of_typ ret ^ string_of_where w ^ string_of_block e ^ "\n"

let string_of_item = function
  | IStruct (v, s, gs, w, fs) ->
      string_of_vis v ^ "struct " ^ s ^ string_of_gparams gs
      ^ string_of_where w
      ^ braced (fun (SF (x, t)) -> x ^ ": " ^ string_of_typ t) fs
      ^ "\n"
  | ITupStruct (v, s, gs, w, ts) ->
      string_of_vis v ^ "struct " ^ s ^ string_of_gparams gs ^ "("
      ^ concat ", " string_of_typ ts
      ^ ")" ^ string_of_where w ^ ";\n"
  | IEnum (v, e, gs, w, vs) ->
      string_of_vis v ^ "enum " ^ e ^ string_of_gparams gs ^ string_of_where w
      ^ braced (fun (VAR (n, ts)) -> n ^ "(" ^ concat ", " string_of_typ ts ^ ")") vs
      ^ "\n"
  | ITrait (v, d, gs, sup, w, its) ->
      string_of_vis v ^ "trait " ^ d ^ string_of_gparams gs
      ^ (if sup = [] then "" else ": " ^ concat " + " string_of_bound sup)
      ^ string_of_where w ^ "{\n"
      ^ String.concat "" (List.map string_of_titem its)
      ^ "}\n"
  | IImpl (v, gs, tr, self, w, its) ->
      string_of_vis v ^ "impl" ^ string_of_gparams gs ^ " "
      ^ string_of_traitref tr ^ " for " ^ string_of_typ self
      ^ string_of_where w ^ "{\n"
      ^ String.concat "" (List.map string_of_iitem its)
      ^ "}\n"
  | IFn (v, aq, f, gs, ps, ret, w, e) ->
      string_of_vis v ^ string_of_aq aq ^ "fn " ^ f ^ string_of_gparams gs
      ^ string_of_fparams ps ^ " -> " ^ string_of_typ ret ^ string_of_where w
      ^ string_of_block e ^ "\n"
  | IConst (v, n, t, e) ->
      string_of_vis v ^ "const " ^ n ^ ": " ^ string_of_typ t ^ " = "
      ^ string_of_term ~prec:0 e ^ ";\n"
  | IAlias (v, a, gs, t) ->
      string_of_vis v ^ "type " ^ a ^ string_of_gparams gs ^ " = "
      ^ string_of_typ t ^ ";\n"

let string_of_crate (CRATE (c, its)) =
  Printf.sprintf "//@ crate %s %s %s\n" c.cname
    (match c.cloc with Local -> "local" | Foreign -> "foreign")
    (match c.ced with E2021 -> "2021" | E2024 -> "2024")
  ^ String.concat "" (List.map string_of_item its)

(* No trailing newline: the CLI adds one when it prints the result. *)
let string_of_pgm (PGM cs) =
  let s = String.concat "" (List.map string_of_crate cs) in
  if String.length s > 0 && s.[String.length s - 1] = '\n' then
    String.sub s 0 (String.length s - 1)
  else s

(* ------------------------------------------------------------------ *)
(* Task interface                                                      *)
(* ------------------------------------------------------------------ *)

let unparse ~spec:_ (values : Lang.Il.Value.t list) : string =
  match values with
  | [ v ] -> string_of_pgm (Value.to_pgm v)
  | _ -> failwith "rust unparse expects a single program value"
