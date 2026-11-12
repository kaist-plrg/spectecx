open El
module Atom = Xl.Atom

type ranked = { formula : Formula.t; rank : Precedence.rank }

let atomic (formula : Formula.t) : ranked = { formula; rank = Atomic }

(* [needs_parens_to_read] skips judgment operators and implication because
   readers already expect them to group last. *)
let is_inner_relation (rank : Precedence.rank) : bool =
  match rank with
  | Subtype | Assignment | Comparison | Arrow -> true
  | _ -> false

let needs_parens_to_read ~(parent : Precedence.rank) (child : Precedence.rank) :
    bool =
  match parent with
  (* Use [(x -> t) :: tenv] so [x -> t] reads as one list item. TeX's
     wider spacing around [->] can otherwise suggest [x -> (t :: tenv)]. *)
  | Cons -> is_inner_relation child
  (* Parenthesize [->] and [:=] under "and" or "or" to show the grouping
     that readers already expect for comparisons and [<:]. *)
  | Conjunction | Disjunction -> (
      match child with Arrow | Assignment -> true | _ -> false)
  (* Parenthesize the inner relation because TeX spaces both relations alike. *)
  | _ -> is_inner_relation parent && is_inner_relation child && child <> parent

let operand ~(parent : Precedence.rank) ~(side : Precedence.side)
    (child : ranked) : Formula.t =
  if
    Precedence.needs_parens ~parent ~side child.rank
    || needs_parens_to_read ~parent child.rank
  then Delimited (Paren, child.formula)
  else child.formula

let sign_after_is_binary (op : Formula.t) : bool =
  match op with
  | Symbol (Slash | Dot | Two_dots | Ellipsis) -> true
  | _ -> false

let infix ~(rank : Precedence.rank) ~(op : Formula.t) (left : ranked)
    (right : ranked) : ranked =
  let left = operand ~parent:rank ~side:Left_operand left in
  let right : Formula.t =
    (* Write [a / (-b)] so TeX spaces the minus as a sign on [b]. *)
    if right.rank = Prefix && sign_after_is_binary op then
      Delimited (Paren, right.formula)
    else operand ~parent:rank ~side:Right_operand right
  in
  { formula = Infix { left; op; right }; rank }

(* Give a leading turnstile its usual rank so [exp : typ] can stay together
   without parentheses in [|- exp : typ]. *)
let rank_of_leading (rank : Precedence.rank) : Precedence.rank =
  match rank with Turnstile | Tilesturn | Step | Colon -> rank | _ -> Prefix

(* The operands are optional because a judgment may start with [|-] and a
   subscripted arrow may end after its subscript. *)
let infix_optional ~(rank : Precedence.rank) ~(op : Formula.t)
    (left : ranked option) (right : ranked option) : ranked =
  match (left, right) with
  | Some left, Some right -> infix ~rank ~op left right
  | None, Some right ->
      let rank = rank_of_leading rank in
      let right = operand ~parent:rank ~side:Right_operand right in
      { formula = Concat [ op; right ]; rank }
  | Some left, None ->
      let left = operand ~parent:rank ~side:Left_operand left in
      { formula = Concat [ left; op ]; rank }
  | None, None -> { formula = op; rank = rank_of_leading rank }

let prefix ~(op : Formula.t) (body : ranked) : ranked =
  let body = operand ~parent:Prefix ~side:Right_operand body in
  { formula = Concat [ op; body ]; rank = Prefix }

let postfix (base : ranked) ~(suffix : Formula.t) : ranked =
  let base = operand ~parent:Postfix ~side:Left_operand base in
  { formula = Concat [ base; suffix ]; rank = Postfix }

(* A superscript's raised position shows its extent, so only the base needs
   a precedence check here. *)
let superscript ~(rank : Precedence.rank) ~(sup : Formula.t) (base : ranked) :
    ranked =
  let base = operand ~parent:rank ~side:Left_operand base in
  { formula = Script { base; sub = None; sup = Some sup }; rank }

let sequence (items : ranked list) : ranked =
  let items =
    List.map (operand ~parent:Juxtaposition ~side:Left_operand) items
  in
  { formula = Seq items; rank = Juxtaposition }

type role = Variable | Type | Function | Keyword | Tag

let formula_of_name ~(role : role) (name : string) : Formula.t =
  match role with
  | Variable -> (
      if String.starts_with ~prefix:"_" name then Word (Sans, "_")
      else
        (* Display [1] and [2] as subscripts in [x_1] and [x_2] to show the
           shared base [x] that elaboration looks up for their type. *)
        match Xl.Var.split_subscript name with
        | base, None -> Word (Sans, base)
        | base, Some sub ->
            Script
              {
                base = Word (Sans, base);
                sub = Some (Word (Sans, sub));
                sup = None;
              })
  | Type | Keyword -> Word (Sans, name)
  | Function -> Word (Roman, name)
  (* The tag's subscript labels the notation without adding a word. *)
  | Tag ->
      Script { base = Concat []; sub = Some (Word (Sans, name)); sup = None }

let formula_of_atom (atom : Atom.t) : Formula.t =
  match atom with
  | Keyword name -> formula_of_name ~role:Keyword (name :> string)
  | Tag name -> formula_of_name ~role:Tag (name :> string)
  | Operator text -> Symbol (Operator (text :> string))
  | Sub -> Symbol Less_colon
  | Sup -> Symbol Colon_greater
  | Turnstile -> Symbol Vdash
  | Tilesturn -> Symbol Dashv
  | Arrow | ArrowSub -> Symbol To
  | DoubleArrowSub -> Symbol Rightarrow
  | DoubleArrowLong -> Symbol Longrightarrow
  | SqArrow -> Symbol Hookrightarrow
  | SqArrowStar -> Symbol Hookrightarrow_star
  | Dot -> Symbol Dot
  | Dot2 -> Symbol Two_dots
  | Dot3 -> Symbol Ellipsis
  | Semicolon -> Symbol Semicolon
  | Colon -> Symbol Colon
  | ColonEq -> Symbol Colon_equal
  | Tilde2 -> Symbol Sim
  | Backslash -> Symbol Setminus
  | LAngle -> Symbol Left_angle
  | RAngle -> Symbol Right_angle
  | LParen -> Symbol Left_paren
  | RParen -> Symbol Right_paren
  | LBrack -> Symbol Left_bracket
  | RBrack -> Symbol Right_bracket
  | LBrace -> Symbol Left_brace
  | RBrace -> Symbol Right_brace

let is_subscripted (atom : Atom.t) : bool =
  match atom with ArrowSub | DoubleArrowSub -> true | _ -> false

let ranked_of_atom_infix ~(atom : Atom.t) ~(sub : Formula.t option)
    (left : ranked option) (right : ranked option) : ranked =
  let op = formula_of_atom atom in
  let op : Formula.t =
    match sub with
    | None -> op
    | Some sub -> Script { base = op; sub = Some sub; sup = None }
  in
  match Precedence.of_atom atom with
  | Juxtaposition ->
      sequence (List.filter_map Fun.id [ left; Some (atomic op); right ])
  | rank -> infix_optional ~rank ~op left right

let ranked_of_infix ~(atom : Atom.t) ~(as_seq_opt : 'a -> 'a list option)
    ~(ranked_of : 'a -> ranked) (left : 'a) (right : 'a) : ranked =
  let operand (side : 'a) : ranked option =
    match as_seq_opt side with Some [] -> None | _ -> Some (ranked_of side)
  in
  let items (items : 'a list) : ranked option =
    match items with
    | [] -> None
    | [ item ] -> Some (ranked_of item)
    | items -> Some (sequence (List.map ranked_of items))
  in
  (* The parser leaves the term after [->_] or [=>_] in the right operand.
     Take that term as the subscript to match P4-SpecTec's notation. *)
  let sub, right =
    if not (is_subscripted atom) then (None, operand right)
    else
      match as_seq_opt right with
      | Some (sub :: rest) -> (Some (ranked_of sub).formula, items rest)
      | Some [] -> (None, None)
      | None -> (Some (ranked_of right).formula, None)
  in
  ranked_of_atom_infix ~atom ~sub (operand left) right

let formula_of_brackets ~(left : atom) ~(right : atom) (body : Formula.t) :
    Formula.t =
  match (left.it, right.it) with
  | LParen, RParen -> Delimited (Paren, body)
  | LBrack, RBrack -> Delimited (Bracket, body)
  | LBrace, RBrace -> Delimited (Brace, body)
  | LAngle, RAngle -> Delimited (Angle, body)
  | _ -> Concat [ formula_of_atom left.it; body; formula_of_atom right.it ]

let formula_of_iter (iter : iter) : Formula.t =
  match iter with Opt -> Symbol Question | List -> Symbol Ast

let formula_of_magnitude ~(numop : numop) (magnitude : Bigint.t) : Formula.t =
  match numop with
  | `DecOp -> Numeral (Bigint.to_string magnitude)
  | `HexOp ->
      Word (Mono, "0x" ^ Z.format "%X" (Bigint.to_zarith_bigint magnitude))

let ranked_of_num ~(numop : numop) (num : num) : ranked =
  match num with
  | `Nat n -> atomic (formula_of_magnitude ~numop n)
  (* An explicit sign distinguishes an integer from a natural number. *)
  | `Int i ->
      let sign : Formula.symbol = if Bigint.(i < zero) then Minus else Plus in
      prefix ~op:(Symbol sign)
        (atomic (formula_of_magnitude ~numop (Bigint.abs i)))

let as_typ_seq_opt (typ : typ) : typ list option =
  match typ with NotationT { it = SeqT typs; _ } -> Some typs | _ -> None

let rec ranked_of_typ (typ : typ) : ranked =
  match typ with
  | PlainT plaintyp -> ranked_of_plaintyp plaintyp
  | NotationT nottyp -> ranked_of_nottyp nottyp

and ranked_of_plaintyp (plaintyp : plaintyp) : ranked =
  match plaintyp.it with
  | BoolT -> atomic (Word (Blackboard, "B"))
  | NumT `NatT -> atomic (Word (Blackboard, "N"))
  | NumT `IntT -> atomic (Word (Blackboard, "Z"))
  | TextT -> atomic (Word (Blackboard, "T"))
  | VarT (id, targs) ->
      atomic
        (Concat [ formula_of_name ~role:Type id.it; formula_of_targs targs ])
  | ParenT plaintyp -> ranked_of_plaintyp plaintyp
  | TupleT plaintyps ->
      atomic
        (Delimited (Paren, Commas (List.map formula_of_plaintyp plaintyps)))
  | IterT (plaintyp, iter) ->
      superscript ~rank:Postfix ~sup:(formula_of_iter iter)
        (ranked_of_plaintyp plaintyp)

and ranked_of_nottyp (nottyp : nottyp) : ranked =
  match nottyp.it with
  | AtomT atom -> atomic (formula_of_atom atom.it)
  | SeqT [] -> atomic (Seq [])
  | SeqT [ typ ] -> ranked_of_typ typ
  | SeqT typs -> sequence (List.map ranked_of_typ typs)
  | InfixT (left, atom, right) ->
      ranked_of_infix ~atom:atom.it ~as_seq_opt:as_typ_seq_opt
        ~ranked_of:ranked_of_typ left right
  | BrackT (left, typ, right) ->
      atomic (formula_of_brackets ~left ~right (formula_of_typ typ))

and formula_of_typ (typ : typ) : Formula.t = (ranked_of_typ typ).formula

and formula_of_plaintyp (plaintyp : plaintyp) : Formula.t =
  (ranked_of_plaintyp plaintyp).formula

and formula_of_targs (targs : targ list) : Formula.t =
  match targs with
  | [] -> Concat []
  | targs -> Delimited (Angle, Commas (List.map formula_of_plaintyp targs))

(* Use two hexadecimal digits per byte escape so the lexer can read the
   literal back. With the three-digit escapes from [El.Unparse], the lexer
   reads the final digit as a separate character. *)
let spelling_of_text (text : string) : string =
  let buffer = Buffer.create (String.length text + 2) in
  let add_byte_escape (char : char) =
    Buffer.add_string buffer (Printf.sprintf "\\%02X" (Char.code char))
  in
  let rec add_from (index : int) =
    if index < String.length text then (
      let decode = String.get_utf_8_uchar text index in
      if not (Uchar.utf_decode_is_valid decode) then (
        add_byte_escape text.[index];
        add_from (index + 1))
      else
        let length = Uchar.utf_decode_length decode in
        (match text.[index] with
        | '"' -> Buffer.add_string buffer "\\\""
        | '\\' -> Buffer.add_string buffer "\\\\"
        | '\n' -> Buffer.add_string buffer "\\n"
        | '\r' -> Buffer.add_string buffer "\\r"
        | '\t' -> Buffer.add_string buffer "\\t"
        | ('\x00' .. '\x1f' | '\x7f') as char -> add_byte_escape char
        | _ -> Buffer.add_string buffer (String.sub text index length));
        add_from (index + length))
  in
  Buffer.add_char buffer '"';
  add_from 0;
  Buffer.add_char buffer '"';
  Buffer.contents buffer

let symbol_of_unop (unop : unop) : Formula.symbol =
  match unop with `NotOp -> Neg | `PlusOp -> Plus | `MinusOp -> Minus

let symbol_of_cmpop (cmpop : cmpop) : Formula.symbol =
  match cmpop with
  | `EqOp -> Equal
  | `NeOp -> Not_equal
  | `LtOp -> Less
  | `GtOp -> Greater
  | `LeOp -> Less_equal
  | `GeOp -> Greater_equal

let as_exp_seq_opt (exp : exp) : exp list option =
  match exp.it with SeqE exps -> Some exps | _ -> None

let rec ranked_of_exp (exp : exp) : ranked =
  match exp.it with
  | BoolE b -> atomic (Word (Sans, Bool.to_string b))
  | NumE (numop, num) -> ranked_of_num ~numop num
  | TextE text -> atomic (Text (spelling_of_text text))
  | VarE id -> atomic (formula_of_name ~role:Variable id.it)
  | UnE (unop, exp) ->
      prefix ~op:(Symbol (symbol_of_unop unop)) (ranked_of_exp exp)
  | BinE (left, binop, right) -> ranked_of_bin_exp ~binop left right
  | CmpE (left, cmpop, right) ->
      infix
        ~rank:(Precedence.of_cmpop cmpop)
        ~op:(Symbol (symbol_of_cmpop cmpop))
        (ranked_of_exp left) (ranked_of_exp right)
  | ArithE exp | ParenE exp -> ranked_of_exp exp
  | EpsE -> atomic (Symbol Epsilon)
  | ListE exps ->
      atomic (Delimited (Bracket, Commas (List.map formula_of_exp exps)))
  | ConsE (left, right) ->
      infix ~rank:Cons ~op:(Symbol Double_colon) (ranked_of_exp left)
        (ranked_of_exp right)
  | CatE (left, right) ->
      infix ~rank:Additive ~op:(Symbol Double_plus) (ranked_of_exp left)
        (ranked_of_exp right)
  | IdxE (base, index) ->
      postfix (ranked_of_exp base)
        ~suffix:(Delimited (Bracket, formula_of_exp index))
  | SliceE (base, low, high) ->
      postfix (ranked_of_exp base)
        ~suffix:(Delimited (Bracket, formula_of_slice low high))
  | LenE exp -> atomic (Delimited (Bar, formula_of_exp exp))
  | MemE (left, right) ->
      infix ~rank:Comparison ~op:(Symbol In) (ranked_of_exp left)
        (ranked_of_exp right)
  | StrE fields ->
      atomic (Delimited (Brace, Commas (List.map formula_of_field fields)))
  (* Use a dot so [x_1.FIELD] keeps [FIELD] separate from the subscript [1]. *)
  | DotE (base, atom) ->
      postfix (ranked_of_exp base)
        ~suffix:(Concat [ Symbol Dot; formula_of_atom atom.it ])
  | UpdE (base, path, field) ->
      let update =
        infix ~rank:Comparison ~op:(Symbol Equal)
          (atomic (formula_of_path path))
          (ranked_of_exp field)
      in
      postfix (ranked_of_exp base) ~suffix:(Delimited (Bracket, update.formula))
  | TupleE exps ->
      atomic (Delimited (Paren, Commas (List.map formula_of_exp exps)))
  | CallE (id, targs, args) ->
      atomic
        (Concat
           [
             formula_of_name ~role:Function id.it;
             formula_of_targs targs;
             formula_of_args args;
           ])
  | IterE (exp, iter) ->
      superscript ~rank:Postfix ~sup:(formula_of_iter iter) (ranked_of_exp exp)
  | SubE (exp, plaintyp) ->
      infix ~rank:Subtype ~op:(Symbol Less_colon) (ranked_of_exp exp)
        (ranked_of_plaintyp plaintyp)
  | AtomE atom -> atomic (formula_of_atom atom.it)
  | SeqE [] -> atomic (Seq [])
  | SeqE [ exp ] -> ranked_of_exp exp
  | SeqE exps -> sequence (List.map ranked_of_exp exps)
  | InfixE (left, atom, right) ->
      ranked_of_infix ~atom:atom.it ~as_seq_opt:as_exp_seq_opt
        ~ranked_of:ranked_of_exp left right
  | BrackE (left, exp, right) ->
      atomic (formula_of_brackets ~left ~right (formula_of_exp exp))
  | HoleE _ -> fail_misplaced exp ~form:"hole"
  | FuseE _ -> fail_misplaced exp ~form:"token concatenation"
  | UnparenE _ -> fail_misplaced exp ~form:"unparenthesize"
  | LatexE _ -> fail_misplaced exp ~form:"LaTeX literal"

and ranked_of_bin_exp ~(binop : binop) (left : exp) (right : exp) : ranked =
  let rank = Precedence.of_binop binop in
  let binary (symbol : Formula.symbol) =
    infix ~rank ~op:(Symbol symbol) (ranked_of_exp left) (ranked_of_exp right)
  in
  match binop with
  | `AndOp -> binary Wedge
  | `OrOp -> binary Vee
  | `ImplOp -> binary Rightarrow
  | `EquivOp -> binary Leftrightarrow
  | `AddOp -> binary Plus
  | `SubOp -> binary Minus
  | `MulOp -> binary Cdot
  | `DivOp -> binary Slash
  | `ModOp -> binary Mod
  | `PowOp -> superscript ~rank ~sup:(formula_of_exp right) (ranked_of_exp left)

and fail_misplaced (exp : exp) ~(form : string) : ranked =
  invalid_arg
    (Printf.sprintf "Latex: misplaced %s at %s" form
       (Common.Location.to_string exp.at))

and formula_of_exp (exp : exp) : Formula.t = (ranked_of_exp exp).formula

and formula_of_slice (low : exp) (high : exp) : Formula.t =
  Infix
    {
      left = formula_of_exp low;
      op = Symbol Colon;
      right = formula_of_exp high;
    }

(* Put the label and the items of its value in one sequence so a field like
   [FIELD x y] prints without parentheses around [x y]. *)
and formula_of_field ((atom, exp) : atom * exp) : Formula.t =
  let values = match exp.it with SeqE exps -> exps | _ -> [ exp ] in
  let label = atomic (formula_of_atom atom.it) in
  (sequence (label :: List.map ranked_of_exp values)).formula

and formula_of_path (path : path) : Formula.t =
  match path.it with
  | RootP -> Concat []
  | IdxP (path, index) ->
      Concat [ formula_of_path path; Delimited (Bracket, formula_of_exp index) ]
  | SliceP (path, low, high) ->
      Concat
        [ formula_of_path path; Delimited (Bracket, formula_of_slice low high) ]
  | DotP (path, atom) ->
      Concat [ formula_of_path path; Symbol Dot; formula_of_atom atom.it ]

and formula_of_arg (arg : arg) : Formula.t =
  match arg.it with
  | ExpA exp -> formula_of_exp exp
  | DefA id -> formula_of_name ~role:Function id.it

(* Omit empty argument parentheses because the parser builds the same call
   from [$f] and [$f()]. *)
and formula_of_args (args : arg list) : Formula.t =
  match args with
  | [] -> Concat []
  | args -> Delimited (Paren, Commas (List.map formula_of_arg args))

let rec ranked_of_prem (prem : prem) : ranked =
  match prem.it with
  | VarPr (id, plaintyp) ->
      infix ~rank:Colon ~op:(Symbol Colon)
        (atomic (formula_of_name ~role:Variable id.it))
        (ranked_of_plaintyp plaintyp)
  | RulePr (_, exp) | IfPr exp -> ranked_of_exp exp
  | RuleNotPr (_, exp) -> prefix ~op:(Symbol Neg) (ranked_of_exp exp)
  | ElsePr -> atomic (Word (Roman, "otherwise"))
  | IterPr (prem, iter) ->
      superscript ~rank:Postfix ~sup:(formula_of_iter iter)
        (ranked_of_prem prem)
  | DebugPr exp ->
      sequence [ atomic (Word (Roman, "debug")); ranked_of_exp exp ]

let formula_of_prem (prem : prem) : Formula.t = (ranked_of_prem prem).formula
