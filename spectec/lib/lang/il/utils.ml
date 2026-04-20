open Types
open Xl.Atom
open Common.Source

(* convert string to atom *)

(* NOTE: ".", ":", and ";" map to TickDot, TickColon, and TickSemicolon
   rather than Dot, Colon, and Semicolon. This is because the helper is
   used by user-facing parsers. *)
let wrap_atom (s : string) : atom =
  match s with
  | "<:" -> Sub $ no_region
  | ":>" -> Sup $ no_region
  | "|-" -> Turnstile $ no_region
  | "-|" -> Tilesturn $ no_region
  | "`" -> Tick $ no_region
  | "\"" -> DoubleQuote $ no_region
  | "_" -> Underscore $ no_region
  | "->" -> Arrow $ no_region
  | "`->" -> TickArrow $ no_region
  | "->_" -> ArrowSub $ no_region
  | "=>" -> DoubleArrow $ no_region
  | "=>_" -> DoubleArrowSub $ no_region
  | "~>" -> SqArrow $ no_region
  | "~>*" -> SqArrowStar $ no_region
  | "." | "`." -> TickDot $ no_region
  | ".." | "`.." -> TickDot2 $ no_region
  | "..." | "`..." -> TickDot3 $ no_region
  | "," -> Comma $ no_region
  | ";" | "`;" -> TickSemicolon $ no_region
  | ":" | "`:" -> TickColon $ no_region
  | "#" -> Hash $ no_region
  | "$" -> Dollar $ no_region
  | "@" -> At $ no_region
  | "?" -> Quest $ no_region
  | "!" -> Bang $ no_region
  | "!=" -> BangEq $ no_region
  | "~" -> Tilde $ no_region
  | "~~" -> Tilde2 $ no_region
  | "<" -> LAngle $ no_region
  | "<<" -> LAngle2 $ no_region
  | "<=" -> LAngleEq $ no_region
  | "<<=" -> LAngle2Eq $ no_region
  | ">" -> RAngle $ no_region
  | ">>" -> RAngle2 $ no_region
  | ">=" -> RAngleEq $ no_region
  | ">>=" -> RAngle2Eq $ no_region
  | "(" -> LParen $ no_region
  | ")" -> RParen $ no_region
  | "[" -> LBrack $ no_region
  | "]" -> RBrack $ no_region
  | "{" -> LBrace $ no_region
  | "{#}" -> LBraceHashRBrace $ no_region
  | "}" -> RBrace $ no_region
  | "``<" -> TickLAngle $ no_region
  | "``>" -> TickRAngle $ no_region
  | "``[" -> TickLBrack $ no_region
  | "``]" -> TickRBrack $ no_region
  | "``{" -> TickLBrace $ no_region
  | "``}" -> TickRBrace $ no_region
  | "+" -> Plus $ no_region
  | "++" -> Plus2 $ no_region
  | "+=" -> PlusEq $ no_region
  | "-" -> Minus $ no_region
  | "-=" -> MinusEq $ no_region
  | "*" -> Star $ no_region
  | "*=" -> StarEq $ no_region
  | "/" -> Slash $ no_region
  | "/=" -> SlashEq $ no_region
  | "\\" -> Backslash $ no_region
  | "%" -> Percent $ no_region
  | "%=" -> PercentEq $ no_region
  | "=" -> Eq $ no_region
  | "==" -> Eq2 $ no_region
  | "&" -> Amp $ no_region
  | "&&" -> Amp2 $ no_region
  | "&&&" -> Amp3 $ no_region
  | "&=" -> AmpEq $ no_region
  | "^" -> Up $ no_region
  | "^=" -> UpEq $ no_region
  | "|" -> Bar $ no_region
  | "||" -> Bar2 $ no_region
  | "|=" -> BarEq $ no_region
  | "|+|" -> SPlus $ no_region
  | "|+|=" -> SPlusEq $ no_region
  | "|-|" -> SMinus $ no_region
  | "|-|=" -> SMinusEq $ no_region
  | s when String.get s 0 = '`' ->
      let s = String.sub s 1 (String.length s - 1) in
      SilentAtom s $ no_region
  | _ -> Atom s $ no_region

(* Construct types with no region *)

let var_t (s : string) : typ' = VarT (s $ no_region, [])
let iter_t (i : iter) (t : typ') : typ' = IterT (t $ no_region, i)

(* convert a symbol list to a CaseV value *)

type symbol = NT of value | Term of string

let case_v (vs : symbol list) : value' =
  let rec build = function
    | [] -> []
    | Term s :: rest -> Xl.Mixop.Atom (wrap_atom s) :: build rest
    | NT _ :: rest -> Xl.Mixop.Arg :: build rest
  in
  let mixop =
    match build vs with [ single ] -> single | parts -> Xl.Mixop.Seq parts
  in
  let values =
    vs |> List.filter_map (function NT v -> Some v | Term _ -> None)
  in
  CaseV (mixop, values)

let ( #@ ) (vs : symbol list) (s : string) : value =
  vs |> case_v |> Value.make_val (var_t s)

let id_of_case_v (v : value) : string =
  match (v.it, v.note.typ) with
  | CaseV _, VarT (id, _) -> id.it
  | _ -> failwith "not a case value"

(* Flatten a CaseV into (typename, atom_names, values) for pattern matching.
   atom_names is the flat list of atom string names in the mixop, ignoring structure. *)

let flatten_case_v (value : value) : string * string list * value list =
  match (value.it, value.note.typ) with
  | CaseV (mixop, values), VarT (id, _) ->
      let atoms =
        Xl.Mixop.atoms mixop |> List.map (fun a -> string_of_atom a.it)
      in
      (id.it, atoms, values)
  | _ -> failwith "Expected a CaseV value"

let flatten_case_v' (value : value) : string * string list * value' list =
  let id, atoms, values = flatten_case_v value in
  (id, atoms, List.map (fun (v : value) -> v.it) values)
