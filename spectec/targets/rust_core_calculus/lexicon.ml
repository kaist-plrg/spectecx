(* The facts about Rust's concrete syntax that the core-calculus grammar
   (syntax.spectec) leaves out, because SpecTec EL has no notation for them:
   which Rust words are atoms, which tokens are lifetimes or strings, which
   lists are comma-separated, and how '()' is tokenized. *)

module Atom = Lang.Xl.Atom
module Value = Lang.Il.Value
module Terminal = Grammar.Terminal
module Earley = Parsegen.Earley

(* Rust words that are keyword atoms in the grammar. Other Rust keywords (if,
   return, unsafe, ...) have no production, so they classify to [None]. *)
let keywords =
  [
    ("as", "AS");
    ("dyn", "DYN");
    ("fn", "FN");
    ("for", "FOR");
    ("impl", "IMPL");
    ("let", "LET");
    ("loop", "LOOP");
    ("move", "MOVE");
    ("mut", "MUT");
    ("self", "SELF");
    ("Self", "Self");
    ("struct", "STRUCT");
    ("trait", "TRAIT");
    ("type", "TYPE");
    ("use", "USE");
    ("where", "WHERE");
    (* The closure traits are atoms too (fnkind), distinct from paths. *)
    ("Fn", "Fn");
    ("FnMut", "FnMut");
    ("FnOnce", "FnOnce");
    (* Sized is an atom so that a bound can relax exactly it (?Sized). *)
    ("Sized", "Sized");
    (* The item keyword static; STATIC is already the lifetime 'static. *)
    ("static", "STATICKW");
  ]

let other_rust_keywords =
  [
    "async";
    "await";
    "break";
    "const";
    "continue";
    "crate";
    "else";
    "enum";
    "extern";
    "false";
    "if";
    "in";
    "match";
    "mod";
    "pub";
    "ref";
    "return";
    "super";
    "true";
    "unsafe";
    "while";
  ]

let classify (tok : Lexer.token) : Terminal.t option =
  let atom a = Some (Terminal.Atom a) in
  let text s = Some (Terminal.Primitive (Grammar.Text, Value.text s)) in
  match tok.kind with
  | Lexer.Ident "_" -> atom (Atom.operator "_")
  | Lexer.Ident s -> (
      match List.assoc_opt s keywords with
      | Some k -> atom (Atom.keyword k)
      | None -> if List.mem s other_rust_keywords then None else text s)
  (* 'static and '_ are atoms; any other lifetime is text of class LT. *)
  | Lexer.Lifetime "static" -> atom (Atom.keyword "STATIC")
  | Lexer.Lifetime "_" -> atom (Atom.keyword "PLACEHOLDER")
  | Lexer.Lifetime s -> text s
  | Lexer.Int n ->
      Some (Terminal.Primitive (Grammar.Num `NatT, Value.nat (Bigint.of_int n)))
  | Lexer.Str s -> text s
  | Lexer.Tree s -> text s
  | Lexer.Punct "(" -> atom Atom.LParen
  | Lexer.Punct ")" -> atom Atom.RParen
  | Lexer.Punct "[" -> atom Atom.LBrack
  | Lexer.Punct "]" -> atom Atom.RBrack
  | Lexer.Punct "{" -> atom Atom.LBrace
  | Lexer.Punct "}" -> atom Atom.RBrace
  | Lexer.Punct "<" -> atom Atom.LAngle
  | Lexer.Punct ">" -> atom Atom.RAngle
  | Lexer.Punct p -> atom (Atom.operator p)
  | Lexer.Unknown _ -> None

(* lname ::= _LT id and expr ::= _STR text: both read text, which a plain
   identifier also is. The class keeps 'a and "a" out of identifier
   positions, and identifiers out of theirs. *)
let lexical_tag (tok : Lexer.token) : string option =
  match tok.kind with
  | Lexer.Lifetime ("static" | "_") -> None
  | Lexer.Lifetime _ -> Some "LT"
  | Lexer.Str _ -> Some "STR"
  | _ -> None

(* The BNF's comma lists (overbar) became plain x* in EL. These are their
   element syntaxes; every other x* (item*, stmt*, pathseg*, ...) has no
   separator. *)
let comma_separated =
  [ "gparam"; "lname"; "pred"; "garg"; "typ"; "param"; "cparam"; "expr"; "pat" ]

let lexicon : Lexer.token Earley.lexicon =
  {
    classify;
    lexical_tag;
    lexical_tags = [ "LT"; "STR" ];
    separator =
      (fun name ->
        if List.mem name comma_separated then Some (Atom.operator ",") else None);
    (* The unit type and value '()' is two tokens, as in rustc. *)
    spell =
      (fun atom ->
        if Atom.is_operator atom "()" then [ Atom.LParen; Atom.RParen ]
        else [ atom ]);
  }
