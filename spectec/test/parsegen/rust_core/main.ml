(* Does the core-calculus grammar (syntax.spectec) derive the Rust listings it
   was written for? This parses each listing with the parser that
   Parsegen.Earley builds from the grammar alone, and checks the outcome
   against goals.txt.

   The grammar is used in two forms (see goals.txt): [as-is], and [macros],
   where expr also admits esrc's macro case. Everything below that is not in
   the grammar is lexical, and is listed in [lexicon]. *)

open Spectec
module Atom = Lang.Xl.Atom
module Value = Lang.Il.Value
module Terminal = Grammar.Terminal
module Earley = Parsegen.Earley
module Lexer = Rust_lexer

(* Grammar *)

let load_il spec : Lang.Il.spec =
  match parse_spec_files [ spec ] with
  | Ok el -> (
      match elaborate el with
      | Ok il -> il
      | Error _ -> failwith ("could not elaborate " ^ spec))
  | Error _ -> failwith ("could not parse " ^ spec)

let find_syntax name (grammar : Grammar.t) : Grammar.syntax =
  List.find (fun (s : Grammar.syntax) -> s.name.it = name) grammar

(* esrc ::= expr | path '!' tokentree is unreachable from program. Like
   bnf_check.py --macros, add its macro case to expr, so a macro may stand
   wherever an expression may. *)
let with_macros (il : Lang.Il.spec) (grammar : Grammar.t) : Grammar.t =
  let esrc_grammar = Grammar.extract ~start:"esrc" il in
  let is_macro (prod : Grammar.production) =
    List.exists
      (function
        | Lang.Il.Mixfix.Atom atom -> Atom.to_string atom.it = "!" | _ -> false)
      prod.notation
  in
  let macro =
    List.find is_macro (find_syntax "esrc" esrc_grammar).productions
  in
  find_syntax "tokentree" esrc_grammar
  :: List.map
       (fun (s : Grammar.syntax) ->
         if s.name.it = "expr" then
           { s with productions = s.productions @ [ macro ] }
         else s)
       grammar

type variant = As_is | Macros

let string_of_variant = function As_is -> "as-is" | Macros -> "macros"

let variant_of_string = function
  | "as-is" -> As_is
  | "macros" -> Macros
  | s -> failwith ("unknown grammar variant " ^ s)

let grammar_of il = function
  | As_is -> Grammar.extract ~start:"program" il
  | Macros -> with_macros il (Grammar.extract ~start:"program" il)

(* Lexicon: the facts about Rust's concrete syntax that the grammar leaves
   out. *)

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
    "static";
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
  [ "gparam"; "lname"; "pred"; "garg"; "typ"; "param"; "expr"; "pat" ]

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

(* Running *)

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let parse grammar path =
  let tokens = Lexer.tokenize (read_file path) in
  (tokens, Earley.run lexicon grammar "program" tokens)

let position (tokens : Lexer.token list) at =
  match List.nth_opt tokens at with
  | Some tok -> Printf.sprintf "%d:%d" tok.line tok.col
  | None -> "end"

let verdict tokens = function
  | Earley.Accepted _ -> "accept"
  | Earley.Rejected { at; _ } -> "reject " ^ position tokens at

(* goals.txt *)

type goal = { file : string; variant : variant; expect : string }

let read_goals dir : goal list =
  read_file (Filename.concat dir "goals.txt")
  |> String.split_on_char '\n'
  |> List.filter_map (fun line ->
         let line = String.trim line in
         if line = "" || line.[0] = '#' then None
         else
           match
             String.split_on_char ' ' line |> List.filter (fun w -> w <> "")
           with
           | file :: variant :: expect ->
               Some
                 {
                   file;
                   variant = variant_of_string variant;
                   expect = String.concat " " expect;
                 }
           | _ -> failwith ("bad goal line: " ^ line))

(* "reject" alone accepts a rejection anywhere. *)
let meets goal got =
  goal.expect = got
  || goal.expect = "reject"
     && String.length got > 6
     && String.sub got 0 6 = "reject"

let with_grammars spec f =
  let il = load_il spec in
  let grammars = List.map (fun v -> (v, grammar_of il v)) [ As_is; Macros ] in
  f (fun variant -> List.assoc variant grammars)

let goals_command dir =
  with_grammars (Filename.concat dir "syntax.spectec") @@ fun grammar ->
  let goals = read_goals dir in
  let met =
    List.fold_left
      (fun met goal ->
        let tokens, outcome =
          parse (grammar goal.variant) (Filename.concat dir goal.file)
        in
        let got = verdict tokens outcome in
        let ok = meets goal got in
        Printf.printf "%s  %-40s %-7s %s%s\n"
          (if ok then "PASS" else "FAIL")
          goal.file
          (string_of_variant goal.variant)
          goal.expect
          (if ok then "" else "   (got: " ^ got ^ ")");
        if ok then met + 1 else met)
      0 goals
  in
  Printf.printf "\n%d/%d goals met\n" met (List.length goals)

(* The tokens from [first] to [last], as source-like text. *)
let excerpt (tokens : Lexer.token list) first last =
  let text =
    List.filteri (fun i _ -> i >= first && i <= last) tokens
    |> List.map (fun (t : Lexer.token) -> Lexer.string_of_kind t.kind)
    |> String.concat " "
  in
  if String.length text > 60 then String.sub text 0 57 ^ "..." else text

let report_file ~show_value grammar path =
  let tokens, outcome = parse grammar path in
  (match outcome with
  | Earley.Accepted { value; trees; ambiguities } ->
      Printf.printf "  accept, %s\n"
        (if trees = 1 then "1 parse"
         else if trees = max_int then "too many parses to count"
         else Printf.sprintf "%d parses" trees);
      List.iter
        (fun (a : Earley.ambiguity) ->
          Printf.printf "    ambiguous %s at %s-%s: %s\n" a.syntax
            (position tokens a.first) (position tokens a.last)
            (excerpt tokens a.first a.last);
          List.iter (Printf.printf "      | %s\n") a.readings)
        ambiguities;
      if show_value then
        Printf.printf "    value: %s\n"
          (Lang.Il.Print_debug.string_of_value value)
  | Earley.Rejected { at; expected } ->
      let found =
        match List.nth_opt tokens at with
        | Some tok -> "`" ^ Lexer.string_of_kind tok.kind ^ "`"
        | None -> "end of input"
      in
      Printf.printf "  reject at %s: found %s, expected one of: %s\n"
        (position tokens at) found
        (String.concat " " expected));
  ()

let report_command dir =
  with_grammars (Filename.concat dir "syntax.spectec") @@ fun grammar ->
  List.iter
    (fun goal ->
      Printf.printf "%s [%s]\n" goal.file (string_of_variant goal.variant);
      report_file ~show_value:false (grammar goal.variant)
        (Filename.concat dir goal.file))
    (read_goals dir)

let parse_command spec variant show_value files =
  with_grammars spec @@ fun grammar ->
  List.iter
    (fun file ->
      Printf.printf "%s [%s]\n" file (string_of_variant variant);
      report_file ~show_value (grammar variant) file)
    files

let command =
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let dir_flag =
    flag "-d" (required string)
      ~doc:"DIR test directory holding syntax.spectec and goals.txt"
  in
  Core.Command.group
    ~summary:
      "parse Rust listings with the parser generated from the core-calculus \
       grammar"
    [
      ( "goals",
        Core.Command.basic ~summary:"check every goal in DIR/goals.txt"
          (let%map dir = dir_flag in
           fun () -> goals_command dir) );
      ( "report",
        Core.Command.basic
          ~summary:
            "show parse counts, ambiguities, and rejections for DIR/goals.txt"
          (let%map dir = dir_flag in
           fun () -> report_command dir) );
      ( "parse",
        Core.Command.basic ~summary:"parse FILEs with the grammar of SPEC"
          (let%map spec = flag "-s" (required string) ~doc:"FILE syntax.spectec"
           and macros =
             flag "--macros" no_arg ~doc:" add esrc's macro case to expr"
           and show_value =
             flag "--value" no_arg ~doc:" print the parsed IL value"
           and files = anon (sequence ("FILE" %: string)) in
           fun () ->
             parse_command spec
               (if macros then Macros else As_is)
               show_value files) );
    ]

let () = Command_unix.run command
