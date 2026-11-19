(* Parsing a Rust file with the parser Parsegen.Earley builds from the
   core-calculus grammar. *)

open Spectec
module Earley = Parsegen.Earley

let load_il spec : Lang.Il.spec =
  match parse_spec_files [ spec ] with
  | Ok el -> (
      match elaborate el with
      | Ok il -> il
      | Error _ -> failwith ("could not elaborate " ^ spec))
  | Error _ -> failwith ("could not parse " ^ spec)

(* The grammar of [spec] (a syntax.spectec), from its start syntax [program]. *)
let grammar_of_spec spec = Grammar.extract ~start:"program" (load_il spec)

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

(* The tokens of the Rust source [src], and the outcome of parsing them as a
   [program]. *)
let parse_source grammar src : Lexer.token list * Earley.outcome =
  let tokens = Lexer.tokenize src in
  (tokens, Earley.run Lexicon.lexicon grammar "program" tokens)

(* The tokens of the Rust file at [path], and the outcome of parsing them as a
   [program]. The tokens give the source positions of the outcome's token
   indices. *)
let parse_file grammar path : Lexer.token list * Earley.outcome =
  parse_source grammar (read_file path)
