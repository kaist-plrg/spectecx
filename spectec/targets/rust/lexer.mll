(* Tokens of the Rust subset of spec/ch02-syntax.tex Figs. 2.1-2.3.

   The subset has no binary operators at all, so `<` is never a comparison and
   `>>` never a shift: both are lexed one character at a time and the parser
   reads them as bracket punctuation.  That is what makes a hand-written
   recursive-descent parser (Parse) straightforward. *)

{
  open Lexing
  open Common.Source

  type token =
    | Ident of string
    | Lifetime of string        (* 'a -> "a", 'static -> "static", '_ -> "_" *)
    | Num of Bigint.t * string option
    | Directive of string       (* //@ ... : the crate label, see GRAMMAR.md *)
    | KwFn | KwStruct | KwEnum | KwTrait | KwImpl | KwType | KwConst
    | KwLet | KwLoop | KwReturn | KwAsync | KwAwait | KwMove | KwDyn
    | KwFor | KwWhere | KwPub | KwSelf | KwAs | KwMatch | KwMut
    | KwTrue | KwFalse | KwUse
    | Underscore
    | LParen | RParen | LBrace | RBrace | LBrack | RBrack
    | Lt | Gt | Comma | Semi | Colon | ColonColon | Arrow | FatArrow
    | Eq | Amp | Star | Dot | Question | Plus | Pipe | PipePipe | Bang
    | Eof

  let string_of_token = function
    | Ident s -> "identifier `" ^ s ^ "`"
    | Lifetime s -> "lifetime `'" ^ s ^ "`"
    | Num (n, _) -> "number `" ^ Bigint.to_string n ^ "`"
    | Directive s -> "directive `//@" ^ s ^ "`"
    | KwFn -> "`fn`" | KwStruct -> "`struct`" | KwEnum -> "`enum`"
    | KwTrait -> "`trait`" | KwImpl -> "`impl`" | KwType -> "`type`"
    | KwConst -> "`const`" | KwLet -> "`let`" | KwLoop -> "`loop`"
    | KwReturn -> "`return`" | KwAsync -> "`async`" | KwAwait -> "`await`"
    | KwMove -> "`move`" | KwDyn -> "`dyn`" | KwFor -> "`for`"
    | KwWhere -> "`where`" | KwPub -> "`pub`" | KwSelf -> "`self`"
    | KwAs -> "`as`" | KwMatch -> "`match`" | KwMut -> "`mut`"
    | KwTrue -> "`true`" | KwFalse -> "`false`" | KwUse -> "`use`"
    | Underscore -> "`_`"
    | LParen -> "`(`" | RParen -> "`)`" | LBrace -> "`{`" | RBrace -> "`}`"
    | LBrack -> "`[`" | RBrack -> "`]`"
    | Lt -> "`<`" | Gt -> "`>`" | Comma -> "`,`" | Semi -> "`;`"
    | Colon -> "`:`" | ColonColon -> "`::`" | Arrow -> "`->`"
    | FatArrow -> "`=>`" | Eq -> "`=`" | Amp -> "`&`" | Star -> "`*`"
    | Dot -> "`.`" | Question -> "`?`" | Plus -> "`+`" | Pipe -> "`|`"
    | PipePipe -> "`||`" | Bang -> "`!`"
    | Eof -> "end of file"

  let keyword = function
    | "fn" -> Some KwFn | "struct" -> Some KwStruct | "enum" -> Some KwEnum
    | "trait" -> Some KwTrait | "impl" -> Some KwImpl | "type" -> Some KwType
    | "const" -> Some KwConst | "let" -> Some KwLet | "loop" -> Some KwLoop
    | "return" -> Some KwReturn | "async" -> Some KwAsync
    | "await" -> Some KwAwait | "move" -> Some KwMove | "dyn" -> Some KwDyn
    | "for" -> Some KwFor | "where" -> Some KwWhere | "pub" -> Some KwPub
    | "self" -> Some KwSelf | "as" -> Some KwAs | "match" -> Some KwMatch
    | "mut" -> Some KwMut | "true" -> Some KwTrue | "false" -> Some KwFalse
    | "use" -> Some KwUse
    | _ -> None

  let pos p = { file = p.pos_fname; line = p.pos_lnum;
                column = p.pos_cnum - p.pos_bol }
  let region lb = { left = pos (lexeme_start_p lb); right = pos (lexeme_end_p lb) }

  (* `0u8` splits into the digits and the suffix; a bare `0` has none. *)
  let split_num (s : string) : Bigint.t * string option =
    let n = String.length s in
    let i = ref 0 in
    while !i < n && s.[!i] >= '0' && s.[!i] <= '9' do incr i done;
    let digits = String.sub s 0 !i in
    let suffix = String.sub s !i (n - !i) in
    (Bigint.of_string digits, if suffix = "" then None else Some suffix)
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let idchar = alpha | digit | '_'
let ident = ('_' idchar+ | alpha idchar*)

rule token = parse
| [' ' '\t' '\r']+            { token lexbuf }
| '\n'                        { new_line lexbuf; token lexbuf }
| "//@" ([^ '\n']* as s)      { Directive s }
| "//" [^ '\n']*              { token lexbuf }
| digit+ (alpha idchar*)? as s { let n, suf = split_num s in Num (n, suf) }
| '\'' (ident as s)           { Lifetime s }
| "'_"                        { Lifetime "_" }
| "_"                         { Underscore }
| ident as s                  { match keyword s with Some t -> t | None -> Ident s }
| "::"                        { ColonColon }
| "->"                        { Arrow }
| "=>"                        { FatArrow }
| "||"                        { PipePipe }
| '('                         { LParen }
| ')'                         { RParen }
| '{'                         { LBrace }
| '}'                         { RBrace }
| '['                         { LBrack }
| ']'                         { RBrack }
| '<'                         { Lt }
| '>'                         { Gt }
| ','                         { Comma }
| ';'                         { Semi }
| ':'                         { Colon }
| '='                         { Eq }
| '&'                         { Amp }
| '*'                         { Star }
| '.'                         { Dot }
| '?'                         { Question }
| '+'                         { Plus }
| '|'                         { Pipe }
| '!'                         { Bang }
| eof                         { Eof }
| _ as c                      { Error.error (region lexbuf)
                                  (Printf.sprintf "lexical error: %c" c) }

{
  (* The parser consumes a list, so it can look ahead by more than one token
     (a turbofish `::<`, an `A::B` path, a `where` clause's end). *)
  let tokenize ~(filename : string) (source : string) : (token * region) list =
    let lb = from_string source in
    set_filename lb filename;
    let rec loop acc =
      let at = (let t = token lb in (t, region lb)) in
      match at with
      | (Eof, _) -> List.rev (at :: acc)
      | _ -> loop (at :: acc)
    in
    loop []
}
