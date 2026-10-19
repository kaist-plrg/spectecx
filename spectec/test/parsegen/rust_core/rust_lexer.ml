(* A lexer for the Rust subset in the core-calculus listings. It only has to
   cut the source into tokens: which tokens the grammar accepts is the grammar's
   business, so anything it does not recognize becomes an [Unknown] token
   rather than an error.

   Two choices follow rustc's parser rather than its lexer, and the core
   calculus BNF relies on both (syntax-review, "token splitting"):
   - Punctuation is one character per token, except [::] and [->]. So [&&0] is
     two [&], [>>] closing two generic lists is two [>], and [||] is an empty
     closure parameter list.
   - A macro invocation's delimited argument, [!( .. )], is one [Tree] token
     holding its source text, like the BNF's opaque token_tree. *)

type kind =
  | Ident of string
  | Lifetime of string  (** ['a] as ["a"]; ['static] and ['_] too. *)
  | Int of int
  | Str of string  (** The contents, escapes left as written. *)
  | Punct of string
  | Tree of string  (** A macro argument, delimiters included. *)
  | Unknown of string

type token = { kind : kind; line : int; col : int }

let string_of_kind = function
  | Ident s -> s
  | Lifetime s -> "'" ^ s
  | Int n -> string_of_int n
  | Str s -> "\"" ^ s ^ "\""
  | Punct s -> s
  | Tree s -> s
  | Unknown s -> s

let is_ident_start c =
  c = '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let is_ident_char c = is_ident_start c || (c >= '0' && c <= '9')
let is_digit c = c >= '0' && c <= '9'
let opener = function '(' | '[' | '{' -> true | _ -> false
let closer = function ')' | ']' | '}' -> true | _ -> false

let tokenize (src : string) : token list =
  let n = String.length src in
  let line = ref 1 and line_start = ref 0 in
  let tokens = ref [] in
  let emit kind start =
    tokens := { kind; line = !line; col = start - !line_start + 1 } :: !tokens
  in
  let newline i =
    incr line;
    line_start := i + 1
  in
  let rec span pred i =
    if i < n && pred src.[i] then span pred (i + 1) else i
  in
  (* The previous two tokens are a path's last segment and [!]: a macro. *)
  let after_macro_bang () =
    match !tokens with
    | { kind = Punct "!"; _ } :: { kind = Ident _; _ } :: _ -> true
    | _ -> false
  in
  let rec skip_tree i depth =
    if i >= n then i
    else
      let c = src.[i] in
      if c = '\n' then newline i;
      if c = '"' then skip_tree (skip_string (i + 1)) depth
      else if opener c then skip_tree (i + 1) (depth + 1)
      else if closer c then
        if depth = 1 then i + 1 else skip_tree (i + 1) (depth - 1)
      else skip_tree (i + 1) depth
  and skip_string i =
    if i >= n then i
    else
      match src.[i] with
      | '"' -> i + 1
      | '\\' -> skip_string (i + 2)
      | '\n' ->
          newline i;
          skip_string (i + 1)
      | _ -> skip_string (i + 1)
  in
  let rec go i =
    if i >= n then ()
    else
      let c = src.[i] in
      if c = '\n' then (
        newline i;
        go (i + 1))
      else if c = ' ' || c = '\t' || c = '\r' then go (i + 1)
      else if c = '/' && i + 1 < n && src.[i + 1] = '/' then
        go (span (fun c -> c <> '\n') i)
      else if opener c && after_macro_bang () then (
        let start_line = !line and start_col = i - !line_start + 1 in
        let stop = skip_tree (i + 1) 1 in
        tokens :=
          {
            kind = Tree (String.sub src i (stop - i));
            line = start_line;
            col = start_col;
          }
          :: !tokens;
        go stop)
      else if is_ident_start c then (
        let stop = span is_ident_char i in
        emit (Ident (String.sub src i (stop - i))) i;
        go stop)
      else if is_digit c then (
        let stop = span is_digit i in
        emit (Int (int_of_string (String.sub src i (stop - i)))) i;
        go stop)
      else if c = '"' then (
        let start_line = !line and start_col = i - !line_start + 1 in
        let stop = skip_string (i + 1) in
        tokens :=
          {
            kind = Str (String.sub src (i + 1) (stop - i - 2));
            line = start_line;
            col = start_col;
          }
          :: !tokens;
        go stop)
      else if c = '\'' && i + 1 < n && is_ident_start src.[i + 1] then
        let stop = span is_ident_char (i + 1) in
        if stop < n && src.[stop] = '\'' then (
          (* A character literal such as 'a': not in the core calculus. *)
          emit (Unknown (String.sub src i (stop + 1 - i))) i;
          go (stop + 1))
        else (
          emit (Lifetime (String.sub src (i + 1) (stop - i - 1))) i;
          go stop)
      else
        let two = if i + 1 < n then String.sub src i 2 else "" in
        if two = "::" || two = "->" then (
          emit (Punct two) i;
          go (i + 2))
        else (
          emit (Punct (String.make 1 c)) i;
          go (i + 1))
  in
  go 0;
  List.rev !tokens
