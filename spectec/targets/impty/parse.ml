(** Recursive-descent parser for impty.

    Consumes the token stream from {!Lexer} and emits an IL [value] tree shaped
    to match the surface syntax in [specs/impty/*/spec.spectec]. Precedence is
    encoded by the call structure of [parse_expr] and friends. *)

open Common.Source
open Lang.Il
open Lang.Il.Value
open Lang.Il.Case

(* ----------------------------------------------------------------- *)
(* Token cursor                                                       *)
(* ----------------------------------------------------------------- *)

(* The cursor never advances past Eof; the trailing Eof token gives a stable
   region for error reporting at end of file. *)
module Cursor = struct
  type t = {
    mutable tokens : (Lexer.token * region) list;
    mutable last : region;
  }

  let make tokens = { tokens; last = no_region }
  let peek c = match c.tokens with [] -> assert false | (t, _) :: _ -> t
  let region c = match c.tokens with [] -> assert false | (_, at) :: _ -> at
  let peek2 c = match c.tokens with _ :: (t, _) :: _ -> t | _ -> Lexer.Eof

  let advance c =
    match c.tokens with
    | (Lexer.Eof, _) :: _ -> ()
    | (_, at) :: rest ->
        c.last <- at;
        c.tokens <- rest
    | [] -> assert false

  let advance_at c =
    let at = region c in
    advance c;
    at

  let since c left = { left = left.left; right = c.last.right }

  let expect_at c expected =
    let tok = peek c in
    if tok = expected then advance_at c
    else
      Error.error (region c)
        (Printf.sprintf "expected %s, got %s"
           (Lexer.string_of_token expected)
           (Lexer.string_of_token tok))

  let expect c expected = ignore (expect_at c expected)

  let expect_ident c =
    match peek c with
    | Lexer.Ident s ->
        let at = region c in
        advance c;
        s $ at
    | tok ->
        Error.error (region c)
          (Printf.sprintf "expected identifier, got %s"
             (Lexer.string_of_token tok))
end

(* ----------------------------------------------------------------- *)
(* Combinators                                                        *)
(* ----------------------------------------------------------------- *)

let surrounded ~left ~right p c =
  Cursor.expect c left;
  let v = p c in
  Cursor.expect c right;
  v

let surrounded_at ~left ~right p c =
  let left_at = Cursor.expect_at c left in
  let v = p c in
  let right_at = Cursor.expect_at c right in
  (left_at, v, right_at)

(* [left_assoc ~op_token ~op_atom ~var ~next c] left-folds [next] over
   repeated [op_token]s, building [next op next op ... op next] as a chain
   of binary case_v values under [var]. *)
let left_assoc ~op_token ~op_atom ~var ~next c =
  let start = Cursor.region c in
  let lhs = next c in
  let rec loop acc =
    if Cursor.peek c = op_token then
      let op_at = Cursor.advance_at c in
      let rhs = next c in
      loop
        ([ arg acc; op ~at:op_at op_atom; arg rhs ]
        |> case_v ~var <<<| Cursor.since c start)
    else acc
  in
  loop lhs

(* ----------------------------------------------------------------- *)
(* IL value constructors                                              *)
(* ----------------------------------------------------------------- *)

(* Tags such as [_ID] have no source token, so they carry no region. *)
let v_id (x : string phrase) : value =
  [ tag "ID"; arg (text x.it <<<| x.at) ] |> case_v ~var:"id" <<<| x.at

let v_lit_num (at : region) (n : int) : value =
  [ tag "NUM"; arg (int (Bigint.of_int n) <<<| at) ]
  |> case_v ~var:"literal" <<<| at

let v_lit_bool (at : region) (b : bool) : value =
  [ tag "BOOL"; arg (bool b <<<| at) ] |> case_v ~var:"literal" <<<| at

(* ----------------------------------------------------------------- *)
(* Types                                                              *)
(* ----------------------------------------------------------------- *)

(* Function-type arrow is right-associative: `int -> int -> int` parses as
   `int -> (int -> int)`. *)
let rec parse_type c : value =
  let start = Cursor.region c in
  let lhs = parse_type_atom c in
  if Cursor.peek c = Lexer.Arrow then
    let arrow_at = Cursor.advance_at c in
    let rhs = parse_type c in
    [ arg lhs; op ~at:arrow_at "->"; arg rhs ]
    |> case_v ~var:"type" <<<| Cursor.since c start
  else lhs

and parse_type_atom c : value =
  let at = Cursor.region c in
  match Cursor.peek c with
  | Lexer.KwInt ->
      Cursor.advance c;
      [ kw ~at "INT" ] |> case_v ~var:"type" <<<| at
  | Lexer.KwBool ->
      Cursor.advance c;
      [ kw ~at "BOOL" ] |> case_v ~var:"type" <<<| at
  | Lexer.Lparen ->
      surrounded ~left:Lexer.Lparen ~right:Lexer.Rparen parse_type c
  | tok ->
      Error.error (Cursor.region c)
        (Printf.sprintf "expected type, got %s" (Lexer.string_of_token tok))

(* ----------------------------------------------------------------- *)
(* Expressions                                                        *)
(* ----------------------------------------------------------------- *)

(* Precedence (loose to tight): ?: , && , <= , + , unary ! , postfix call,
   primary. Binary operators fold left; the ternary is right-associative. *)

let rec parse_expr c : value = parse_ternary c

and parse_ternary c : value =
  let start = Cursor.region c in
  let cond = parse_and c in
  if Cursor.peek c = Lexer.Question then
    let question_at = Cursor.advance_at c in
    let e_then = parse_expr c in
    let colon_at = Cursor.expect_at c Lexer.Colon in
    let e_else = parse_ternary c in
    [
      arg cond;
      op ~at:question_at "?";
      arg e_then;
      op ~at:colon_at ":";
      arg e_else;
    ]
    |> case_v ~var:"expr" <<<| Cursor.since c start
  else cond

and parse_and c =
  left_assoc ~op_token:Lexer.And ~op_atom:"&&" ~var:"expr" ~next:parse_leq c

and parse_leq c =
  left_assoc ~op_token:Lexer.Leq ~op_atom:"<=" ~var:"expr" ~next:parse_add c

and parse_add c =
  left_assoc ~op_token:Lexer.Plus ~op_atom:"+" ~var:"expr" ~next:parse_unary c

and parse_unary c : value =
  if Cursor.peek c = Lexer.Bang then (
    let at = Cursor.region c in
    Cursor.advance c;
    let e = parse_unary c in
    [ op ~at "!"; arg e ] |> case_v ~var:"expr" <<<| Cursor.since c at)
  else parse_postfix c

(* A primary may be followed by zero or more `(expr)` applications. *)
and parse_postfix c : value =
  let start = Cursor.region c in
  let base = parse_primary c in
  let rec loop acc =
    if Cursor.peek c = Lexer.Lparen then
      let lparen_at, arg_e, rparen_at =
        surrounded_at ~left:Lexer.Lparen ~right:Lexer.Rparen parse_expr c
      in
      loop
        ([
           arg acc; lparen ~at:lparen_at (); arg arg_e; rparen ~at:rparen_at ();
         ]
        |> case_v ~var:"expr" <<<| Cursor.since c start)
    else acc
  in
  loop base

and parse_primary c : value =
  let at = Cursor.region c in
  match Cursor.peek c with
  | Lexer.Num n ->
      Cursor.advance c;
      v_lit_num at n
  | Lexer.KwTrue ->
      Cursor.advance c;
      v_lit_bool at true
  | Lexer.KwFalse ->
      Cursor.advance c;
      v_lit_bool at false
  | Lexer.Ident s ->
      Cursor.advance c;
      v_id (s $ at)
  | Lexer.Lparen ->
      surrounded ~left:Lexer.Lparen ~right:Lexer.Rparen parse_expr c
  | Lexer.KwFun ->
      Cursor.advance c;
      let lparen_at, (t_arg, id_str), rparen_at =
        surrounded_at ~left:Lexer.Lparen ~right:Lexer.Rparen
          (fun c ->
            let t = parse_type c in
            (t, Cursor.expect_ident c))
          c
      in
      let arrow_at = Cursor.expect_at c Lexer.Arrow in
      let t_ret = parse_type c in
      let lbrace_at, body, rbrace_at =
        surrounded_at ~left:Lexer.Lbrace ~right:Lexer.Rbrace parse_expr c
      in
      [
        kw ~at "FUN";
        lparen ~at:lparen_at ();
        arg t_arg;
        arg (v_id id_str);
        rparen ~at:rparen_at ();
        op ~at:arrow_at "->";
        arg t_ret;
        lbrace ~at:lbrace_at ();
        arg body;
        rbrace ~at:rbrace_at ();
      ]
      |> case_v ~var:"expr" <<<| Cursor.since c at
  | tok ->
      Error.error (Cursor.region c)
        (Printf.sprintf "expected expression, got %s"
           (Lexer.string_of_token tok))

(* ----------------------------------------------------------------- *)
(* Statements                                                         *)
(* ----------------------------------------------------------------- *)

(* Sequencing is left-associative via `;`. *)
and parse_stmt c =
  left_assoc ~op_token:Lexer.Semi ~op_atom:";" ~var:"command"
    ~next:parse_stmt_atom c

and parse_stmt_atom c : value =
  let at = Cursor.region c in
  match Cursor.peek c with
  | Lexer.KwSkip ->
      Cursor.advance c;
      [ kw ~at "SKIP" ] |> case_v ~var:"command" <<<| at
  | Lexer.KwIf ->
      Cursor.advance c;
      let cond = parse_expr c in
      let then_at = Cursor.expect_at c Lexer.KwThen in
      let s1 = parse_stmt c in
      let else_at = Cursor.expect_at c Lexer.KwElse in
      let s2 = parse_stmt c in
      let end_at = Cursor.expect_at c Lexer.KwEnd in
      [
        kw ~at "IF";
        arg cond;
        kw ~at:then_at "THEN";
        arg s1;
        kw ~at:else_at "ELSE";
        arg s2;
        kw ~at:end_at "END";
      ]
      |> case_v ~var:"command" <<<| Cursor.since c at
  | Lexer.KwWhile ->
      Cursor.advance c;
      let cond = parse_expr c in
      let do_at = Cursor.expect_at c Lexer.KwDo in
      let body = parse_stmt c in
      let end_at = Cursor.expect_at c Lexer.KwEnd in
      [
        kw ~at "WHILE";
        arg cond;
        kw ~at:do_at "DO";
        arg body;
        kw ~at:end_at "END";
      ]
      |> case_v ~var:"command" <<<| Cursor.since c at
  | Lexer.KwRec ->
      Cursor.advance c;
      let fname = Cursor.expect_ident c in
      let lparen_at, (t_arg, x_str), rparen_at =
        surrounded_at ~left:Lexer.Lparen ~right:Lexer.Rparen
          (fun c ->
            let t = parse_type c in
            (t, Cursor.expect_ident c))
          c
      in
      let arrow_at = Cursor.expect_at c Lexer.Arrow in
      let t_ret = parse_type c in
      let lbrace_at, body, rbrace_at =
        surrounded_at ~left:Lexer.Lbrace ~right:Lexer.Rbrace parse_expr c
      in
      [
        kw ~at "REC";
        arg (v_id fname);
        lparen ~at:lparen_at ();
        arg t_arg;
        arg (v_id x_str);
        rparen ~at:rparen_at ();
        op ~at:arrow_at "->";
        arg t_ret;
        lbrace ~at:lbrace_at ();
        arg body;
        rbrace ~at:rbrace_at ();
      ]
      |> case_v ~var:"command" <<<| Cursor.since c at
  (* No command other than a declaration starts with `(`. *)
  | Lexer.KwInt | Lexer.KwBool | Lexer.Lparen -> parse_var_decl c
  | Lexer.Ident _ when Cursor.peek2 c = Lexer.Eq ->
      let id_str = Cursor.expect_ident c in
      let eq_at = Cursor.expect_at c Lexer.Eq in
      let rhs = parse_expr c in
      [ arg (v_id id_str); op ~at:eq_at "="; arg rhs ]
      |> case_v ~var:"command" <<<| Cursor.since c at
  | tok ->
      Error.error (Cursor.region c)
        (Printf.sprintf "expected a command, got %s"
           (Lexer.string_of_token tok))

(* Surface form `T id = e`.  No trailing `;` is consumed: that `;` is the
   stmt-sequence separator parsed by [parse_stmt]. *)
and parse_var_decl c : value =
  let at = Cursor.region c in
  let t = parse_type c in
  let id_str = Cursor.expect_ident c in
  let eq_at = Cursor.expect_at c Lexer.Eq in
  let rhs = parse_expr c in
  [ arg t; arg (v_id id_str); op ~at:eq_at "="; arg rhs ]
  |> case_v ~var:"command" <<<| Cursor.since c at

(* ----------------------------------------------------------------- *)
(* Entry points                                                       *)
(* ----------------------------------------------------------------- *)

(* `prog = stmt`, so a program is just a stmt — no enclosing block. *)
let parse_prog c : value = parse_stmt c

(* Requires every token to be consumed. Raises [Error.ImptyParseError] on a parse
   failure or a trailing token. *)
let parse_to_eof ~filename (parse : Cursor.t -> value) (source : string) : value
    =
  let cursor = Cursor.make (Lexer.tokenize ~filename source) in
  let result = parse cursor in
  (match Cursor.peek cursor with
  | Lexer.Eof -> ()
  | tok ->
      Error.error (Cursor.region cursor)
        (Printf.sprintf "unexpected trailing token %s"
           (Lexer.string_of_token tok)));
  result

let parse_string_exn ~filename source = parse_to_eof ~filename parse_prog source

(* An expression on its own is not a valid program, so it needs its own
   entrypoint. *)
let parse_expr_exn ~filename source = parse_to_eof ~filename parse_expr source

let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  Bytes.unsafe_to_string buf

(* ----------------------------------------------------------------- *)
(* Public facade — exception → result mapping for Spectec.Task.S      *)
(* ----------------------------------------------------------------- *)

let parse_file filename =
  try Ok (parse_string_exn ~filename (read_file filename))
  with Error.ImptyParseError (at, msg) ->
    Error (Spectec.Error.TaskParseError (at, msg))

let parse_string ~spec:_ ~filename content =
  try Ok [ parse_string_exn ~filename content ]
  with Error.ImptyParseError (at, msg) ->
    Error (Spectec.Error.TaskParseError (at, msg))
