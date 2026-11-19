(* Does the core-calculus grammar (syntax.spectec) derive the Rust listings it
   was written for, and in exactly one way? This parses each listing with the
   parser that Parsegen.Earley builds from the grammar alone, and checks the
   outcome against goals.txt. The Rust lexer and the lexical facts the
   grammar leaves out are in spectec/targets/rust_core_calculus. *)

module Earley = Parsegen.Earley
module Rust = Targets_rust_core_calculus
module Lexer = Rust.Lexer

let grammar_of = Rust.Parse.grammar_of_spec
let read_file = Rust.Parse.read_file
let parse = Rust.Parse.parse_file

(* Running *)

let position (tokens : Lexer.token list) at =
  match List.nth_opt tokens at with
  | Some tok -> Printf.sprintf "%d:%d" tok.line tok.col
  | None -> "end"

let verdict tokens = function
  | Earley.Accepted { trees = 1; _ } -> "accept unique"
  | Earley.Accepted { trees; _ } -> Printf.sprintf "accept (%d parses)" trees
  | Earley.Rejected { at; _ } -> "reject " ^ position tokens at

(* goals.txt *)

type goal = { file : string; expect : string }

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
           | file :: (_ :: _ as expect) ->
               Some { file; expect = String.concat " " expect }
           | _ -> failwith ("bad goal line: " ^ line))

(* "accept" and "reject" alone also take any parse count or stop position. *)
let meets goal got =
  let starts_with prefix =
    String.length got >= String.length prefix
    && String.sub got 0 (String.length prefix) = prefix
  in
  goal.expect = got
  || (goal.expect = "accept" || goal.expect = "reject")
     && starts_with goal.expect

let goals_command dir =
  let grammar = grammar_of (Filename.concat dir "syntax.spectec") in
  let goals = read_goals dir in
  let met =
    List.fold_left
      (fun met goal ->
        let tokens, outcome = parse grammar (Filename.concat dir goal.file) in
        let got = verdict tokens outcome in
        let ok = meets goal got in
        Printf.printf "%s  %-40s %s%s\n"
          (if ok then "PASS" else "FAIL")
          goal.file goal.expect
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
  let grammar = grammar_of (Filename.concat dir "syntax.spectec") in
  List.iter
    (fun goal ->
      print_endline goal.file;
      report_file ~show_value:false grammar (Filename.concat dir goal.file))
    (read_goals dir)

let parse_command spec show_value files =
  let grammar = grammar_of spec in
  List.iter
    (fun file ->
      print_endline file;
      report_file ~show_value grammar file)
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
           and show_value =
             flag "--value" no_arg ~doc:" print the parsed IL value"
           and files = anon (sequence ("FILE" %: string)) in
           fun () -> parse_command spec show_value files) );
    ]

let () = Command_unix.run command
