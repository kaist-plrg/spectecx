(* `spectecx rust check`: parse a Rust program with the grammar of the given
   specs, run one relation of those specs on it, and print the verdict.

     rust check -s SPEC... -p FILE.rs [--prelude FILE | --no-prelude]
                [--relation NAME] [--sl] [--det]

   The prelude and FILE are parsed separately, each as one program; the
   checked program is FILE's, inner attributes included, with the prelude's
   items put before FILE's items. The relation takes that program as its only
   input. The last line on stdout is the verdict:

     - a relation with outputs prints all but the last as "output: v" and the
       last as "verdict: v" (Check: |- program => verdict gives ACCEPT or
       REJECT);
     - a relation without outputs prints "verdict: A" when it holds and
       "verdict: NOT_A" when it does not, A being its last atom (ElabForm: |-
       program ELAB gives ELAB or NOT_ELAB).

   Exit code: 0 verdict positive, 1 negative (NOT_A or REJECT), 2 parse reject
   or ambiguity ("parse: reject" / "parse: ambiguous", or "prelude parse: .."
   for the prelude), 3 a spec, interpreter,
   or usage error. Diagnostics go to stderr. *)

open Spectec
module Il = Lang.Il
module Earley = Parsegen.Earley

module Target : Spectec.Target.S = struct
  let name = "rust"

  (* Unused: check takes its specs from -s. *)
  let spec_dir = "06-core-calculus/spectec"
  let builtins = []
  let is_impure_func _ = false
  let is_impure_rel _ = false
  let state_version = ref 0

  let with_state f =
    let next_vid = ref 0 in
    let fresh_vid () =
      let vid = !next_vid in
      incr next_vid;
      vid
    in
    Il.Value.GlobalVidProvider.with_provider fresh_vid f
end

let default_prelude = "06-core-calculus/elab/std-prelude.rs"

let fail fmt =
  Printf.ksprintf
    (fun msg ->
      prerr_endline ("error: " ^ msg);
      exit 3)
    fmt

let print_bag bag =
  if not (Diagnostic.Bag.is_empty bag) then
    prerr_endline
      (Diagnostic.Render.render_bag ~ansi:Diagnostic.Ansi.plain bag)

(* Specs *)

let load_specs specs =
  let result, bag =
    with_diagnostics (fun () -> Result.bind (parse_spec_files specs) elaborate)
  in
  print_bag bag;
  match result with Ok il -> il | Error _ -> exit 3

let find_relation (il : Il.spec) name =
  List.find_map
    (fun (def : Il.def) ->
      match def.it with
      | Il.RelD { relid; reltyp; _ } when relid.it = name -> Some reltyp
      | _ -> None)
    il

(* The relation's last atom in its notation, by which "holds" is printed. *)
let last_atom (reltyp : Il.reltyp) name =
  match List.rev (Il.Mixfix.atoms reltyp.it) with
  | atom :: _ -> Xl.Atom.to_string atom.it
  | [] -> name

(* Values: an atoms-only case value, such as ACCEPT, as its atoms. *)
let string_of_value (value : Il.value) =
  match value.it with
  | Il.CaseV vc when Il.Mixfix.args vc = [] ->
      Il.Mixfix.atoms vc
      |> List.filter_map (fun (atom : Il.Mixfix.atom) ->
             match atom.it with
             | Xl.Atom.Tag _ -> None
             | atom -> Some (Xl.Atom.to_string atom))
      |> String.concat " "
  | _ -> Il.Print_debug.string_of_value value

(* Program *)

let prelude_file ~prelude ~no_prelude =
  if no_prelude then None
  else
    match prelude with
    | Some path ->
        if Sys.file_exists path then Some path
        else fail "prelude %s does not exist" path
    | None ->
        if Sys.file_exists default_prelude then Some default_prelude
        else (
          prerr_endline
            ("warning: no prelude (" ^ default_prelude
           ^ " not found); checking without one");
          None)

(* The value of [file] parsed as one [program], in exactly one way. A failure
   prints "[label]: reject" or "[label]: ambiguous" and exits with 2. *)
let parse_program grammar ~label file =
  if not (Sys.file_exists file) then fail "%s does not exist" file;
  let tokens, outcome =
    try Parse.parse_file grammar file with Failure msg -> fail "parse: %s" msg
  in
  let where at =
    match List.nth_opt tokens at with
    | Some (tok : Lexer.token) -> Printf.sprintf "%s:%d:%d" file tok.line tok.col
    | None -> file ^ ": end of input"
  in
  match outcome with
  | Earley.Accepted { value; trees = 1; _ } -> value
  | Earley.Accepted { trees; ambiguities; _ } ->
      print_endline (label ^ ": ambiguous");
      Printf.eprintf "%s parses\n"
        (if trees = max_int then "too many" else string_of_int trees);
      List.iter
        (fun (a : Earley.ambiguity) ->
          Printf.eprintf "ambiguous %s at %s-%s\n" a.syntax (where a.first)
            (where a.last);
          List.iter (Printf.eprintf "  | %s\n") a.readings)
        ambiguities;
      exit 2
  | Earley.Rejected { at; expected } ->
      print_endline (label ^ ": reject");
      let found =
        match List.nth_opt tokens at with
        | Some tok -> "`" ^ Lexer.string_of_kind tok.kind ^ "`"
        | None -> "end of input"
      in
      Printf.eprintf "reject at %s: found %s, expected one of: %s\n" (where at)
        found
        (String.concat " " expected);
      exit 2

(* A [program] value keeps its items in its last argument, as in
   [_PROGRAM iattr* item*], or is the item list itself when the syntax is a
   plain iteration. *)
let items_of (program : Il.value) =
  match program.it with
  | Il.ListV items -> Some items
  | Il.CaseV vc -> (
      match List.rev (Il.Mixfix.args vc) with
      | { it = Il.ListV items; _ } :: _ -> Some items
      | _ -> None)
  | _ -> None

let with_items (program : Il.value) items =
  match program.it with
  | Il.ListV _ -> Il.Value.Make.list program.note.typ items
  | Il.CaseV vc ->
      let rec replace_last = function
        | [] -> []
        | Il.Mixfix.Arg (list : Il.value) :: rest_rev ->
            Il.Mixfix.Arg (Il.Value.Make.list list.note.typ items) :: rest_rev
        | mixeme :: rest_rev -> mixeme :: replace_last rest_rev
      in
      Il.Value.Make.case program.note.typ (List.rev (replace_last (List.rev vc)))
  | _ -> program

(* The prelude is parsed on its own, not pasted before FILE's text: FILE may
   start with inner attributes (#![..]), which must come first. The checked
   program is FILE's, with the prelude's items put before its own. *)
let load_program il ~prelude ~no_prelude file =
  let prelude = prelude_file ~prelude ~no_prelude in
  let grammar =
    try Grammar.extract ~start:"program" il
    with Failure msg | Invalid_argument msg -> fail "grammar: %s" msg
  in
  let program = parse_program grammar ~label:"parse" file in
  match prelude with
  | None -> program
  | Some path -> (
      let prelude = parse_program grammar ~label:"prelude parse" path in
      match (items_of prelude, items_of program) with
      | Some prelude_items, Some items -> with_items program (prelude_items @ items)
      | _ -> fail "cannot find the item list of a program value to merge")

(* Running *)

type run =
  | Holds of Il.value list
  | Fails of Diagnostic.t  (** no rule applies *)
  | Broken of Diagnostic.t

let run_relation ~sl ~det il name program file =
  if sl then
    match
      Eval_sl.run ~check_determinism:det
        (module Target)
        (structure il) name [ program ] file
    with
    | Ok (_, outputs) -> Holds outputs
    (* SL reports a top-level relation that no rule matches only by this
       message (lib/interp/eval_sl/eval_sl.ml). *)
    | Error ((_, "relation was not matched") as e) ->
        Fails (Eval_sl.error_to_diagnostic e)
    | Error e -> Broken (Eval_sl.error_to_diagnostic e)
  else
    match
      Eval_il.run ~check_determinism:det
        (module Target)
        il name [ program ] file
    with
    | Ok (_, outputs) -> Holds outputs
    | Error (Eval_il.Backtrack _ as e) ->
        Fails (Eval_il.error_to_diagnostic e)
    | Error e -> Broken (Eval_il.error_to_diagnostic e)

let check specs file ~prelude ~no_prelude ~relation ~sl ~det =
  if specs = [] then fail "no spec given (-s SPEC)";
  let il = load_specs specs in
  let reltyp =
    match find_relation il relation with
    | Some reltyp -> reltyp
    | None -> fail "no relation %s in the specs" relation
  in
  let inputs = List.length (Il.Mode.inputs reltyp.it) in
  if inputs <> 1 then
    fail "relation %s takes %d inputs; check needs exactly one (the program)"
      relation inputs;
  let has_outputs = Il.Mode.outputs reltyp.it <> [] in
  let holds = last_atom reltyp relation in
  Target.with_state @@ fun () ->
  let program = load_program il ~prelude ~no_prelude file in
  let result, bag =
    with_warnings (fun () -> run_relation ~sl ~det il relation program file)
  in
  print_bag bag;
  match result with
  | Holds outputs when has_outputs -> (
      match List.rev outputs with
      | [] -> fail "relation %s gave no output" relation
      | last :: rest ->
          List.iter
            (fun v -> print_endline ("output: " ^ string_of_value v))
            (List.rev rest);
          let verdict = string_of_value last in
          print_endline ("verdict: " ^ verdict);
          exit (if verdict = "REJECT" then 1 else 0))
  | Holds _ ->
      print_endline ("verdict: " ^ holds);
      exit 0
  | Fails diag when not has_outputs ->
      print_bag (Diagnostic.Bag.singleton diag);
      print_endline ("verdict: NOT_" ^ holds);
      exit 1
  | Fails diag ->
      print_bag (Diagnostic.Bag.singleton diag);
      fail "relation %s derives no output for %s" relation file
  | Broken diag ->
      print_bag (Diagnostic.Bag.singleton diag);
      exit 3

let check_command =
  Core.Command.basic
    ~summary:"parse a Rust program and run a relation of the specs on it"
    ~readme:(fun () ->
      "The prelude and FILE.rs are parsed separately with the same grammar, \
       each in exactly one way. The checked program is FILE.rs's program \
       (its inner attributes included) with the prelude's items put before \
       its own items. The last stdout line is the verdict: \"verdict: V\" for \
       a relation's last output V, or \"verdict: A\" / \"verdict: NOT_A\" for \
       a relation without outputs, A its last atom. Exit: 0 positive, 1 \
       negative (NOT_A, REJECT), 2 parse or prelude parse reject/ambiguous, 3 \
       spec, interpreter, or usage error.")
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map specs =
    flag "-s" (listed string)
      ~doc:"SPEC spec file, repeatable; elaborated together in the given order"
  and file = flag "-p" (required string) ~doc:"FILE.rs program to check"
  and prelude =
    flag "--prelude" (optional string)
      ~doc:
        ("FILE Rust program whose items go before FILE.rs's items; parsed on \
          its own (default: " ^ default_prelude ^ ")")
  and no_prelude = flag "--no-prelude" no_arg ~doc:" check without a prelude"
  and relation =
    flag "--relation"
      (optional_with_default "Check" string)
      ~doc:"NAME relation to run on the program (default: Check)"
  and sl = flag "--sl" no_arg ~doc:" use the SL interpreter (default: IL)"
  and det =
    flag "--det" no_arg
      ~doc:" reject a relation that applies through more than one rule"
  in
  fun () ->
    if no_prelude && Option.is_some prelude then
      fail "--prelude and --no-prelude are mutually exclusive";
    check specs file ~prelude ~no_prelude ~relation ~sl ~det

module Cli : Cli.Target_cli.S = struct
  module Target = Target

  let command =
    Core.Command.group ~summary:"Rust core-calculus commands"
      [ ("check", check_command) ]
end
