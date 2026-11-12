(** Prints LaTeX math for variable types, relation types, rule conclusions and
    premises in the supplied spec files. If the renderer rejects a form, the
    test prints the renderer's error message in its place. *)

open Lang.El
module Diagnostic = Spectec.Diagnostic

let print_formula (render : unit -> Latex.t) : unit =
  match render () with
  | formula -> print_endline (Latex.to_string formula)
  | exception Invalid_argument message -> print_endline message

let print_def (def : def) : unit =
  match def.it with
  | VarD (id, plaintyp, _) ->
      Printf.printf "[%s]\n" id.it;
      print_formula (fun () -> Latex.of_typ (PlainT plaintyp))
  | RelD (id, nottyp, _) ->
      Printf.printf "[%s]\n" id.it;
      print_formula (fun () -> Latex.of_typ (NotationT nottyp))
  | RuleD (id_rel, id_rule, exp, prems) ->
      Printf.printf "[%s/%s]\n" id_rel.it id_rule.it;
      print_formula (fun () -> Latex.of_exp exp);
      List.iter
        (fun prem ->
          print_string "-- ";
          print_formula (fun () -> Latex.of_prem prem))
        prems
  | SynD _ | TypD _ | BuiltinDecD _ | DecD _ | DefD _ | SepD -> ()

let () =
  match Spectec.parse_spec_files (List.tl (Array.to_list Sys.argv)) with
  | Error error ->
      prerr_string
        (Diagnostic.Render.render_bag ~ansi:Diagnostic.Ansi.plain
           (Spectec.Error.to_diagnostics error));
      exit 1
  | Ok spec -> List.iter print_def spec
