(* Finding written premises by file and line, on the Mini-ML spec. *)

module Sites = Spectec_debugger.Sites
module Location = Common.Location

let spec_file = "../../../specs/miniml/spec.spectec"

let sites =
  let il =
    Result.bind (Spectec.parse_spec_files [ spec_file ]) Spectec.elaborate
  in
  match il with
  | Ok il -> Sites.index il
  | Error _ -> failwith ("cannot elaborate " ^ spec_file)

let print_result to_string = function
  | Ok value -> print_endline (to_string value)
  | Error message -> print_endline ("error: " ^ message)

let file ?label name =
  Printf.printf "file %s: " (Option.value label ~default:name);
  print_result Fun.id (Sites.file sites name)

let premise_at line =
  Printf.printf "line %d: " line;
  print_result
    (fun (site : Sites.site) ->
      site.owner ^ " at " ^ Location.to_string site.span)
    (Sites.premise_at sites ~file:spec_file ~line)

let () =
  print_endline "## files";
  file "spec.spectec";
  file "miniml/spec.spectec";
  file ~label:"<absolute>" (Sys.getcwd () ^ "/" ^ spec_file);
  file "other.spectec";
  print_endline "\n## premises";
  (* a written premise of a function clause *)
  premise_at 44;
  (* a clause without written premises *)
  premise_at 55;
  (* a rule's first line: its first written premise *)
  premise_at 92;
  (* a written premise of a rule *)
  premise_at 95;
  (* between definitions *)
  premise_at 99;
  (* a syntax definition *)
  premise_at 8
