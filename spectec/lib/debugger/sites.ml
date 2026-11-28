module Al = Lang.Al
module Il = Lang.Il
module Location = Common.Location
open Common.Annotated

type site = { owner : string; span : Location.t; hit : Location.t }

(* A rule or function clause, with its written premises in order. *)
type body = { owner : string; at : Location.t; sites : site list }
type t = body list

let error fmt = Format.kasprintf Result.error fmt

(* An iterated premise is entered at the premise it iterates. *)
let rec entered_at (prem : Al.instr) =
  match prem.it with Al.IterI (prem, _) -> entered_at prem | _ -> prem.at

let body owner at prems =
  let sites =
    List.filter (fun prem -> not (Al.is_synthesized prem)) prems
    |> List.map (fun (prem : Al.instr) ->
           { owner; span = prem.at; hit = entered_at prem })
  in
  { owner; at; sites }

let empty = []

let index (spec : Al.spec) =
  List.concat_map
    (fun (def : Al.def) ->
      match def.it with
      | Al.RelD { relid; rules; _ } ->
          List.map
            (fun (rule : Al.rule) ->
              let owner =
                if rule.it.ruleid.it = "" then relid.it
                else relid.it ^ "/" ^ rule.it.ruleid.it
              in
              body owner rule.at rule.it.instrs)
            rules
      | Al.DecD { defid; clauses; _ } ->
          List.map
            (fun (clause : Al.clause) ->
              body ("$" ^ defid.it) clause.at clause.it.instrs)
            clauses
      | _ -> [])
    spec

let lines_of = function
  | Location.Span { left; right; _ } -> Some (left.line, right.line)
  | Location.Unknown | Location.File _ -> None

let covers line at =
  match lines_of at with
  | Some (first, last) -> first <= line && line <= last
  | None -> false

let starts_after line at =
  match lines_of at with Some (first, _) -> first > line | None -> false

let canonical file =
  match Unix.realpath file with
  | path -> path
  | exception Unix.Unix_error _ ->
      if Filename.is_relative file then Filename.concat (Sys.getcwd ()) file
      else file

let file bodies name =
  let files =
    List.filter_map (fun (body : body) -> Location.file body.at) bodies
    |> List.sort_uniq String.compare
  in
  let names =
    if Filename.is_relative name then
      let suffix = "/" ^ name in
      fun file -> file = name || String.ends_with ~suffix file
    else
      let name = canonical name in
      fun file -> canonical file = name
  in
  match List.filter names files with
  | [ file ] -> Ok file
  | [] -> error "No rule or clause in `%s`." name
  | files ->
      error "`%s` matches several files: %s." name (String.concat ", " files)

let premise_at bodies ~file ~line =
  let bodies =
    List.filter
      (fun (body : body) ->
        Location.file body.at = Some file && covers line body.at)
      bodies
  in
  let at_line =
    List.concat_map (fun (body : body) -> body.sites) bodies
    |> List.filter (fun site -> covers line site.span)
  in
  let latest a b =
    match (lines_of a.span, lines_of b.span) with
    | Some (first_a, _), Some (first_b, _) when first_b > first_a -> b
    | _ -> a
  in
  match (at_line, bodies) with
  | site :: sites, _ ->
      (* select the last premise covering the line *)
      Ok (List.fold_left latest site sites)
  | [], [] -> error "No rule or clause at %s:%d." file line
  | [], body :: _ -> (
      (* no premise on the line, but it is inside a rule or clause *)
      match
        List.find_opt (fun site -> starts_after line site.span) body.sites
      with
      | Some site -> Ok site
      | None -> error "No premise at or after line %d in %s." line body.owner)
