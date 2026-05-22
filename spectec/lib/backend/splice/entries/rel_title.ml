(** [${rel-title-source: ids}] and [${rel-title-prose: ids}] — relation headers,
    keyed by relation id. *)

open Lang
open Common.Source

(* Source *)

let source_key_of_def (def : El.def) : (string * El.def) option =
  match def.it with RelD (id, _, _) -> Some (id.it, def) | _ -> None

let source_extract (spec_el : El.spec) : (string * string) list =
  spec_el
  |> List.filter_map source_key_of_def
  |> List.map (fun (key, def) -> (key, El.Print.string_of_def def))

let source : Anchor.Source.entry =
  {
    frame =
      {
        name = "rel-title-source";
        prefix = Anchor.prefix_source;
        suffix = Anchor.suffix_source;
        header = false;
      };
    extract = source_extract;
  }

(* Prose *)

let prose_extract (spec_pl : Pl.spec) : (string * string) list =
  spec_pl
  |> List.filter_map (fun (def_pl : Pl.def) ->
         match def_pl.node.it with
         | RelD (id, sig_, exps, _, _) ->
             Some
               (id.it, Pl.Render.render_rel_title_adoc def_pl.hints id sig_ exps)
         | _ -> None)

let prose : Anchor.Prose.entry =
  {
    frame =
      {
        name = "rel-title-prose";
        prefix = "[.sidebar-title]\n" ^ Anchor.prefix_prose;
        suffix = Anchor.suffix_prose;
        header = true;
      };
    extract = prose_extract;
  }
