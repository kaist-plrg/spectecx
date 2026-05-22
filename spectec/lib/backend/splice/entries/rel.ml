(** [${rel-source: ids}] and [${rel-prose: ids}] — rule bodies for a relation,
    keyed by relation id. Every rule declared against the same relation is
    aggregated into one block. *)

open Lang
open Common.Source

let source_extract (spec_el : El.spec) : (string * string) list =
  spec_el
  |> List.filter_map (fun (def : El.def) ->
         match def.it with
         | RuleD (relid, _, _, _) -> Some (relid.it, def)
         | _ -> None)
  |> Group.in_order
  |> List.map (fun (key, defs) ->
         let body =
           defs |> List.map El.Print.string_of_def |> String.concat "\n\n"
         in
         (key, body))

let source : Anchor.Source.entry =
  {
    frame =
      {
        name = "rel-source";
        prefix = Anchor.prefix_source;
        suffix = Anchor.suffix_source;
        header = false;
      };
    extract = source_extract;
  }

let prose_extract (spec_pl : Pl.spec) : (string * string) list =
  List.filter_map
    (fun (def_pl : Pl.def) ->
      match def_pl.node.it with
      | RelD (id, _, _, block, elseblock_opt) ->
          Some
            ( id.it,
              Pl.Render.strip_leading_newline (Pl.Render.render_instrs block)
              ^ Pl.Render.render_elseblock elseblock_opt )
      | _ -> None)
    spec_pl

let prose : Anchor.Prose.entry =
  {
    frame =
      {
        name = "rel-prose";
        prefix = Anchor.prefix_prose;
        suffix = Anchor.suffix_prose;
        header = false;
      };
    extract = prose_extract;
  }
