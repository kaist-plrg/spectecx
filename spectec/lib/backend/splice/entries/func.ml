(** [${func-source: ids}] and [${func-prose: ids}] — function bodies, keyed by
    function id.

    The source side aggregates every [DefD] equation declared for the same
    function id. The prose side reads the function body block from the PL
    [DecD]. *)

open Lang
open Common.Source

let source_extract (spec_el : El.spec) : (string * string) list =
  spec_el
  |> List.filter_map (fun (def : El.def) ->
         match def.it with
         | DefD (id, _, _, _, _) -> Some (id.it, def)
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
        name = "func-source";
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
      | DecD (id, _, _, block, elseblock_opt) ->
          Some
            ( id.it,
              Pl.Print.string_of_block block
              ^ Pl.Print.string_of_elseblock_opt ~index:(List.length block)
                  elseblock_opt )
      | _ -> None)
    spec_pl

let prose : Anchor.Prose.entry =
  {
    frame =
      {
        name = "func-prose";
        prefix = Anchor.prefix_prose;
        suffix = Anchor.suffix_prose;
        header = true;
      };
    extract = prose_extract;
  }
