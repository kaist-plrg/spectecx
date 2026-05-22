(** [${func-title-source: ids}] and [${func-title-prose: ids}] — function
    signatures, keyed by function id.

    On the source side, both [DecD] and [BuiltinDecD] declarations are
    available. The prose side mirrors the split. *)

open Lang
open Common.Source

(* Source *)

let source_key_of_def (def : El.def) : (string * El.def) option =
  match def.it with
  | DecD (id, _, _, _, _) -> Some (id.it, def)
  | BuiltinDecD (id, _, _, _, _) -> Some (id.it, def)
  | _ -> None

let source_extract (spec_el : El.spec) : (string * string) list =
  spec_el
  |> List.filter_map source_key_of_def
  |> List.map (fun (key, def) -> (key, El.Print.string_of_def def))

let source : Anchor.Source.entry =
  {
    frame =
      {
        name = "func-title-source";
        prefix = Anchor.prefix_source;
        suffix = Anchor.suffix_source;
        header = false;
      };
    extract = source_extract;
  }

(* Prose: PL header only (id + tparams + args), no body. *)

let prose_extract (spec_pl : Pl.spec) : (string * string) list =
  List.filter_map
    (fun (def_pl : Pl.def) ->
      match def_pl.node.it with
      | DecD (id, tparams, args, _, _) | BuiltinDecD (id, tparams, args) ->
          Some
            ( id.it,
              Pl.Print.string_of_defid id
              ^ Pl.Print.string_of_tparams tparams
              ^ Pl.Print.string_of_args args )
      | _ -> None)
    spec_pl

let prose : Anchor.Prose.entry =
  {
    frame =
      {
        name = "func-title-prose";
        prefix = "[.sidebar-title]\n" ^ Anchor.prefix_prose;
        suffix = Anchor.suffix_prose;
        header = true;
      };
    extract = prose_extract;
  }
