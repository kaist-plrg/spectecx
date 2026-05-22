(** The splice driver.

    Walks each input file once, replacing every recognised [${name: ids}] anchor
    with rendered text, and writes the result to the matching output path.
    Unrecognised anchor names emit a diagnostic with a file:line region instead
    of leaking through. *)

(** Run the driver over [filenames] (input/output pairs), substituting
    [${name: ids}] anchors using [source_entries] (extracted from the elaborated
    [El.spec]) and [prose_entries] (extracted from the rendered [Pl.spec]).

    Returns a {!Report.t} listing every key registered in a store but never
    referenced by any anchor. *)
val run :
  spec_el:Lang.El.spec ->
  spec_pl:Lang.Pl.spec ->
  source_entries:Anchor.Source.entry list ->
  prose_entries:Anchor.Prose.entry list ->
  filenames:(string * string) list ->
  Report.t
