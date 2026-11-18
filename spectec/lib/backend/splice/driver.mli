(** The splice driver.

    Walks each input file once, replacing every recognised [${name: ids}] anchor
    with rendered text, and writes the result to the matching output path.
    Unrecognised anchor names emit a diagnostic with a file:line location
    instead of leaking through. *)

(** Each pair in [filenames] gives an input path and its output path. The driver
    recognises anchor names from the [frame.name] fields of entries in
    [entries_el] and [entries_pl].

    [run] raises [Invalid_argument] if two entries share a [frame.name], within
    either list or across [entries_el] and [entries_pl].

    Returns a {!Report.t} listing every key registered in a store but never
    referenced by any anchor. *)
val run :
  spec_el:Lang.El.spec ->
  spec_pl:Lang.Pl.spec ->
  entries_el:Lang.El.spec Anchor.entry list ->
  entries_pl:Lang.Pl.spec Anchor.entry list ->
  filenames:(string * string) list ->
  Report.t
