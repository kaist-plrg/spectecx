(** Registered splicer entries.

    Entry values such as [Entries.Rel.source] and [Entries.Rel.prose] are named
    after the text they produce. The lists [el] and [pl] group entries by the
    spec they read. [Driver.run] takes these two lists. *)

let el : Lang.El.spec Anchor.entry list =
  [
    Entries.Syntax.source;
    Entries.Rel_title.source;
    Entries.Rel.source;
    Entries.Func.source;
    Entries.Func_title.source;
  ]

let pl : Lang.Pl.spec Anchor.entry list =
  [
    Entries.Rel_title.prose;
    Entries.Rel.prose;
    Entries.Func.prose;
    Entries.Func_title.prose;
  ]
