(** Registered splicer entries.

    Each entry file under [entries/] exposes one or both of [source] and
    [prose]; this module collects them into the two lists [Driver.run] consumes.
*)

let source : Anchor.Source.entry list =
  [
    Entries.Syntax.source;
    Entries.Rel_title.source;
    Entries.Rel.source;
    Entries.Func.source;
    Entries.Func_title.source;
  ]

let prose : Anchor.Prose.entry list =
  [
    Entries.Rel_title.prose;
    Entries.Rel.prose;
    Entries.Func.prose;
    Entries.Func_title.prose;
  ]
