(** User-authored hint storage.

    The validation walk in elaborate confirms each EL hint matches a Registry
    entry; this module collects the same hints into per-subject lookups so later
    passes can read them by node.

    Alter-kind hints on Rel and Func subjects are stored under {!find_alter}.
    Typcase subjects are stored under {!find_alter_typcase} for [prose] and
    {!find_fields} for [hint(fields ...)]; both are keyed by the IL [mixop] of
    the case constructor, which is the shape that the annotate pass sees on a
    [CaseE] expression.

    Rel input positions (the [hint(input %N)] hints) are stored alongside so
    prose_out can be realigned against the inputs. *)

type subject = Rel of El.id' | Func of El.id'
type t

val empty : t
val add_alter : t -> hid:string -> subject:subject -> Alter.t -> t
val add_rel_inputs : t -> rel:El.id' -> Input.t -> t

val add_alter_typcase :
  t -> hid:string -> mixop:unit Il.Mixfix.t -> Alter.t -> t

val add_fields : t -> mixop:unit Il.Mixfix.t -> Fields.t -> t
val find_alter : t -> hid:string -> subject:subject -> Alter.t option
val find_rel_inputs : t -> rel:El.id' -> Input.t option

val find_alter_typcase :
  t -> hid:string -> mixop:unit Il.Mixfix.t -> Alter.t option

val find_fields : t -> mixop:unit Il.Mixfix.t -> Fields.t option

(** Build an HEnv from the EL spec, harvesting Alter-kind hints on Rel and Func
    subjects and rel input positions. Typcase hints (which need an IL mixop key)
    are added by {!load_il_spec}. *)
val of_el_spec : El.spec -> t

(** Extend an HEnv with Typcase-scoped hints (prose alter and fields) from the
    IL spec. *)
val load_il_spec : t -> Il.spec -> t
