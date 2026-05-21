(** IL type-based arbitrary value generator.

    Derives [Il.Value.t Gen.t] from [typ'] and [deftyp'] in [Lang.Il]. Generates
    arbitrary values for types defined in a SpecTec spec, used as inputs for
    property-based testing. *)

open Lang.Il

(** {2 Type-based generation} *)

(** [gen_of_typ spec typ] returns a generator for arbitrary values of [typ].

    Generation rules by type:
    - [BoolT] → [BoolV]
    - [NumT `NatT] → [NumV (`Nat n)] (0 to size)
    - [NumT `IntT] → [NumV (`Int n)] ([-size, size] range)
    - [TextT] → [TextV s]
    - [TupleT typs] → [TupleV vs] (each field generated recursively)
    - [IterT (t, Opt)] → [OptV v]
    - [IterT (t, List)] → [ListV vs] (length bounded by size)
    - [VarT (id, _)] → looks up definition in spec and generates recursively
    - [FuncT] → raises an exception (function values cannot be generated) *)
val gen_of_typ : spec -> typ -> Value.t Gen.t

(** [gen_of_deftyp spec outer_typ deftyp] handles [PlainT], [StructT],
    [VariantT]. [outer_typ] is used for the [vnote.typ] annotation of generated
    values. For [VariantT], reduces the size parameter to prevent infinite loops
    on recursive types. *)
val gen_of_deftyp : spec -> typ -> deftyp -> Value.t Gen.t

(** [shrink spec v] returns a list of values strictly smaller than [v], used by
    the QuickCheck shrinker to minimise counterexamples. Handles [TextV],
    [ListV], [OptV], [TupleV], [StructV], and [CaseV]. For [CaseV], looks up the
    variant definition in [spec] via [find_typdef] to try nullary non-recursive
    cases and shrink each constructor argument. Returns [[]] for [BoolV],
    [NumV], [FuncV], and unknown types. *)
val shrink : spec -> Value.t -> Value.t list
