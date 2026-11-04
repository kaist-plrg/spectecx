(** Top-level definitions of a spec, named as the spec spells them. A syntax
    type, metavariable or relation is named by its identifier, as in [typeIR] or
    [Expr_ok]. A rule is named by its relation and rule name, as in
    [Expr_ok/this], or by its relation alone when it has no name. A function's
    declaration and clauses are named by the function's name with a dollar sign,
    as in [$bound]. *)

type kind = Syntax | Var | Relation | Rule | Dec | BuiltinDec | Def

type definition = private {
  kind : kind;
  name : string;
  location : Common.Location.t;
}

type t = definition list

(** [of_def def] lists the definitions [def] introduces. A [syntax] declaration
    that names several types introduces one definition for each. *)
val of_def : Lang.El.def -> t

(** [string_of_kind kind] is the keyword or keywords that introduce a [kind]
    definition. *)
val string_of_kind : kind -> string
