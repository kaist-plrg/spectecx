open Lang.Il

(** Checks the value and its contents against [typ], resolving type aliases.
    Opaque values must have the type definition listed under the declaration's
    name in [opaque_types]. Record fields must appear in declaration order. *)
val check :
  opaque_types:Opaque_types.t ->
  find_typdef_opt:(id -> (tparam list * deftyp) option) ->
  typ:typ ->
  value ->
  (unit, string) result

(** Applies [check] to each value and its corresponding type in [typs]. Rejects
    unequal lengths. *)
val check_values :
  opaque_types:Opaque_types.t ->
  find_typdef_opt:(id -> (tparam list * deftyp) option) ->
  typs:typ list ->
  value list ->
  (unit, string) result
