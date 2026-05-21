type error = Diag.t list
type 'a result = ('a, error) Stdlib.result

module Fresh : sig
  val fresh_id :
    Common.Domain.IdSet.t -> Common.Domain.Id.t -> Common.Domain.Id.t

  val fresh_var_from_exp :
    ?wildcard:bool ->
    Common.Domain.IdSet.t ->
    Lang.Il.exp ->
    Common.Domain.Id.t * Lang.Il.typ * Lang.Il.iter list

  val fresh_exp_from_typ :
    Common.Domain.IdSet.t -> Lang.Il.typ -> Lang.Il.exp * Common.Domain.IdSet.t
end

val elab_spec : Lang.El.spec -> Lang.Il.spec result

val elab_prems_in_spec :
  Lang.Il.spec ->
  (Lang.El.id * Lang.El.plaintyp) list ->
  Lang.El.prem list ->
  (Lang.Il.prem list * (Lang.Il.id' * Lang.Il.typ) list) result

val error_to_string : error -> string
val error_to_diagnostics : error -> Diag.Bag.t
