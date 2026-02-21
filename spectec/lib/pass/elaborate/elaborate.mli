open Common.Source
open Common.Attempt

type elaboration_error = region * failtrace list

exception Error of region * failtrace list

module Fresh : sig
  val fresh_id :
    Common.Domain.IdSet.t -> Common.Domain.Id.t -> Common.Domain.Id.t
end

val elab_spec : Lang.El.spec -> (Lang.Il.spec, elaboration_error list) result
