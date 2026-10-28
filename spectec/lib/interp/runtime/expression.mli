module type Context = sig
  (** Binding updates must preserve the original context. *)
  type t

  type annotation

  module Func : sig
    type t
  end

  val find_value_exn : t -> var:Envs.Interp.Var.t -> Lang.Il.value
  val opaque_types : t -> Opaque_types.t
  val find_typdef_exn : t -> id:Common.Domain.TId.t -> Envs.Il.Typdef.t
  val find_func_exn : t -> id:Common.Domain.FId.t -> Func.t
  val add_func_exn : t -> id:Common.Domain.FId.t -> func:Func.t -> t
  val set_value : t -> var:Envs.Interp.Var.t -> value:Lang.Il.value -> t
  val set_values : t -> bindings:(Envs.Interp.Var.t * Lang.Il.value) list -> t
  val without_values : t -> t

  val expand_iteration_inputs :
    t ->
    iter:Lang.Il.iter ->
    vars:Lang.Il.var list ->
    t list Common.Attempt.attempt

  val collect_iteration_value :
    Lang.Il.value list ->
    iter:Lang.Il.iter ->
    result_typ:Lang.Il.typ' ->
    Lang.Il.value

  val collect_iteration_bindings :
    t list ->
    iter:Lang.Il.iter ->
    vars:Lang.Il.var list ->
    (Envs.Interp.Var.t * Lang.Il.value) list

  (** Raise the native interpreter's failure exception on [Error]. *)
  val get_exn : 'a Common.Attempt.attempt -> 'a

  val error : Common.Source.region -> string -> 'a
end

module Make (Ctx : Context) : sig
  type exp = Ctx.annotation Lang.Il.Expression.t
  type arg = Ctx.annotation Lang.Il.Expression.arg

  (** Bind values to variables, replacing existing bindings. Constructor names
      and record-field layouts must already agree. Iterations export only their
      declared variables. *)
  val assign_exp : Ctx.t -> exp:exp -> value:Lang.Il.value -> Ctx.t

  val assign_exps : Ctx.t -> exps:exp list -> values:Lang.Il.value list -> Ctx.t

  (** Bind arguments in the callee. Resolve function references in the caller.
  *)
  val assign_args :
    Ctx.t -> caller:Ctx.t -> args:arg list -> values:Lang.Il.value list -> Ctx.t

  (** The value must already satisfy the target type's runtime check. *)
  val downcast :
    Lang.Il.value -> to_typ:Lang.Il.typ -> ctx:Ctx.t -> Lang.Il.value

  (** Evaluate the [SubE] type check on values from well-typed programs. This is
      not a general validator for arbitrary values. *)
  val matches_type : Lang.Il.value -> typ:Lang.Il.typ -> ctx:Ctx.t -> bool

  val matches_pattern : Lang.Il.value -> pattern:Lang.Il.pattern -> bool

  (** [call_func] receives unevaluated arguments and must evaluate them in the
      supplied caller context. *)
  val eval_exp :
    exp ->
    ctx:Ctx.t ->
    call_func:
      (Ctx.t -> Lang.Il.id -> Lang.Il.targ list -> arg list -> Lang.Il.value) ->
    Lang.Il.value

  val eval_exps :
    exp list ->
    ctx:Ctx.t ->
    call_func:
      (Ctx.t -> Lang.Il.id -> Lang.Il.targ list -> arg list -> Lang.Il.value) ->
    Lang.Il.value list

  val eval_args :
    arg list ->
    ctx:Ctx.t ->
    call_func:
      (Ctx.t -> Lang.Il.id -> Lang.Il.targ list -> arg list -> Lang.Il.value) ->
    Lang.Il.value list
end
