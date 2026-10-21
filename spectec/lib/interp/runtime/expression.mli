module type Context = sig
  (** Binding updates must preserve the original context. *)
  type t

  type annotation

  val find_value_exn : t -> var:Envs.Interp.Var.t -> Lang.Il.value
  val opaque_types : t -> Opaque_types.t
  val find_typdef_exn : t -> id:Common.Domain.TId.t -> Envs.Il.Typdef.t

  (** Bind [vars] to their optional contents. Return [Ok None] when all options
      are absent, or [Error] for mixed presence. Empty [vars] returns
      [Ok (Some ctx)]. *)
  val for_optional_iteration :
    t -> vars:Lang.Il.var list -> t option Common.Attempt.attempt

  (** Bind [vars] to their list contents, returning one context per
      corresponding position. Unequal lengths return [Error]. Empty [vars]
      returns [Ok []]. *)
  val for_list_iteration :
    t -> vars:Lang.Il.var list -> t list Common.Attempt.attempt

  (** Raise the native interpreter's failure exception on [Error]. *)
  val get_exn : 'a Common.Attempt.attempt -> 'a

  val error : Common.Source.region -> string -> 'a
end

module Make (Ctx : Context) : sig
  type exp = Ctx.annotation Lang.Il.Expression.t
  type arg = Ctx.annotation Lang.Il.Expression.arg

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
