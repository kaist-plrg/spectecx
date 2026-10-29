open Common.Domain
module Var = Envs.Interp.Var
module Value = Lang.Il.Value
module Typdef = Envs.Il.Typdef

exception Error of Common.Source.region * string

(** Return the values at each corresponding position, or [Error] for unequal
    input lengths. An empty input produces [Ok []]. *)
val transpose_values :
  Value.t list list -> Value.t list list Common.Attempt.attempt

(** Raise [Error] with the combined failure regions and messages on failure. *)
val get_exn : 'a Common.Attempt.attempt -> 'a

module Make (Func : sig
  type t
end) (Rel : sig
  type t
end) : sig
  (** Binding updates preserve the original context. Derived contexts share
      global definitions, builtins, and the mutable invocation cache. *)
  type t

  module Global : sig
    type t

    type definition =
      | Syntax of TId.t * Typdef.t
      | Function of FId.t * Func.t
      | Relation of RId.t * Rel.t

    (** Reject duplicate identifiers within each definition category. *)
    val of_definitions_exn : definition list -> t
  end

  val make :
    Global.t ->
    builtins:Builtins.t ->
    cache:Cache.t ->
    opaque_types:Opaque_types.t ->
    check_determinism:bool ->
    t

  val find_value_opt : t -> var:Var.t -> Value.t option
  val find_value_exn : t -> var:Var.t -> Value.t

  (** Type and function lookups prefer local bindings to global definitions. *)
  val find_typdef_opt : t -> id:TId.t -> Typdef.t option

  val find_typdef_exn : t -> id:TId.t -> Typdef.t
  val find_func_opt : t -> id:FId.t -> Func.t option
  val find_func_exn : t -> id:FId.t -> Func.t
  val find_rel_opt : t -> id:RId.t -> Rel.t option
  val find_rel_exn : t -> id:RId.t -> Rel.t
  val has_value : t -> var:Var.t -> bool
  val has_local_func : t -> id:FId.t -> bool

  (** Checked insertion rejects existing bindings and repeated incoming keys.
      Type and function checks also include visible global definitions. *)
  val add_value_exn : t -> var:Var.t -> value:Value.t -> t

  val add_values_exn : t -> bindings:(Var.t * Value.t) list -> t
  val add_typdef_exn : t -> id:TId.t -> typdef:Typdef.t -> t
  val add_typdefs_exn : t -> bindings:(TId.t * Typdef.t) list -> t
  val add_func_exn : t -> id:FId.t -> func:Func.t -> t

  (** Insert or replace a value binding. *)
  val set_value : t -> var:Var.t -> value:Value.t -> t

  (** Insert or replace value bindings. Later entries take precedence when a
      variable occurs more than once. *)
  val set_values : t -> bindings:(Var.t * Value.t) list -> t

  (** Remove local type, function, and value bindings. Global definitions,
      builtins, the cache, and the determinism setting are retained. *)
  val for_call : t -> t

  (** Retain local types and functions, without local value bindings. *)
  val without_values : t -> t

  val fold_local_typdefs :
    t -> init:'a -> f:('a -> TId.t -> Typdef.t -> 'a) -> 'a

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

  val has_determinism_checks : t -> bool
  val opaque_types : t -> Opaque_types.t
  val has_builtin : t -> id:Lang.Il.id -> bool

  val call_builtin :
    t ->
    id:Lang.Il.id ->
    targs:Lang.Il.targ list ->
    values:Value.t list ->
    Value.t Builtins.result

  (** Return the result of [f], or the result of an earlier call with equal
      [values], paired with whether it came from the cache. *)
  val memoize_func :
    t ->
    id:FId.t ->
    values:Value.t list ->
    f:(unit -> (Value.t, 'error) result) ->
    (Value.t, 'error) result * bool

  (** Like {!memoize_func}, for relation outputs. *)
  val memoize_rel :
    t ->
    id:RId.t ->
    values:Value.t list ->
    f:(unit -> (Value.t list, 'error) result) ->
    (Value.t list, 'error) result * bool

  (** Report [value], built by an iteration of kind [iter] over [vars], as a
      [Combined] value event whose sources are the lists or options bound to
      [vars] in the context, followed by the elements or content of [value].
      Does nothing when no value observer subscribed. *)
  val emit_iteration :
    t -> vars:Lang.Il.var list -> iter:Lang.Il.iter -> value:Value.t -> unit
end
