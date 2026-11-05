module Location = Common.Location
open Lang.Il

type implementation =
  at:Location.t -> targ list -> value list -> Value.t Error.result

type t

val make : implementation -> t

(** [args] is a [T0], [T1], or [T2] function with its readers supplied. *)
val make_with_callback :
  args:('impl -> t) -> (callback:Callback.t -> 'impl) -> t

val invoke : t -> callback:Callback.t -> implementation

(* Builtins with no type arguments *)
module T0 : sig
  val a0 : (at:Location.t -> (Value.t, Error.t) result) -> t
  val a1 : 'a Arg.t -> (at:Location.t -> 'a -> (Value.t, Error.t) result) -> t

  val a2 :
    'a Arg.t ->
    'b Arg.t ->
    (at:Location.t -> 'a -> 'b -> (Value.t, Error.t) result) ->
    t

  val a3 :
    'a Arg.t ->
    'b Arg.t ->
    'c Arg.t ->
    (at:Location.t -> 'a -> 'b -> 'c -> (Value.t, Error.t) result) ->
    t

  val a4 :
    'a Arg.t ->
    'b Arg.t ->
    'c Arg.t ->
    'd Arg.t ->
    (at:Location.t -> 'a -> 'b -> 'c -> 'd -> (Value.t, Error.t) result) ->
    t

  val a5 :
    'a Arg.t ->
    'b Arg.t ->
    'c Arg.t ->
    'd Arg.t ->
    'e Arg.t ->
    (at:Location.t -> 'a -> 'b -> 'c -> 'd -> 'e -> (Value.t, Error.t) result) ->
    t
end

(* Builtins with one type argument *)

module T1 : sig
  val a1 :
    'a Arg.t -> (at:Location.t -> targ -> 'a -> (Value.t, Error.t) result) -> t

  val a2 :
    'a Arg.t ->
    'b Arg.t ->
    (at:Location.t -> targ -> 'a -> 'b -> (Value.t, Error.t) result) ->
    t

  val a3 :
    'a Arg.t ->
    'b Arg.t ->
    'c Arg.t ->
    (at:Location.t -> targ -> 'a -> 'b -> 'c -> (Value.t, Error.t) result) ->
    t
end

(* Builtins with two type arguments *)
module T2 : sig
  val a1 :
    'a Arg.t ->
    (at:Location.t -> targ -> targ -> 'a -> (Value.t, Error.t) result) ->
    t

  val a2 :
    'a Arg.t ->
    'b Arg.t ->
    (at:Location.t -> targ -> targ -> 'a -> 'b -> (Value.t, Error.t) result) ->
    t

  val a3 :
    'a Arg.t ->
    'b Arg.t ->
    'c Arg.t ->
    (at:Location.t ->
    targ ->
    targ ->
    'a ->
    'b ->
    'c ->
    (Value.t, Error.t) result) ->
    t
end
