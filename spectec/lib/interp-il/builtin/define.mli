open Il
open Util.Source

type t = at:region -> targ list -> value list -> (Value.t, Err.t) result

(* Builtins with no type arguments *)
module T0 : sig
  val a0 : (at:region -> (Value.t, Err.t) result) -> t
  val a1 : 'a Arg.t -> (at:region -> 'a -> (Value.t, Err.t) result) -> t

  val a2 :
    'a Arg.t ->
    'b Arg.t ->
    (at:region -> 'a -> 'b -> (Value.t, Err.t) result) ->
    t

  val a3 :
    'a Arg.t ->
    'b Arg.t ->
    'c Arg.t ->
    (at:region -> 'a -> 'b -> 'c -> (Value.t, Err.t) result) ->
    t
end

(* Builtins with one type argument *)

module T1 : sig
  val a1 : 'a Arg.t -> (at:region -> targ -> 'a -> (Value.t, Err.t) result) -> t

  val a2 :
    'a Arg.t ->
    'b Arg.t ->
    (at:region -> targ -> 'a -> 'b -> (Value.t, Err.t) result) ->
    t
end

(* Builtins with two type arguments *)
module T2 : sig
  val a1 :
    'a Arg.t ->
    (at:region -> targ -> targ -> 'a -> (Value.t, Err.t) result) ->
    t

  val a2 :
    'a Arg.t ->
    'b Arg.t ->
    (at:region -> targ -> targ -> 'a -> 'b -> (Value.t, Err.t) result) ->
    t

  val a3 :
    'a Arg.t ->
    'b Arg.t ->
    'c Arg.t ->
    (at:region -> targ -> targ -> 'a -> 'b -> 'c -> (Value.t, Err.t) result) ->
    t
end
