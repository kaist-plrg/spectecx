(** Alter-kind hint payloads: a textual notation with `%N` and `%` holes that
    the renderer substitutes with rendered argument text. *)

open Common.Source
open Xl

type hole = [ `Next | `Num of int ]

type t =
  | TextH of string
  | AtomH of Atom.t phrase
  | SeqH of t list
  | BrackH of Atom.t phrase * t * Atom.t phrase
  | HoleH of hole
  | FuseH of t * t
  | OtherH of El.exp

val parse : El.exp -> t
val to_string : t -> string

(** [alternate hint print items] walks [hint], substituting each [HoleH `Next]
    by [print items.[cursor]] (advancing the cursor) and each [HoleH (`Num k)]
    by [print items.[k]] (without advancing). The optional [base_text],
    [base_atom], and [base_exp] arguments format the literal pieces; defaults
    leave text as-is and print atoms / exps via their standard formatters. *)
val alternate :
  ?base_text:(string -> string) ->
  ?base_atom:(Atom.t phrase -> string) ->
  ?base_exp:(El.exp -> string) ->
  t ->
  ('a -> string) ->
  'a list ->
  string

(** Indices of explicit [%N] holes appearing anywhere inside the hint. *)
val collect : t -> int list

(** Rewrite each [%N] in [hint] to [%K] where K is N's position among the
    non-input indices in [inputs @ outputs]. Used to realign a [prose_out] hint
    (which references positions in the full argument list) against the
    renderer's output-only view. *)
val realign : t -> Input.t -> t
