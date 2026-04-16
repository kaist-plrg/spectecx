(** IL mixfix form: a flat sequence of atoms and argument placeholders.

    Polymorphic in the argument type, so arguments live inline with the
    structure. Common specializations: [unit t] (mixop only), [typ t], [exp t],
    [value t]. *)

type atom = Xl.Atom.t Common.Source.phrase
type 'a mixeme = Arg of 'a | Atom of atom
type 'a t = 'a mixeme list
type mixop = unit t

exception Arity_mismatch of string

(** {1 Projections} *)

val args : 'a t -> 'a list
val atoms : 'a t -> atom list
val arity : 'a t -> int

(** Erase argument payloads, retaining the mixop (atom skeleton). *)
val to_mixop : 'a t -> unit t

(** {1 Transformation} *)

val map : ('a -> 'b) -> 'a t -> 'b t

(** {1 Construction / deconstruction} *)

(** Attach arguments to the [Arg] positions of a mixop. Raises [Arity_mismatch]
    if lengths differ. *)
val fill : unit t -> 'a list -> 'a t

val split : 'a t -> unit t * 'a list

(** {1 Comparison}

    All comparisons ignore source positions of atoms. *)

val compare_mixop : 'a t -> 'b t -> int
val eq_mixop : 'a t -> 'b t -> bool
val compare : compare_arg:('a -> 'b -> int) -> 'a t -> 'b t -> int
val eq : eq_arg:('a -> 'b -> bool) -> 'a t -> 'b t -> bool

(** {1 Rendering} *)

(** Render by interleaving atom and argument strings. Empty renderings are
    skipped. When [pad_brackets] is [true], a space is inserted on the inner
    side of bracket atoms ([{ x }] instead of [{x}]). *)
val render :
  ?pad_brackets:bool ->
  string_of_atom:(atom -> string) ->
  string_of_arg:('a -> string) ->
  'a t ->
  string

(** Default string: atoms plus [%] for each argument position without bracket
    padding. *)
val to_string : 'a t -> string
