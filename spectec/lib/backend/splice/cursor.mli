(** Mutable string cursor used by the splice driver.

    The cursor walks the input file from beginning to end exactly once. All
    mutation lives inside a single call to {!Driver.splice_file}; no caller of
    [Driver] observes it. *)

type t = { file : string; s : string; mutable i : int }

val make : file:string -> string -> t

(** True when the cursor sits past the last character. *)
val eos : t -> bool

(** Character at the current cursor position. Undefined when {!eos} holds. *)
val peek : t -> char

(** Advance one character forward. *)
val adv : t -> unit

(** True when the upcoming characters match [prefix]. Does not advance. *)
val starts_with : t -> string -> bool

(** If the upcoming characters match [prefix], advance past them and return
    [true]. Otherwise leave the cursor unchanged and return [false]. *)
val consume : t -> string -> bool

(** Zero-width region anchored at the current position. *)
val region : t -> Common.Source.region

(** Current line/column position. *)
val pos : t -> Common.Source.pos
