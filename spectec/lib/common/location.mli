(** [line] counts from 1. [column] counts bytes from the start of the line, from
    0. *)
type pos = { line : int; column : int }

(** [right] is the position just after the last character. *)
type t =
  | Unknown
  | File of string
  | Span of { file : string; left : pos; right : pos }

(** Converts the start and end positions of a lexeme or a Menhir [$sloc]. *)
val of_lexing : Lexing.position * Lexing.position -> t

(** The smallest location that contains every location in the list. [File]
    contains every span in its file, and [Unknown] contributes nothing.
    @raise Invalid_argument if the locations name two files. *)
val cover : t list -> t

val file : t -> string option

(** Orders by file, with [Unknown] first, then [File] before [Span], then by the
    left position. *)
val compare : t -> t -> int

(** Prints [<unknown>], [file], [file:3:5] or [file:3:5-4:2], with columns from
    1. *)
val to_string : t -> string

(** Prints the [3:5] or [3:5-4:2] part of a span, with columns from 1. *)
val string_of_range : pos -> pos -> string
