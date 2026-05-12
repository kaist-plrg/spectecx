(** Lazy file-content cache for source snippets used by the diagnostic renderer.

    Source files are read on first access and split into lines, then memoized.
    Missing or unreadable files are cached as absent so subsequent lookups don't
    re-attempt I/O. *)

type t

val create : unit -> t

(** [get_line cache file lineno] returns the 1-indexed source line, or [None] if
    the file cannot be read or [lineno] is out of range. *)
val get_line : t -> string -> int -> string option
