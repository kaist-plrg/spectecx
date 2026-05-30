(** Lexers for splice anchors.

    The driver lexes [${name:] once to obtain the anchor name, then dispatches
    by name. After dispatch, {!parse_ids} consumes the keys list and the closing
    [}]. *)

(** Skip whitespace (space, tab, newline). *)
val parse_space : Cursor.t -> unit

(** Attempt to lex an anchor opener [${name:]. On success, advance past the
    opener and return the anchor name plus the region it spanned. On failure,
    leave the cursor unchanged. *)
val parse_anchor_open : Cursor.t -> (string * Common.Source.region) option

(** Consume the keys list and the closing [}]. Whitespace between keys is
    skipped. *)
val parse_ids : Cursor.t -> string list
