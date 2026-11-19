(** Follow restriction hint for variant case constructors.

    A [hint(not_followed_by '(')] on a [typcase] declares that the case never
    ends right before the named atom: where that atom comes next, the input is
    read some other way. Rust uses this for a field access [e.f], which is never
    followed by [(] because [e.f(..)] is a method call (rustc's
    [Parser::parse_dot_suffix] in compiler/rustc_parse/src/parser/expr.rs). EL
    cannot write a lone bracket atom such as [`(] here; [Parsegen.Earley]
    matches atoms by text, so ['('] stands for it.

    This is a follow restriction in the sense of SDF (Klint and Visser 1994; see
    [Parsegen.Earley]), but on one case rather than on a whole syntax. *)

val parse : El.exp -> Xl.Atom.t option

(** The [not_followed_by] atom carried by [hints], if present. *)
val find : Il.hint list -> Xl.Atom.t option
