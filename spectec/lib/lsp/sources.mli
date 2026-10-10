type scope = { directory : string; recursive : bool }

type t = private {
  scope : scope;
  files : string list;
  contents : Spectec.spec_source list;
  errors : (string * string) list;
}

val canonical : string -> string
val scope_of : string -> scope
val contains : scope -> string -> bool

(** Load one spec in parser order, using open buffers before disk contents.
    Unmarked directories include immediate siblings only. Open files that do not
    yet exist on disk participate in the same ordering. *)
val load : buffers:Spectec.spec_source list -> path:string -> t

val equal : t -> t -> bool

(** Look up text by the exact filename recorded in [contents]. *)
val text : t -> string -> string option
