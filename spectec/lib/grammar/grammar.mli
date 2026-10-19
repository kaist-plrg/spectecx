(** The grammar of the defined target. *)

type primitive = Num of Xl.Num.typ | Bool | Text

(** What fills an [Il.Mixfix.Arg] position: a reference to another syntax
    ([Nonterminal]), a primitive terminal, or an iteration of either ([x*],
    [x?]). *)
type arg =
  | Nonterminal of Il.id
  | Primitive of primitive
  | Iter of arg * Il.iter

(** Whether the production is left- and/or right-recursive in its own syntax. *)
type recursion = Neither | Left | Right | Both

(** [Tighter a]: binds more tightly than the sibling production whose operator
    is [a]. *)
type precedence = Tighter of Xl.Atom.t

(** [Case] builds a node tagged with the syntax; [Alias] (a plain inclusion like
    [prog = command]) passes its single argument through unchanged. *)
type construction = Case | Alias

type production = {
  notation : arg Il.Mixfix.t;
  recursion : recursion;
  precedence : precedence option;
  not_followed_by : Xl.Atom.t option;
      (** From [hint(not_followed_by a)]: the production never ends right before
          [a]. *)
  construction : construction;
  origin : Il.id;
}

type syntax = { name : Il.id; productions : production list }
type t = syntax list

(** [extract ~start spec] is the grammar of the syntaxes reachable from [start],
    read from [spec]'s variant and plain-alias [syntax] declarations. Raises
    [Failure] if a reachable production uses a type the grammar cannot render as
    surface syntax. *)
val extract : start:string -> Il.spec -> t

(** The operator or keyword atom that names a production, if it has one. *)
val operator_atom : production -> Xl.Atom.t option

(** [rank syntax atom] is the precedence height of the production named by
    [atom] within [syntax], counted along the [tighter_than] chain (0 = loosest
    bind). *)
val rank : syntax -> Xl.Atom.t -> int

(** [production_rank syntax prod] is [prod]'s precedence height: one above the
    production its [tighter_than] names, or 0 without one. Unlike {!rank}, this
    also places a production that has no operator atom of its own, such as a
    call [e ( e* )]. *)
val production_rank : syntax -> production -> int

(** What a lexer token denotes: a fixed atom, or a primitive carrying its IL
    value. The parser reads tokens into terminals. The printer writes terminals
    back out as surface text. *)
module Terminal : sig
  type t = Atom of Xl.Atom.t | Primitive of primitive * Il.value
end

val string_of_t : t -> string
