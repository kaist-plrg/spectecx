(** A general parser for object-language concrete syntax, driven by the
    extracted {!Grammar.t}.

    {!Parse} reads one token ahead and commits, so it needs a grammar it can
    predict, and it has no [x*] or [x?]. This parser takes any context-free
    grammar the extraction yields: left-recursive productions (e.g. a call
    [e (e* )]), alternatives that share a prefix, iterations, and ambiguity. It
    is an Earley parser: it keeps every partial parse alive, so a sentence is
    accepted iff the grammar derives it. When the grammar derives a sentence in
    more than one way, it builds one value and reports where the readings
    diverge.

    Some facts about the concrete syntax are not in a SpecTec grammar, because
    EL has no notation for them. The {!lexicon} supplies them.

    References:
    - Recognition follows Jay Earley, "An efficient context-free parsing
      algorithm", Communications of the ACM 13(2):94-102, 1970.
    - Nullable nonterminals are handled as in John Aycock and R. Nigel Horspool,
      "Practical Earley Parsing", The Computer Journal 45(6):620-630, 2002
      (doi:10.1093/comjnl/45.6.620).
    - Counting parses, building the value, and reporting ambiguities are not
      from these papers. *)

type 'tok lexicon = {
  classify : 'tok -> Grammar.Terminal.t option;
      (** The grammar terminal a token stands for. [None] for a token no
          production can consume. Atoms are matched by their surface text
          ([Xl.Atom.to_string]), so [Colon] and [Operator ":"] are the same
          terminal. *)
  lexical_tag : 'tok -> string option;
      (** The lexical class a primitive token belongs to, such as ["LT"] for a
          Rust lifetime ['a], when the lexer distinguishes it from other tokens
          of the same primitive type. *)
  lexical_tags : string list;
      (** The tags that name lexical classes. In a production, a tag [_T] in
          this list, followed by a primitive or an alias of one, matches only a
          token of class [T]; elsewhere a primitive matches only a token with no
          class. Every other tag has no surface form and is skipped. *)
  separator : string -> Xl.Atom.t option;
      (** [separator x] is the atom that separates the elements of an [x*] list,
          if any (e.g. [,] between Rust generic arguments). A trailing separator
          is accepted after a nonempty list. *)
  spell : Xl.Atom.t -> Xl.Atom.t list;
      (** The tokens that spell a grammar atom: [[a]] for most atoms, but
          several when the lexer splits it, e.g. ['()'] spelled [(] [)]. *)
}

(** A lexicon with no lexical classes, no separators, and one token per atom. *)
val plain_lexicon : ('tok -> Grammar.Terminal.t option) -> 'tok lexicon

(** A place where the grammar derives the same tokens in more than one way:
    syntax [syntax] spans tokens [first] to [last] (inclusive, 0-based), and
    [readings] lists each reading as the productions it starts with, e.g.
    ["expr ::= expr ( expr* )"] against ["expr ::= expr . id ( expr* )"]. *)
type ambiguity = {
  syntax : string;
  first : int;
  last : int;
  readings : string list;
}

type outcome =
  | Accepted of {
      value : Il.value;  (** One parse, the first reading at each ambiguity. *)
      trees : int;  (** How many parses there are, capped at [max_int]. *)
      ambiguities : ambiguity list;
          (** Where [value]'s derivation had more than one reading, outermost
              first. Empty iff [trees = 1]. *)
    }
  | Rejected of {
      at : int;
          (** Index of the first token no parse can consume; the token count
              when the input ended early. *)
      expected : string list;  (** The terminals a parse could consume there. *)
    }

(** [run lexicon grammar start tokens] parses [tokens] as one instance of syntax
    [start]. Raises [Failure] when the grammar itself is malformed, for example
    a reference to a syntax that does not exist. *)
val run : 'tok lexicon -> Grammar.t -> string -> 'tok list -> outcome

(** A production in the form used by {!ambiguity.readings}. *)
val string_of_production : string -> Grammar.production -> string
