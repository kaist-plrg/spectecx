(** Ports are nonnegative. [hex] is uppercase hexadecimal with [*] matching one
    digit. [exact] requires a full match. Otherwise [hex] matches a prefix. *)
type expect = private { port : int; hex : string; exact : bool }

(** [Hex] and the [hex] field of [Prefix] omit [0x]. [Binary] omits [0b].
    Hexadecimal digits are uppercase. Each [*] matches one digit in [Hex] or
    [Binary]. [Decimal] has no wildcards. The [length] field of [Prefix] counts
    bits. *)
type key_value = private
  | Hex of string
  | Binary of string
  | Decimal of string
  | Prefix of { hex : string; length : int }

(** [name] keeps the STF line's spelling of the action name. *)
type action = { name : string; args : (string * Bigint.t) list }

(** Table and key names keep their STF spellings. *)
type statement = private
  | Packet of { port : int; hex : string }
  | Expect of expect
  | Add of {
      table : string;
      priority : Bigint.t option;
      keys : (string * key_value) list;
      action : action;
    }
  | SetDefault of { table : string; action : action }

(** Accept [packet], [expect], [add] and [setdefault] lines, with [#] comments.
    A trailing [$] makes [expect] exact. Return [TaskParseError] for invalid
    input or file errors. *)
val parse_file : string -> statement list Spectec.Task.result
