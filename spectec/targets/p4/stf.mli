(** Ports are nonnegative. [hex] is uppercase hexadecimal with [*] matching one
    digit. [exact] requires a full match. Otherwise [hex] matches a prefix. *)
type expect = private { port : int; hex : string; exact : bool }

type statement = private
  | Packet of { port : int; hex : string }
  | Expect of expect

(** Accept only [packet] and [expect] lines with [#] comments. A trailing [$]
    makes [expect] exact. Return [TaskParseError] for invalid input or file
    errors. *)
val parse_file : string -> statement list Spectec.Task.result
