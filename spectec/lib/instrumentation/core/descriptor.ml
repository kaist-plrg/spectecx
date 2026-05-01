(** Plugin declarations for instrumentation handlers.

    {!S} describes a handler spec statically — its CLI parameters, how to build
    a live handler from parsed flags, and optionally how to checkpoint state.
    {!Handler.S} is the counterpart: what the interpreter calls at runtime. The
    two are split so the CLI can enumerate specs without constructing handlers.
*)

(** Checkpoint serialization hooks for handlers with persistent state. *)
type checkpoint_ops = {
  snapshot : unit -> bytes;
  restore : bytes -> unit;
  merge : bytes -> bytes -> bytes;
}

(** A handler selected from CLI flags.

    Fields here are the ones callers need to act on {i generically} after
    selection — [name] for identity (e.g. checkpoint matching), [mode] for
    {!Config.validate_mode}, [output] for {!Config.close_outputs}. Everything
    else stays encapsulated inside the handler module. *)
type selected_handler = {
  name : string;
  mode : [ `IL | `SL | `Both ];
  handler : (module Handler.S);
  output : Output.t;
}

module type S = sig
  val name : string
  val mode : [ `IL | `SL | `Both ]

  (** [(param_name, doc)] entries for CLI help. *)
  val params : (string * string) list

  val parse : (string * string option) list -> selected_handler option
  val checkpoint : checkpoint_ops option
end

(** First-class module wrapper so handler specs can live in a heterogeneous
    list. *)
type t = (module S)
