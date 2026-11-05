type t

(** Copy the input bits. *)
val make : bool array -> t

(** [ctx] must bind type [T] and header [hdr]. Return the updated packet,
    context, and P4 call result. For a short packet, return [Ok] with a
    [PacketTooShort] rejection and leave the packet and context unchanged. *)
val extract :
  callback:Builtins.Callback.t ->
  at:Common.Location.t ->
  ctx:Lang.Il.Value.t ->
  t ->
  (t * Lang.Il.Value.t * Lang.Il.Value.t) Builtins.result
