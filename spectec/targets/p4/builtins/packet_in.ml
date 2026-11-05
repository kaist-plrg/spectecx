open Builtins

let ( let* ) = Result.bind

type t = { bits : bool array; cursor : int }

let make (bits : bool array) : t = { bits = Array.copy bits; cursor = 0 }
let length (packet : t) : int = Array.length packet.bits

(* [void extract<T>(out T hdr)] *)

let extract ~callback ~at ~ctx (packet : t) :
    (t * Lang.Il.Value.t * Lang.Il.Value.t) result =
  let* typ = Spec_calls.find_type_local_opt ~callback ~at ctx ~name:"T" in
  let* typ =
    Option.to_result
      ~none:(Error.runtime at "packet_in.extract: type argument T is unbound")
      typ
  in
  let typ = Spec_calls.subst_type_local ~callback ~ctx typ in
  let* size = Spec_calls.max_size_in_bits ~callback typ |> Arg.nat at in
  let size = Bigint.to_int_exn size in
  if packet.cursor + size > length packet then
    Ok (packet, ctx, P4_values.reject (P4_values.error_value "PacketTooShort"))
  else
    let bits = Array.sub packet.bits packet.cursor size |> Array.to_list in
    let packet = { packet with cursor = packet.cursor + size } in
    let hdr = Spec_calls.find_var_local_exn ~callback ctx ~name:"hdr" in
    let hdr = Spec_calls.write_value_from_bits ~callback hdr ~bits in
    let ctx =
      Spec_calls.update_var_local ~callback ctx ~name:"hdr" ~value:hdr
    in
    Ok (packet, ctx, P4_values.return_empty)
