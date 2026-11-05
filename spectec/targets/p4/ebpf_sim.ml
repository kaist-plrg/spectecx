(* The eBPF packet pipeline. Each input packet is parsed and then filtered. The
   filter writes [accept], and the packet is transmitted unchanged on its input
   port when [accept] is true. *)

open Lang.Il
module Location = Common.Location
module P4_values = Builtins_p4.P4_values
module Ebpf = Builtins_p4.Ebpf

let ( let* ) = Result.bind
let fail message = Error (Spectec.Error.TaskError (Location.Unknown, message))
let unexpected relation = fail ("unexpected outputs from relation " ^ relation)

let bits_of_hex (hex : string) : bool array =
  hex |> String.to_seq |> List.of_seq
  |> List.concat_map (fun digit ->
         let n = int_of_string ("0x" ^ String.make 1 digit) in
         [ n land 8 <> 0; n land 4 <> 0; n land 2 <> 0; n land 1 <> 0 ])
  |> Array.of_list

(* Returns the context and state after the pipeline processes the packet, and
   whether the filter accepted the packet. *)
let drive_packet ~eval_rel (ctx, arch) ~(hex : string) =
  let packet_in = Ebpf.packet_in_state (bits_of_hex hex) in
  let* outputs = eval_rel "EBPF_init_packet_in" [ ctx; arch; packet_in ] in
  let* ctx, arch =
    match outputs with
    | [ ctx; arch ] -> Ok (ctx, arch)
    | _ -> unexpected "EBPF_init_packet_in"
  in
  let* outputs = eval_rel "EBPF_init_globals" [ ctx; arch ] in
  let* ctx =
    match outputs with [ ctx ] -> Ok ctx | _ -> unexpected "EBPF_init_globals"
  in
  let* outputs = eval_rel "EBPF_parse" [ ctx; arch ] in
  let* ctx, arch, parse_result =
    match outputs with
    | [ ctx; arch; parse_result ] -> Ok (ctx, arch, parse_result)
    | _ -> unexpected "EBPF_parse"
  in
  if P4_values.is_reject parse_result then Ok ((ctx, arch), false)
  else
    let* outputs = eval_rel "EBPF_filter" [ ctx; arch ] in
    let* ctx, arch =
      match outputs with
      | [ ctx; arch; _ ] -> Ok (ctx, arch)
      | _ -> unexpected "EBPF_filter"
    in
    let* outputs =
      eval_rel "Lvalue_read"
        [ P4_values.global; ctx; arch; P4_values.bare_name "accept" ]
    in
    let* accept =
      match outputs with
      | [ value ] -> (
          match P4_values.bool_of_value Location.Unknown value with
          | Ok accept -> Ok accept
          | Error _ -> fail "accept is not a P4 boolean")
      | _ -> unexpected "Lvalue_read"
    in
    Ok ((ctx, arch), accept)

let matches hex (expect : Stf.expect) =
  String.length hex >= String.length expect.hex
  && ((not expect.exact) || String.length hex = String.length expect.hex)
  && Seq.for_all2
       (fun digit digit_expected ->
         digit_expected = '*' || digit = digit_expected)
       (String.to_seq hex) (String.to_seq expect.hex)

(* Packets on different ports may be transmitted in any relative order, so the
   packets of each port are compared with that port's expect lines in order. *)
let check_transmitted transmitted ~(expected : Stf.expect list) =
  let ports =
    List.map fst transmitted
    @ List.map (fun (expect : Stf.expect) -> expect.port) expected
    |> List.sort_uniq Int.compare
  in
  let matches_on port =
    let transmitted =
      List.filter_map
        (fun (port_transmitted, hex) ->
          if port_transmitted = port then Some hex else None)
        transmitted
    in
    let expected =
      List.filter (fun (expect : Stf.expect) -> expect.port = port) expected
    in
    List.length transmitted = List.length expected
    && List.for_all2 matches transmitted expected
  in
  if List.for_all matches_on ports then Ok ()
  else
    let string_of_packet (port, hex) = Printf.sprintf "(%d) %s" port hex in
    let string_of_expect (expect : Stf.expect) =
      Printf.sprintf "(%d) %s%s" expect.port expect.hex
        (if expect.exact then "$" else "")
    in
    fail
      (Printf.sprintf "transmitted [%s] but expected [%s]"
         (List.map string_of_packet transmitted |> String.concat ", ")
         (List.map string_of_expect expected |> String.concat ", "))

let value_of_transmitted (port, hex) =
  Value.tuple [ Value.nat (Bigint.of_int port); Value.text hex ]

let string_of_transmitted (value : Value.t) : string =
  match value.it with
  | TupleV [ { it = NumV port; _ }; { it = TextV hex; _ } ] ->
      Printf.sprintf "Transmitted (%s) %s" (Lang.Xl.Num.string_of_num port) hex
  | _ -> Value.to_string value

(* Runs [program] on the packets of [statements] in order, carrying the context
   and state from one packet to the next, and returns the transmitted packets.
   The result is an error when the transmitted packets do not match the [Expect]
   statements. *)
let drive (program : Value.t) ~(statements : Stf.statement list) ~eval_rel :
    Value.t list Spectec.Task.result =
  let* outputs = eval_rel "EBPF_init" [ program ] in
  let* state =
    match outputs with
    | [ ctx; arch ] -> Ok (ctx, arch)
    | _ -> unexpected "EBPF_init"
  in
  let* _, transmitted, expected =
    List.fold_left
      (fun result statement ->
        let* state, transmitted, expected = result in
        match (statement : Stf.statement) with
        | Packet { port; hex } ->
            let* state, accept = drive_packet ~eval_rel state ~hex in
            let transmitted =
              if accept then (port, hex) :: transmitted else transmitted
            in
            Ok (state, transmitted, expected)
        | Expect expect -> Ok (state, transmitted, expect :: expected))
      (Ok (state, [], []))
      statements
  in
  let transmitted = List.rev transmitted in
  let* () = check_transmitted transmitted ~expected:(List.rev expected) in
  Ok (List.map value_of_transmitted transmitted)
