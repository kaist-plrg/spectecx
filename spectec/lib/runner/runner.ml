open Il
open Util.Source
module Error = Error

type 'a pipeline_result = ('a, Error.t) result

let ( let* ) = Result.bind

module Handlers = struct
  let il f =
    let vid_counter = ref 0 in
    let tid_counter = ref 0 in
    Effect.Deep.try_with f ()
      {
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | FreshVid ->
                Some
                  (fun (k : (a, _) Effect.Deep.continuation) ->
                    let id = !vid_counter in
                    incr vid_counter;
                    Effect.Deep.continue k (fun () -> id))
            | FreshTid ->
                Some
                  (fun (k : (a, _) Effect.Deep.continuation) ->
                    let tid = "FRESH__" ^ string_of_int !tid_counter in
                    incr tid_counter;
                    Effect.Deep.continue k (fun () -> tid))
            | ValueCreated _ ->
                Some
                  (fun (k : (a, _) Effect.Deep.continuation) ->
                    (* No-op *)
                    Effect.Deep.continue k ())
            | _ -> None (* Other effects *));
      }

  (* SL interpreter uses IL handler for now *)
  let sl = il
end

let parse_p4_file includes_target filename_target : Il.Value.t pipeline_result =
  let parse_p4_file () =
    P4.Parse.parse_file includes_target filename_target |> Result.ok
  in
  try Handlers.il parse_p4_file
  with P4.Error.P4ParseError (at, msg) ->
    Error.P4ParseError (at, msg) |> Result.error

let parse_p4_string filename_target string : Il.Value.t pipeline_result =
  let parse_p4_string () =
    P4.Parse.parse_string filename_target string |> Result.ok
  in
  try Handlers.il parse_p4_string
  with P4.Error.P4ParseError (at, msg) ->
    Error.P4ParseError (at, msg) |> Result.error

let parse_spec_files filenames : El.Ast.spec pipeline_result =
  let parse_spec_files () =
    List.concat_map Frontend.Parse.parse_file filenames |> Result.ok
  in
  try parse_spec_files ()
  with Frontend.Error.ParseError (at, msg) ->
    Error.ParseError (at, msg) |> Result.error

let elaborate spec_el : Il.spec pipeline_result =
  let elaborate () =
    Elaborate.Elab.elab_spec spec_el
    |> Result.map_error (fun elab_err_list -> Error.ElabError elab_err_list)
  in
  try elaborate ()
  with Elaborate.Error.ElabError (at, failtraces) ->
    Error.ElabError [ (at, failtraces) ] |> Result.error

let interp_il ~debug ~profile spec_il includes_target filename_target :
    (Interp_il.Ctx.t * Il.Value.t list) pipeline_result =
  let interp_il () =
    let* value_program = parse_p4_file includes_target filename_target in
    let ctx_init = Interp_il.Runner.init ~debug ~profile filename_target in
    Interp_il.Runner.run_relation ctx_init spec_il "Program_ok"
      [ value_program ]
    |> Result.ok
  in
  try Handlers.il interp_il
  with Interp_il.Error.InterpError (at, msg) ->
    Error.IlInterpError (at, msg) |> Result.error

let structure spec_il : Sl.Ast.spec = Structure.Struct.struct_spec spec_il

let interp_sl spec_il includes_target filename_target :
    (Interp_sl.Ctx.t * Il.Value.t list) pipeline_result =
  let interp_sl () =
    let* value_program = parse_p4_file includes_target filename_target in
    Interp_sl.Runner.run_relation_fresh spec_il "Program_ok" [ value_program ]
      filename_target
    |> Result.ok
  in
  try Handlers.sl interp_sl
  with Interp_sl.Error.InterpError (at, msg) ->
    Error.SlInterpError (at, msg) |> Result.error

(* Composed functions *)

let parse_p4_file_with_roundtrip roundtrip filenames_spec includes_target
    filename_target : string pipeline_result =
  let* spec_el = parse_spec_files filenames_spec in
  let* spec_il = elaborate spec_el in
  let* value_program = parse_p4_file includes_target filename_target in
  let unparsed_string =
    Format.asprintf "%a\n" (Concrete.Pp.pp_program spec_il) value_program
  in
  if roundtrip then
    let* value_program_rt = parse_p4_string filename_target unparsed_string in
    let eq = Il.Eq.eq_value ~dbg:true value_program value_program_rt in
    if eq then unparsed_string |> Result.ok
    else Error.RoundtripError (no_region, "Roundtrip failed") |> Result.error
  else unparsed_string |> Result.ok
