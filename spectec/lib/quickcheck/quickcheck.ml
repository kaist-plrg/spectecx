open Il_gen
open Lang.Il

let json_escape s =
  let buf = Buffer.create (String.length s + 2) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when Char.code c < 0x20 -> Printf.bprintf buf "\\u%04x" (Char.code c)
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let json_string s = "\"" ^ json_escape s ^ "\""

let save_cases_to_json ~name ~num_tests (cases : (id' * value) list list) =
  if cases <> [] then (
    let filename = name ^ ".json" in
    let oc = open_out filename in
    let w = output_string oc in
    w "{\n";
    w ("  \"property\": " ^ json_string name ^ ",\n");
    w ("  \"num_tests\": " ^ string_of_int num_tests ^ ",\n");
    w "  \"cases\": [\n";
    List.iteri
      (fun i bindings ->
        w "    {";
        List.iteri
          (fun j (id, v) ->
            if j > 0 then w ", ";
            w (json_string id ^ ": " ^ json_string (Print.string_of_value v)))
          bindings;
        w "}";
        if i < List.length cases - 1 then w ",";
        w "\n")
      cases;
    w "  ]\n}\n";
    close_out oc)

type error =
  | ParseError of string
  | ElabError of string
  | NoManualGenerator of string

type 'a result = ('a, error) Stdlib.result

let error_to_string = function
  | ParseError msg -> Printf.sprintf "parse error: %s" msg
  | ElabError msg -> Printf.sprintf "elaboration error: %s" msg
  | NoManualGenerator name ->
      Printf.sprintf
        "quickcheck: no manual generator named '%s'. Add a case in \
         manual_gen.ml gen_inputs."
        name

let error_to_diagnostic e =
  Diag.error ~source:"quickcheck" Common.Source.no_region (error_to_string e)

module Nop_target : Interp.Target.S = struct
  let builtins = []

  let handler f =
    let vid_counter = ref 0 in
    Value.GlobalVidProvider.set (fun () ->
        let v = !vid_counter in
        incr vid_counter;
        v);
    f ()

  let is_impure_func _ = false
  let is_impure_rel _ = false
  let state_version = ref 0
end

let shrink_env spec (env : (id' * value) list) : (id' * value) list list =
  let shrink_value = shrink spec in
  List.concat_map
    (fun (i, (_, vi)) ->
      List.map
        (fun vi' ->
          List.mapi
            (fun j (idj, vj) -> if j = i then (idj, vi') else (idj, vj))
            env)
        (shrink_value vi))
    (List.mapi (fun i p -> (i, p)) env)

let show_env (bindings : (id' * value) list) : string =
  String.concat ", "
    (List.map (fun (id, v) -> id ^ "=" ^ Print.string_of_value v) bindings)

let gen_free_vars (spec_il : spec) (free_vars : Qc_ir.ir_var list) :
    (id' * value) list Gen.t =
  Gen.sequence
    (List.map
       (fun v ->
         Gen.map
           (fun value -> (v.Qc_ir.iv_id, value))
           (gen_of_typ spec_il v.Qc_ir.iv_typ))
       free_vars)

let gen_free_vars_manual (spec_il : spec) (name : string) :
    ((id' * value) list Gen.t, error) Stdlib.result =
  match Manual_gen.gen_inputs spec_il name with
  | Some gen -> Ok gen
  | None -> Error (NoManualGenerator name)

let call_rel ~max_steps spec rel_id input_vals =
  try
    `R (Qc_eval_il.run ~max_steps (module Nop_target) spec rel_id input_vals "")
  with Qc_eval_il.StepLimitExceeded -> `Timeout

let run_command ~generalize ~max_steps ~num_tests ~save spec
    (command : Qc_ir.qc_command) :
    (Test.outcome * Test.opt * (id' * value) list list, error) Stdlib.result =
  let config = { Test.default_config with Test.num_tests } in
  match command with
  | Qc_ir.QcProp { name = _; free_vars; generator; prems_rel; goal_rel } -> (
      match
        match generator with
        | Some gen_name -> gen_free_vars_manual spec gen_name
        | None -> Ok (gen_free_vars spec free_vars)
      with
      | Error _ as e -> e
      | Ok gen ->
          let generalize_fn =
            if generalize then Some (Generalize.generalize_env spec) else None
          in
          let env_cell : (id' * value) list ref = ref [] in
          let capturing_gen =
            if save then
              Gen.map
                (fun env ->
                  env_cell := env;
                  env)
                gen
            else gen
          in
          let prop =
            Property.for_all ~shrink:(shrink_env spec) ?generalize:generalize_fn
              ~show:show_env capturing_gen (fun initial_env ->
                let prems_inputs =
                  List.map
                    (fun id -> List.assoc id initial_env)
                    prems_rel.Qc_ir.sr_inputs
                in
                match
                  call_rel ~max_steps spec prems_rel.Qc_ir.sr_id prems_inputs
                with
                | `Timeout | `R (Error _) ->
                    Property.of_result Property.Result.nothing
                | `R (Ok (_, output_vals)) -> (
                    let output_env =
                      List.mapi
                        (fun i (id, _) -> (id, List.nth output_vals i))
                        prems_rel.Qc_ir.sr_outputs
                    in
                    let full_env = initial_env @ output_env in
                    let goal_inputs =
                      List.map
                        (fun id -> List.assoc id full_env)
                        goal_rel.Qc_ir.sr_inputs
                    in
                    match
                      call_rel ~max_steps spec goal_rel.Qc_ir.sr_id goal_inputs
                    with
                    | `Timeout -> Property.of_result Property.Result.nothing
                    | `R (Error _) -> Property.Bool_testable.property false
                    | `R (Ok _) -> Property.Bool_testable.property true))
          in
          let collected = ref [] in
          let tracking_prop =
            if save then
              let g = Property.evaluate prop in
              Property.Prop
                (Gen.map
                   (fun result ->
                     (match result.Property.Result.ok with
                     | Some true -> collected := !env_cell :: !collected
                     | _ -> ());
                     result)
                   g)
            else prop
          in
          let outcome = Test.check ~config tracking_prop in
          Ok (outcome, Test.Prop, List.rev !collected))
  | Qc_ir.QcGen { name = _; free_vars; generator; prems_rel } -> (
      match
        match generator with
        | Some gen_name -> gen_free_vars_manual spec gen_name
        | None -> Ok (gen_free_vars spec free_vars)
      with
      | Error _ as e -> e
      | Ok gen ->
          let prop =
            Property.for_all ~show:show_env gen (fun initial_env ->
                let prems_inputs =
                  List.map
                    (fun id -> List.assoc id initial_env)
                    prems_rel.Qc_ir.sr_inputs
                in
                match
                  call_rel ~max_steps spec prems_rel.Qc_ir.sr_id prems_inputs
                with
                | `Timeout | `R (Error _) ->
                    Property.of_result Property.Result.nothing
                | `R (Ok (_, output_vals)) ->
                    let output_env =
                      List.mapi
                        (fun i (id, _) -> (id, List.nth output_vals i))
                        prems_rel.Qc_ir.sr_outputs
                    in
                    let full_env = initial_env @ output_env in
                    Property.label (show_env full_env)
                      (Property.of_result (Property.Result.with_ok true)))
          in
          let config = { config with Test.max_size = 5 } in

          Ok (Test.check ~config prop, Test.Gen, []))

let quickcheck_file ~generalize ~max_steps ~num_tests ~save spec_il path :
    unit result =
  match Qc_parse.parse_file path with
  | Error msg -> Error (ParseError msg)
  | Ok ast -> (
      match Qc_elab.elaborate spec_il ast with
      | Error msg -> Error (ElabError msg)
      | Ok (cmds, synthetic_defs) ->
          let spec_with_synth = spec_il @ synthetic_defs in
          List.fold_left
            (fun acc cmd ->
              match acc with
              | Error _ -> acc
              | Ok () -> (
                  let name, mode_label =
                    match cmd with
                    | Qc_ir.QcProp { name; _ } -> (name, "Test")
                    | Qc_ir.QcGen { name; _ } -> (name, "Generation")
                  in
                  Printf.printf "[Quickcheck %s: %s]\n" name mode_label;
                  match
                    run_command ~generalize ~max_steps ~num_tests ~save
                      spec_with_synth cmd
                  with
                  | Error _ as e -> e
                  | Ok (outcome, opt, collected) ->
                      Test.print_outcome opt outcome;
                      (match (outcome, opt) with
                      | Test.Pass { num_tests = n; _ }, Test.Prop when save ->
                          save_cases_to_json ~name ~num_tests:n collected
                      | _ -> ());
                      Ok ()))
            (Ok ()) cmds)
