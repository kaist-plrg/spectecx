module Arbitrary = Arbitrary
module Gen = Gen
module Il_gen = Il_gen
open Il_gen
open Lang.Il
open Common.Source

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

type error = NoManualGenerator of string
type 'a result = ('a, error) Stdlib.result

let error_to_string = function
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

let make_notexp (vars : (id' * typ) list) : notexp =
  List.map
    (fun (id, typ) ->
      Mixfix.Arg { it = VarE (id $ no_region); note = typ.it; at = no_region })
    vars

let make_synth_rel_def (rel_id : id') (all_vars : (id' * typ) list)
    (n_inputs : int) (prems : prem list) : def =
  let notexp = make_notexp all_vars in
  let inputs = List.init n_inputs Fun.id in
  let rule_id = "rule" $ no_region in
  let rule = (rule_id, notexp, prems) $ no_region in
  let nottyp = List.map (fun (_, typ) -> Mixfix.Arg typ) all_vars $ no_region in
  RelD (rel_id $ no_region, nottyp, inputs, [ rule ]) $ no_region

let find_generator_hint (hints : hint list) : string option =
  List.find_map
    (fun (h : hint) ->
      if h.hintid.it = "generator" then
        match h.hintexp.it with
        | Lang.El.CallE (id, [], []) -> Some id.it
        | _ -> None
      else None)
    hints

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

let gen_free_vars (spec_il : spec) (inputs : (id * typ) list) :
    (id' * value) list Gen.t =
  Gen.sequence
    (List.map
       (fun (id, typ) ->
         Gen.map (fun value -> (id.it, value)) (gen_of_typ spec_il typ))
       inputs)

type manual_gen = spec -> (id' * value) list Gen.t

let gen_free_vars_manual ~(manual_gens : (string * manual_gen) list)
    (spec_il : spec) (name : string) :
    ((id' * value) list Gen.t, error) Stdlib.result =
  match List.assoc_opt name manual_gens with
  | Some gen_fn -> Ok (gen_fn spec_il)
  | None -> Error (NoManualGenerator name)

let call_rel ~max_steps spec rel_id input_vals =
  let max_steps = if max_steps < 0 then None else Some max_steps in
  try
    Step_budget.with_budget ?max_steps spec (fun () ->
        `R (Eval_il.run (module Nop_target) spec rel_id input_vals ""))
  with Step_budget.StepLimitExceeded -> `Timeout

let run_property ~generalize ~max_steps ~num_tests ~save
    ~(manual_gens : (string * manual_gen) list) (core_spec : spec)
    ~(name : string) ~(side_prems : prem list) ~(goal : prem)
    ~(hints : hint list) :
    (Test.outcome * Test.opt * (id' * value) list list, error) Stdlib.result =
  let config = { Test.default_config with Test.num_tests } in
  let generator = find_generator_hint hints in
  let inputs = Free_vars.of_premises ~core_spec (side_prems @ [ goal ]) in
  let prems_outputs = Free_vars.outputs_of_premises ~core_spec side_prems in
  let input_pairs = List.map (fun (id, t) -> (id.it, t)) inputs in
  let prems_rel_id = Printf.sprintf "__qc_%s_prems__" name in
  let prems_all = input_pairs @ prems_outputs in
  let prems_def =
    make_synth_rel_def prems_rel_id prems_all (List.length input_pairs)
      side_prems
  in
  let goal_rel_id = Printf.sprintf "__qc_%s_goal__" name in
  let goal_def =
    make_synth_rel_def goal_rel_id prems_all (List.length prems_all) [ goal ]
  in
  let spec = core_spec @ [ prems_def; goal_def ] in
  let prems_input_ids = List.map fst input_pairs in
  let goal_input_ids = List.map fst prems_all in
  match
    match generator with
    | Some gen_name -> gen_free_vars_manual ~manual_gens core_spec gen_name
    | None -> Ok (gen_free_vars core_spec inputs)
  with
  | Error _ as e -> e
  | Ok gen ->
      let generalize_fn =
        if generalize then Some (Generalize.generalize_env core_spec) else None
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
        Property.for_all ~shrink:(shrink_env core_spec)
          ?generalize:generalize_fn ~show:show_env capturing_gen
          (fun initial_env ->
            let prems_inputs =
              List.map (fun id -> List.assoc id initial_env) prems_input_ids
            in
            match call_rel ~max_steps spec prems_rel_id prems_inputs with
            | `Timeout | `R (Error _) ->
                Property.of_result Property.Result.nothing
            | `R (Ok (_, output_vals)) -> (
                let output_env =
                  List.mapi
                    (fun i (id, _) -> (id, List.nth output_vals i))
                    prems_outputs
                in
                let full_env = initial_env @ output_env in
                let goal_inputs =
                  List.map (fun id -> List.assoc id full_env) goal_input_ids
                in
                match call_rel ~max_steps spec goal_rel_id goal_inputs with
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
      Ok (outcome, Test.Prop, List.rev !collected)

let quickcheck_spec ~generalize ~max_steps ~num_tests ~save ~manual_gens
    (spec_il : spec) (qc_spec : Qc_il.spec) : unit result =
  List.fold_left
    (fun acc qc_def ->
      match acc with
      | Error _ -> acc
      | Ok () -> (
          match qc_def with
          | Qc_il.BuiltinGeneratorD _ -> Ok ()
          | Qc_il.PropertyD (name_id, side_prems, goal, hints) -> (
              let name = name_id.it in
              Printf.printf "[Quickcheck %s: Test]\n" name;
              match
                run_property ~generalize ~max_steps ~num_tests ~save
                  ~manual_gens spec_il ~name ~side_prems ~goal ~hints
              with
              | Error _ as e -> e
              | Ok (outcome, opt, collected) ->
                  Test.print_outcome opt outcome;
                  (match (outcome, opt) with
                  | Test.Pass { num_tests = n; _ }, Test.Prop when save ->
                      save_cases_to_json ~name ~num_tests:n collected
                  | _ -> ());
                  Ok ())))
    (Ok ()) qc_spec
