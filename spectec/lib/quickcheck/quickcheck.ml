open Il_gen
open Lang.Il

module Nop_target : Interp.Target.S = struct
  let builtins = []
  let handler f =
    let vid_counter = ref 0 in
    Value.GlobalVidProvider.set (fun () ->
      let v = !vid_counter in incr vid_counter; v);
    f ()
  let is_impure_func _ = false
  let is_impure_rel _ = false
  let state_version = ref 0
end

let shrink_env spec (env : (id' * value) list) : (id' * value) list list =
  let shrink_value = shrink spec in
  List.concat_map (fun (i, (_, vi)) ->
    List.map (fun vi' ->
      List.mapi (fun j (idj, vj) -> if j = i then (idj, vi') else (idj, vj)) env)
    (shrink_value vi))
  (List.mapi (fun i p -> (i, p)) env)

let generalize_env _spec (_counter_env : (id' * value) list) : (string * ((id' * value) list Gen.t)) list =
  (* TO DO *)
  (* output every generalized version of counter_env*)
  []

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

let gen_free_vars_manual (spec_il : spec) (i : int) :
    (id' * value) list Gen.t =
  match Manual_gen.gen_inputs spec_il i with
  | Some gen -> gen
  | None ->
    failwith (Printf.sprintf
      "quickcheck --manual: no manual generator for block %d. \
       Add a case in manual_gen.ml gen_inputs." i)

let call_rel spec rel_id input_vals =
  try `R (Interp.eval_il ~max_steps:10_000
            (module Nop_target) spec rel_id input_vals "")
  with Interp.StepLimitExceeded -> `Timeout

let dispatch ~use_manual ~idx spec (command : Qc_ir.qc_command) =
  match command with
  | Qc_ir.QcProp { free_vars; prems_rel; goal_rel } ->
    let _ = Printf.printf "Test]\n" in
    let gen =
      if use_manual then gen_free_vars_manual spec idx
      else gen_free_vars spec free_vars
    in
    let prop =
      Property.for_all ~shrink:(shrink_env spec) ~generalize:(generalize_env spec) ~show:show_env gen (fun initial_env ->
        let prems_inputs =
          List.map (fun id -> List.assoc id initial_env) prems_rel.Qc_ir.sr_inputs
        in
        match call_rel spec prems_rel.Qc_ir.sr_id prems_inputs with
        | `Timeout | `R (Error _) ->
          Property.of_result Property.Result.nothing
        | `R (Ok (_, output_vals)) ->
          let output_env =
            List.mapi (fun i (id, _) -> (id, List.nth output_vals i))
              prems_rel.Qc_ir.sr_outputs
          in
          let full_env = initial_env @ output_env in
          let goal_inputs =
            List.map (fun id -> List.assoc id full_env) goal_rel.Qc_ir.sr_inputs
          in
          (match call_rel spec goal_rel.Qc_ir.sr_id goal_inputs with
           | `Timeout -> Property.of_result Property.Result.nothing
           | `R (Error _) -> Property.Bool_testable.property false
           | `R (Ok _) -> Property.Bool_testable.property true))
    in
    Test.quickcheck prop Test.Prop
  | Qc_ir.QcGen { free_vars; prems_rel } ->
    let _ = Printf.printf "Generation]\n" in
    let gen =
      if use_manual then gen_free_vars_manual spec idx
      else gen_free_vars spec free_vars
    in
    let prop =
      Property.for_all ~show:show_env gen (fun initial_env ->
        let prems_inputs =
          List.map (fun id -> List.assoc id initial_env) prems_rel.Qc_ir.sr_inputs
        in
        match call_rel spec prems_rel.Qc_ir.sr_id prems_inputs with
        | `Timeout | `R (Error _) ->
          Property.of_result Property.Result.nothing
        | `R (Ok (_, output_vals)) ->
          let output_env =
            List.mapi (fun i (id, _) -> (id, List.nth output_vals i))
              prems_rel.Qc_ir.sr_outputs
          in
          let full_env = initial_env @ output_env in
          Property.label (show_env full_env)
            (Property.of_result (Property.Result.with_ok true)))
    in
    let config = { Test.default_config with Test.max_size = 5 } in
    Test.quickcheck ~config:config prop Test.Gen

let quickcheck_file ?(manual = []) spec_il path =
  match Qc_parse.parse_file path with
  | Error msg ->
    failwith (Printf.sprintf "quickcheck: failed to parse '%s': %s" path msg)
  | Ok ast ->
    match Qc_elab.elaborate spec_il ast with
    | Error msg ->
      failwith
        (Printf.sprintf "quickcheck: failed to elaborate '%s': %s" path msg)
    | Ok (cmds, synthetic_defs) ->
      let spec_with_synth = spec_il @ synthetic_defs in
      List.iteri (fun i cmd ->
        Printf.printf "\n[Quickcheck %d: " i;
        dispatch ~use_manual:(List.mem i manual) ~idx:i spec_with_synth cmd) cmds
