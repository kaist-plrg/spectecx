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

let dispatch ~use_manual ~idx spec_il (command : Qc_ir.qc_command) =
  match command with
  | Qc_ir.QcProp { free_vars; goal; prems } ->
    let _ = Printf.printf "Test]\n" in
    let gen =
      if use_manual then gen_free_vars_manual spec_il idx
      else gen_free_vars spec_il free_vars
    in
    let run env prems =
      try `R (Interp.run_prems ~max_steps:10_000
                (module Nop_target) spec_il env prems "")
      with Interp.StepLimitExceeded -> `Timeout
    in
    let prop =
      Property.for_all ~shrink:(shrink_env spec_il) ~generalize:(generalize_env spec_il) ~show:show_env gen (fun initial_env ->
        match run initial_env prems with
        | `Timeout | `R (Error _) ->
          Property.of_result Property.Result.nothing
        | `R (Ok env) ->
          (match run env [goal] with
           | `Timeout -> Property.of_result Property.Result.nothing
           | `R (Error _) -> Property.Bool_testable.property false
           | `R (Ok _) -> Property.Bool_testable.property true))
    in
    Test.quickcheck prop Test.PROP
  | Qc_ir.QcGen { free_vars; prems } ->
    let _ = Printf.printf "Generation]\n" in
    let gen =
      if use_manual then gen_free_vars_manual spec_il idx
      else gen_free_vars spec_il free_vars
    in
    let prop =
      Property.for_all ~show:show_env gen (fun initial_env ->
        match
          (try `R (Interp.run_prems ~max_steps:10_000
                     (module Nop_target) spec_il initial_env prems "")
           with Interp.StepLimitExceeded -> `Timeout)
        with
        | `Timeout | `R (Error _) ->
          Property.of_result Property.Result.nothing
        | `R (Ok env) ->
          Property.label (show_env env)
            (Property.of_result (Property.Result.with_ok true)))
    in
    let config = { Test.default_config with Test.max_size = 5 } in
    Test.quickcheck ~config:config prop Test.GEN

let quickcheck_file ?(manual = []) spec_il path =
  match Qc_parse.parse_file path with
  | Error msg ->
    failwith (Printf.sprintf "quickcheck: failed to parse '%s': %s" path msg)
  | Ok ast ->
    match Qc_elab.elaborate spec_il ast with
    | Error msg ->
      failwith
        (Printf.sprintf "quickcheck: failed to elaborate '%s': %s" path msg)
    | Ok cmds -> List.iteri (fun i cmd ->
      Printf.printf "\n[Quickcheck %d: " i;
      dispatch ~use_manual:(List.mem i manual) ~idx:i spec_il cmd) cmds
