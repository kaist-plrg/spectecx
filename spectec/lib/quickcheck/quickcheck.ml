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
  | Qc_ir.QcProp { free_vars; all_var_names; goal; prems } ->
    let _ = Printf.printf "Test]\n" in
    let gen =
      if use_manual then gen_free_vars_manual spec_il idx
      else gen_free_vars spec_il free_vars
    in
    let prop =
      Property.for_all ~show:show_env gen (fun initial_env ->
        match
          Interp.run_prems
            (module Nop_target) spec_il initial_env prems all_var_names ""
        with
        | Error _ ->
          Property.of_result Property.Result.nothing
        | Ok env ->
          let passed =
            Result.is_ok
              (Interp.run_prems
                 (module Nop_target) spec_il env [goal] all_var_names "")
          in
          Property.Bool_testable.property passed)
    in
    (match Test.check prop with
     | Test.Pass { num_tests; stamps } ->
       Printf.printf "OK, passed %d tests.\n" num_tests;
       if stamps <> [] then
         List.iter (fun (lbl, n) ->
           Printf.printf "%3d%% %s\n" (n * 100 / num_tests) lbl) stamps
     | Test.Fail { num_tests; counterexample } ->
       Printf.printf "Falsifiable, after %d tests:\n" num_tests;
       List.iter (fun s -> Printf.printf "  %s\n" s) counterexample
     | Test.Gave_up { num_tests } ->
       Printf.printf "Gave up after %d tests (too many discarded).\n" num_tests)
  | Qc_ir.QcGen { free_vars; all_var_names; prems } ->
    let _ = Printf.printf "Generation]\n" in
    let gen =
      if use_manual then gen_free_vars_manual spec_il idx
      else gen_free_vars spec_il free_vars
    in
    let count = ref 0 in
    while !count < 100 do
      let initial_env = Gen.sample gen in
      (match
         Interp.run_prems
           (module Nop_target) spec_il initial_env prems all_var_names ""
       with
       | Error _ -> ()
       | Ok env ->
         print_string (show_env env);
         print_newline ();
         incr count)
    done

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
