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
    (List.filter_map
       (fun v ->
         match v.Qc_ir.iv_origin with
         | Qc_ir.Free ->
           Some
             (Gen.map
                (fun value -> (v.Qc_ir.iv_id, value))
                (gen_of_typ spec_il v.Qc_ir.iv_typ))
         | _ -> None)
       free_vars)

let dispatch spec_il (command : Qc_ir.qc_command) =
  match command with
  | Qc_ir.QcProp { free_vars; goal; prems } ->
    let all_ids = List.map (fun v -> v.Qc_ir.iv_id) free_vars in
    let gen = gen_free_vars spec_il free_vars in
    let prop =
      Property.for_all ~show:show_env gen (fun initial_env ->
        match
          Interp.run_prems
            (module Nop_target) spec_il initial_env prems all_ids ""
        with
        | Error _ ->
          Property.of_result Property.Result.nothing
        | Ok env ->
          let passed =
            Result.is_ok
              (Interp.run_prems
                 (module Nop_target) spec_il env [goal] all_ids "")
          in
          Property.Bool_testable.property passed)
    in
    Test.quickcheck prop
  | Qc_ir.QcGen { free_vars; prems } ->
    let all_ids = List.map (fun v -> v.Qc_ir.iv_id) free_vars in
    let gen = gen_free_vars spec_il free_vars in
    let count = ref 0 in
    while !count < 100 do
      let initial_env = Gen.sample gen in
      (match
         Interp.run_prems
           (module Nop_target) spec_il initial_env prems all_ids ""
       with
       | Error _ -> ()
       | Ok env ->
         print_string (show_env env);
         print_newline ();
         incr count)
    done

let quickcheck_file spec_il path =
  match Qc_parse.parse_file path with
  | Error msg ->
    failwith (Printf.sprintf "quickcheck: failed to parse '%s': %s" path msg)
  | Ok ast ->
    match Qc_elab.elaborate spec_il ast with
    | Error msg ->
      failwith
        (Printf.sprintf "quickcheck: failed to elaborate '%s': %s" path msg)
    | Ok cmds -> List.iter (dispatch spec_il) cmds
