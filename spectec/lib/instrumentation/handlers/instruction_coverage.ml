(* An empty result for the unused mode lets restore and merge handle both
   modes uniformly. *)
type result = {
  al : Instruction_coverage_al.result;
  sl : Instruction_coverage_sl.result;
}

let config = ref Instruction_coverage_al.default_config
let handler : (module Instrumentation_api.Handler.S) option ref = ref None

let make_for_spec : Instrumentation_api.Handler.spec -> _ = function
  | AlSpec _ -> Instruction_coverage_al.make !config
  | SlSpec _ -> Instruction_coverage_sl.make !config

module M : Instrumentation_api.Handler.S = struct
  (* [Instr_uid] skips SL specs. *)
  let static_dependencies = Instruction_coverage_al.M.static_dependencies

  let init ~spec =
    Instruction_coverage_al.State.reset ();
    Instruction_coverage_sl.State.reset ();
    let (module H : Instrumentation_api.Handler.S) = make_for_spec spec in
    handler := Some (module H);
    H.init ~spec

  let handle event =
    Option.iter
      (fun (module H : Instrumentation_api.Handler.S) -> H.handle event)
      !handler

  let handle_value = None

  let finish () =
    Option.iter
      (fun (module H : Instrumentation_api.Handler.S) -> H.finish ())
      !handler
end

let get_result () =
  {
    al = Instruction_coverage_al.get_result ();
    sl = Instruction_coverage_sl.get_result ();
  }

let restore { al; sl } =
  Instruction_coverage_al.restore al;
  Instruction_coverage_sl.restore sl

let merge_results r1 r2 =
  {
    al = Instruction_coverage_al.merge_results r1.al r2.al;
    sl = Instruction_coverage_sl.merge_results r1.sl r2.sl;
  }

let make cfg =
  config := cfg;
  (module M : Instrumentation_api.Handler.S)

module Spec : Instrumentation_spec.Spec.S = struct
  let name = "instruction-coverage"
  let modes = [ `AL; `SL ]

  let params =
    [
      Instrumentation_spec.Param_utils.level_param;
      Instrumentation_spec.Param_utils.output_param;
    ]

  let parse alist =
    match Instrumentation_spec.Param_utils.get alist "level" with
    | None -> None
    | Some s ->
        let output =
          Instrumentation_spec.Param_utils.output_of
            (Instrumentation_spec.Param_utils.get alist "output")
        in
        let cfg : Instruction_coverage_al.config =
          {
            level =
              Instrumentation_spec.Param_utils.parse_level
                ~summary:Instruction_coverage_al.Summary
                ~full:Instruction_coverage_al.Full s;
            output;
          }
        in
        Some
          {
            Instrumentation_config.Handler_config.name;
            modes;
            handler = make cfg;
            output;
          }

  let checkpoint =
    Some
      Instrumentation_spec.Spec.
        {
          snapshot = (fun () -> Marshal.to_bytes (get_result ()) []);
          restore = (fun b -> restore (Marshal.from_bytes b 0));
          merge =
            (fun b1 b2 ->
              Marshal.to_bytes
                (merge_results (Marshal.from_bytes b1 0)
                   (Marshal.from_bytes b2 0))
                []);
        }
end

let spec : Instrumentation_spec.Spec.t = (module Spec)
