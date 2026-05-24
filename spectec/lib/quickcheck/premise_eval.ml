open Lang.Il
open Common.Source

type bindings = (string * Value.t) list
type outcome = Holds | Fails | StepLimit | Unsupported of string
type env = { target : (module Target.S); core_spec : spec; max_steps : int }

let rel_inputs_of (core_spec : spec) (rel_id : id) : int list option =
  List.find_map
    (fun def ->
      match def.it with
      | RelD { relid = id; inputs; _ } when id.it = rel_id.it -> Some inputs
      | _ -> None)
    core_spec

let rel_input_args (core_spec : spec) (rel_id : id) (args : exp list) : exp list
    =
  let indices =
    match rel_inputs_of core_spec rel_id with
    | Some indices -> indices
    | None -> List.mapi (fun i _ -> i) args
  in
  List.filteri (fun i _ -> List.mem i indices) args

(* Bare VarE only: anything else would need an in-process exp evaluator. *)
let lookup_input (bindings : bindings) (arg : exp) : (Value.t, string) result =
  match arg.it with
  | VarE id -> (
      match List.assoc_opt id.it bindings with
      | Some v -> Ok v
      | None ->
          Error
            (Printf.sprintf "input variable %s not bound by generator" id.it))
  | _ ->
      Error
        "non-VarE input arguments in property premises are not yet supported"

let eval_rule_pr (env : env) ~(bindings : bindings) (rel_id : id)
    (args : exp list) : outcome =
  let input_exps = rel_input_args env.core_spec rel_id args in
  let rec collect_values acc = function
    | [] -> Ok (List.rev acc)
    | a :: rest -> (
        match lookup_input bindings a with
        | Ok v -> collect_values (v :: acc) rest
        | Error msg -> Error msg)
  in
  match collect_values [] input_exps with
  | Error msg -> Unsupported msg
  | Ok values -> (
      let max_steps_opt =
        if env.max_steps < 0 then None else Some env.max_steps
      in
      try
        Step_budget.with_budget ?max_steps:max_steps_opt env.core_spec
          (fun () ->
            match
              Eval_il.run env.target env.core_spec rel_id.it values
                "<quickcheck>"
            with
            | Ok _ -> Holds
            | Error _ -> Fails)
      with Step_budget.StepLimitExceeded -> StepLimit)

let eval (env : env) ~bindings (prem : prem) : outcome =
  match prem.it with
  | RulePr { relid = rel_id; notexp } | IfHoldPr { relid = rel_id; notexp } ->
      eval_rule_pr env ~bindings rel_id (Mixfix.args notexp)
  | IfNotHoldPr { relid = rel_id; notexp } -> (
      match eval_rule_pr env ~bindings rel_id (Mixfix.args notexp) with
      | Holds -> Fails
      | Fails -> Holds
      | other -> other)
  | _ ->
      Unsupported
        "only relation premises (RulePr, IfHoldPr, IfNotHoldPr) are supported \
         in property and generator bodies"

let rec eval_side (env : env) ~bindings = function
  | [] -> Holds
  | p :: rest -> (
      match eval env ~bindings p with
      | Holds -> eval_side env ~bindings rest
      | other -> other)
