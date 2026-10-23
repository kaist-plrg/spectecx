module H = Harness
module V = Lang.Il.Value
module Typ = Lang.Il.Typ

let ( $ ) = Common.Source.( $ )
let no_region = Common.Source.no_region
let nat n = V.nat (Bigint.of_int n)
let nats ns = V.list (Typ.nat $ no_region) (List.map nat ns)
let spec = H.compile_file "determinism.spectec"

let first_match ?modes ~name ~relation n expectation =
  H.check spec ?modes ~name ~relation ~args:(fun () -> [ nat n ]) expectation

let checked ?modes ~name ~relation n expectation =
  H.check spec ?modes ~check_determinism:true ~name ~relation
    ~args:(fun () -> [ nat n ])
    expectation

let returns n = H.returns (fun () -> [ nat n ])

let first_match_list ?modes ~name ~relation ns expectation =
  H.check spec ?modes ~name ~relation ~args:(fun () -> [ nats ns ]) expectation

let checked_list ?modes ~name ~relation ns expectation =
  H.check spec ?modes ~check_determinism:true ~name ~relation
    ~args:(fun () -> [ nats ns ])
    expectation

(* The IL diagnostic names the rules or the clauses that overlap, while the SL
   and PL diagnostics point at the source regions the alternatives start at. *)

let overlapping ~name ~relation n ~il =
  checked ~modes:[ H.Il ] ~name ~relation n (H.fails_with il);
  checked ~modes:[ H.Sl; H.Pl ] ~name ~relation n
    (H.fails_with "and so does the")

let overlapping_list ~name ~relation ns ~il =
  checked_list ~modes:[ H.Il ] ~name ~relation ns (H.fails_with il);
  checked_list ~modes:[ H.Sl; H.Pl ] ~name ~relation ns
    (H.fails_with "and so does the")

let check_undeclared_builtin () =
  let module Target : Interp.Target.S = struct
    include H.Pure_target

    let builtins =
      [ ("boom", fun ~at:_ _ _ -> failwith "undeclared builtin was invoked") ]
  end in
  (* Elaboration requires declarations, so remove them from the compiled forms. *)
  let spec =
    {
      spec with
      spec_il =
        List.filter
          (fun (def : Lang.Il.def) ->
            match def.it with Lang.Il.BuiltinDecD _ -> false | _ -> true)
          spec.spec_il;
      spec_sl =
        List.filter
          (fun (def : Lang.Sl.def) ->
            match def.it with Lang.Sl.BuiltinDecD _ -> false | _ -> true)
          spec.spec_sl;
      spec_pl =
        List.filter
          (fun (def : Lang.Pl.def) ->
            match def.node.it with Lang.Pl.BuiltinDecD _ -> false | _ -> true)
          spec.spec_pl;
    }
  in
  H.check spec
    ~target:(module Target)
    ~name:"undeclared builtin prevents invocation and fallback"
    ~relation:"Undeclared_builtin"
    ~args:(fun () -> [ nat 1 ])
    (H.fails_with "builtin $boom is invoked without a spec declaration")

let run () =
  first_match ~name:"disagreeing rules" ~relation:"Disagree" 5 (returns 1);
  overlapping ~name:"disagreeing rules" ~relation:"Disagree" 5
    ~il:"applies through both rule Disagree/low and rule Disagree/small";
  checked ~name:"one rule applies" ~relation:"Disagree" 15 (returns 2);
  checked ~name:"no rule applies" ~relation:"Partial" 50 H.fails;
  checked ~name:"disjoint rules, low" ~relation:"Disjoint" 5 (returns 1);
  checked ~name:"disjoint rules, high" ~relation:"Disjoint" 15 (returns 2);
  first_match ~name:"unconditional rules" ~relation:"Always" 5 (returns 1);
  overlapping ~name:"unconditional rules" ~relation:"Always" 5
    ~il:"applies through both rule Always/a and rule Always/b";
  overlapping ~name:"rules behind the same premise, equal outputs"
    ~relation:"Same_guard_equal" 5
    ~il:
      "applies through both rule Same_guard_equal/a and rule Same_guard_equal/b";
  checked ~modes:[ H.Il ] ~name:"unnamed rules" ~relation:"Unnamed" 5
    (H.fails_with "applies through both the rule at determinism.spectec:");
  first_match ~name:"ordinary rule over a preceding fallback"
    ~relation:"Fallback_first" 5 (returns 1);
  checked ~name:"ordinary rule over a preceding fallback"
    ~relation:"Fallback_first" 5 (returns 1);
  first_match ~name:"preceding fallback after no ordinary rule"
    ~relation:"Fallback_first" 50 (returns 99);
  checked ~name:"preceding fallback after no ordinary rule"
    ~relation:"Fallback_first" 50 (returns 99);
  overlapping ~name:"overlap beside a fallback" ~relation:"Fallback_overlap" 5
    ~il:
      "applies through both rule Fallback_overlap/low and rule \
       Fallback_overlap/small";
  checked ~modes:[ H.Il ] ~name:"fallback that fails too, ordinary trace"
    ~relation:"Fallback_fails" 5
    (H.fails_with "rule Fallback_fails/low");
  checked ~modes:[ H.Il ] ~name:"fallback that fails too, fallback trace"
    ~relation:"Fallback_fails" 5
    (H.fails_with "rule Fallback_fails/rest");
  checked ~modes:[ H.Sl; H.Pl ] ~name:"fallback that fails too"
    ~relation:"Fallback_fails" 5 H.fails;
  overlapping ~name:"overlap under a shared premise" ~relation:"Shared_premise"
    5 ~il:"applies through both rule Shared_premise/a and rule Shared_premise/b";
  checked ~name:"shared premise, one rule" ~relation:"Shared_premise" 2
    (returns 1);
  first_match ~name:"overlap under a binding premise" ~relation:"Bind_overlap" 5
    (returns 6);
  overlapping ~name:"overlap under a binding premise" ~relation:"Bind_overlap" 5
    ~il:"applies through both rule Bind_overlap/a and rule Bind_overlap/b";
  first_match ~name:"rebinding after a failed rule" ~relation:"Rebind" 5
    (returns 7);
  checked ~name:"rebinding after a failed rule" ~relation:"Rebind" 5 (returns 7);
  checked ~name:"disjunction that holds on both sides" ~relation:"Disjunction" 5
    (returns 1);
  overlapping ~name:"nested overlap" ~relation:"Nested" 5
    ~il:"applies through both rule Disagree/low and rule Disagree/small";
  first_match ~name:"fatal rule after a match" ~relation:"Fatal_after_match" 5
    (returns 1);
  checked ~name:"fatal rule after a match" ~relation:"Fatal_after_match" 5
    (H.fails_with "boom");
  check_undeclared_builtin ();
  first_match ~name:"disagreeing clauses" ~relation:"Call_pick" 5 (returns 1);
  overlapping ~name:"disagreeing clauses" ~relation:"Call_pick" 5
    ~il:"applies through both clause 0 and clause 1";
  checked ~name:"one clause applies" ~relation:"Call_pick" 15 (returns 2);
  checked ~name:"disjoint clauses" ~relation:"Call_pick_disjoint" 5 (returns 1);
  first_match ~name:"unconditional clauses" ~relation:"Call_pick_always" 5
    (returns 1);
  overlapping ~name:"unconditional clauses" ~relation:"Call_pick_always" 5
    ~il:"applies through both clause 0 and clause 1";
  overlapping ~name:"clauses behind the same premise, equal outputs"
    ~relation:"Call_pick_same_guard_equal" 5
    ~il:"applies through both clause 0 and clause 1";
  first_match ~name:"ordinary clause over a preceding fallback"
    ~relation:"Call_pick_fallback_first" 5 (returns 1);
  checked ~name:"ordinary clause over a preceding fallback"
    ~relation:"Call_pick_fallback_first" 5 (returns 1);
  first_match ~name:"preceding fallback after no ordinary clause"
    ~relation:"Call_pick_fallback_first" 50 (returns 99);
  checked ~name:"preceding fallback after no ordinary clause"
    ~relation:"Call_pick_fallback_first" 50 (returns 99);
  checked ~name:"repeated call without overlap"
    ~relation:"Call_pick_disjoint_twice" 5 (returns 2);
  first_match_list ~name:"cons clause over a fixed-length-one clause"
    ~relation:"Call_overlap_cons_first" [ 1 ] (returns 10);
  overlapping_list ~name:"cons clause over a fixed-length-one clause"
    ~relation:"Call_overlap_cons_first" [ 1 ]
    ~il:"applies through both clause 0 and clause 1";
  first_match_list ~name:"fixed-length-one clause over a cons clause"
    ~relation:"Call_overlap_singleton_first" [ 1 ] (returns 20);
  overlapping_list ~name:"fixed-length-one clause over a cons clause"
    ~relation:"Call_overlap_singleton_first" [ 1 ]
    ~il:"applies through both clause 0 and clause 1";
  checked_list ~name:"cons pattern beside the empty pattern, nonempty"
    ~relation:"Call_cons_or_nil" [ 1 ] (returns 1);
  checked_list ~name:"cons pattern beside the empty pattern, empty"
    ~relation:"Call_cons_or_nil" [] (returns 2);
  first_match_list ~name:"cons clause whose premise fails falls through"
    ~relation:"Call_overlap_first_fails" [ 1 ] (returns 20);
  checked_list ~name:"cons clause whose premise fails falls through"
    ~relation:"Call_overlap_first_fails" [ 1 ] (returns 20);
  checked_list ~name:"cons clause over a two-element list, reversed order"
    ~relation:"Call_overlap_singleton_first" [ 1; 2 ] (returns 10)
