module H = Harness
module V = Lang.Il.Value
module B = Interp.Builtins
module Typ = Lang.Il.Typ

let ( $ ) = Common.Annotated.( $ )
let at = Common.Location.Unknown
let ( let* ) = Result.bind
let nat n = V.nat (Bigint.of_int n)

let typdef_state =
  Lang.Il.Opaque.make_typdef ~name:"state" ~compare:Int.compare
    ~hash:Hashtbl.hash

let state n = V.opaque typdef_state n
let saved_callback = ref None

let define_with_callback impl =
  B.Define.make_with_callback ~args:(B.Define.T0.a1 B.Arg.value)
    (fun ~callback ->
      saved_callback := Some callback;
      impl ~callback)

module Target = struct
  include H.Pure_target

  let opaque_types = [ Lang.Il.Opaque.Typdef typdef_state ]
  let state_version = ref 0
  let probe_calls = ref 0

  let with_state f =
    let previous = !state_version in
    state_version := 0;
    probe_calls := 0;
    Fun.protect
      (fun () -> H.Pure_target.with_state f)
      ~finally:(fun () -> state_version := previous)

  let builtins =
    [
      ( "host_step",
        define_with_callback (fun ~callback ~at value ->
            let* count = B.Arg.opaque typdef_state at value in
            let value = callback ("next" $ at) [] [ nat count ] in
            let value = callback ("keep" $ at) [ Typ.nat $ at ] [ value ] in
            let* count = B.Arg.nat at value in
            Ok (state (Bigint.to_int_exn count))) );
      ( "host_identity",
        B.Define.make_with_callback ~args:(B.Define.T1.a1 B.Arg.value)
          (fun ~callback ~at targ value ->
            Ok (callback ("keep" $ at) [ targ ] [ value ])) );
      ( "fresh",
        B.Define.T0.a1 B.Arg.nat (fun ~at:_ _ ->
            let value = nat !state_version in
            incr state_version;
            Ok value) );
      ( "host_choice",
        define_with_callback (fun ~callback ~at value ->
            Ok (callback ("choice" $ at) [] [ value ])) );
      ( "host_fatal",
        define_with_callback (fun ~callback ~at _ ->
            Ok (callback ("explode" $ at) [] [])) );
      ( "host_bad_input",
        define_with_callback (fun ~callback ~at _ ->
            Ok (callback ("keep" $ at) [ Typ.nat $ at ] [ V.text "wrong" ])) );
      ( "host_bad_arity",
        define_with_callback (fun ~callback ~at _ ->
            Ok (callback ("keep" $ at) [ Typ.nat $ at ] [])) );
      ( "host_bad_type_arity",
        define_with_callback (fun ~callback ~at value ->
            Ok (callback ("keep" $ at) [] [ value ])) );
      ( "host_unmatched",
        define_with_callback (fun ~callback ~at value ->
            Ok (callback ("positive" $ at) [] [ value ])) );
      ( "probe",
        B.Define.T0.a1 B.Arg.value (fun ~at:_ value ->
            incr probe_calls;
            Ok value) );
      ( "host_probe",
        define_with_callback (fun ~callback ~at value ->
            Ok (callback ("probe" $ at) [] [ value ])) );
      ( "host_local",
        define_with_callback (fun ~callback ~at value ->
            Ok (callback ("f" $ at) [] [ value ])) );
      ( "explode",
        B.Define.T0.a0 (fun ~at ->
            Error (B.Error.runtime at "callback exploded")) );
    ]
end

let spec = H.compile_file "callbacks.spectec"

module Observer = struct
  let static_dependencies = []
  let functions = ref []
  let init ~spec:_ = functions := []

  let handle = function
    | Instrumentation.Event.Func_enter { id; _ } ->
        functions := id :: !functions
    | _ -> ()

  let handle_value = None
  let finish () = ()
end

let check_expired ~name =
  match !saved_callback with
  | None -> failwith "builtin received no callback"
  | Some callback -> (
      match callback ("keep" $ at) [ Typ.nat $ at ] [ nat 0 ] with
      | exception Invalid_argument _ -> ()
      | _ -> failwith ("expired callback was accepted " ^ name))

let check_nested_callbacks () =
  List.iter
    (fun mode ->
      let instrumented_spec =
        match mode with
        | H.Il -> Instrumentation.Static.IlSpec spec.spec_il
        | Sl | Pl -> SlSpec spec.spec_sl
      in
      let config : Instrumentation.Handler.Config.t =
        {
          name = "callback-test";
          modes = [ `IL; `SL; `PL ];
          handler = (module Observer);
          output = Instrumentation.Output.stdout;
        }
      in
      Instrumentation.with_instrumentation [ config ] instrumented_spec
        (fun () ->
          H.check spec
            ~target:(module Target)
            ~modes:[ mode ]
            ~name:"nested callbacks preserve explicit and target state"
            ~relation:"Nested_callbacks"
            ~args:(fun () -> [ state 4 ])
            (H.returns (fun () ->
                 if !Target.probe_calls <> 1 then
                   failwith "callback did not reuse the caller's cache";
                 [ state 4; state 7; nat 2; nat 42 ])));
      if
        not
          (List.mem "next" !Observer.functions
          && List.mem "keep" !Observer.functions)
      then failwith "callback instrumentation is missing")
    [ H.Il; H.Sl; H.Pl ]

let run () =
  check_nested_callbacks ();
  check_expired ~name:"after the builtin returned";
  H.check spec
    ~target:(module Target)
    ~name:"callbacks reject caller-local function names"
    ~relation:"Local_callback"
    ~args:(fun () -> [ nat 0 ])
    (H.fails_with "unknown function f");
  H.check spec
    ~target:(module Target)
    ~name:"callback uses first matching alternative" ~relation:"Callback_choice"
    ~args:(fun () -> [ nat 0 ])
    (H.returns (fun () -> [ nat 1 ]));
  H.check spec
    ~target:(module Target)
    ~check_determinism:true ~name:"callback inherits determinism checking"
    ~relation:"Callback_choice"
    ~args:(fun () -> [ nat 0 ])
    H.fails;
  List.iter
    (fun (relation, message) ->
      H.check spec
        ~target:(module Target)
        ~name:(relation ^ " rejects the callback")
        ~relation
        ~args:(fun () -> [ nat 0 ])
        (H.fails_with message))
    [
      ("Fatal", "callback exploded");
      ("Bad_input", "function keep input");
      ("Bad_arity", "function keep input: arity mismatch");
      ("Bad_type_arity", "function keep input: arity mismatch in type arguments");
    ];
  check_expired ~name:"after the builtin raised";
  H.check spec
    ~target:(module Target)
    ~name:"unmatched function callback fails" ~relation:"Unmatched"
    ~args:(fun () -> [ nat 0 ])
    H.fails
