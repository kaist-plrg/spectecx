module Location = Common.Location
open Lang.Il

type implementation =
  at:Location.t -> targ list -> value list -> Value.t Error.result

type t =
  | Plain of implementation
  | With_callback of (Callback.t -> implementation)

let make implementation = Plain implementation

let with_active (callback : Callback.t) ~f =
  let active = ref true in
  let check_active () =
    if not !active then invalid_arg "builtin callback is no longer active"
  in
  let scoped : Callback.t =
   fun id targs values ->
    check_active ();
    callback id targs values
  in
  Fun.protect (fun () -> f scoped) ~finally:(fun () -> active := false)

let invoke t ~callback ~at targs values =
  match t with
  | Plain implementation -> implementation ~at targs values
  | With_callback implementation ->
      with_active callback ~f:(fun callback ->
          implementation callback ~at targs values)

let make_with_callback ~args impl =
  With_callback (fun callback -> invoke (args (impl ~callback)) ~callback)

let ( let* ) = Result.bind

module Extract = struct
  let extract_targs (n : int) (at : Location.t) (targs : targ list) :
      (unit, Error.t) result =
    if List.length targs = n then Ok ()
    else
      Error
        (Error.arity at
           (Printf.sprintf "Expected %d type arguments, got %d" n
              (List.length targs)))

  let extract_values (n : int) (at : Location.t) (values : value list) :
      (value list, Error.t) result =
    if List.length values = n then Ok values
    else
      Error
        (Error.arity at
           (Printf.sprintf "Expected %d value arguments, got %d" n
              (List.length values)))

  let extract ~(targs_num : int) ~(args_num : int) (at : Location.t)
      (targs : targ list) (values : value list) :
      (unit * value list, Error.t) result =
    let* () = extract_targs targs_num at targs in
    let* vs = extract_values args_num at values in
    Ok ((), vs)
end

module T0 = struct
  let a0 (impl : at:Location.t -> (Value.t, Error.t) result) : t =
    make @@ fun ~at targs values ->
    let* (), _ = Extract.extract ~targs_num:0 ~args_num:0 at targs values in
    impl ~at

  let a1 (p1 : 'a Arg.t)
      (impl : at:Location.t -> 'a -> (Value.t, Error.t) result) : t =
    make @@ fun ~at targs values ->
    let* (), vs = Extract.extract ~targs_num:0 ~args_num:1 at targs values in
    match vs with
    | [ v1 ] ->
        let* arg1 = p1 at v1 in
        impl ~at arg1
    | _ ->
        Error
          (Error.arity at
             (Printf.sprintf "Expected 1 argument, got %d" (List.length vs)))

  let a2 (p1 : 'a Arg.t) (p2 : 'b Arg.t)
      (impl : at:Location.t -> 'a -> 'b -> (Value.t, Error.t) result) : t =
    make @@ fun ~at targs values ->
    let* (), vs = Extract.extract ~targs_num:0 ~args_num:2 at targs values in
    match vs with
    | [ v1; v2 ] ->
        let* arg1 = p1 at v1 in
        let* arg2 = p2 at v2 in
        impl ~at arg1 arg2
    | _ ->
        Error
          (Error.arity at
             (Printf.sprintf "Expected 2 arguments, got %d" (List.length vs)))

  let a3 (p1 : 'a Arg.t) (p2 : 'b Arg.t) (p3 : 'c Arg.t)
      (impl : at:Location.t -> 'a -> 'b -> 'c -> (Value.t, Error.t) result) : t
      =
    make @@ fun ~at targs values ->
    let* (), vs = Extract.extract ~targs_num:0 ~args_num:3 at targs values in
    match vs with
    | [ v1; v2; v3 ] ->
        let* arg1 = p1 at v1 in
        let* arg2 = p2 at v2 in
        let* arg3 = p3 at v3 in
        impl ~at arg1 arg2 arg3
    | _ ->
        Error
          (Error.arity at
             (Printf.sprintf "Expected 3 arguments, got %d" (List.length vs)))

  let a4 (p1 : 'a Arg.t) (p2 : 'b Arg.t) (p3 : 'c Arg.t) (p4 : 'd Arg.t)
      (impl :
        at:Location.t -> 'a -> 'b -> 'c -> 'd -> (Value.t, Error.t) result) : t
      =
    make @@ fun ~at targs values ->
    let* (), vs = Extract.extract ~targs_num:0 ~args_num:4 at targs values in
    match vs with
    | [ v1; v2; v3; v4 ] ->
        let* arg1 = p1 at v1 in
        let* arg2 = p2 at v2 in
        let* arg3 = p3 at v3 in
        let* arg4 = p4 at v4 in
        impl ~at arg1 arg2 arg3 arg4
    | _ ->
        Error
          (Error.arity at
             (Printf.sprintf "Expected 4 arguments, got %d" (List.length vs)))

  let a5 (p1 : 'a Arg.t) (p2 : 'b Arg.t) (p3 : 'c Arg.t) (p4 : 'd Arg.t)
      (p5 : 'e Arg.t)
      (impl :
        at:Location.t -> 'a -> 'b -> 'c -> 'd -> 'e -> (Value.t, Error.t) result)
      : t =
    make @@ fun ~at targs values ->
    let* (), vs = Extract.extract ~targs_num:0 ~args_num:5 at targs values in
    match vs with
    | [ v1; v2; v3; v4; v5 ] ->
        let* arg1 = p1 at v1 in
        let* arg2 = p2 at v2 in
        let* arg3 = p3 at v3 in
        let* arg4 = p4 at v4 in
        let* arg5 = p5 at v5 in
        impl ~at arg1 arg2 arg3 arg4 arg5
    | _ ->
        Error
          (Error.arity at
             (Printf.sprintf "Expected 5 arguments, got %d" (List.length vs)))
end

module T1 = struct
  let a1 (p1 : 'a Arg.t)
      (impl : at:Location.t -> targ -> 'a -> (Value.t, Error.t) result) : t =
    make @@ fun ~at targs values ->
    let* (), vs = Extract.extract ~targs_num:1 ~args_num:1 at targs values in
    match (targs, vs) with
    | [ targ1 ], [ v1 ] ->
        let* arg1 = p1 at v1 in
        impl ~at targ1 arg1
    | _ ->
        Error
          (Error.arity at
             (Printf.sprintf
                "Expected 1 type argument and 1 value argument, got %d type \
                 arguments and %d value arguments"
                (List.length targs) (List.length vs)))

  let a2 (p1 : 'a Arg.t) (p2 : 'b Arg.t)
      (impl : at:Location.t -> targ -> 'a -> 'b -> (Value.t, Error.t) result) :
      t =
    make @@ fun ~at targs values ->
    let* (), vs = Extract.extract ~targs_num:1 ~args_num:2 at targs values in
    match (targs, vs) with
    | [ targ1 ], [ v1; v2 ] ->
        let* arg1 = p1 at v1 in
        let* arg2 = p2 at v2 in
        impl ~at targ1 arg1 arg2
    | _ ->
        Error
          (Error.arity at
             (Printf.sprintf
                "Expected 1 type argument and 2 value arguments, got %d type \
                 arguments and %d value arguments"
                (List.length targs) (List.length vs)))

  let a3 (p1 : 'a Arg.t) (p2 : 'b Arg.t) (p3 : 'c Arg.t)
      (impl :
        at:Location.t -> targ -> 'a -> 'b -> 'c -> (Value.t, Error.t) result) :
      t =
    make @@ fun ~at targs values ->
    let* (), vs = Extract.extract ~targs_num:1 ~args_num:3 at targs values in
    match (targs, vs) with
    | [ targ1 ], [ v1; v2; v3 ] ->
        let* arg1 = p1 at v1 in
        let* arg2 = p2 at v2 in
        let* arg3 = p3 at v3 in
        impl ~at targ1 arg1 arg2 arg3
    | _ ->
        Error
          (Error.arity at
             (Printf.sprintf
                "Expected 1 type argument and 3 value arguments, got %d type \
                 arguments and %d value arguments"
                (List.length targs) (List.length vs)))
end

module T2 = struct
  let a1 (p1 : 'a Arg.t)
      (impl : at:Location.t -> targ -> targ -> 'a -> (Value.t, Error.t) result)
      : t =
    make @@ fun ~at targs values ->
    let* (), vs = Extract.extract ~targs_num:2 ~args_num:1 at targs values in
    match (targs, vs) with
    | [ targ1; targ2 ], [ v1 ] ->
        let* arg1 = p1 at v1 in
        impl ~at targ1 targ2 arg1
    | _ ->
        Error
          (Error.arity at
             (Printf.sprintf
                "Expected 2 type arguments and 1 value argument, got %d type \
                 arguments and %d value arguments"
                (List.length targs) (List.length vs)))

  let a2 (p1 : 'a Arg.t) (p2 : 'b Arg.t)
      (impl :
        at:Location.t -> targ -> targ -> 'a -> 'b -> (Value.t, Error.t) result)
      : t =
    make @@ fun ~at targs values ->
    let* (), vs = Extract.extract ~targs_num:2 ~args_num:2 at targs values in
    match (targs, vs) with
    | [ targ1; targ2 ], [ v1; v2 ] ->
        let* arg1 = p1 at v1 in
        let* arg2 = p2 at v2 in
        impl ~at targ1 targ2 arg1 arg2
    | _ ->
        Error
          (Error.arity at
             (Printf.sprintf
                "Expected 2 type arguments and 2 value arguments, got %d type \
                 arguments and %d value arguments"
                (List.length targs) (List.length vs)))

  let a3 (p1 : 'a Arg.t) (p2 : 'b Arg.t) (p3 : 'c Arg.t)
      (impl :
        at:Location.t ->
        targ ->
        targ ->
        'a ->
        'b ->
        'c ->
        (Value.t, Error.t) result) : t =
    make @@ fun ~at targs values ->
    let* (), vs = Extract.extract ~targs_num:2 ~args_num:3 at targs values in
    match (targs, vs) with
    | [ targ1; targ2 ], [ v1; v2; v3 ] ->
        let* arg1 = p1 at v1 in
        let* arg2 = p2 at v2 in
        let* arg3 = p3 at v3 in
        impl ~at targ1 targ2 arg1 arg2 arg3
    | _ ->
        Error
          (Error.arity at
             (Printf.sprintf
                "Expected 2 type arguments and 3 value arguments, got %d type \
                 arguments and %d value arguments"
                (List.length targs) (List.length vs)))
end
