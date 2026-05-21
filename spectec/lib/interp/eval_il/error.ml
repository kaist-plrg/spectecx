open Common.Source

exception InterpError of region * string
exception StepLimitExceeded

(* Interpreter errors *)

let error (at : region) (msg : string) = raise (InterpError (at, msg))
let warn (at : region) (msg : string) = Diag.warn at "il-interp" msg

(* Builtin errors *)

let unwrap_builtin (result : 'a Builtins.result) : 'a =
  match result with
  | Ok res -> res
  | Error err ->
      let at, msg =
        match err with
        | Builtins.Error.TypeError (at, expected, v) ->
            ( at,
              Printf.sprintf "Builtin type error: expected %s, got %s" expected
                (Lang.Il.Value.to_string v) )
        | Builtins.Error.RuntimeError (at, msg) ->
            (at, Printf.sprintf "Builtin arity error: %s" msg)
        | Builtins.Error.ArityError (at, msg) ->
            (at, Printf.sprintf "Builtin runtime error: %s" msg)
        | Builtins.Error.MissingImplError (at, msg) ->
            (at, Printf.sprintf "Builtin missing implementation: %s" msg)
      in
      error at msg

(* Check *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at msg

let check_warn (b : bool) (at : region) (msg : string) : unit =
  if not b then warn at msg

(* Formatting *)

type error = region * string

let to_string ((at, msg) : error) = Common.Error.string_of_located_error at msg
