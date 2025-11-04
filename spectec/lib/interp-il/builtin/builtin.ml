open Il
open Error

let ( let* ) = Result.bind

module StringMap = Map.Make (String)

let funcs : Define.t StringMap.t =
  let builtins =
    [
      Nats.builtins;
      Texts.builtins;
      Lists.builtins;
      Sets.builtins;
      Maps.builtins;
      Numerics.builtins;
      Fresh.builtins;
    ]
    |> List.concat
  in
  (* Check for duplicates *)
  let map, dups =
    List.fold_left
      (fun (m, dups) (name, def) ->
        if StringMap.mem name m then (m, name :: dups)
        else (StringMap.add name def m, dups))
      (StringMap.empty, []) builtins
  in
  if dups = [] then map
  else
    let dup_list = String.concat ", " (List.rev dups) in
    let msg =
      Printf.sprintf "Duplicate builtin function definitions: %s" dup_list
    in
    failwith msg

let is_builtin (id : id) : bool = StringMap.mem id.it funcs

let invoke (id : id) (targs : targ list) (args : value list) : value =
  let func = StringMap.find_opt id.it funcs in
  check (Option.is_some func) id.at
    (Format.asprintf "implementation for builtin %s is missing" id.it);
  let func = Option.get func in
  let result = func ~at:id.at targs args in
  match result with
  | Ok v -> v
  | Error err ->
      let at, msg =
        match err with
        | Err.TypeError (at, msg, v) ->
            (at, Printf.sprintf "%s (got: %s)" msg (Value.to_string v))
        | Err.RuntimeError (at, msg) -> (at, msg)
        | Err.ArityError (at, msg) -> (at, msg)
      in
      error at msg
