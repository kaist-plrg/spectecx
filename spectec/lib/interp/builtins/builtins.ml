open Lang.Il
open Error
module Error = Error
module Define = Define
module Arg = Arg
module Callback = Callback

let ( let* ) = Result.bind

type 'a result = 'a Error.result

module StringMap = Map.Make (String)

type t = {
  invoke :
    callback:Callback.t -> id -> targ list -> Value.t list -> Value.t result;
  is_builtin : id -> bool;
}

let make (builtins : (string * Define.t) list) : t =
  let funcs : Define.t StringMap.t =
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
  in

  let is_builtin (id : id) : bool = StringMap.mem id.it funcs in

  let invoke ~callback (id : id) (targs : targ list) (args : Value.t list) :
      Value.t Error.result =
    let func = StringMap.find_opt id.it funcs in
    if Option.is_none func then
      Format.asprintf "implementation for builtin %s is missing" id.it
      |> missing_impl id.at |> Result.error
    else
      let func = Option.get func in
      Define.invoke func ~callback ~at:id.at targs args
  in

  { invoke; is_builtin }

let check_result ~opaque_types ~find_typdef_opt ~signature:(tparams, typ) ~targs
    value =
  let* theta =
    Envs_il.Typ.make_theta ~tparams ~targs
    |> Result.map_error (fun _ -> "arity mismatch in builtin type arguments")
  in
  let typ = Envs_il.Typ.subst_typ theta typ in
  Value_typecheck.check ~opaque_types ~find_typdef_opt ~typ value
