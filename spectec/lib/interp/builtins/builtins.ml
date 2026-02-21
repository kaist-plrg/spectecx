open Lang.Il
open Error
module Error = Error
module Define = Define
module Arg = Arg

let ( let* ) = Result.bind

type 'a result = 'a Error.result

module StringMap = Map.Make (String)

type t = {
  invoke : id -> targ list -> Value.t list -> Value.t result;
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

  let invoke (id : id) (targs : targ list) (args : Value.t list) :
      Value.t Error.result =
    let func = StringMap.find_opt id.it funcs in
    if Option.is_none func then
      Format.asprintf "implementation for builtin %s is missing" id.it
      |> missing_impl id.at |> Result.error
    else
      let func = Option.get func in
      func ~at:id.at targs args
  in

  { invoke; is_builtin }
