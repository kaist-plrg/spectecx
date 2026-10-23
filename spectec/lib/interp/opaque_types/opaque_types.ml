open Common.Source
open Lang.Il
module StringMap = Map.Make (String)

type t = Opaque.typdef_as_any StringMap.t

let ( let* ) = Result.bind

let make types =
  List.fold_left
    (fun result (Opaque.Typdef typdef as binding) ->
      let* types = result in
      let name = Opaque.typdef_name typdef in
      if StringMap.mem name types then
        Error (Printf.sprintf "opaque type %s has multiple host bindings" name)
      else Ok (StringMap.add name binding types))
    (Ok StringMap.empty) types

let find types name =
  match StringMap.find_opt name types with
  | Some typdef -> Ok typdef
  | None -> Error (Printf.sprintf "opaque type %s has no host binding" name)

let check_declaration types ~id ~deftyp =
  match deftyp.it with
  | OpaqueT -> Result.map (fun _ -> ()) (find types id.it)
  | _ -> Ok ()

let has_type types ~name ~value =
  match StringMap.find_opt name types with
  | Some (Opaque.Typdef typdef) -> Opaque.has_type value ~typdef
  | None -> false

let check_type types ~name ~value =
  let* (Opaque.Typdef typdef) = find types name in
  if Opaque.has_type value ~typdef then Ok ()
  else
    Error
      (Printf.sprintf
         "opaque value does not match the registered opaque type definition \
          for %s"
         name)
