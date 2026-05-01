module Il = Lang.Il
module Sl = Lang.Sl

type spec = IlSpec of Il.spec | SlSpec of Sl.spec

module type S = sig
  type export_data

  val name : string
  val init : spec -> unit
  val reset : unit -> unit
  val export : unit -> export_data option
  val restore : export_data -> unit
end

let registered : (string, (module S)) Hashtbl.t = Hashtbl.create 16

let register (module M : S) =
  if not (Hashtbl.mem registered M.name) then
    Hashtbl.replace registered M.name (module M : S)

let get name : (module S) option = Hashtbl.find_opt registered name

let init_all spec =
  Hashtbl.iter (fun _name (module M : S) -> M.init spec) registered

let reset_all () =
  Hashtbl.iter (fun _name (module M : S) -> M.reset ()) registered

let export_all () : (string * Marshal.extern_flags list * bytes) list =
  Hashtbl.fold
    (fun name (module M : S) acc ->
      match M.export () with
      | Some data -> (name, [], Marshal.to_bytes data []) :: acc
      | None -> acc)
    registered []

let restore name (data : bytes) =
  match get name with
  | Some (module M : S) ->
      let restored : M.export_data = Marshal.from_bytes data 0 in
      M.restore restored
  | None ->
      failwith
        (Printf.sprintf "Static analysis '%s' not found for restore" name)
