(* Static analysis registry for instrumentation.
 *
 * Provides a framework for static analyses that run once during initialization
 * and can be shared across multiple handlers. Handlers register their static
 * dependencies during make(), and the dispatcher initializes all registered
 * static analyses before calling handler init() methods.
 *)

module Il = Lang.Il
module Sl = Lang.Sl

(* Spec type - duplicated here to avoid circular dependency with Handler *)
type spec = IlSpec of Il.spec | SlSpec of Sl.spec

(* Signature for static analysis modules *)
module type S = sig
  type export_data

  val name : string
  val init : spec -> unit
  val reset : unit -> unit

  (* Optional: for checkpointing - export returns None if not checkpointable *)
  val export : unit -> export_data option
  val restore : export_data -> unit
end

(* Registry of static analyses *)
let registered : (string, (module S)) Hashtbl.t = Hashtbl.create 16

(* Register a static analysis module (idempotent - safe to call multiple times) *)
let register (module M : S) =
  if Hashtbl.mem registered M.name then
    (* Already registered - skip (enables automatic deduplication) *)
    ()
  else Hashtbl.replace registered M.name (module M : S)

(* Get a registered static analysis by name *)
let get name : (module S) option = Hashtbl.find_opt registered name

(* Initialize all registered static analyses *)
let init_all spec =
  Hashtbl.iter (fun _name (module M : S) -> M.init spec) registered

(* Reset all registered static analyses *)
let reset_all () =
  Hashtbl.iter (fun _name (module M : S) -> M.reset ()) registered

(* Export state from all registered static analyses (for checkpointing)
   Returns a list of (name, Marshal.t) pairs for serialization *)
let export_all () : (string * Marshal.extern_flags list * bytes) list =
  Hashtbl.fold
    (fun name (module M : S) acc ->
      match M.export () with
      | Some data ->
          let serialized = Marshal.to_bytes data [] in
          (name, [], serialized) :: acc
      | None -> acc)
    registered []

(* Restore state for a specific static analysis (for checkpointing) *)
let restore name (data : bytes) =
  match get name with
  | Some (module M : S) ->
      let restored : M.export_data = Marshal.from_bytes data 0 in
      M.restore restored
  | None ->
      failwith
        (Printf.sprintf "Static analysis '%s' not found for restore" name)
