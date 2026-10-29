open Common.Source
module Il = Lang.Il
module Origin = Instrumentation_api.Origin

type t = { table : (Il.vid, Origin.Set.t) Hashtbl.t }

let make () = { table = Hashtbl.create 1024 }

let find tracker ~(value : Il.Value.t) =
  Hashtbl.find_opt tracker.table value.note.vid
  |> Option.value ~default:Origin.Set.empty

let add tracker ~(value : Il.Value.t) ~origins =
  if not (Origin.Set.is_empty origins) then
    Hashtbl.replace tracker.table value.note.vid
      (Origin.Set.union (find tracker ~value) origins)

let union_of tracker ~values =
  List.fold_left
    (fun origins value -> Origin.Set.union origins (find tracker ~value))
    Origin.Set.empty values

let children (value : Il.Value.t) : (Origin.step * Il.Value.t) list =
  match value.it with
  | StructV fields ->
      List.map (fun (atom, field) -> (Origin.Field atom.it, field)) fields
  | CaseV valuecase ->
      List.mapi
        (fun index arg -> (Origin.Case index, arg))
        (Il.Mixfix.args valuecase)
  | TupleV values ->
      List.mapi (fun index value -> (Origin.Tuple index, value)) values
  | OptV (Some value) -> [ (Origin.Opt, value) ]
  | ListV values ->
      List.mapi (fun index value -> (Origin.Index index, value)) values
  | BoolV _ | NumV _ | TextV _ | OptV None | FuncV _ | OpaqueV _ -> []

let rec seed tracker ~value ~origin =
  add tracker ~value ~origins:(Origin.Set.singleton origin);
  List.iter
    (fun (step, child) ->
      seed tracker ~value:child ~origin:(Origin.extend origin ~step))
    (children value)

let rec add_within tracker ~value ~origins =
  add tracker ~value ~origins;
  List.iter
    (fun (_, child) -> add_within tracker ~value:child ~origins)
    (children value)

(* A value that already has origins was reported by an earlier event, which
   also accounted for its parts, so the walk stops there and keeps them. *)
let rec add_to_untraced tracker ~value ~origins =
  if Origin.Set.is_empty (find tracker ~value) then (
    add tracker ~value ~origins;
    List.iter
      (fun (_, child) -> add_to_untraced tracker ~value:child ~origins)
      (children value))

let observe tracker : Instrumentation_api.Value_event.t -> unit = function
  | Created { value; origin } -> seed tracker ~value ~origin
  | Derived { value; source } ->
      add tracker ~value ~origins:(find tracker ~value:source)
  | Combined { value; sources } ->
      add tracker ~value ~origins:(union_of tracker ~values:sources)
  | Updated { value; base; element } ->
      add tracker ~value ~origins:(union_of tracker ~values:[ base; element ])
  | Invoked { inputs; outputs; cached = false; _ } ->
      let origins = union_of tracker ~values:inputs in
      if not (Origin.Set.is_empty origins) then
        List.iter
          (fun output -> add_to_untraced tracker ~value:output ~origins)
          outputs
  | Invoked { inputs; outputs; cached = true; _ } ->
      let origins = union_of tracker ~values:inputs in
      List.iter
        (fun output -> add_within tracker ~value:output ~origins)
        outputs

let handler_config tracker : Instrumentation_config.Handler_config.t =
  let module Handler = struct
    let static_dependencies = []
    let init ~spec:_ = Hashtbl.reset tracker.table

    let handle : Instrumentation_api.Event.t -> unit = function
      | Test_start _ -> Hashtbl.reset tracker.table
      | _ -> ()

    let observe_value = Some (observe tracker)
    let finish () = ()
  end in
  {
    name = "origin-tracking";
    modes = [ `IL; `SL; `PL ];
    handler = (module Handler);
    output = Instrumentation_api.Output.stdout;
  }

let origins tracker ~value = Origin.Set.elements (find tracker ~value)
