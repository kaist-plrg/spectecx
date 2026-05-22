type subject = Rel of El.id' | Func of El.id'

module Key = struct
  type t = string * subject

  let compare = compare
end

module AlterMap = Map.Make (Key)
module RelMap = Map.Make (String)

type t = { alter : Alter.t AlterMap.t; rel_inputs : Input.t RelMap.t }

let empty : t = { alter = AlterMap.empty; rel_inputs = RelMap.empty }

let add_alter (henv : t) ~(hid : string) ~(subject : subject) (hint : Alter.t) :
    t =
  { henv with alter = AlterMap.add (hid, subject) hint henv.alter }

let add_rel_inputs (henv : t) ~(rel : El.id') (inputs : Input.t) : t =
  { henv with rel_inputs = RelMap.add rel inputs henv.rel_inputs }

let find_alter (henv : t) ~(hid : string) ~(subject : subject) : Alter.t option
    =
  AlterMap.find_opt (hid, subject) henv.alter

let find_rel_inputs (henv : t) ~(rel : El.id') : Input.t option =
  RelMap.find_opt rel henv.rel_inputs

let load_alter_hint (henv : t) (subject : subject) (hint : El.hint) : t =
  let El.{ hintid; hintexp } = hint in
  match Registry.lookup hintid.it with
  | Some { kind = Registry.Alter; _ } ->
      add_alter henv ~hid:hintid.it ~subject (Alter.parse hintexp)
  | _ -> henv

let load_alter_hints (henv : t) (subject : subject) (hints : El.hint list) : t =
  List.fold_left (fun henv hint -> load_alter_hint henv subject hint) henv hints

let load_rel_inputs (henv : t) (rel : El.id') (hints : El.hint list) : t =
  List.fold_left
    (fun henv El.{ hintid; hintexp } ->
      if hintid.it = "input" then
        match Input.parse hintexp with
        | Some inputs -> add_rel_inputs henv ~rel inputs
        | None -> henv
      else henv)
    henv hints

let load_def (henv : t) (def : El.def) : t =
  match def.it with
  | El.RelD (id, _, hints) ->
      let henv = load_rel_inputs henv id.it hints in
      load_alter_hints henv (Rel id.it) hints
  | El.BuiltinDecD (id, _, _, _, hints) | El.DecD (id, _, _, _, hints) ->
      load_alter_hints henv (Func id.it) hints
  | _ -> henv

let of_el_spec (spec : El.spec) : t = List.fold_left load_def empty spec
