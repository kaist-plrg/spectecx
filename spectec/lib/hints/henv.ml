type subject = Rel of El.id' | Func of El.id'

module Key = struct
  type t = string * subject

  let compare = compare
end

module AlterMap = Map.Make (Key)
module RelMap = Map.Make (String)

module MixopMap = Map.Make (struct
  type t = unit Il.Mixfix.t

  let compare = Il.Mixfix.compare_mixop
end)

module HidMixopMap = Map.Make (struct
  type t = string * unit Il.Mixfix.t

  let compare (hid_a, mx_a) (hid_b, mx_b) =
    let c = String.compare hid_a hid_b in
    if c <> 0 then c else Il.Mixfix.compare_mixop mx_a mx_b
end)

type t = {
  alter : Alter.t AlterMap.t;
  rel_inputs : Input.t RelMap.t;
  alter_typcase : Alter.t HidMixopMap.t;
  fields : Fields.t MixopMap.t;
}

let empty : t =
  {
    alter = AlterMap.empty;
    rel_inputs = RelMap.empty;
    alter_typcase = HidMixopMap.empty;
    fields = MixopMap.empty;
  }

let add_alter (henv : t) ~(hid : string) ~(subject : subject) (hint : Alter.t) :
    t =
  { henv with alter = AlterMap.add (hid, subject) hint henv.alter }

let add_rel_inputs (henv : t) ~(rel : El.id') (inputs : Input.t) : t =
  { henv with rel_inputs = RelMap.add rel inputs henv.rel_inputs }

let add_alter_typcase (henv : t) ~(hid : string) ~(mixop : unit Il.Mixfix.t)
    (hint : Alter.t) : t =
  {
    henv with
    alter_typcase = HidMixopMap.add (hid, mixop) hint henv.alter_typcase;
  }

let add_fields (henv : t) ~(mixop : unit Il.Mixfix.t) (hint : Fields.t) : t =
  { henv with fields = MixopMap.add mixop hint henv.fields }

let find_alter (henv : t) ~(hid : string) ~(subject : subject) : Alter.t option
    =
  AlterMap.find_opt (hid, subject) henv.alter

let find_rel_inputs (henv : t) ~(rel : El.id') : Input.t option =
  RelMap.find_opt rel henv.rel_inputs

let find_alter_typcase (henv : t) ~(hid : string) ~(mixop : unit Il.Mixfix.t) :
    Alter.t option =
  HidMixopMap.find_opt (hid, mixop) henv.alter_typcase

let find_fields (henv : t) ~(mixop : unit Il.Mixfix.t) : Fields.t option =
  MixopMap.find_opt mixop henv.fields

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

(* IL pass: load Typcase hints by mixop. *)

let load_typcase_hint (henv : t) (mixop : unit Il.Mixfix.t) (hint : Il.hint) : t
    =
  let Il.{ hintid; hintexp } = hint in
  match Registry.lookup hintid.it with
  | Some { kind = Registry.Alter; _ } ->
      add_alter_typcase henv ~hid:hintid.it ~mixop (Alter.parse hintexp)
  | Some { kind = Registry.Fields; _ } -> (
      match Fields.parse hintexp with
      | Some f -> add_fields henv ~mixop f
      | None -> henv)
  | _ -> henv

let load_typcase (henv : t) (typcase : Il.typcase) : t =
  let mixop = Il.Mixfix.to_mixop typcase.notation.it in
  List.fold_left
    (fun henv h -> load_typcase_hint henv mixop h)
    henv typcase.hints

let load_deftyp (henv : t) (deftyp : Il.deftyp) : t =
  match deftyp.it with
  | Il.VariantT typcases -> List.fold_left load_typcase henv typcases
  | _ -> henv

let load_il_def (henv : t) (def : Il.def) : t =
  match def.it with
  | Il.TypD { deftyp; _ } -> load_deftyp henv deftyp
  | _ -> henv

let load_il_spec (henv : t) (spec : Il.spec) : t =
  List.fold_left load_il_def henv spec
