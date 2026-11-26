module Location = Common.Location
module Il = Lang.Il
module VEnv = Envs_interp.VEnv
open Request

(* Stack frames *)

let stack_trace session request =
  let frames = Engine.frames () in
  let start = max 0 (int_or "startFrame" ~default:0 request) in
  let levels = int_or "levels" ~default:0 request in
  let shown =
    List.filteri
      (fun index _ -> index >= start && (levels <= 0 || index < start + levels))
      frames
  in
  `Assoc
    [
      ( "stackFrames",
        `List
          (List.mapi
             (fun offset (frame : Engine.frame) ->
               `Assoc
                 (("id", `Int (start + offset))
                 :: ("name", `String frame.name)
                 :: Convert.position session frame))
             shown) );
      ("totalFrames", `Int (List.length frames));
    ]

(* Variables *)

(* What a variablesReference stands for: a scope's entries, or a value whose
   parts the client can expand. *)
type container = Entries of (string * Il.Value.t) list | Parts of Il.Value.t

let containers : (int, container) Hashtbl.t = Hashtbl.create 16
let reset () = Hashtbl.reset containers

let new_reference container =
  let reference = Hashtbl.length containers + 1 in
  Hashtbl.replace containers reference container;
  reference

(* The parts of a value, named as the client shows them. *)
let parts (value : Il.Value.t) =
  let indexed values = List.mapi (fun i v -> (string_of_int i, v)) values in
  match value.it with
  | StructV fields ->
      List.map (fun (atom, v) -> (Il.Print.string_of_atom atom, v)) fields
  | CaseV case -> indexed (Il.Mixfix.args case)
  | TupleV values | ListV values -> indexed values
  | OptV (Some v) -> indexed [ v ]
  | OptV None | BoolV _ | NumV _ | TextV _ | FuncV _ | OpaqueV _ -> []

(* The value as the CLI prints it, folded onto one line. *)
let one_line (value : Il.Value.t) =
  Il.Print.string_of_value ~parens:true value
  |> String.split_on_char '\n' |> List.map String.trim
  |> List.filter (( <> ) "")
  |> String.concat " "

(* [value_key] is "value" in a variable and "result" in an evaluation. *)
let describe ~value_key (value : Il.Value.t) =
  [
    (value_key, `String (one_line value));
    ( "type",
      `String
        (Il.Print.string_of_typ
           Common.Annotated.(value.note.typ $ Location.Unknown)) );
    ( "variablesReference",
      `Int (match parts value with [] -> 0 | _ -> new_reference (Parts value))
    );
  ]

let variable (name, value) =
  `Assoc (("name", `String name) :: describe ~value_key:"value" value)

let frame_at index =
  match List.nth_opt (Engine.frames ()) index with
  | Some frame when index >= 0 -> frame
  | _ | (exception Invalid_argument _) -> bad_request "No frame %d." index

let locals (frame : Engine.frame) =
  VEnv.bindings frame.env
  |> List.map (fun (var, value) -> (Envs_interp.Var.to_string var, value))

let scopes request =
  let frame =
    match member "frameId" request with
    | `Int index -> frame_at index
    | _ -> bad_request "`frameId` is not a number."
  in
  let scope name hint entries =
    `Assoc
      [
        ("name", `String name);
        ("presentationHint", `String hint);
        ( "variablesReference",
          `Int
            (match entries with
            | [] -> 0
            | _ -> new_reference (Entries entries)) );
        ("expensive", `Bool false);
      ]
  in
  (* bound in turn, so that references are handed out in the order shown *)
  let inputs =
    scope "Inputs" "arguments"
      (List.mapi (fun i v -> (string_of_int i, v)) frame.inputs)
  in
  let locals = scope "Locals" "locals" (locals frame) in
  `Assoc [ ("scopes", `List [ inputs; locals ]) ]

let variables request =
  let reference =
    match member "variablesReference" request with
    | `Int reference -> reference
    | _ -> bad_request "`variablesReference` is not a number."
  in
  let entries =
    match Hashtbl.find_opt containers reference with
    | Some (Entries entries) -> entries
    | Some (Parts value) -> parts value
    | None -> bad_request "No variables with reference %d." reference
  in
  `Assoc [ ("variables", `List (List.map variable entries)) ]

let evaluate request =
  let expression =
    match member "expression" request with
    | `String expression -> String.trim expression
    | _ -> bad_request "`expression` is not a string."
  in
  let frame = frame_at (int_or "frameId" ~default:0 request) in
  let value =
    match Engine.find_local frame expression with
    | [ (_, value) ] -> value
    | [] -> bad_request "No local `%s`." expression
    | several ->
        bad_request "`%s` could be %s; give the full name." expression
          (String.concat " or "
             (List.map (fun (var, _) -> Envs_interp.Var.to_string var) several))
  in
  `Assoc (describe ~value_key:"result" value)
