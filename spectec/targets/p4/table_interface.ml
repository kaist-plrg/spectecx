open Lang.Il
open Common.Annotated
module Location = Common.Location

let ( let* ) = Result.bind
let fail message = Error (Spectec.Error.TaskError (Location.Unknown, message))
let unexpected relation = fail ("unexpected outputs from relation " ^ relation)
let typ name = Typ.var name [] $ Location.Unknown

let option_of_value (value : Value.t) =
  match value.it with
  | OptV value -> Ok value
  | _ -> fail ("expected an optional value: " ^ Value.to_string value)

let value_of_key_value (key_value : Stf.key_value) =
  let case = Case.case_v ~var:"tableKeyValueInterface" in
  match key_value with
  | Hex digits -> case [ Case.tag "HEX"; Case.arg (Value.text digits) ]
  | Binary digits -> case [ Case.tag "BIN"; Case.arg (Value.text digits) ]
  | Decimal digits -> case [ Case.tag "DEC"; Case.arg (Value.text digits) ]
  | Prefix { hex; length } ->
      case
        [
          Case.arg (Value.text ("0x" ^ hex));
          Case.tag "SLASH";
          Case.arg (Value.nat (Bigint.of_int length));
        ]

let value_of_keys keys =
  keys
  |> List.map (fun (name, value) -> Value.tuple [ name; value ])
  |> Value.list (typ "tableKeyInterface")

let value_of_action ({ name; args } : Stf.action) =
  let args =
    List.map
      (fun (arg, number) -> Value.tuple [ Value.text arg; Value.int number ])
      args
  in
  Value.tuple
    [ Value.text name; Value.list (typ "tableActionArgumentInterface") args ]

let value_of_priority priority =
  Option.map Value.int priority |> Value.opt (Typ.int $ Location.Unknown)

type table_id = Qualified of Value.t | Unqualified of Value.t

let find_table ~eval_func arch ~table_name =
  let names = String.split_on_char '.' table_name in
  let unqualified = Value.text (List.hd (List.rev names)) in
  let find_unqualified () =
    let* table = eval_func "find_object_unqualified_e" [ arch; unqualified ] in
    let* table = option_of_value table in
    match table with
    | Some table -> Ok (table, Unqualified unqualified)
    | None -> fail ("unknown table " ^ table_name)
  in
  match names with
  | [ _ ] -> find_unqualified ()
  | _ -> (
      let qualified = Value.list (typ "nameIR") (List.map Value.text names) in
      let* table = eval_func "find_object_qualified_e" [ arch; qualified ] in
      let* table = option_of_value table in
      match table with
      | Some table -> Ok (table, Qualified qualified)
      | None -> find_unqualified ())

let update_table ~eval_func arch ~table_id ~table =
  match table_id with
  | Qualified id -> eval_func "update_object_qualified_e" [ arch; id; table ]
  | Unqualified id ->
      eval_func "update_object_unqualified_e" [ arch; id; table ]

(* Selector keys take no value from STF. *)
let required_key_names ~eval_func table =
  let* keys = eval_func "key_interface_of_tableObject" [ table ] in
  let add_key_name (key : Value.t) names =
    let* names = names in
    match key.it with
    | TupleV [ _; { it = TextV "selector"; _ }; _ ] -> Ok names
    | TupleV [ name; _; _ ] -> Ok (name :: names)
    | _ -> fail ("unexpected table key " ^ Value.to_string key)
  in
  match keys.it with
  | ListV keys -> List.fold_right add_key_name keys (Ok [])
  | _ -> fail ("unexpected table keys " ^ Value.to_string keys)

let keys_for_table ~eval_func ~table ~table_name
    (keys : (string * Stf.key_value) list) =
  let* required = required_key_names ~eval_func table in
  let values = List.map (fun (_, value) -> value_of_key_value value) keys in
  let names = List.map fst keys in
  if
    List.sort_uniq String.compare names
    = List.sort_uniq String.compare (List.map Value.get_text required)
  then Ok (List.combine (List.map Value.text names) values)
  else if List.length required = List.length values then
    Ok (List.combine required values)
  else
    fail
      (Printf.sprintf "table %s has %d keys but the entry supplies %d"
         table_name (List.length required) (List.length values))

let add_entry ~eval_rel ~eval_func ~ctx arch ~table_name ~priority ~keys ~action
    =
  let* table, table_id = find_table ~eval_func arch ~table_name in
  (* SL and PL stop with an error when a relation premise of
     [TableObject_add_entry/ok] matches no rule, instead of applying
     [TableObject_add_entry/fail]. [keys_for_table] therefore picks the key
     names before the call. *)
  let* keys = keys_for_table ~eval_func ~table ~table_name keys in
  let* outputs =
    eval_rel "TableObject_add_entry"
      [
        ctx;
        table;
        value_of_priority priority;
        value_of_keys keys;
        value_of_action action;
      ]
  in
  let* table =
    match outputs with
    | [ table ] -> option_of_value table
    | _ -> unexpected "TableObject_add_entry"
  in
  match table with
  | Some table -> update_table ~eval_func arch ~table_id ~table
  | None -> fail ("table " ^ table_name ^ " rejected the entry")

let add_default_action ~eval_rel ~eval_func ~ctx arch ~table_name ~action =
  let* table, table_id = find_table ~eval_func arch ~table_name in
  let* outputs =
    eval_rel "TableObject_add_default_action"
      [ ctx; table; value_of_action action ]
  in
  match outputs with
  | [ table ] -> update_table ~eval_func arch ~table_id ~table
  | _ -> unexpected "TableObject_add_default_action"
