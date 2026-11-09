open Xl
open Types
open Common.Print
open Common.Annotated

let string_of_defid defid = "$" ^ defid.it
let string_of_atom atom = Atom.to_string atom.it
let string_of_mixop mixop = Mixfix.to_string mixop

let rec string_of_value ?(short = false) ?(level = 0) value =
  match value.it with
  | BoolV b -> string_of_bool b
  | NumV n -> Num.string_of_num n
  | TextV s -> Printf.sprintf {|"%s"|} s
  | StructV [] -> "{}"
  | StructV valuefields when short ->
      Format.asprintf "{ .../%d }" (List.length valuefields)
  | StructV valuefields ->
      Format.asprintf "{ %s }"
        (String.concat ";\n"
           (List.mapi
              (fun idx (atom, value) ->
                let indent = if idx = 0 then "" else indent (level + 1) in
                Format.asprintf "%s%s %s" indent (string_of_atom atom)
                  (string_of_value ~short ~level:(level + 2) value))
              valuefields))
  | CaseV vc when short -> string_of_mixop (Mixfix.to_mixop vc)
  | CaseV vc -> "(" ^ string_of_notval vc ^ ")"
  | TupleV values ->
      Format.asprintf "(%s)"
        (String.concat ", "
           (List.map (string_of_value ~short ~level:(level + 1)) values))
  | OptV (Some value) ->
      Format.asprintf "Some(%s)"
        (string_of_value ~short ~level:(level + 1) value)
  | OptV None -> "None"
  | ListV [] -> "[]"
  | ListV values when short -> Format.asprintf "[ .../%d ]" (List.length values)
  | ListV values ->
      Format.asprintf "[ %s ]"
        (String.concat ",\n"
           (List.mapi
              (fun idx value ->
                let indent = if idx = 0 then "" else indent (level + 1) in
                indent ^ string_of_value ~short ~level:(level + 2) value)
              values))
  | FuncV id -> string_of_defid id
  | OpaqueV value -> "<opaque " ^ Opaque.name value ^ ">"

and string_of_notval notval =
  Mixfix.render ~pad_brackets:true ~string_of_atom
    ~string_of_arg:string_of_value notval
