open Common.Annotated
module Location = Common.Location
open Types

(* Numbers *)

let string_of_num num = Il.Print.string_of_num num

(* Texts *)

let string_of_text text = Il.Print.string_of_text text

(* Identifiers *)

let string_of_varid varid = Il.Print.string_of_varid varid
let string_of_typid typid = Il.Print.string_of_typid typid
let string_of_relid relid = Il.Print.string_of_relid relid
let string_of_ruleid ruleid = Il.Print.string_of_ruleid ruleid
let string_of_defid defid = Il.Print.string_of_defid defid

(* Atoms *)

let string_of_atom atom = Il.Print.string_of_atom atom
let string_of_atoms atoms = atoms |> List.map string_of_atom |> String.concat ""

(* Mixfix operators *)

let string_of_mixop mixop = Il.Print.string_of_mixop mixop

(* Iterators *)

let string_of_iter iter = Il.Print.string_of_iter iter

(* Variables *)

let string_of_var var = Il.Print.string_of_var var

(* Types *)

let string_of_typ typ = Il.Print.string_of_typ typ
let string_of_typs sep typs = Il.Print.string_of_typs sep typs
let string_of_nottyp nottyp = Il.Print.string_of_nottyp nottyp
let string_of_deftyp deftyp = Il.Print.string_of_deftyp deftyp
let string_of_typfield typfield = Il.Print.string_of_typfield typfield

let string_of_typfields sep typfields =
  Il.Print.string_of_typfields sep typfields

let string_of_typcase typcase = Il.Print.string_of_typcase typcase
let string_of_typcases sep typcases = Il.Print.string_of_typcases sep typcases

(* Values *)

let string_of_vid vid = "@" ^ string_of_int vid

let string_of_value ?(short = false) ?(level = 0) value =
  Il.Print.string_of_value ~short ~level value

(* Operators *)

let string_of_unop unop = Il.Print.string_of_unop unop
let string_of_binop binop = Il.Print.string_of_binop binop
let string_of_cmpop cmpop = Il.Print.string_of_cmpop cmpop

(* Expressions *)

let string_of_exp exp = Il.Print.string_of_exp exp
let string_of_exps sep exps = Il.Print.string_of_exps sep exps
let string_of_notexp notexp = Il.Print.string_of_notexp notexp
let string_of_iterexp iterexp = Il.Print.string_of_iterexp iterexp
let string_of_iterexps iterexps = Il.Print.string_of_iterexps iterexps

let string_of_iterinstr (iterinstr : iterinstr) =
  Il.Print.string_of_iter iterinstr.iter

let string_of_iterinstrs iterinstrs =
  iterinstrs |> List.map string_of_iterinstr |> String.concat ""

(* Patterns *)

let string_of_pattern pattern = Il.Print.string_of_pattern pattern

(* Paths *)

let string_of_path path = Il.Print.string_of_path path

(* Parameters *)

let string_of_param param = Il.Print.string_of_param param
let string_of_params params = Il.Print.string_of_params params

(* Type parameters *)

let string_of_tparam tparam = Il.Print.string_of_tparam tparam
let string_of_tparams tparams = Il.Print.string_of_tparams tparams

(* Arguments *)

let string_of_arg arg = Il.Print.string_of_arg arg
let string_of_args args = Il.Print.string_of_args args

(* Type arguments *)

let string_of_targ targ = Il.Print.string_of_targ targ
let string_of_targs targs = Il.Print.string_of_targs targs

(* Path conditions *)

let string_of_pid pid = Format.asprintf "Phantom#%d" pid

let string_of_phantom phantom =
  let pid, _ = phantom in
  string_of_pid pid

let rec string_of_pathcond pathcond =
  match pathcond with
  | ForallC (pathcond, iterexps) ->
      Format.asprintf "(forall %s)%s"
        (string_of_pathcond pathcond)
        (string_of_iterexps iterexps)
  | ExistsC (pathcond, iterexps) ->
      Format.asprintf "(exists %s)%s"
        (string_of_pathcond pathcond)
        (string_of_iterexps iterexps)
  | PlainC exp -> "(" ^ string_of_exp exp ^ ")"
  | RelAssertC { call = { relid; notexp }; expect } ->
      Format.asprintf "(%s: %s %s)" (string_of_relid relid)
        (string_of_notexp notexp)
        (if expect then "holds" else "does not hold")

let string_of_pathconds pathconds =
  List.map string_of_pathcond pathconds |> String.concat " /\\ "

(* Case analysis *)

let string_of_guard guard =
  match guard with
  | BoolG b -> string_of_bool b
  | CmpG (cmpop, _, exp) ->
      "(% " ^ string_of_cmpop cmpop ^ " " ^ string_of_exp exp ^ ")"
  | SubG typ -> "(% <: " ^ string_of_typ typ ^ ")"
  | MatchG pattern -> "(% matches " ^ string_of_pattern pattern ^ ")"
  | MemG exp -> "(% <- " ^ string_of_exp exp ^ ")"

let rec string_of_case ?(level = 0) ?(index = 0) case =
  let indent = String.make (level * 2) ' ' in
  let order = Format.asprintf "%s%d. " indent index in
  let guard, instrs = case in
  Format.asprintf "%sCase %s\n\n%s" order (string_of_guard guard)
    (string_of_instrs ~level:(level + 1) instrs)

and string_of_cases ?(level = 0) cases =
  cases
  |> List.mapi (fun idx case -> string_of_case ~level ~index:(idx + 1) case)
  |> String.concat "\n\n"

(* Instructions *)

and string_of_instr ?(short = false) ?(level = 0) ?(index = 0) instr =
  if short then string_of_instr_short instr
  else string_of_instr_full ~level ~index instr

and string_of_instr_short instr =
  match instr.it with
  | RelI { call = { relid; notexp }; iterinstrs; block = _ } ->
      Format.asprintf "(%s: %s)%s" (string_of_relid relid)
        (string_of_notexp notexp)
        (string_of_iterinstrs iterinstrs)
  | RelAssertI { call = { relid; notexp }; expect; iterexps; _ } ->
      Format.asprintf "If (%s: %s %s)%s, then" (string_of_relid relid)
        (string_of_notexp notexp)
        (if expect then "holds" else "does not hold")
        (string_of_iterexps iterexps)
  | IfI (exp_cond, iterexps, _, _) ->
      Format.asprintf "If (%s)%s, then" (string_of_exp exp_cond)
        (string_of_iterexps iterexps)
  | CaseI (exp, _, _) ->
      Format.asprintf "Case analysis on %s" (string_of_exp exp)
  | LetI (exp_l, exp_r, iterinstrs, _) ->
      Format.asprintf "(Let %s be %s)%s" (string_of_exp exp_l)
        (string_of_exp exp_r)
        (string_of_iterinstrs iterinstrs)
  | ResultI [] -> "The relation holds"
  | ResultI exps -> Format.asprintf "Result in %s" (string_of_exps ", " exps)
  | ReturnI exp -> Format.asprintf "Return %s" (string_of_exp exp)
  | DebugI (exp, _) -> Format.asprintf "Debug: %s" (string_of_exp exp)

and string_of_instr_full ~level ~index instr =
  let indent = String.make (level * 2) ' ' in
  let order = Format.asprintf "%s%d. " indent index in
  let header = string_of_instr_short instr in
  let string_of_else = function
    | Some phantom -> "\n\n" ^ order ^ "Else " ^ string_of_phantom phantom
    | None -> ""
  in
  match instr.it with
  | RelI _ | LetI _ | DebugI _ ->
      fst (string_of_instr_with_next ~level ~index instr)
  | RelAssertI { block = instrs_then; phantom = phantom_opt; _ }
  | IfI (_, _, instrs_then, phantom_opt) ->
      Format.asprintf "%s%s\n\n%s%s" order header
        (string_of_instrs ~level:(level + 1) instrs_then)
        (string_of_else phantom_opt)
  | CaseI (_, cases, phantom_opt) ->
      Format.asprintf "%s%s\n\n%s%s" order header
        (string_of_cases ~level:(level + 1) cases)
        (string_of_else phantom_opt)
  | ResultI _ | ReturnI _ -> order ^ header

and string_of_instr_with_next ?(level = 0) ~(index : int) instr =
  let indent = String.make (level * 2) ' ' in
  let order = Format.asprintf "%s%d. " indent index in
  match instr.it with
  | LetI (_, _, _, block) | RelI { block; _ } ->
      let block, next = string_of_instrs_from ~level ~index:(index + 1) block in
      ( Format.asprintf "%s%s\n\n%s" order (string_of_instr_short instr) block,
        next )
  | DebugI (_, instr_body) ->
      let body, next =
        string_of_instr_with_next ~level ~index:(index + 1) instr_body
      in
      ( Format.asprintf "%s%s\n\n%s" order (string_of_instr_short instr) body,
        next )
  | _ -> (string_of_instr ~level ~index instr, index + 1)

and string_of_instrs_from ?(level = 0) ~(index : int) instrs =
  List.fold_left
    (fun (strings, index) instr ->
      let string, index = string_of_instr_with_next ~level ~index instr in
      (strings @ [ string ], index))
    ([], index) instrs
  |> fun (strings, index) -> (String.concat "\n\n" strings, index)

and string_of_instrs ?(level = 0) instrs =
  fst (string_of_instrs_from ~level ~index:1 instrs)

and string_of_block ?(level = 0) ?(index = 0) block =
  fst (string_of_instrs_from ~level ~index:(index + 1) block)

and string_of_elseblock ?(level = 0) ?(index = 0) elseblock =
  let indent = String.make (level * 2) ' ' in
  let order = Format.asprintf "%s%d. " indent (index + 1) in
  Format.asprintf "%sOtherwise\n\n%s" order
    (string_of_instrs ~level:(level + 1) elseblock)

and string_of_elseblock_opt ?(level = 0) ?(index = 0) elseblock_opt =
  match elseblock_opt with
  | None -> ""
  | Some elseblock -> "\n\n" ^ string_of_elseblock ~level ~index elseblock

(* Definitions *)

let rec string_of_def def =
  ";; " ^ Location.to_string def.at ^ ":\n"
  ^
  match def.it with
  | TypD (id, _, { it = Il.OpaqueT; _ }) ->
      "opaque syntax " ^ string_of_typid id
  | TypD (typid, tparams, deftyp) ->
      "syntax " ^ string_of_typid typid ^ string_of_tparams tparams ^ " = "
      ^ string_of_deftyp deftyp
  | RelD (relid, mode, block, elseblock_opt) ->
      "relation " ^ string_of_relid relid ^ ": "
      ^ Il.Mode.render_inputs ~sep:", " ~string_of_arg:string_of_exp mode
      ^ "\n\n" ^ string_of_block block
      ^ string_of_elseblock_opt ~index:(List.length block) elseblock_opt
  | BuiltinDecD (defid, tparams, args_input, _) ->
      "builtin dec " ^ string_of_defid defid ^ string_of_tparams tparams
      ^ string_of_args args_input
  | DecD (defid, tparams, args_input, block, elseblock_opt) ->
      "dec " ^ string_of_defid defid ^ string_of_tparams tparams
      ^ string_of_args args_input ^ "\n\n" ^ string_of_block block
      ^ string_of_elseblock_opt ~index:(List.length block) elseblock_opt

and string_of_defs defs = String.concat "\n\n" (List.map string_of_def defs)

(* Spec *)

let string_of_spec spec = string_of_defs spec
