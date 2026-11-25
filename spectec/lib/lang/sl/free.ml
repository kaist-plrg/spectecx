open Common.Domain
open Types

type t = IdSet.t

let empty = IdSet.empty
let ( + ) = IdSet.union
let unions (sets : t list) : t = List.fold_left ( + ) empty sets
let free_exp = Il.Free.free_exp
let free_exps = Il.Free.free_exps
let free_args = Il.Free.free_args

let free_iterexp ((_, vars) : iterexp) : t =
  vars |> List.map (fun ({ Il.varid; _ } : var) -> varid) |> IdSet.of_list

let free_iterexps (iterexps : iterexp list) : t =
  unions (List.map free_iterexp iterexps)

let free_iterinstrs (iterinstrs : iterinstr list) : t =
  iterinstrs |> List.map Al.iterexp_of_iterinstr |> free_iterexps

let free_relcall ({ notexp; _ } : relcall) : t =
  free_exps (Il.Mixfix.args notexp)

let free_guard (guard : guard) : t =
  match guard with
  | BoolG _ | SubG _ | MatchG _ -> empty
  | CmpG (_, _, exp) | MemG exp -> free_exp exp

let free_instr_without_block (instr : instr) : t =
  match instr.it with
  | RelI { call; iterinstrs; block = _ } ->
      free_relcall call + free_iterinstrs iterinstrs
  | RelAssertI { call; iterexps; _ } ->
      free_relcall call + free_iterexps iterexps
  | IfI (exp_cond, iterexps, _, _) -> free_exp exp_cond + free_iterexps iterexps
  | CaseI (exp, cases, _) ->
      free_exp exp
      + unions (List.map (fun (guard, _) -> free_guard guard) cases)
  | LetI (exp_l, exp_r, iterinstrs, _) ->
      free_exp exp_l + free_exp exp_r + free_iterinstrs iterinstrs
  | ResultI exps -> free_exps exps
  | ReturnI exp | DebugI (exp, _) -> free_exp exp

let rec free_instr (instr : instr) : t =
  let free_inner =
    match instr.it with
    | RelI { block; _ }
    | RelAssertI { block; _ }
    | IfI (_, _, block, _)
    | LetI (_, _, _, block) ->
        free_block block
    | CaseI (_, cases, _) ->
        unions (List.map (fun (_, block) -> free_block block) cases)
    | DebugI (_, instr) -> free_instr instr
    | ResultI _ | ReturnI _ -> empty
  in
  free_instr_without_block instr + free_inner

and free_block (block : block) : t = unions (List.map free_instr block)

let free_elseblock_opt (elseblock_opt : elseblock option) : t =
  elseblock_opt |> Option.map free_block |> Option.value ~default:empty

let free_def (def : def) : t =
  match def.it with
  | TypD _ | BuiltinDecD _ -> empty
  | RelD (_, mode, block, elseblock_opt) ->
      free_exps (Il.Mode.inputs mode)
      + free_block block
      + free_elseblock_opt elseblock_opt
  | DecD (_, _, args, block, elseblock_opt) ->
      free_args args + free_block block + free_elseblock_opt elseblock_opt
