open Common.Domain
open Types

type t = IdSet.t

let empty = IdSet.empty
let ( + ) = IdSet.union
let unions (sets : t list) : t = List.fold_left ( + ) empty sets
let free_exp = Il.Free.free_exp
let free_exps = Il.Free.free_exps

let free_relcall ({ notexp; _ } : relcall) : t =
  free_exps (Il.Mixfix.args notexp)

let free_guard (guard : guard) : t =
  match guard with
  | BoolG _ | SubG _ | MatchG _ -> empty
  | CmpG (_, _, exp) | MemG exp | CheckLetSubG (_, exp) | CheckLetMatchG (_, exp)
    ->
      free_exp exp

let free_instr_without_block (instr : instr) : t =
  match instr.node.it with
  | RelI { call; iterinstrs } ->
      free_relcall call + Sl.Free.free_iterinstrs iterinstrs
  | RelAssertI { call; iterexps; _ } ->
      free_relcall call + Sl.Free.free_iterexps iterexps
  | IfI (exp_cond, iterexps, _, _) ->
      free_exp exp_cond + Sl.Free.free_iterexps iterexps
  | CaseI (exp, cases, _) ->
      free_exp exp
      + unions (List.map (fun (guard, _) -> free_guard guard) cases)
  | OtherwiseI _ | TryI _ -> empty
  | LetI (exp_l, exp_r, iterinstrs) ->
      free_exp exp_l + free_exp exp_r + Sl.Free.free_iterinstrs iterinstrs
  | ResultI exps -> free_exps exps
  | ReturnI exp | DebugI exp -> free_exp exp
  | DestructI (fields, exp_source) ->
      free_exps (List.map snd fields) + free_exp exp_source
  | CheckLetI { exp_l; exp_r; _ } | OptionGetI (exp_l, exp_r, _) ->
      free_exp exp_l + free_exp exp_r
