open Lang
open Ol.Ast
open Common.Annotated

(* Insert phantom instructions at dangling else branches,
   with the path condition necessary to reach the else branch

   Note that this does not take fall-through into account,
   so the path condition is not precise

   Fall-through may happen due to the heuristic-driven syntactic optimization of SL,

   (i) Good case

   -- if i >= 0   and   -- if i < 0
   -- if j >= 0         -- if j >= 0

   are nicely merged into

   if i >= 0 then
     if j >= 0 then ...
     else Phantom: i >= 0 && j < 0
   else
     if j >= 0 then ...
     else Phantom: i < 0 && j < 0

   (ii) Bad case

   -- if j >= 0   and  -- if i < 0
   -- if i >= 0        -- if j >= 0

   are merged into

   if j >= 0 then
     if i >= 0 then ...
     else Phantom: j >= 0 && i < 0
   else Phantom: j < 0

   ... if i = -1, j = 3 is given as input, it falls through

   if i < 0 then
      if j >= 0 then ...
      else Phantom: i < 0 && j < 0
   else Phantom: i >= 0 *)

(* Phantom id generator *)

let tick = ref 0

let pid () =
  let pid = !tick in
  tick := !tick + 1;
  pid

(* Path condition *)

let negate_exp (exp : exp) : exp =
  Il.Expression.UnE (`NotOp, `BoolT, exp) $$ (exp.at, exp.note)

let rec negate_pathcond (pathcond : pathcond) : pathcond =
  match pathcond with
  | ForallC (pathcond, iterexps) -> ExistsC (negate_pathcond pathcond, iterexps)
  | ExistsC (pathcond, iterexps) -> ForallC (negate_pathcond pathcond, iterexps)
  | PlainC exp_cond -> PlainC (negate_exp exp_cond)
  | RelAssertC { call; expect } -> RelAssertC { call; expect = not expect }

(* Conversion to SL *)

let rec sl_of_instrs (instrs : instr list) ~(insert_phantoms : bool)
    ~(pathconds : pathcond list) : Sl.instr list =
  List.map (sl_of_instr ~insert_phantoms ~pathconds) instrs

and sl_of_instr (instr : instr) ~(insert_phantoms : bool)
    ~(pathconds : pathcond list) : Sl.instr =
  let at = instr.at in
  let make_phantom pathconds_else =
    if insert_phantoms then Some (pid (), pathconds_else) else None
  in
  match instr.it with
  | RelI { call; iterinstrs; block } ->
      let block = sl_of_instrs block ~insert_phantoms ~pathconds in
      Sl.RelI { call; iterinstrs; block } $ at
  | RelAssertI { call; expect; iterexps; block } ->
      let pathcond =
        if iterexps = [] then RelAssertC { call; expect }
        else ForallC (RelAssertC { call; expect }, iterexps)
      in
      let block =
        sl_of_instrs block ~insert_phantoms ~pathconds:(pathconds @ [ pathcond ])
      in
      let phantom = make_phantom (pathconds @ [ negate_pathcond pathcond ]) in
      Sl.RelAssertI { call; expect; iterexps; block; phantom } $ at
  | IfI (exp_cond, iterexps, instrs_then) ->
      let pathcond =
        if iterexps = [] then PlainC exp_cond
        else ForallC (PlainC exp_cond, iterexps)
      in
      let instrs_then =
        sl_of_instrs instrs_then ~insert_phantoms
          ~pathconds:(pathconds @ [ pathcond ])
      in
      let phantom = make_phantom (pathconds @ [ negate_pathcond pathcond ]) in
      Sl.IfI (exp_cond, iterexps, instrs_then, phantom) $ at
  | CaseI (exp, cases, total) ->
      let pathconds_cases =
        List.map
          (fun (guard, _) -> PlainC (Optimize.guard_as_exp exp guard))
          cases
      in
      let cases =
        List.map2
          (fun pathcond_case (guard, instrs) ->
            ( guard,
              sl_of_instrs instrs ~insert_phantoms
                ~pathconds:(pathconds @ [ pathcond_case ]) ))
          pathconds_cases cases
      in
      let phantom =
        if total then None
        else make_phantom (pathconds @ List.map negate_pathcond pathconds_cases)
      in
      Sl.CaseI (exp, cases, phantom) $ at
  | LetI (exp_l, exp_r, iterinstrs, block) ->
      let block = sl_of_instrs block ~insert_phantoms ~pathconds in
      Sl.LetI (exp_l, exp_r, iterinstrs, block) $ at
  | ResultI exps -> Sl.ResultI exps $ at
  | ReturnI exp -> Sl.ReturnI exp $ at
  | DebugI (exp, instr_body) ->
      let instr_body = sl_of_instr instr_body ~insert_phantoms ~pathconds in
      Sl.DebugI (exp, instr_body) $ at

(* Instrumentation *)

let instrument (block : instr list) (elseblock_opt : instr list option) :
    Sl.instr list * Sl.instr list option =
  match elseblock_opt with
  | Some elseblock ->
      let block = sl_of_instrs block ~insert_phantoms:false ~pathconds:[] in
      let elseblock =
        sl_of_instrs elseblock ~insert_phantoms:false ~pathconds:[]
      in
      (block, Some elseblock)
  | None ->
      let block = sl_of_instrs block ~insert_phantoms:true ~pathconds:[] in
      (block, None)
