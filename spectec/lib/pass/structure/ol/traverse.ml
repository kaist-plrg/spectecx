open Common.Annotated
open Ast

let map_instr_blocks (instr : instr) ~(f : instr list -> instr list) : instr =
  let it =
    match instr.it with
    | RelI { call; iterinstrs; block } ->
        RelI { call; iterinstrs; block = f block }
    | RelAssertI { call; expect; iterexps; block } ->
        RelAssertI { call; expect; iterexps; block = f block }
    | IfI (exp_cond, iterexps, instrs_then) ->
        IfI (exp_cond, iterexps, f instrs_then)
    | CaseI (exp, cases, total) ->
        let cases = List.map (fun (guard, block) -> (guard, f block)) cases in
        CaseI (exp, cases, total)
    | LetI (exp_l, exp_r, iterinstrs, block) ->
        LetI (exp_l, exp_r, iterinstrs, f block)
    | ResultI _ | ReturnI _ | DebugI _ -> instr.it
  in
  { instr with it }

let rec rewrite_instrs_postorder (instrs : instr list) ~(f : instr -> instr) :
    instr list =
  List.map
    (fun instr ->
      instr |> map_instr_blocks ~f:(rewrite_instrs_postorder ~f) |> f)
    instrs

let rec rewrite_siblings_preorder (instrs : instr list)
    ~(f : instr -> instr list -> instr list option) : instr list =
  match instrs with
  | [] -> []
  | instr_h :: instrs_t -> (
      match f instr_h instrs_t with
      | Some instrs -> rewrite_siblings_preorder instrs ~f
      | None ->
          let instr_h =
            map_instr_blocks instr_h ~f:(rewrite_siblings_preorder ~f)
          in
          instr_h :: rewrite_siblings_preorder instrs_t ~f)

type sibling_decision = Take of instr | Skip | Stop

let merge_siblings_preorder (instrs : instr list)
    ~(f : instr -> instr -> sibling_decision) : instr list =
  let rec take_sibling_opt (instr_h : instr) (instrs_skipped : instr list)
      (instrs : instr list) : (instr * instr list) option =
    match instrs with
    | [] -> None
    | instr_sibling :: instrs_rest -> (
        match f instr_h instr_sibling with
        | Take instr_merged ->
            Some (instr_merged, List.rev_append instrs_skipped instrs_rest)
        | Skip ->
            take_sibling_opt instr_h
              (instr_sibling :: instrs_skipped)
              instrs_rest
        | Stop -> None)
  in
  rewrite_siblings_preorder instrs ~f:(fun instr_h instrs_t ->
      take_sibling_opt instr_h [] instrs_t
      |> Option.map (fun (instr_merged, instrs_t) -> instr_merged :: instrs_t))
