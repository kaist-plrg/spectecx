open Common.Source
module Sl = Lang.Sl
module Il = Lang.Il
module Linearize = Linearize

(* Structural LL-to-PL copy: wrap each instruction, definition, and
   expression in Pl.Annot.bare. Hint inlining lands later. *)

let bare_instr (instr' : Pl.instr' phrase) : Pl.instr = Pl.Annot.bare instr'
let bare_def (def' : Pl.def' phrase) : Pl.def = Pl.Annot.bare def'

let bare_exp (inner : (Pl.exp', Pl.typ') note_phrase) : Pl.exp =
  Pl.Annot.bare inner

(* Expressions *)

let rec annotate_exp (exp : Sl.exp) : Pl.exp =
  let exp_inner : Pl.exp' =
    match exp.it with
    | Il.BoolE b -> Pl.BoolE b
    | Il.NumE n -> Pl.NumE n
    | Il.TextE t -> Pl.TextE t
    | Il.VarE id -> Pl.VarE id
    | Il.UnE (op, optyp, e) -> Pl.UnE (op, optyp, annotate_exp e)
    | Il.BinE (op, optyp, e1, e2) ->
        Pl.BinE (op, optyp, annotate_exp e1, annotate_exp e2)
    | Il.CmpE (op, optyp, e1, e2) ->
        Pl.CmpE (op, optyp, annotate_exp e1, annotate_exp e2)
    | Il.UpCastE (typ, e) -> Pl.UpCastE (typ, annotate_exp e)
    | Il.DownCastE (typ, e) -> Pl.DownCastE (typ, annotate_exp e)
    | Il.SubE (e, typ) -> Pl.SubE (annotate_exp e, typ)
    | Il.MatchE (e, pat) -> Pl.MatchE (annotate_exp e, pat)
    | Il.TupleE es -> Pl.TupleE (List.map annotate_exp es)
    | Il.CaseE notexp -> Pl.CaseE (annotate_notexp notexp)
    | Il.StrE fields ->
        Pl.StrE (List.map (fun (a, e) -> (a, annotate_exp e)) fields)
    | Il.OptE eo -> Pl.OptE (Option.map annotate_exp eo)
    | Il.ListE es -> Pl.ListE (List.map annotate_exp es)
    | Il.ConsE (e1, e2) -> Pl.ConsE (annotate_exp e1, annotate_exp e2)
    | Il.CatE (e1, e2) -> Pl.CatE (annotate_exp e1, annotate_exp e2)
    | Il.MemE (e1, e2) -> Pl.MemE (annotate_exp e1, annotate_exp e2)
    | Il.LenE e -> Pl.LenE (annotate_exp e)
    | Il.DotE (e, atom) -> Pl.DotE (annotate_exp e, atom)
    | Il.IdxE (e1, e2) -> Pl.IdxE (annotate_exp e1, annotate_exp e2)
    | Il.SliceE (e1, e2, e3) ->
        Pl.SliceE (annotate_exp e1, annotate_exp e2, annotate_exp e3)
    | Il.UpdE (e1, path, e2) ->
        Pl.UpdE (annotate_exp e1, annotate_path path, annotate_exp e2)
    | Il.CallE (id, targs, args) ->
        Pl.CallE (id, targs, List.map annotate_arg args)
    | Il.IterE (e, iterexp) -> Pl.IterE (annotate_exp e, iterexp)
  in
  bare_exp (exp_inner $$ (exp.at, exp.note))

and annotate_notexp (notexp : Sl.notexp) : Pl.notexp =
  Il.Mixfix.map annotate_exp notexp

and annotate_path (path : Sl.path) : Pl.path =
  let path_inner : Pl.path' =
    match path.it with
    | Il.RootP -> Pl.RootP
    | Il.IdxP (p, e) -> Pl.IdxP (annotate_path p, annotate_exp e)
    | Il.SliceP (p, e1, e2) ->
        Pl.SliceP (annotate_path p, annotate_exp e1, annotate_exp e2)
    | Il.DotP (p, atom) -> Pl.DotP (annotate_path p, atom)
  in
  path_inner $$ (path.at, path.note)

and annotate_arg (arg : Sl.arg) : Pl.arg =
  let arg_inner : Pl.arg' =
    match arg.it with
    | Il.ExpA e -> Pl.ExpA (annotate_exp e)
    | Il.DefA id -> Pl.DefA id
  in
  arg_inner $ arg.at

(* Cases and guards *)

let annotate_guard (g : Ll.guard) : Pl.guard =
  match g with
  | Ll.BoolG b -> Pl.BoolG b
  | Ll.CmpG (op, optyp, e) -> Pl.CmpG (op, optyp, annotate_exp e)
  | Ll.SubG typ -> Pl.SubG typ
  | Ll.MatchG pat -> Pl.MatchG pat
  | Ll.MemG e -> Pl.MemG (annotate_exp e)

(* Instructions *)

let rec annotate_instr (instr : Ll.instr) : Pl.instr =
  let instr_inner : Pl.instr' =
    match instr.it with
    | Ll.IfI (cond, iterexps, block, phantom_opt) ->
        Pl.IfI (annotate_exp cond, iterexps, annotate_block block, phantom_opt)
    | Ll.IfHoldI (id, notexp, iterexps, block, phantom_opt) ->
        Pl.IfHoldI
          ( id,
            annotate_notexp notexp,
            iterexps,
            annotate_block block,
            phantom_opt )
    | Ll.IfNotHoldI (id, notexp, iterexps, block, phantom_opt) ->
        Pl.IfNotHoldI
          ( id,
            annotate_notexp notexp,
            iterexps,
            annotate_block block,
            phantom_opt )
    | Ll.CaseI (exp, cases, phantom_opt) ->
        Pl.CaseI (annotate_exp exp, List.map annotate_case cases, phantom_opt)
    | Ll.OtherwiseI block -> (
        let block = annotate_block block in
        match block with
        | [ single ] -> Pl.OtherwiseI single
        | _ ->
            (* Wrap multi-instruction otherwise blocks in a synthetic
               instr by emitting them as a TryI of one arm. *)
            Pl.OtherwiseI (bare_instr (Pl.TryI [ block ] $ instr.at)))
    | Ll.TryI arms -> Pl.TryI (List.map annotate_block arms)
    | Ll.LetI (exp_l, exp_r, iterexps) ->
        Pl.LetI (annotate_exp exp_l, annotate_exp exp_r, iterexps)
    | Ll.RuleI (id, notexp, iterexps) ->
        Pl.RuleI (id, annotate_notexp notexp, iterexps)
    | Ll.ResultI exps -> Pl.ResultI (List.map annotate_exp exps)
    | Ll.ReturnI exp -> Pl.ReturnI (annotate_exp exp)
    | Ll.DebugI exp -> Pl.DebugI (annotate_exp exp)
  in
  bare_instr (instr_inner $ instr.at)

and annotate_case (case : Ll.case) : Pl.case =
  let g, block = case in
  (annotate_guard g, annotate_block block)

and annotate_block (block : Ll.block) : Pl.block = List.map annotate_instr block

let annotate_elseblock_opt (elseblock_opt : Ll.elseblock option) :
    Pl.elseblock option =
  Option.map annotate_block elseblock_opt

(* Definitions *)

let annotate_def (def : Ll.def) : Pl.def =
  let def_inner : Pl.def' =
    match def.it with
    | Ll.TypD (id, tparams, deftyp) -> Pl.TypD (id, tparams, deftyp)
    | Ll.RelD (id, sig_, exps, block, elseblock_opt) ->
        Pl.RelD
          ( id,
            sig_,
            List.map annotate_exp exps,
            annotate_block block,
            annotate_elseblock_opt elseblock_opt )
    | Ll.BuiltinDecD (id, tparams, args) ->
        Pl.BuiltinDecD (id, tparams, List.map annotate_arg args)
    | Ll.DecD (id, tparams, args, block, elseblock_opt) ->
        Pl.DecD
          ( id,
            tparams,
            List.map annotate_arg args,
            annotate_block block,
            annotate_elseblock_opt elseblock_opt )
  in
  bare_def (def_inner $ def.at)

(* Spec *)

let annotate_spec (spec : Ll.spec) : Pl.spec = List.map annotate_def spec
