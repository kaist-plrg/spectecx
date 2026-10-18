open Common.Source
open Types.Expression
open Types

(** [as_exp var] rebuilds the expression form of [var], wrapping one [IterE] per
    iteration dimension. *)
let as_exp ({ varid; typ; iters } : var) : exp =
  let exp_base = VarE varid $$ (varid.at, { typ = typ.it; annot = () }) in
  let exp, _ =
    List.fold_left
      (fun ((exp : exp), iters_below) iter ->
        let typ' = IterT { typ = exp.note.typ $ exp.at; iter } in
        let iterexp = (iter, [ { varid; typ; iters = iters_below } ]) in
        let exp =
          IterE (exp, iterexp) $$ (exp.at, { typ = typ'; annot = () })
        in
        (exp, iters_below @ [ iter ]))
      (exp_base, []) iters
  in
  exp
