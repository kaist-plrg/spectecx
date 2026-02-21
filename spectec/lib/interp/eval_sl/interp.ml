open Common.Source
open Semantics.Env
open Lang.Xl
module Il = Lang.Il
module Value = Lang.Il.Value
open Lang.Sl
module Hint = Semantics.Static.Rel.Hint
module Typ = Semantics.Dynamic.Typ
module Cache = Semantics.Dynamic.Cache
open Semantics.Dynamic_Sl.Envs
module Rel = Semantics.Dynamic_Sl.Rel
open Error
module F = Format

(* Option monad *)

let ( let* ) = Option.bind

(* Cache *)

let func_cache = ref (Cache.Cache.create ~size:10000)
let rule_cache = ref (Cache.Cache.create ~size:10000)

(* Assignments *)

(* Assigning a value to an expression *)

let rec assign_exp (ctx : Ctx.t) (exp : exp) (value : value) : Ctx.t =
  let note = value.note.typ in
  match (exp.it, value.it) with
  | VarE id, _ ->
      let ctx = Ctx.add_value Local ctx (id, []) value in
      ctx
  | TupleE exps_inner, TupleV values_inner ->
      let ctx = assign_exps ctx exps_inner values_inner in
      ctx
  | CaseE notexp, CaseV (_mixop_value, values_inner) ->
      let _mixop_exp, exps_inner = notexp in
      let ctx = assign_exps ctx exps_inner values_inner in
      ctx
  | OptE exp_opt, OptV value_opt -> (
      match (exp_opt, value_opt) with
      | Some exp_inner, Some value_inner ->
          let ctx = assign_exp ctx exp_inner value_inner in
          ctx
      | None, None -> ctx
      | _ -> assert false)
  | ListE exps_inner, ListV values_inner ->
      let ctx = assign_exps ctx exps_inner values_inner in
      ctx
  | ConsE (exp_h, exp_t), ListV values_inner ->
      let value_h = List.hd values_inner in
      let value_t = List.tl values_inner |> Value.Make.list note in
      let ctx = assign_exp ctx exp_h value_h in
      let ctx = assign_exp ctx exp_t value_t in
      ctx
  | IterE (_, (Opt, vars)), OptV None ->
      (* Per iterated variable, make an option out of the value *)
      List.fold_left
        (fun ctx (id, typ, iters) ->
          let value_sub =
            let typ = Il.Typ.iterate typ (iters @ [ Il.Opt ]) in
            None |> Value.Make.opt typ.it
          in
          Ctx.add_value Local ctx (id, iters @ [ Il.Opt ]) value_sub)
        ctx vars
  | IterE (exp, (Opt, vars)), OptV (Some value) ->
      (* Assign the value to the iterated expression *)
      let ctx = assign_exp ctx exp value in
      (* Per iterated variable, make an option out of the value *)
      List.fold_left
        (fun ctx (id, typ, iters) ->
          let value_sub =
            let value = Ctx.find_value Local ctx (id, iters) in
            let typ = Il.Typ.iterate typ (iters @ [ Il.Opt ]) in
            Some value |> Value.Make.opt typ.it
          in
          Ctx.add_value Local ctx (id, iters @ [ Il.Opt ]) value_sub)
        ctx vars
  | IterE (exp, (List, vars)), ListV values ->
      (* Map over the value list elements,
         and assign each value to the iterated expression *)
      let ctxs_rev =
        List.fold_left
          (fun ctxs_rev value ->
            let ctx =
              { ctx with local = { ctx.local with venv = VEnv.empty } }
            in
            let ctx = assign_exp ctx exp value in
            ctx :: ctxs_rev)
          [] values
      in
      let ctxs = List.rev ctxs_rev in
      (* Per iterated variable, collect its elementwise value,
         then make a sequence out of them *)
      List.fold_left
        (fun ctx (id, typ, iters) ->
          let values =
            List.map (fun ctx -> Ctx.find_value Local ctx (id, iters)) ctxs
          in
          let value_sub =
            let typ = Il.Typ.iterate typ (iters @ [ Il.List ]) in
            values |> Value.Make.list typ.it
          in
          Ctx.add_value Local ctx (id, iters @ [ Il.List ]) value_sub)
        ctx vars
  | _ ->
      error exp.at
        (F.asprintf "(TODO) match failed %s <- %s"
           (Lang.Sl.Print.string_of_exp exp)
           (Lang.Sl.Print.string_of_value ~short:true value))

and assign_exps (ctx : Ctx.t) (exps : exp list) (values : value list) : Ctx.t =
  check
    (List.length exps = List.length values)
    (over_region (List.map at exps))
    (F.asprintf
       "mismatch in number of expressions and values while assigning, expected \
        %d value(s) but got %d"
       (List.length exps) (List.length values));
  List.fold_left2 assign_exp ctx exps values

(* Assigning a value to an argument *)

and assign_arg (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (arg : arg)
    (value : value) : Ctx.t =
  let assign_arg_exp ctx exp value = assign_exp ctx exp value in
  let assign_arg_def ctx_caller ctx_callee id value =
    match value.it with
    | Il.FuncV id_f ->
        let func = Ctx.find_func Local ctx_caller id_f in
        Ctx.add_func Local ctx_callee id func
    | _ ->
        error id.at
          (F.asprintf "cannot assign a value %s to a definition %s"
             (Lang.Sl.Print.string_of_value ~short:true value)
             id.it)
  in
  match arg.it with
  | ExpA exp -> assign_arg_exp ctx_callee exp value
  | DefA id -> assign_arg_def ctx_caller ctx_callee id value

and assign_args (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (args : arg list)
    (values : value list) : Ctx.t =
  check
    (List.length args = List.length values)
    (over_region (List.map at args))
    (F.asprintf
       "mismatch in number of arguments and values while assigning, expected \
        %d value(s) but got %d"
       (List.length args) (List.length values));
  List.fold_left2 (assign_arg ctx_caller) ctx_callee args values

(* Type coercion and subtyping *)

(* DownCastE and SubE performs subtype checks that are not guaranteed by the type system,
    because in SpecTec assignment should be able to revert the type cast expression

     - Numeric subtyping:
       - e.g., -- if (int) n = $foo() when $foo() returns a positive integer +2
     - Variant subtyping:
       - e.g., -- if (typ) objtyp = $foo() when $foo() returns a variant of objtyp specifically
     - Tuple subtyping: recursive, but the type system guarantees that their lengths are equal
     - Iteration subtyping

   Note that structs are invariant in SpecTec, so we do not need to check for subtyping *)

let rec upcast (ctx : Ctx.t) (typ : typ) (value : value) : Ctx.t * value =
  match typ.it with
  | NumT `IntT -> (
      match value.it with
      | NumV (`Nat n) -> (ctx, Value.int n)
      | NumV (`Int _) -> (ctx, value)
      | _ -> assert false)
  | VarT (tid, targs) -> (
      let tparams, deftyp = Ctx.find_typdef Local ctx tid in
      let theta = List.combine tparams targs |> TIdMap.of_list in
      match deftyp.it with
      | PlainT typ ->
          let typ = Typ.subst_typ theta typ in
          upcast ctx typ value
      | _ -> (ctx, value))
  | TupleT typs -> (
      match value.it with
      | TupleV values ->
          let ctx, values =
            List.fold_left2
              (fun (ctx, values) typ value ->
                let ctx, value = upcast ctx typ value in
                (ctx, values @ [ value ]))
              (ctx, []) typs values
          in
          (ctx, Value.Make.tuple typ.it values)
      | _ -> assert false)
  | _ -> (ctx, value)

let rec downcast (ctx : Ctx.t) (typ : typ) (value : value) : Ctx.t * value =
  match typ.it with
  | NumT `NatT -> (
      match value.it with
      | NumV (`Nat _) -> (ctx, value)
      | NumV (`Int i) when Bigint.(i >= zero) -> (ctx, Value.nat i)
      | _ -> assert false)
  | VarT (tid, targs) -> (
      let tparams, deftyp = Ctx.find_typdef Local ctx tid in
      let theta = List.combine tparams targs |> TIdMap.of_list in
      match deftyp.it with
      | PlainT typ ->
          let typ = Typ.subst_typ theta typ in
          downcast ctx typ value
      | _ -> (ctx, value))
  | TupleT typs -> (
      match value.it with
      | TupleV values ->
          let ctx, values =
            List.fold_left2
              (fun (ctx, values) typ value ->
                let ctx, value = downcast ctx typ value in
                (ctx, values @ [ value ]))
              (ctx, []) typs values
          in
          (ctx, Value.Make.tuple typ.it values)
      | _ -> assert false)
  | _ -> (ctx, value)

let rec subtyp (ctx : Ctx.t) (typ : typ) (value : value) : bool =
  match typ.it with
  | NumT `NatT -> (
      match value.it with
      | NumV (`Nat _) -> true
      | NumV (`Int i) -> Bigint.(i >= zero)
      | _ -> assert false)
  | VarT (tid, targs) -> (
      let tparams, deftyp = Ctx.find_typdef Local ctx tid in
      let theta = List.combine tparams targs |> TIdMap.of_list in
      match (deftyp.it, value.it) with
      | PlainT typ, _ ->
          let typ = Typ.subst_typ theta typ in
          subtyp ctx typ value
      | VariantT typcases, CaseV (mixop_v, _) ->
          List.exists
            (fun (nottyp, _) ->
              let mixop_t, _ = nottyp.it in
              Mixop.eq mixop_t mixop_v)
            typcases
      | _ -> true)
  | TupleT typs -> (
      match value.it with
      | TupleV values ->
          List.length typs = List.length values
          && List.for_all2 (subtyp ctx) typs values
      | _ -> false)
  | _ -> true

(* Expression evaluation *)

let rec eval_exp (ctx : Ctx.t) (exp : exp) : Ctx.t * value =
  let eval_bool_exp note ctx b = (ctx, Value.Make.bool note b) in
  let eval_num_exp note ctx n = (ctx, Value.Make.num note n) in
  let eval_text_exp note ctx s = (ctx, Value.Make.text note s) in
  let eval_var_exp _note ctx id =
    let value = Ctx.find_value Local ctx (id, []) in
    (ctx, value)
  in
  let at, note = (exp.at, exp.note) in
  match exp.it with
  | BoolE b -> eval_bool_exp note ctx b
  | NumE n -> eval_num_exp note ctx n
  | TextE s -> eval_text_exp note ctx s
  | VarE id -> eval_var_exp note ctx id
  | UnE (unop, optyp, exp) -> eval_un_exp note ctx unop optyp exp
  | BinE (binop, optyp, exp_l, exp_r) ->
      eval_bin_exp note ctx binop optyp exp_l exp_r
  | CmpE (cmpop, optyp, exp_l, exp_r) ->
      eval_cmp_exp note ctx cmpop optyp exp_l exp_r
  | UpCastE (typ, exp) -> eval_upcast_exp note ctx typ exp
  | DownCastE (typ, exp) -> eval_downcast_exp note ctx typ exp
  | SubE (exp, typ) -> eval_sub_exp note ctx exp typ
  | MatchE (exp, pattern) -> eval_match_exp note ctx exp pattern
  | TupleE exps -> eval_tuple_exp note ctx exps
  | CaseE notexp -> eval_case_exp note ctx notexp
  | StrE fields -> eval_str_exp note ctx fields
  | OptE exp_opt -> eval_opt_exp note ctx exp_opt
  | ListE exps -> eval_list_exp note ctx exps
  | ConsE (exp_h, exp_t) -> eval_cons_exp note ctx exp_h exp_t
  | CatE (exp_l, exp_r) -> eval_cat_exp note ctx at exp_l exp_r
  | MemE (exp_e, exp_s) -> eval_mem_exp note ctx exp_e exp_s
  | LenE exp -> eval_len_exp note ctx exp
  | DotE (exp_b, atom) -> eval_dot_exp note ctx exp_b atom
  | IdxE (exp_b, exp_i) -> eval_idx_exp note ctx exp_b exp_i
  | SliceE (exp_b, exp_l, exp_h) -> eval_slice_exp note ctx exp_b exp_l exp_h
  | UpdE (exp_b, path, exp_f) -> eval_upd_exp note ctx exp_b path exp_f
  | CallE (id, targs, args) -> eval_call_exp note ctx id targs args
  | HoldE (id, notexp) -> eval_hold_exp note ctx id notexp
  | IterE (exp, iterexp) -> eval_iter_exp note ctx exp iterexp

and eval_exps (ctx : Ctx.t) (exps : exp list) : Ctx.t * value list =
  List.fold_left
    (fun (ctx, values) exp ->
      let ctx, value = eval_exp ctx exp in
      (ctx, values @ [ value ]))
    (ctx, []) exps

(* Unary expression evaluation *)

and eval_un_exp (note : typ') (ctx : Ctx.t) (unop : unop) (_optyp : optyp)
    (exp : exp) : Ctx.t * value =
  let eval_un_bool note unop value =
    match unop with
    | `NotOp -> (not (Value.get_bool value)) |> Value.Make.bool note
  in
  let eval_un_num note unop value =
    let num = Value.get_num value in
    let num = Num.un unop num in
    num |> Value.Make.num note
  in
  let ctx, value = eval_exp ctx exp in
  let value_res =
    match unop with
    | #Bool.unop as unop -> eval_un_bool note unop value
    | #Num.unop as unop -> eval_un_num note unop value
  in
  (ctx, value_res)

(* Binary expression evaluation *)

and eval_bin_exp (note : typ') (ctx : Ctx.t) (binop : binop) (_optyp : optyp)
    (exp_l : exp) (exp_r : exp) : Ctx.t * value =
  let eval_bin_bool note binop value_l value_r =
    let bool_l = Value.get_bool value_l in
    let bool_r = Value.get_bool value_r in
    let bool_res =
      match binop with
      | `AndOp -> bool_l && bool_r
      | `OrOp -> bool_l || bool_r
      | `ImplOp -> (not bool_l) || bool_r
      | `EquivOp -> bool_l = bool_r
    in
    bool_res |> Value.Make.bool note
  in
  let eval_bin_num note binop value_l value_r =
    let num_l = Value.get_num value_l in
    let num_r = Value.get_num value_r in
    Num.bin binop num_l num_r |> Value.Make.num note
  in
  let ctx, value_l = eval_exp ctx exp_l in
  let ctx, value_r = eval_exp ctx exp_r in
  let value_res =
    match binop with
    | #Bool.binop as binop -> eval_bin_bool note binop value_l value_r
    | #Num.binop as binop -> eval_bin_num note binop value_l value_r
  in
  (ctx, value_res)

(* Comparison expression evaluation *)

and eval_cmp_exp (note : typ') (ctx : Ctx.t) (cmpop : cmpop) (_optyp : optyp)
    (exp_l : exp) (exp_r : exp) : Ctx.t * value =
  let eval_cmp_bool note cmpop value_l value_r =
    let eq = Value.eq value_l value_r in
    let bool_res = match cmpop with `EqOp -> eq | `NeOp -> not eq in
    Value.Make.bool note bool_res
  in
  let eval_cmp_num note cmpop value_l value_r =
    let num_l = Value.get_num value_l in
    let num_r = Value.get_num value_r in
    Num.cmp cmpop num_l num_r |> Value.Make.bool note
  in
  let ctx, value_l = eval_exp ctx exp_l in
  let ctx, value_r = eval_exp ctx exp_r in
  let value_res =
    match cmpop with
    | #Bool.cmpop as cmpop -> eval_cmp_bool note cmpop value_l value_r
    | #Num.cmpop as cmpop -> eval_cmp_num note cmpop value_l value_r
  in
  (ctx, value_res)

(* Upcast expression evaluation *)

and eval_upcast_exp (_note : typ') (ctx : Ctx.t) (typ : typ) (exp : exp) :
    Ctx.t * value =
  let ctx, value = eval_exp ctx exp in
  let ctx, value_res = upcast ctx typ value in
  (ctx, value_res)

(* Downcast expression evaluation *)

and eval_downcast_exp (_note : typ') (ctx : Ctx.t) (typ : typ) (exp : exp) :
    Ctx.t * value =
  let ctx, value = eval_exp ctx exp in
  let ctx, value_res = downcast ctx typ value in
  (ctx, value_res)

(* Subtype check expression evaluation *)

and eval_sub_exp (note : typ') (ctx : Ctx.t) (exp : exp) (typ : typ) :
    Ctx.t * value =
  let ctx, value = eval_exp ctx exp in
  let sub = subtyp ctx typ value in
  let value_res = Value.Make.bool note sub in
  (ctx, value_res)

(* Pattern match check expression evaluation *)

and eval_match_exp (note : typ') (ctx : Ctx.t) (exp : exp) (pattern : pattern) :
    Ctx.t * value =
  let ctx, value = eval_exp ctx exp in
  let matches =
    match (pattern, value.it) with
    | CaseP mixop_p, CaseV (mixop_v, _) -> Mixop.eq mixop_p mixop_v
    | ListP listpattern, ListV values -> (
        let len_v = List.length values in
        match listpattern with
        | `Cons -> len_v > 0
        | `Fixed len_p -> len_v = len_p
        | `Nil -> len_v = 0)
    | OptP `Some, OptV (Some _) -> true
    | OptP `None, OptV None -> true
    | _ -> false
  in
  let value_res = Value.Make.bool note matches in
  (ctx, value_res)

(* Tuple expression evaluation *)

and eval_tuple_exp (note : typ') (ctx : Ctx.t) (exps : exp list) : Ctx.t * value
    =
  let ctx, values = eval_exps ctx exps in
  let value_res = Value.Make.tuple note values in
  (ctx, value_res)

(* Case expression evaluation *)

and eval_case_exp (note : typ') (ctx : Ctx.t) (notexp : notexp) : Ctx.t * value
    =
  let mixop, exps = notexp in
  let ctx, values = eval_exps ctx exps in
  let value_res = Value.Make.case note (mixop, values) in
  (ctx, value_res)

(* Struct expression evaluation *)

and eval_str_exp (note : typ') (ctx : Ctx.t) (fields : (atom * exp) list) :
    Ctx.t * value =
  let atoms, exps = List.split fields in
  let ctx, values = eval_exps ctx exps in
  let fields = List.combine atoms values in
  let value_res = Value.Make.record note fields in
  (ctx, value_res)

(* Option expression evaluation *)

and eval_opt_exp (note : typ') (ctx : Ctx.t) (exp_opt : exp option) :
    Ctx.t * value =
  let ctx, value_opt =
    match exp_opt with
    | Some exp ->
        let ctx, value = eval_exp ctx exp in
        (ctx, Some value)
    | None -> (ctx, None)
  in
  let value_res = Value.Make.opt note value_opt in
  (ctx, value_res)

(* List expression evaluation *)

and eval_list_exp (note : typ') (ctx : Ctx.t) (exps : exp list) : Ctx.t * value
    =
  let ctx, values = eval_exps ctx exps in
  let value_res = Value.Make.list note values in
  (ctx, value_res)

(* Cons expression evaluation *)

and eval_cons_exp (note : typ') (ctx : Ctx.t) (exp_h : exp) (exp_t : exp) :
    Ctx.t * value =
  let ctx, value_h = eval_exp ctx exp_h in
  let ctx, value_t = eval_exp ctx exp_t in
  let values_t = Value.get_list value_t in
  let value_res = Value.Make.list note (value_h :: values_t) in
  (ctx, value_res)

(* Concatenation expression evaluation *)

and eval_cat_exp (note : typ') (ctx : Ctx.t) (at : region) (exp_l : exp)
    (exp_r : exp) : Ctx.t * value =
  let ctx, value_l = eval_exp ctx exp_l in
  let ctx, value_r = eval_exp ctx exp_r in
  let value_res =
    match (value_l.it, value_r.it) with
    | TextV s_l, TextV s_r -> s_l ^ s_r |> Value.Make.text note
    | ListV values_l, ListV values_r ->
        values_l @ values_r |> Value.Make.list note
    | _ -> error at "concatenation expects either two texts or two lists"
  in
  (ctx, value_res)

(* Membership expression evaluation *)

and eval_mem_exp (note : typ') (ctx : Ctx.t) (exp_e : exp) (exp_s : exp) :
    Ctx.t * value =
  let ctx, value_e = eval_exp ctx exp_e in
  let ctx, value_s = eval_exp ctx exp_s in
  let values_s = Value.get_list value_s in
  let value_res =
    List.exists (Value.eq value_e) values_s |> Value.Make.bool note
  in
  (ctx, value_res)

(* Length expression evaluation *)

and eval_len_exp (note : typ') (ctx : Ctx.t) (exp : exp) : Ctx.t * value =
  let ctx, value = eval_exp ctx exp in
  let len = value |> Value.get_list |> List.length |> Bigint.of_int in
  let value_res = Value.Make.nat note len in
  (ctx, value_res)

(* Dot expression evaluation *)

and eval_dot_exp (_note : typ') (ctx : Ctx.t) (exp_b : exp) (atom : atom) :
    Ctx.t * value =
  let ctx, value_b = eval_exp ctx exp_b in
  let fields = Value.get_struct value_b in
  let value_res =
    fields
    |> List.map (fun (atom, value) -> (atom.it, value))
    |> List.assoc atom.it
  in
  (ctx, value_res)

(* Index expression evaluation *)

and eval_idx_exp (_note : typ') (ctx : Ctx.t) (exp_b : exp) (exp_i : exp) :
    Ctx.t * value =
  let ctx, value_b = eval_exp ctx exp_b in
  let ctx, value_i = eval_exp ctx exp_i in
  let values = Value.get_list value_b in
  let idx = value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn in
  let value_res = List.nth values idx in
  (ctx, value_res)

(* Slice expression evaluation *)

and eval_slice_exp (note : typ') (ctx : Ctx.t) (exp_b : exp) (exp_i : exp)
    (exp_n : exp) : Ctx.t * value =
  let ctx, value_b = eval_exp ctx exp_b in
  let values = Value.get_list value_b in
  let ctx, value_i = eval_exp ctx exp_i in
  let idx_l = value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn in
  let ctx, value_n = eval_exp ctx exp_n in
  let idx_n = value_n |> Value.get_num |> Num.to_int |> Bigint.to_int_exn in
  let idx_h = idx_l + idx_n in
  let values_slice =
    List.mapi
      (fun idx value ->
        if idx_l <= idx && idx < idx_h then Some value else None)
      values
    |> List.filter_map Fun.id
  in
  let value_res = Value.Make.list note values_slice in
  (ctx, value_res)

(* Update expression evaluation *)

and eval_upd_exp (_note : typ') (ctx : Ctx.t) (exp_b : exp) (path : path)
    (exp_f : exp) : Ctx.t * value =
  (* Path operations *)
  let rec eval_access_path value_b path =
    match path.it with
    | Il.RootP -> value_b
    | Il.DotP (path, atom) ->
        let value = eval_access_path value_b path in
        let fields = value |> Value.get_struct in
        fields
        |> List.map (fun (atom, value) -> (atom.it, value))
        |> List.assoc atom.it
    | _ -> failwith "(TODO) access_path"
  and eval_update_path value_b path value_n =
    match path.it with
    | Il.RootP -> value_n
    | Il.DotP (path, atom) ->
        let value = eval_access_path value_b path in
        let fields = value |> Value.get_struct in
        let fields =
          List.map
            (fun (atom_f, value_f) ->
              if atom_f.it = atom.it then (atom_f, value_n)
              else (atom_f, value_f))
            fields
        in
        let value = Value.Make.record path.note fields in
        eval_update_path value_b path value
    | _ -> failwith "(TODO eval_update_path)"
  in
  let ctx, value_b = eval_exp ctx exp_b in
  let ctx, value_f = eval_exp ctx exp_f in
  let value_res = eval_update_path value_b path value_f in
  (ctx, value_res)

(* Function call expression evaluation *)

and eval_call_exp (_note : typ') (ctx : Ctx.t) (id : id) (targs : targ list)
    (args : arg list) : Ctx.t * value =
  let ctx, value_res = invoke_func ctx id targs args in
  (ctx, value_res)

(* Conditional relation holds expression evaluation *)

and eval_hold_exp (note : typ') (ctx : Ctx.t) (id : id) (notexp : notexp) :
    Ctx.t * value =
  let _, exps_input = notexp in
  let ctx, values_input = eval_exps ctx exps_input in
  let ctx, hold =
    match invoke_rel ctx id values_input with
    | Some (ctx, _) -> (ctx, true)
    | None -> (ctx, false)
  in
  let value_res = hold |> Value.Make.bool note in
  (ctx, value_res)

(* Iterated expression evaluation *)

and eval_iter_exp (note : typ') (ctx : Ctx.t) (exp : exp) (iterexp : iterexp) :
    Ctx.t * value =
  (* Iteration helpers *)
  let eval_iter_exp_opt note ctx exp vars =
    let ctx_sub_opt = Ctx.sub_opt ctx vars in
    let ctx, value_res =
      match ctx_sub_opt with
      | Some ctx_sub ->
          let ctx_sub, value = eval_exp ctx_sub exp in
          let ctx = Ctx.commit ctx ctx_sub in
          let value_res = Some value |> Value.Make.opt note in
          (ctx, value_res)
      | None ->
          let value_res = None |> Value.Make.opt note in
          (ctx, value_res)
    in
    (ctx, value_res)
  in
  let eval_iter_exp_list note ctx exp vars =
    let ctxs_sub = Ctx.sub_list ctx vars in
    let ctx, values_rev =
      List.fold_left
        (fun (ctx, values_rev) ctx_sub ->
          let ctx_sub, value = eval_exp ctx_sub exp in
          let ctx = Ctx.commit ctx ctx_sub in
          (ctx, value :: values_rev))
        (ctx, []) ctxs_sub
    in
    let value_res = values_rev |> List.rev |> Value.Make.list note in
    (ctx, value_res)
  in
  let iter, vars = iterexp in
  match iter with
  | Opt -> eval_iter_exp_opt note ctx exp vars
  | List -> eval_iter_exp_list note ctx exp vars

(* Argument evaluation *)

and eval_args (ctx : Ctx.t) (args : arg list) : Ctx.t * value list =
  let eval_arg ctx arg =
    match arg.it with
    | Il.ExpA exp -> eval_exp ctx exp
    | Il.DefA id ->
        let value_res = Value.func id in
        (ctx, value_res)
  in
  List.fold_left
    (fun (ctx, values) arg ->
      let ctx, value = eval_arg ctx arg in
      (ctx, values @ [ value ]))
    (ctx, []) args

(* Instruction evaluation *)

and eval_instr (ctx : Ctx.t) (instr : instr) : Ctx.t * Sign.t =
  Instrumentation.Dispatcher.notify_instr ~instr ~at:instr.at;
  (* Result instruction evaluation *)
  let eval_result_instr ctx exps =
    let ctx, values = eval_exps ctx exps in
    (ctx, Sign.Res values)
  in
  (* Return instruction evaluation *)
  let eval_return_instr ctx exp =
    let ctx, value = eval_exp ctx exp in
    (ctx, Sign.Ret value)
  in
  (* Debug instruction evaluation *)
  let eval_debug_instr ctx exp =
    let ctx, value = eval_exp ctx exp in
    print_endline
    @@ F.sprintf "%s: %s" (string_of_region exp.at) (Il.Print.string_of_exp exp);
    print_endline @@ Il.Print.string_of_value value;
    (ctx, Sign.Cont)
  in
  match instr.it with
  | IfI (exp_cond, iterexps, instrs_then, _phantom_opt) ->
      eval_if_instr ctx exp_cond iterexps instrs_then
  | CaseI (exp, cases, _phantom_opt) -> eval_case_instr ctx exp cases
  | OtherwiseI instr -> eval_instr ctx instr
  | LetI (exp_l, exp_r, iterexps) -> eval_let_instr ctx exp_l exp_r iterexps
  | RuleI (id, notexp, iterexps) -> eval_rule_instr ctx id notexp iterexps
  | ResultI exps -> eval_result_instr ctx exps
  | ReturnI exp -> eval_return_instr ctx exp
  | DebugI exp -> eval_debug_instr ctx exp

and eval_instrs (ctx : Ctx.t) (sign : Sign.t) (instrs : instr list) :
    Ctx.t * Sign.t =
  List.fold_left
    (fun (ctx, sign) instr ->
      match sign with Sign.Cont -> eval_instr ctx instr | _ -> (ctx, sign))
    (ctx, sign) instrs

(* If instruction evaluation *)

and eval_if_instr (ctx : Ctx.t) (exp_cond : exp) (iterexps : iterexp list)
    (instrs_then : instr list) : Ctx.t * Sign.t =
  let eval_if_cond_iter ctx exp_cond iterexps =
    let rec eval_if_cond_iter' ctx exp_cond iterexps =
      let eval_if_cond ctx exp_cond =
        let ctx, value_cond = eval_exp ctx exp_cond in
        let cond = Value.get_bool value_cond in
        (ctx, cond, value_cond)
      in
      let eval_if_cond_list ctx exp_cond vars iterexps =
        let ctxs_sub = Ctx.sub_list ctx vars in
        let ctx, cond, values_cond_rev =
          List.fold_left
            (fun (ctx, cond, values_cond_rev) ctx_sub ->
              if not cond then (ctx, cond, values_cond_rev)
              else
                let ctx_sub, cond, value_cond =
                  eval_if_cond_iter' ctx_sub exp_cond iterexps
                in
                let ctx = Ctx.commit ctx ctx_sub in
                let values_cond_rev = value_cond :: values_cond_rev in
                (ctx, cond, values_cond_rev))
            (ctx, true, []) ctxs_sub
        in
        (ctx, cond, List.rev values_cond_rev)
      in
      match iterexps with
      | [] -> eval_if_cond ctx exp_cond
      | iterexp_h :: iterexps_t -> (
          let iter_h, vars_h = iterexp_h in
          match iter_h with
          | Il.Opt -> error no_region "(TODO)"
          | Il.List ->
              let ctx, cond, values_cond =
                eval_if_cond_list ctx exp_cond vars_h iterexps_t
              in
              let value_cond =
                let typ_inner = Il.BoolT $ no_region in
                Value.list typ_inner values_cond
              in
              (ctx, cond, value_cond))
    in
    let iterexps = List.rev iterexps in
    eval_if_cond_iter' ctx exp_cond iterexps
  in
  let ctx, cond, _value_cond = eval_if_cond_iter ctx exp_cond iterexps in
  if cond then eval_instrs ctx Cont instrs_then else (ctx, Cont)

(* Case analysis instruction evaluation *)

and eval_case_instr (ctx : Ctx.t) (exp : exp) (cases : case list) :
    Ctx.t * Sign.t =
  let eval_cases ctx exp cases =
    cases
    |> List.fold_left
         (fun (ctx, block_match, values_cond_rev) (guard, block) ->
           match block_match with
           | Some _ -> (ctx, block_match, values_cond_rev)
           | None ->
               let exp_cond =
                 match guard with
                 | BoolG true -> exp.it
                 | BoolG false -> Il.UnE (`NotOp, `BoolT, exp)
                 | CmpG (cmpop, optyp, exp_r) ->
                     Il.CmpE (cmpop, optyp, exp, exp_r)
                 | SubG typ -> Il.SubE (exp, typ)
                 | MatchG pattern -> Il.MatchE (exp, pattern)
                 | MemG exp_s -> Il.MemE (exp, exp_s)
               in
               let exp_cond = exp_cond $$ (exp.at, Il.BoolT) in
               let ctx, value_cond = eval_exp ctx exp_cond in
               let values_cond_rev = value_cond :: values_cond_rev in
               let cond = Value.get_bool value_cond in
               if cond then (ctx, Some block, values_cond_rev)
               else (ctx, None, values_cond_rev))
         (ctx, None, [])
    |> fun (ctx, block_match, values_cond) ->
    let value_cond =
      let typ_inner = Il.BoolT $ no_region in
      values_cond |> List.rev |> Value.list typ_inner
    in
    (ctx, block_match, value_cond)
  in
  let ctx, instrs_opt, _value_cond = eval_cases ctx exp cases in
  match instrs_opt with
  | Some instrs -> eval_instrs ctx Cont instrs
  | None -> (ctx, Cont)

(* Let instruction evaluation *)

and eval_let_instr (ctx : Ctx.t) (exp_l : exp) (exp_r : exp)
    (iterexps : iterexp list) : Ctx.t * Sign.t =
  let eval_let_iter ctx exp_l exp_r iterexps =
    let rec eval_let_iter' ctx exp_l exp_r iterexps =
      let eval_let ctx exp_l exp_r =
        let ctx, value = eval_exp ctx exp_r in
        assign_exp ctx exp_l value
      in
      let eval_let_opt ctx exp_l exp_r vars iterexps =
        (* Discriminate between bound and binding variables *)
        let vars_bound, vars_binding =
          List.partition
            (fun (id, _typ, iters) ->
              Ctx.bound_value Local ctx (id, iters @ [ Il.Opt ]))
            vars
        in
        let ctx_sub_opt = Ctx.sub_opt ctx vars_bound in
        let ctx, values_binding =
          match ctx_sub_opt with
          (* If the bound variable supposed to guide the iteration is already empty,
            then the binding variables are also empty *)
          | None ->
              let values_binding =
                List.map
                  (fun (_id_binding, typ_binding, iters_binding) ->
                    let value_binding =
                      let typ =
                        Il.Typ.iterate typ_binding (iters_binding @ [ Il.Opt ])
                      in
                      None |> Value.Make.opt typ.it
                    in
                    value_binding)
                  vars_binding
              in
              (ctx, values_binding)
          (* Otherwise, evaluate the premise for the subcontext *)
          | Some ctx_sub ->
              let ctx_sub = eval_let_iter' ctx_sub exp_l exp_r iterexps in
              let ctx = Ctx.commit ctx ctx_sub in
              let values_binding =
                List.map
                  (fun (id_binding, typ_binding, iters_binding) ->
                    let value_binding =
                      Ctx.find_value Local ctx_sub (id_binding, iters_binding)
                    in
                    let value_binding =
                      let typ =
                        Il.Typ.iterate typ_binding (iters_binding @ [ Il.Opt ])
                      in
                      Some value_binding |> Value.Make.opt typ.it
                    in
                    value_binding)
                  vars_binding
              in
              (ctx, values_binding)
        in
        (* Finally, bind the resulting values *)
        List.fold_left2
          (fun ctx (id_binding, _typ_binding, iters_binding) value_binding ->
            Ctx.add_value Local ctx
              (id_binding, iters_binding @ [ Il.Opt ])
              value_binding)
          ctx vars_binding values_binding
      in
      let eval_let_list ctx exp_l exp_r vars iterexps =
        (* Discriminate between bound and binding variables *)
        let vars_bound, vars_binding =
          List.partition
            (fun (id, _typ, iters) ->
              Ctx.bound_value Local ctx (id, iters @ [ Il.List ]))
            vars
        in
        (* Create a subcontext for each batch of bound values *)
        let ctxs_sub = Ctx.sub_list ctx vars_bound in
        let ctx, values_binding =
          match ctxs_sub with
          (* If the bound variable supposed to guide the iteration is already empty,
            then the binding variables are also empty *)
          | [] ->
              let values_binding =
                List.init (List.length vars_binding) (fun _ -> [])
              in
              (ctx, values_binding)
              (* Otherwise, evaluate the premise for each batch of bound values,
            and collect the resulting binding batches *)
          | _ ->
              let ctx, values_binding_batch_rev =
                List.fold_left
                  (fun (ctx, values_binding_batch_rev) ctx_sub ->
                    let ctx_sub = eval_let_iter' ctx_sub exp_l exp_r iterexps in
                    let ctx = Ctx.commit ctx ctx_sub in
                    let value_binding_batch =
                      List.map
                        (fun (id_binding, _typ_binding, iters_binding) ->
                          Ctx.find_value Local ctx_sub
                            (id_binding, iters_binding))
                        vars_binding
                    in
                    let values_binding_batch_rev =
                      value_binding_batch :: values_binding_batch_rev
                    in
                    (ctx, values_binding_batch_rev))
                  (ctx, []) ctxs_sub
              in
              let values_binding =
                values_binding_batch_rev |> List.rev |> Ctx.transpose
              in
              (ctx, values_binding)
        in
        (* Finally, bind the resulting binding batches *)
        List.fold_left2
          (fun ctx (id_binding, typ_binding, iters_binding) values_binding ->
            let value_binding =
              let typ =
                Il.Typ.iterate typ_binding (iters_binding @ [ Il.List ])
              in
              values_binding |> Value.Make.list typ.it
            in
            Ctx.add_value Local ctx
              (id_binding, iters_binding @ [ Il.List ])
              value_binding)
          ctx vars_binding values_binding
      in
      match iterexps with
      | [] -> eval_let ctx exp_l exp_r
      | iterexp_h :: iterexps_t -> (
          let iter_h, vars_h = iterexp_h in
          match iter_h with
          | Il.Opt -> eval_let_opt ctx exp_l exp_r vars_h iterexps_t
          | Il.List -> eval_let_list ctx exp_l exp_r vars_h iterexps_t)
    in
    let iterexps = List.rev iterexps in
    eval_let_iter' ctx exp_l exp_r iterexps
  in
  let ctx = eval_let_iter ctx exp_l exp_r iterexps in
  (ctx, Cont)

(* Rule instruction evaluation *)

and eval_rule_instr (ctx : Ctx.t) (id : id) (notexp : notexp)
    (iterexps : iterexp list) : Ctx.t * Sign.t =
  let eval_rule_iter ctx id notexp iterexps =
    let rec eval_rule_iter' ctx id notexp iterexps =
      (* Single rule evaluation *)
      let eval_rule ctx id notexp =
        let rel = Ctx.find_rel Local ctx id in
        let exps_input, exps_output =
          let inputs, _, _ = rel in
          let _, exps = notexp in
          Hint.split_exps_without_idx inputs exps
        in
        let ctx, values_input = eval_exps ctx exps_input in
        let ctx, values_output =
          match invoke_rel ctx id values_input with
          | Some (ctx, values_output) -> (ctx, values_output)
          | None -> error id.at "relation was not matched"
        in
        assign_exps ctx exps_output values_output
      in
      (* Rule list evaluation *)
      let eval_rule_list ctx id notexp vars iterexps =
        (* Discriminate between bound and binding variables *)
        let vars_bound, vars_binding =
          List.partition
            (fun (id, _typ, iters) ->
              Ctx.bound_value Local ctx (id, iters @ [ Il.List ]))
            vars
        in
        (* Create a subcontext for each batch of bound values *)
        let ctxs_sub = Ctx.sub_list ctx vars_bound in
        let ctx, values_binding =
          match ctxs_sub with
          (* If the bound variable supposed to guide the iteration is already empty,
            then the binding variables are also empty *)
          | [] ->
              let values_binding =
                List.init (List.length vars_binding) (fun _ -> [])
              in
              (ctx, values_binding)
          (* Otherwise, evaluate the premise for each batch of bound values,
            and collect the resulting binding batches *)
          | _ ->
              let ctx, values_binding_batch_rev =
                List.fold_left
                  (fun (ctx, values_binding_batch_rev) ctx_sub ->
                    let ctx_sub = eval_rule_iter' ctx_sub id notexp iterexps in
                    let ctx = Ctx.commit ctx ctx_sub in
                    let value_binding_batch =
                      List.map
                        (fun (id_binding, _typ_binding, iters_binding) ->
                          Ctx.find_value Local ctx_sub
                            (id_binding, iters_binding))
                        vars_binding
                    in
                    let values_binding_batch_rev =
                      value_binding_batch :: values_binding_batch_rev
                    in
                    (ctx, values_binding_batch_rev))
                  (ctx, []) ctxs_sub
              in
              let values_binding =
                values_binding_batch_rev |> List.rev |> Ctx.transpose
              in
              (ctx, values_binding)
        in
        (* Finally, bind the resulting binding batches *)
        List.fold_left2
          (fun ctx (id_binding, typ_binding, iters_binding) values_binding ->
            let value_binding =
              let typ =
                Il.Typ.iterate typ_binding (iters_binding @ [ Il.List ])
              in
              values_binding |> Value.Make.list typ.it
            in
            Ctx.add_value Local ctx
              (id_binding, iters_binding @ [ Il.List ])
              value_binding)
          ctx vars_binding values_binding
      in
      (* Optional rule evaluation *)
      let eval_rule_opt _ctx _id _notexp _vars _iterexps =
        failwith "TODO: eval_rule_opt"
      in
      match iterexps with
      | [] -> eval_rule ctx id notexp
      | iterexp_h :: iterexps_t -> (
          let iter_h, vars_h = iterexp_h in
          match iter_h with
          | Il.Opt -> eval_rule_opt ctx id notexp vars_h iterexps_t
          | Il.List -> eval_rule_list ctx id notexp vars_h iterexps_t)
    in
    let iterexps = List.rev iterexps in
    eval_rule_iter' ctx id notexp iterexps
  in
  let ctx = eval_rule_iter ctx id notexp iterexps in
  (ctx, Cont)

(* Invoke a relation *)

and invoke_rel (ctx : Ctx.t) (id : id) (values_input : value list) :
    (Ctx.t * value list) option =
  Instrumentation.Dispatcher.notify_rel_enter ~id:id.it ~at:id.at
    ~values:values_input;
  let _inputs, exps_input, instrs = Ctx.find_rel Local ctx id in
  check (instrs <> []) id.at "relation has no instructions";
  let attempt_rules () =
    Instrumentation.Dispatcher.notify_rule_enter ~id:id.it ~rule_id:"0"
      ~at:id.at;
    let ctx_local = Ctx.localize ctx in
    let ctx_local = Ctx.localize_inputs ctx_local values_input in
    let ctx_local = assign_exps ctx_local exps_input values_input in
    let ctx_local, sign = eval_instrs ctx_local Cont instrs in
    let ctx = Ctx.commit ctx ctx_local in
    let result =
      match sign with
      | Res values_output -> Some (ctx, values_output)
      | _ -> None
    in
    Instrumentation.Dispatcher.notify_rule_exit ~id:id.it ~rule_id:"0" ~at:id.at
      ~success:(Option.is_some result);
    result
  in
  let result =
    if Cache.is_cached_rule id.it then
      let invoke () =
        match attempt_rules () with
        | Some (_, values_output) -> Ok values_output
        | None -> Error ()
      in
      match invoke |> Cache.with_cache rule_cache (id.it, values_input) with
      | Ok values_output -> Some (ctx, values_output)
      | Error _ -> None
    else attempt_rules ()
  in
  Instrumentation.Dispatcher.notify_rel_exit ~id:id.it ~at:id.at
    ~success:(Option.is_some result);
  result

(* Invoke a function *)

and invoke_func (ctx : Ctx.t) (id : id) (targs : targ list) (args : arg list) :
    Ctx.t * value =
  let ctx, values_input = eval_args ctx args in
  (* Builtin function invocation *)
  let invoke_func_builtin () =
    let value_output =
      ctx.builtins.invoke id targs values_input |> unwrap_builtin
    in
    (ctx, value_output)
  in
  (* User-defined function invocation *)
  let invoke_func_def () =
    let tparams, args_input, instrs = Ctx.find_func Local ctx id in
    check (instrs <> []) id.at "function has no instructions";
    let ctx_local = Ctx.localize ctx in
    check
      (List.length targs = List.length tparams)
      id.at "arity mismatch in type arguments";
    let targs =
      match targs with
      | [] -> []
      | targs ->
          let theta =
            TDEnv.fold
              (fun tid typdef theta ->
                let tparams, deftyp = typdef in
                match (tparams, deftyp.it) with
                | [], Il.PlainT typ -> TIdMap.add tid typ theta
                | _ -> theta)
              ctx.local.tdenv TIdMap.empty
          in
          List.map (Typ.subst_typ theta) targs
    in
    let ctx_local =
      List.fold_left2
        (fun ctx_local tparam targ ->
          Ctx.add_typdef Local ctx_local tparam ([], Il.PlainT targ $ targ.at))
        ctx_local tparams targs
    in
    let attempt_clauses () =
      Instrumentation.Dispatcher.notify_clause_enter ~id:id.it ~clause_idx:0
        ~at:id.at;
      let ctx_local = Ctx.localize_inputs ctx_local values_input in
      let ctx_local = assign_args ctx ctx_local args_input values_input in
      let ctx_local, sign = eval_instrs ctx_local Cont instrs in
      let ctx = Ctx.commit ctx ctx_local in
      match sign with
      | Ret value_output ->
          Instrumentation.Dispatcher.notify_clause_exit ~id:id.it ~clause_idx:0
            ~at:id.at ~success:true;
          (ctx, value_output)
      | _ ->
          Instrumentation.Dispatcher.notify_clause_exit ~id:id.it ~clause_idx:0
            ~at:id.at ~success:false;
          error id.at "function was not matched"
    in
    attempt_clauses ()
  in
  (* Main dispatch *)
  Instrumentation.Dispatcher.notify_func_enter ~id:id.it ~at:id.at ~values:[];
  let invoke_func' () =
    let invoke () =
      let _, v =
        if ctx.builtins.is_builtin id then invoke_func_builtin ()
        else invoke_func_def ()
      in
      Ok v
    in
    let value_output_result =
      if
        (not (Cache.is_cached_func id.it))
        || targs <> []
        || List.exists
             (fun value ->
               match value.it with Lang.Il.FuncV _ -> true | _ -> false)
             values_input
      then invoke ()
      else invoke |> Cache.with_cache func_cache (id.it, values_input)
    in
    (ctx, value_output_result |> Result.get_ok)
  in
  let result = invoke_func' () in
  Instrumentation.Dispatcher.notify_func_exit ~id:id.it ~at:id.at;
  result

(* Load definitions into the context *)

let load_def (ctx : Ctx.t) (def : def) : Ctx.t =
  match def.it with
  | TypD (id, tparams, deftyp) ->
      let typdef = (tparams, deftyp) in
      Ctx.add_typdef Global ctx id typdef
  | RelD (id, (_, inputs), exps_input, instrs) ->
      let rel = (inputs, exps_input, instrs) in
      Ctx.add_rel Global ctx id rel
  | DecD (id, tparams, args_input, instrs) ->
      let func = (tparams, args_input, instrs) in
      Ctx.add_func Global ctx id func

let load_spec (ctx : Ctx.t) (spec : spec) : Ctx.t =
  List.fold_left load_def ctx spec
