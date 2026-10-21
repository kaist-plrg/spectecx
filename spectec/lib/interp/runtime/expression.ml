open Common.Source
open Lang.Xl
module Il = Lang.Il
module Value = Il.Value
open Il.Expression
open Il
open Envs.Make
module Typ = Envs.Il.Typ

module type Context = sig
  type t
  type annotation

  val find_value_exn : t -> var:Envs.Interp.Var.t -> Lang.Il.value
  val opaque_types : t -> Opaque_types.t
  val find_typdef_exn : t -> id:Common.Domain.TId.t -> Envs.Il.Typdef.t

  val for_optional_iteration :
    t -> vars:Lang.Il.var list -> t option Common.Attempt.attempt

  val for_list_iteration :
    t -> vars:Lang.Il.var list -> t list Common.Attempt.attempt

  val get_exn : 'a Common.Attempt.attempt -> 'a
  val error : Common.Source.region -> string -> 'a
end

module Make (Ctx : Context) = struct
  type exp = Ctx.annotation Il.Expression.t
  type path = Ctx.annotation Il.Expression.path
  type arg = Ctx.annotation Il.Expression.arg

  let error = Ctx.error

  let matches_pattern (value : value) ~(pattern : pattern) : bool =
    match (pattern, value.it) with
    | CaseP mixop_p, CaseV valuecase -> Mixfix.eq_mixop mixop_p valuecase
    | ListP listpattern, ListV values -> (
        let len_v = List.length values in
        match listpattern with
        | `Cons -> len_v > 0
        | `Fixed len_p -> len_v = len_p
        | `Nil -> len_v = 0)
    | OptP `Some, OptV (Some _) -> true
    | OptP `None, OptV None -> true
    | _ -> false

  let rec upcast (value : value) ~to_typ:(typ : typ) ~(ctx : Ctx.t) : value =
    match typ.it with
    | NumT `IntT -> (
        match value.it with
        | NumV (`Nat n) -> Value.int n
        | NumV (`Int _) -> value
        | _ -> assert false)
    | VarT { synid = tid; targs } -> (
        let tparams, deftyp = Ctx.find_typdef_exn ctx ~id:tid in
        let theta = List.combine tparams targs |> TIdMap.of_list in
        match deftyp.it with
        | PlainT typ ->
            let typ = Typ.subst_typ theta typ in
            upcast value ~to_typ:typ ~ctx
        | _ -> value)
    | TupleT typs -> (
        match value.it with
        | TupleV values ->
            let values =
              List.map2
                (fun typ value -> upcast value ~to_typ:typ ~ctx)
                typs values
            in
            Value.Make.tuple typ.it values
        | _ -> assert false)
    | _ -> value

  let rec downcast (value : value) ~to_typ:(typ : typ) ~(ctx : Ctx.t) : value =
    match typ.it with
    | NumT `NatT -> (
        match value.it with
        | NumV (`Nat _) -> value
        | NumV (`Int i) when Bigint.(i >= zero) -> Value.nat i
        | _ -> assert false)
    | VarT { synid = tid; targs } -> (
        let tparams, deftyp = Ctx.find_typdef_exn ctx ~id:tid in
        let theta = List.combine tparams targs |> TIdMap.of_list in
        match deftyp.it with
        | PlainT typ ->
            let typ = Typ.subst_typ theta typ in
            downcast value ~to_typ:typ ~ctx
        | _ -> value)
    | TupleT typs -> (
        match value.it with
        | TupleV values ->
            let values =
              List.map2
                (fun typ value -> downcast value ~to_typ:typ ~ctx)
                typs values
            in
            Value.Make.tuple typ.it values
        | _ -> assert false)
    | _ -> value

  let rec matches_type (value : value) ~(typ : typ) ~(ctx : Ctx.t) : bool =
    match typ.it with
    | BoolT -> ( match value.it with BoolV _ -> true | _ -> false)
    | NumT `NatT -> (
        match value.it with
        | NumV (`Nat _) -> true
        | NumV (`Int i) -> Bigint.(i >= zero)
        | _ -> false)
    | NumT `IntT -> ( match value.it with NumV _ -> true | _ -> false)
    | TextT -> ( match value.it with TextV _ -> true | _ -> false)
    | VarT { synid = tid; targs } -> (
        let tparams, deftyp = Ctx.find_typdef_exn ctx ~id:tid in
        let theta = List.combine tparams targs |> TIdMap.of_list in
        match (deftyp.it, value.it) with
        | OpaqueT, OpaqueV value ->
            Opaque_types.has_type (Ctx.opaque_types ctx) ~name:tid.it ~value
        | OpaqueT, _ -> false
        | PlainT typ, _ ->
            let typ = Typ.subst_typ theta typ in
            matches_type value ~typ ~ctx
        | VariantT typcases, CaseV valuecase ->
            List.exists
              (fun typcase ->
                let { Il.notation = nottyp; _ } = typcase in
                let nottyp' = Mixfix.map (Typ.subst_typ theta) nottyp.it in
                Mixfix.eq_mixop nottyp' valuecase
                && matches_types (Mixfix.args valuecase)
                     ~typs:(Mixfix.args nottyp') ~ctx)
              typcases
        | _ -> true)
    | TupleT typs -> (
        match value.it with
        | TupleV values -> matches_types values ~typs ~ctx
        | _ -> false)
    | IterT { typ = typ_inner; iter = Opt } -> (
        match value.it with
        | OptV value_opt -> (
            match value_opt with
            | Some value_inner -> matches_type value_inner ~typ:typ_inner ~ctx
            | None -> true)
        | _ -> false)
    | IterT { typ = typ_inner; iter = List } -> (
        match value.it with
        | ListV values -> List.for_all (matches_type ~typ:typ_inner ~ctx) values
        | _ -> false)
    | _ -> false

  and matches_types (values : value list) ~(typs : typ list) ~(ctx : Ctx.t) :
      bool =
    List.length typs = List.length values
    && List.for_all2 (fun typ value -> matches_type value ~typ ~ctx) typs values

  let index_value (value : value) ~index ~at ~bounds_at : value =
    match value.it with
    | TextV text when index < 0 || index >= String.length text ->
        error bounds_at
          (Format.asprintf "index %d out of bounds [0, %d)" index
             (String.length text))
    | TextV text ->
        Value.Make.text Il.TextT (String.make 1 (String.get text index))
    | ListV values when index < 0 || index >= List.length values ->
        error bounds_at
          (Format.asprintf "index %d out of bounds [0, %d)" index
             (List.length values))
    | ListV values -> List.nth values index
    | _ ->
        error at
          (Format.asprintf
             "indexing expects either a text or a list, but got %s"
             (Il.Print.string_of_value ~short:true value))

  let slice_value (value : value) ~start ~length ~list_typ ~at ~bounds_at :
      value =
    let stop = start + length in
    match value.it with
    | TextV text when start < 0 || stop > String.length text ->
        error bounds_at
          (Format.asprintf "slice [%d, %d) out of bounds [0, %d)" start stop
             (String.length text))
    | TextV text ->
        Value.Make.text Il.TextT (String.sub text start (stop - start))
    | ListV values when start < 0 || stop > List.length values ->
        error bounds_at
          (Format.asprintf "slice [%d, %d) out of bounds [0, %d)" start stop
             (List.length values))
    | ListV values ->
        let values =
          List.mapi
            (fun index value ->
              if start <= index && index < stop then Some value else None)
            values
          |> List.filter_map Fun.id
        in
        Value.Make.list list_typ values
    | _ ->
        error at
          (Format.asprintf "slicing expects either a text or a list, but got %s"
             (Il.Print.string_of_value ~short:true value))

  let eval_exp exp ~ctx ~call_func =
    let rec eval_exp (ctx : Ctx.t) (exp : exp) : value =
      let at, typ = (exp.at, exp.note.typ) in
      match exp.it with
      | BoolE b -> Value.Make.bool typ b
      | NumE n -> Value.Make.num typ n
      | TextE s -> Value.Make.text typ s
      | VarE id -> Ctx.find_value_exn ctx ~var:(id, [])
      | UnE (unop, _, exp) -> eval_un_exp typ ctx unop exp
      | BinE (binop, _, exp_l, exp_r) -> eval_bin_exp typ ctx binop exp_l exp_r
      | CmpE (cmpop, _, exp_l, exp_r) -> eval_cmp_exp typ ctx cmpop exp_l exp_r
      | UpCastE (typ, exp) -> upcast (eval_exp ctx exp) ~to_typ:typ ~ctx
      | DownCastE (typ, exp) -> downcast (eval_exp ctx exp) ~to_typ:typ ~ctx
      | SubE (exp, typ_target) ->
          Value.Make.bool typ
            (matches_type (eval_exp ctx exp) ~typ:typ_target ~ctx)
      | MatchE (exp, pattern) ->
          Value.Make.bool typ (matches_pattern (eval_exp ctx exp) ~pattern)
      | TupleE exps -> Value.Make.tuple typ (eval_exps ctx exps)
      | CaseE notexp ->
          let mixop, exps = Mixfix.split notexp in
          let values = eval_exps ctx exps in
          Value.Make.case typ (Mixfix.fill mixop values)
      | StrE fields ->
          let atoms, exps = List.split fields in
          let values = eval_exps ctx exps in
          Value.Make.record typ (List.combine atoms values)
      | OptE exp_opt -> Value.Make.opt typ (Option.map (eval_exp ctx) exp_opt)
      | ListE exps -> Value.Make.list typ (eval_exps ctx exps)
      | ConsE (exp_h, exp_t) -> eval_cons_exp typ ctx exp_h exp_t
      | CatE (exp_l, exp_r) -> eval_cat_exp typ ctx at exp_l exp_r
      | MemE (exp_e, exp_s) -> eval_mem_exp typ ctx exp_e exp_s
      | LenE exp -> eval_len_exp typ ctx exp
      | DotE (exp_b, atom) -> eval_dot_exp ctx exp_b atom
      | IdxE (exp_b, exp_i) -> eval_idx_exp ctx exp_b exp_i
      | SliceE (exp_b, exp_i, exp_n) -> eval_slice_exp typ ctx exp_b exp_i exp_n
      | UpdE (exp_b, path, exp_f) -> eval_upd_exp ctx exp_b path exp_f
      | CallE (id, targs, args) -> call_func ctx id targs args
      | IterE (exp, iterexp) -> eval_iter_exp typ ctx exp iterexp
    and eval_exps (ctx : Ctx.t) (exps : exp list) : value list =
      List.map (eval_exp ctx) exps
    and eval_un_exp (typ : typ') (ctx : Ctx.t) (unop : unop) (exp : exp) : value
        =
      let eval_un_bool typ unop value =
        match unop with
        | `NotOp -> (not (Value.get_bool value)) |> Value.Make.bool typ
      in
      let eval_un_num typ unop value =
        let num = Value.get_num value in
        let num = Num.un unop num in
        num |> Value.Make.num typ
      in
      let value = eval_exp ctx exp in
      match unop with
      | #Bool.unop as unop -> eval_un_bool typ unop value
      | #Num.unop as unop -> eval_un_num typ unop value
    and eval_bin_exp (typ : typ') (ctx : Ctx.t) (binop : binop) (exp_l : exp)
        (exp_r : exp) : value =
      let eval_bin_bool typ binop value_l value_r =
        let bool_l = Value.get_bool value_l in
        let bool_r = Value.get_bool value_r in
        let bool_res =
          match binop with
          | `AndOp -> bool_l && bool_r
          | `OrOp -> bool_l || bool_r
          | `ImplOp -> (not bool_l) || bool_r
          | `EquivOp -> bool_l = bool_r
        in
        bool_res |> Value.Make.bool typ
      in
      let eval_bin_num typ binop value_l value_r =
        let num_l = Value.get_num value_l in
        let num_r = Value.get_num value_r in
        Num.bin binop num_l num_r |> Value.Make.num typ
      in
      let value_l = eval_exp ctx exp_l in
      let value_r = eval_exp ctx exp_r in
      match binop with
      | #Bool.binop as binop -> eval_bin_bool typ binop value_l value_r
      | #Num.binop as binop -> eval_bin_num typ binop value_l value_r
    and eval_cmp_exp (typ : typ') (ctx : Ctx.t) (cmpop : cmpop) (exp_l : exp)
        (exp_r : exp) : value =
      let eval_cmp_bool typ cmpop value_l value_r =
        let eq = Value.eq value_l value_r in
        let bool_res = match cmpop with `EqOp -> eq | `NeOp -> not eq in
        Value.Make.bool typ bool_res
      in
      let eval_cmp_num typ cmpop value_l value_r =
        let num_l = Value.get_num value_l in
        let num_r = Value.get_num value_r in
        Num.cmp cmpop num_l num_r |> Value.Make.bool typ
      in
      let value_l = eval_exp ctx exp_l in
      let value_r = eval_exp ctx exp_r in
      match cmpop with
      | #Bool.cmpop as cmpop -> eval_cmp_bool typ cmpop value_l value_r
      | #Num.cmpop as cmpop -> eval_cmp_num typ cmpop value_l value_r
    and eval_cons_exp (typ : typ') (ctx : Ctx.t) (exp_h : exp) (exp_t : exp) :
        value =
      let value_h = eval_exp ctx exp_h in
      let value_t = eval_exp ctx exp_t in
      let values_t = Value.get_list value_t in
      Value.Make.list typ (value_h :: values_t)
    and eval_cat_exp (typ : typ') (ctx : Ctx.t) (at : region) (exp_l : exp)
        (exp_r : exp) : value =
      let value_l = eval_exp ctx exp_l in
      let value_r = eval_exp ctx exp_r in
      match (value_l.it, value_r.it) with
      | TextV s_l, TextV s_r -> s_l ^ s_r |> Value.Make.text typ
      | ListV values_l, ListV values_r ->
          values_l @ values_r |> Value.Make.list typ
      | _ -> error at "concatenation expects either two texts or two lists"
    and eval_mem_exp (typ : typ') (ctx : Ctx.t) (exp_e : exp) (exp_s : exp) :
        value =
      let value_e = eval_exp ctx exp_e in
      let value_s = eval_exp ctx exp_s in
      let values_s = Value.get_list value_s in
      List.exists (Value.eq value_e) values_s |> Value.Make.bool typ
    and eval_len_exp (typ : typ') (ctx : Ctx.t) (exp : exp) : value =
      let value = eval_exp ctx exp in
      let len =
        match value.it with
        | TextV s -> s |> String.length |> Bigint.of_int
        | ListV values -> values |> List.length |> Bigint.of_int
        | _ ->
            error exp.at
              (Format.asprintf
                 "length operation expects either a text or a list, but got %s"
                 (Il.Print.string_of_value ~short:true value))
      in
      Value.Make.nat typ len
    and eval_dot_exp (ctx : Ctx.t) (exp_b : exp) (atom : atom) : value =
      let value_b = eval_exp ctx exp_b in
      let fields = Value.get_struct value_b in
      fields
      |> List.map (fun (atom, value) -> (atom.it, value))
      |> List.assoc atom.it
    and eval_idx_exp (ctx : Ctx.t) (exp_b : exp) (exp_i : exp) : value =
      let value_b = eval_exp ctx exp_b in
      let value_i = eval_exp ctx exp_i in
      let idx = value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn in
      index_value value_b ~index:idx ~at:exp_b.at ~bounds_at:exp_i.at
    and eval_slice_exp (typ : typ') (ctx : Ctx.t) (exp_b : exp) (exp_i : exp)
        (exp_n : exp) : value =
      let value_b = eval_exp ctx exp_b in
      let value_i = eval_exp ctx exp_i in
      let idx_l = value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn in
      let value_len = eval_exp ctx exp_n in
      let idx_n =
        value_len |> Value.get_num |> Num.to_int |> Bigint.to_int_exn
      in
      let bounds_at =
        match value_b.it with TextV _ -> exp_i.at | _ -> exp_n.at
      in
      slice_value value_b ~start:idx_l ~length:idx_n ~list_typ:typ ~at:exp_b.at
        ~bounds_at
    and eval_upd_exp (ctx : Ctx.t) (exp_b : exp) (path : path) (exp_f : exp) :
        value =
      let rec eval_access_path ctx value_b path =
        match path.it with
        | RootP -> value_b
        | IdxP (path, exp_i) ->
            let value = eval_access_path ctx value_b path in
            let value_i = eval_exp ctx exp_i in
            let idx =
              value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn
            in
            index_value value ~index:idx ~at:path.at ~bounds_at:exp_i.at
        | SliceP (path, exp_i, exp_n) ->
            let value = eval_access_path ctx value_b path in
            let value_i = eval_exp ctx exp_i in
            let idx_l =
              value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn
            in
            let value_len = eval_exp ctx exp_n in
            let idx_n =
              value_len |> Value.get_num |> Num.to_int |> Bigint.to_int_exn
            in
            slice_value value ~start:idx_l ~length:idx_n ~list_typ:path.note
              ~at:path.at ~bounds_at:exp_n.at
        | DotP (path, atom) ->
            let value = eval_access_path ctx value_b path in
            let fields = value |> Value.get_struct in
            let value =
              fields
              |> List.map (fun (atom, value) -> (atom.it, value))
              |> List.assoc atom.it
            in
            value
      and eval_update_path ctx value_b path value_new =
        match path.it with
        | RootP -> value_new
        | IdxP (path, exp_i) -> (
            let value = eval_access_path ctx value_b path in
            let value_i = eval_exp ctx exp_i in
            let idx_target =
              value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn
            in
            match value.it with
            | TextV s when idx_target < 0 || idx_target >= String.length s ->
                error exp_i.at
                  (Format.asprintf "index %d out of bounds [0, %d)" idx_target
                     (String.length s))
            | TextV s ->
                let s_new = Value.get_text value_new in
                if String.length s_new <> 1 then
                  error exp_i.at
                    (Format.asprintf
                       "updating a character requires a single-character text, \
                        but got %s"
                       (Il.Print.string_of_value ~short:true value_new))
                else
                  let s_updated =
                    String.sub s 0 idx_target ^ s_new
                    ^ String.sub s (idx_target + 1)
                        (String.length s - idx_target - 1)
                  in
                  let value = Value.Make.text Il.TextT s_updated in
                  eval_update_path ctx value_b path value
            | ListV values
              when idx_target < 0 || idx_target >= List.length values ->
                error exp_i.at
                  (Format.asprintf "index %d out of bounds [0, %d)" idx_target
                     (List.length values))
            | ListV values ->
                let values_updated =
                  List.mapi
                    (fun idx value ->
                      if idx = idx_target then value_new else value)
                    values
                in
                let value = Value.Make.list path.note values_updated in
                eval_update_path ctx value_b path value
            | _ ->
                error path.at
                  (Format.asprintf
                     "indexing expects either a text or a list, but got %s"
                     (Il.Print.string_of_value ~short:true value)))
        | SliceP (path, exp_i, exp_n) -> (
            let value = eval_access_path ctx value_b path in
            let value_i = eval_exp ctx exp_i in
            let idx_l =
              value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn
            in
            let value_len = eval_exp ctx exp_n in
            let idx_n =
              value_len |> Value.get_num |> Num.to_int |> Bigint.to_int_exn
            in
            let idx_h = idx_l + idx_n in
            match value.it with
            | TextV s when idx_l < 0 || idx_h > String.length s ->
                error exp_n.at
                  (Format.asprintf "slice [%d, %d) out of bounds [0, %d)" idx_l
                     idx_h (String.length s))
            | TextV s ->
                let s_new = Value.get_text value_new in
                if String.length s_new <> idx_n then
                  error exp_n.at
                    (Format.asprintf
                       "updating a slice of length %d requires a text of \
                        length %d, but got %s"
                       idx_n (String.length s_new)
                       (Il.Print.string_of_value ~short:true value_new))
                else
                  let s_updated =
                    String.sub s 0 idx_l ^ s_new
                    ^ String.sub s idx_h (String.length s - idx_h)
                  in
                  let value = Value.Make.text Il.TextT s_updated in
                  eval_update_path ctx value_b path value
            | ListV values when idx_l < 0 || idx_h > List.length values ->
                error exp_n.at
                  (Format.asprintf "slice [%d, %d) out of bounds [0, %d)" idx_l
                     idx_h (List.length values))
            | ListV values ->
                let values_new = Value.get_list value_new in
                if List.length values_new <> idx_n then
                  error exp_n.at
                    (Format.asprintf
                       "updating a slice of length %d requires a list of \
                        length %d, but got %s"
                       idx_n (List.length values_new)
                       (Il.Print.string_of_value ~short:true value_new))
                else
                  let values_updated =
                    List.mapi
                      (fun idx value ->
                        if idx_l <= idx && idx < idx_h then
                          List.nth values_new (idx - idx_l)
                        else value)
                      values
                  in
                  let value = Value.Make.list path.note values_updated in
                  eval_update_path ctx value_b path value
            | _ ->
                error path.at
                  (Format.asprintf
                     "slicing expects either a text or a list, but got %s"
                     (Il.Print.string_of_value ~short:true value)))
        | DotP (path, atom) ->
            let value = eval_access_path ctx value_b path in
            let fields = value |> Value.get_struct in
            let fields =
              List.map
                (fun (atom_f, value_f) ->
                  if atom_f.it = atom.it then (atom_f, value_new)
                  else (atom_f, value_f))
                fields
            in
            let value = Value.Make.record path.note fields in
            eval_update_path ctx value_b path value
      in
      let value_b = eval_exp ctx exp_b in
      let value_f = eval_exp ctx exp_f in
      eval_update_path ctx value_b path value_f
    and eval_iter_exp (typ : typ') (ctx : Ctx.t) (exp : exp) (iterexp : iterexp)
        : value =
      let eval_iter_exp_opt typ ctx exp vars =
        let ctx_sub_opt = Ctx.for_optional_iteration ctx ~vars |> Ctx.get_exn in
        match ctx_sub_opt with
        | Some ctx_sub ->
            let value = eval_exp ctx_sub exp in
            Some value |> Value.Make.opt typ
        | None -> None |> Value.Make.opt typ
      in
      let eval_iter_exp_list typ ctx exp vars =
        let ctxs_sub = Ctx.for_list_iteration ctx ~vars |> Ctx.get_exn in
        let values = List.map (fun ctx_sub -> eval_exp ctx_sub exp) ctxs_sub in
        Value.Make.list typ values
      in
      let iter, vars = iterexp in
      match iter with
      | Opt -> eval_iter_exp_opt typ ctx exp vars
      | List -> eval_iter_exp_list typ ctx exp vars
    in
    eval_exp ctx exp

  let eval_exps exps ~ctx ~call_func = List.map (eval_exp ~call_func ~ctx) exps

  let eval_args args ~ctx ~call_func =
    List.map
      (fun (arg : arg) ->
        match arg.it with
        | ExpA exp -> eval_exp exp ~ctx ~call_func
        | DefA id -> Value.func id)
      args
end
