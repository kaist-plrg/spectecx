(* IL type-based value generator.
   Derives Value.t Gen.t from typ' and deftyp' in Lang.Il.
   gen_of_typ and gen_of_deftyp are mutually recursive.
   gen_of_deftyp receives the outer typ for value typ' annotations. *)

open Lang.Il
open Common.Source

(* Returns tparams alongside deftyp so the caller can substitute type arguments *)
(* A case is recursive if any of its sub-types contains a VarT with the given name *)
let rec typ_has_varname name (typ : typ) =
  match typ.it with
  | VarT (id, _) -> id.it = name
  | TupleT typs -> List.exists (typ_has_varname name) typs
  | IterT (inner, _) -> typ_has_varname name inner
  | _ -> false

let find_typdef (spec : spec) (name : string) : (tparam list * deftyp) option =
  List.find_map
    (fun def ->
      match def.it with
      | TypD (id, tparams, deftyp) when id.it = name -> Some (tparams, deftyp)
      | _ -> None)
    spec

let rec gen_of_typ (spec : spec) (typ : typ) : Value.t Gen.t =
  let open Gen in
  match typ.it with
  | BoolT ->
    let* b = Arbitrary.Bool.arbitrary in
    return (Value.make_val BoolT (BoolV b))

  | NumT `NatT ->
    let* n = Gen.sized (fun s -> Gen.choose_int (0, max 0 s)) in
    return (Value.make_val (NumT `NatT) (NumV (`Nat (Bigint.of_int n))))

  | NumT `IntT ->
    let* n = Gen.sized (fun s -> Gen.choose_int (-s, s)) in
    return (Value.make_val (NumT `IntT) (NumV (`Int (Bigint.of_int n))))

  | TextT ->
    let* s = Arbitrary.Text.arbitrary in
    return (Value.make_val TextT (TextV s))

  | TupleT typs ->
    let* vs = Gen.sequence (List.map (gen_of_typ spec) typs) in
    return (Value.make_val (TupleT typs) (TupleV vs))

  | IterT (inner, Opt) ->
    let* v_opt = Gen.option_of (gen_of_typ spec inner) in
    return (Value.make_val typ.it (OptV v_opt))

  | IterT (inner, List) ->
    let* vs = Gen.list_of (gen_of_typ spec inner) in
    return (Value.make_val typ.it (ListV vs))

  | VarT (id, targs) ->
    (match find_typdef spec id.it with
     | Some (tparams, deftyp) ->
       (* generate recursively using an extended spec with type parameters
          substituted by actual type arguments; targ' = typ' so convert via targ.it $ targ.at *)
       let spec' =
         List.fold_left2
           (fun acc tparam targ ->
             let typ_of_targ = targ.it $ targ.at in
             acc @ [ TypD (tparam.it $ no_region, [], PlainT typ_of_targ $ no_region) $ no_region ])
           spec tparams targs
       in
       gen_of_deftyp spec' typ deftyp
     | None ->
       failwith (Printf.sprintf "Il_gen.gen_of_typ: unknown type '%s'" id.it))

  | FuncT ->
    failwith "Il_gen.gen_of_typ: cannot generate values of FuncT"

(* outer_typ: the external type from VarT or PlainT.
   Provides the typ' used in make_val for StructV and CaseV. *)
and gen_of_deftyp (spec : spec) (outer_typ : typ) (deftyp : deftyp) : Value.t Gen.t =
  let open Gen in
  match deftyp.it with
  | PlainT typ ->
    gen_of_typ spec typ

  | StructT fields ->
    let* vfields =
      Gen.sequence
        (List.map
           (fun (atom, ftyp) ->
             let* v = gen_of_typ spec ftyp in
             return (atom, v))
           fields)
    in
    return (Value.make_val outer_typ.it (StructV vfields))

  | VariantT cases ->
    let outer_name = match outer_typ.it with
      | VarT (id, _) -> Some id.it
      | _ -> None
    in
    let make_case_gen (nottyp, _, _) =
      let mixop, typs = Mixfix.split nottyp.it in
      Gen.scale (fun n -> max 0 (n - 1))
        (let* vs = Gen.sequence (List.map (gen_of_typ spec) typs) in
         return (Value.make_val outer_typ.it (CaseV (Mixfix.fill mixop vs))))
    in
    let is_recursive (nottyp, _, _) =
      let typs = Mixfix.args nottyp.it in
      match outer_name with
      | None -> false
      | Some name -> List.exists (typ_has_varname name) typs
    in
    Gen.sized (fun size ->
      let candidate_cases =
        if size = 0 then
          let base_cases = List.filter (fun c -> not (is_recursive c)) cases in
          if base_cases = [] then cases
          else base_cases
        else
          cases
      in
      match List.map make_case_gen candidate_cases with
      | [] -> failwith "Il_gen.gen_of_deftyp: VariantT with no cases"
      | gens -> Gen.oneof gens)

let shrink (spec : spec) =
  let rec shrink (v : Value.t) : Value.t list =
    let t = v.note.typ in
    match v.it with
    | ListV vs ->
      let n = List.length vs in
      if n = 0 then []
      else
        Value.make_val t (ListV []) ::
        List.init n (fun i ->
          Value.make_val t (ListV (List.filteri (fun j _ -> j <> i) vs)))
        @
        List.concat_map (fun (i, vi) ->
          List.map (fun vi' ->
            Value.make_val t
              (ListV (List.mapi (fun j vj -> if j = i then vi' else vj) vs)))
          (shrink vi))
        (List.mapi (fun i vi -> (i, vi)) vs)
    | OptV (Some inner) ->
      Value.make_val t (OptV None) ::
      List.map (fun inner' -> Value.make_val t (OptV (Some inner')))
        (shrink inner)
    | TupleV vs ->
      List.concat_map (fun (i, vi) ->
        List.map (fun vi' ->
          Value.make_val t
            (TupleV (List.mapi (fun j vj -> if j = i then vi' else vj) vs)))
        (shrink vi))
      (List.mapi (fun i vi -> (i, vi)) vs)
    | StructV fields ->
      List.concat_map (fun (i, (_, vi)) ->
        List.map (fun vi' ->
          Value.make_val t
            (StructV (List.mapi (fun j (aj, vj) ->
              if j = i then (aj, vi') else (aj, vj)) fields)))
        (shrink vi))
      (List.mapi (fun i f -> (i, f)) fields)
    | CaseV vc ->
      let args = Mixfix.args vc in
      (match v.note.typ with
       | VarT (id, _) ->
         (match find_typdef spec id.it with
          | Some (_, deftyp) ->
            (match deftyp.it with
             | VariantT cases ->
               let outer_name = id.it in
               (* Find which case in the variant matches the current value
                  by comparing atom structure after filling the type-level mixop *)
               let current_case =
                 List.find_opt (fun (nottyp, _, _) ->
                   let mixop, typs = Mixfix.split nottyp.it in
                   List.length typs = List.length args &&
                   let filled = Mixfix.fill mixop args in
                   List.length filled = List.length vc &&
                   List.for_all2 (fun p1 p2 ->
                     match p1, p2 with
                     | Mixfix.Atom a1, Mixfix.Atom a2 ->
                       Xl.Atom.compare a1.it a2.it = 0
                     | Mixfix.Arg _, Mixfix.Arg _ -> true
                     | _ -> false)
                   vc filled)
                 cases
               in
               (* Strategy 1: if current case is recursive, return same-type subcomponents *)
               let recursive_subcomponents =
                 match current_case with
                 | None -> []
                 | Some (nottyp, _, _) ->
                   let _, typs = Mixfix.split nottyp.it in
                   List.filter_map (fun (typ, vi) ->
                     match typ.it with
                     | VarT (sub_id, _) when sub_id.it = outer_name -> Some vi
                     | _ -> None)
                   (List.combine typs args)
               in
               (* Shrink each argument of the current case *)
               let shrunk_args =
                 List.concat_map (fun (i, vi) ->
                   List.map (fun vi' ->
                     let arg_idx = ref 0 in
                     let new_vc =
                       List.map (fun part ->
                         match part with
                         | Mixfix.Atom a -> Mixfix.Atom a
                         | Mixfix.Arg _ ->
                           let idx = !arg_idx in incr arg_idx;
                           Mixfix.Arg (if idx = i then vi' else List.nth args idx))
                       vc
                     in
                     Value.make_val t (CaseV new_vc))
                   (shrink vi))
                 (List.mapi (fun i vi -> (i, vi)) args)
               in
               recursive_subcomponents @ shrunk_args
             | _ -> [])
          | None -> [])
       | _ -> [])
    | _ -> []
  in shrink
