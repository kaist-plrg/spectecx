(* IL 타입 기반 값 생성기.
   Lang.Il의 typ'와 deftyp'로부터 Value.t Gen.t를 파생한다.
   gen_of_typ / gen_of_deftyp은 상호 재귀로 정의된다.
   gen_of_deftyp은 값의 typ' 어노테이션을 위해 외부 typ을 인자로 받는다. *)

open Lang.Il
open Common.Source

(* tparams와 deftyp을 함께 반환하여 호출자가 타입 인자를 치환할 수 있게 한다 *)
(* case의 sub-type 중 하나라도 name과 같은 VarT를 포함하면 재귀 케이스로 분류 *)
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
       (* 타입 파라미터를 실제 타입 인자로 치환한 확장 spec으로 재귀 생성 *)
       (* targ' = typ' 이므로 targ.it $ targ.at로 typ으로 변환한다 *)
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

(* outer_typ: VarT나 PlainT에서 온 외부 타입.
   StructV, CaseV의 make_val에 사용할 typ'를 제공한다. *)
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
