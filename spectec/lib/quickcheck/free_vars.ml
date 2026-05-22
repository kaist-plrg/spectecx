open Lang.Il
open Common.Source

(* Parallels [Lang.Il.Free.free_exp]'s structure but collects (id, typ)
   pairs — the type is needed to dispatch each free input to the right
   generator and is recovered from [VarE]'s [.note]. *)
let rec vars_of_exp (exp : exp) : (id * typ) list =
  match exp.it with
  | BoolE _ | NumE _ | TextE _ | OptE None -> []
  | VarE id -> [ (id, exp.note $ exp.at) ]
  | UnE (_, _, e)
  | UpCastE (_, e)
  | DownCastE (_, e)
  | SubE (e, _)
  | MatchE (e, _)
  | OptE (Some e)
  | LenE e
  | DotE (e, _)
  | IterE (e, _) ->
      vars_of_exp e
  | BinE (_, _, l, r)
  | CmpE (_, _, l, r)
  | ConsE (l, r)
  | CatE (l, r)
  | MemE (l, r)
  | IdxE (l, r) ->
      vars_of_exp l @ vars_of_exp r
  | TupleE es | ListE es -> List.concat_map vars_of_exp es
  | CaseE notexp -> List.concat_map vars_of_exp (Mixfix.args notexp)
  | StrE fields -> List.concat_map (fun (_, e) -> vars_of_exp e) fields
  | SliceE (b, l, h) -> vars_of_exp b @ vars_of_exp l @ vars_of_exp h
  | UpdE (b, p, f) -> vars_of_exp b @ vars_of_path p @ vars_of_exp f
  | CallE (_, _, args) -> List.concat_map vars_of_arg args

and vars_of_path (path : path) : (id * typ) list =
  match path.it with
  | RootP -> []
  | IdxP (p, e) -> vars_of_path p @ vars_of_exp e
  | SliceP (p, l, h) -> vars_of_path p @ vars_of_exp l @ vars_of_exp h
  | DotP (p, _) -> vars_of_path p

and vars_of_arg (arg : arg) : (id * typ) list =
  match arg.it with ExpA e -> vars_of_exp e | DefA _ -> []

let partition_rel_args (input_indices : int list) (args : exp list) :
    exp list * exp list =
  List.mapi (fun i a -> (i, a)) args
  |> List.partition_map (fun (i, a) ->
         if List.mem i input_indices then Either.Left a else Either.Right a)

type prem_vars = { free : (id * typ) list; bound : (id * typ) list }

let rec vars_of_prem (rel_inputs : string -> int list option) (prem : prem) :
    prem_vars =
  match prem.it with
  | RulePr (rel_id, notexp) ->
      let args = Mixfix.args notexp in
      let input_indices =
        match rel_inputs rel_id.it with
        | Some idxs -> idxs
        | None ->
            Common.InternalError.disallowed prem.at
              (Printf.sprintf "relation %s not in spec" rel_id.it)
      in
      let in_args, out_args = partition_rel_args input_indices args in
      {
        free = List.concat_map vars_of_exp in_args;
        bound = List.concat_map vars_of_exp out_args;
      }
  | IfPr e | DebugPr e -> { free = vars_of_exp e; bound = [] }
  | IfHoldPr (_, notexp) | IfNotHoldPr (_, notexp) ->
      { free = List.concat_map vars_of_exp (Mixfix.args notexp); bound = [] }
  | LetPr (lhs, rhs) -> { free = vars_of_exp rhs; bound = vars_of_exp lhs }
  | ElsePr -> { free = []; bound = [] }
  | IterPr (p, _) -> vars_of_prem rel_inputs p

let rec dedup_by_id : (id * typ) list -> (id * typ) list = function
  | [] -> []
  | ((id, _) as v) :: rest ->
      v :: dedup_by_id (List.filter (fun (id', _) -> id'.it <> id.it) rest)

let rel_inputs_of core_spec rel_name =
  List.find_map
    (fun def ->
      match def.it with
      | RelD (id, _, inputs, _) when id.it = rel_name -> Some inputs
      | _ -> None)
    core_spec

let of_premises ~(core_spec : spec) (prems : prem list) : (id * typ) list =
  let vs = List.map (vars_of_prem (rel_inputs_of core_spec)) prems in
  let free = List.concat_map (fun v -> v.free) vs in
  let bound_ids =
    List.concat_map (fun v -> v.bound) vs |> List.map (fun (id, _) -> id.it)
  in
  let is_unbound (id, _) = not (List.mem id.it bound_ids) in
  free |> List.filter is_unbound |> dedup_by_id
