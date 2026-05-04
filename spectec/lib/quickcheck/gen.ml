(* Gen monad: newtype Gen a = Gen (Int -> Rand -> a)
   Direct OCaml translation of the Haskell spec. *)

type 'a t = Gen of (int -> Random.t -> 'a)

(* --- Monad interface --- *)

let return a = Gen (fun _ _ -> a)

(* bind: splits r0 into r1/r2 — direct translation of >>=
   Gen m1 >>= k = Gen (\n r0 -> let (r1,r2) = split r0
                                  Gen m2 = k (m1 n r1)
                                  in m2 n r2) *)
let bind (Gen m1) k =
  Gen (fun n r0 ->
    let r1, r2 = Random.split r0 in
    let (Gen m2) = k (m1 n r1) in
    m2 n r2)

let map f (Gen m) = Gen (fun n r -> f (m n r))

let ( let* ) = bind
let ( let+ ) g f = map f g

let ( and* ) (Gen ga) (Gen gb) =
  Gen (fun n r ->
    let r1, r2 = Random.split r in
    (ga n r1, gb n r2))

(* --- Execution --- *)

let of_fun f = Gen f

let run (Gen m) ~size ~rand = m size rand

let sample g = run g ~size:5 ~rand:(Random.make_self_init ())

(* --- Core combinators --- *)

let sized f = Gen (fun n r -> let (Gen m) = f n in m n r)

let resize n (Gen m) = Gen (fun _ r -> m n r)

let scale f g = sized (fun n -> resize (f n) g)

let choose_int (lo, hi) = Gen (fun _ r -> Random.int ~lo ~hi r)

let elements = function
  | [] -> invalid_arg "Gen.elements: empty list"
  | xs ->
    let* i = choose_int (0, List.length xs - 1) in
    return (List.nth xs i)

let oneof = function
  | [] -> invalid_arg "Gen.oneof: empty list"
  | gens ->
    let* g = elements gens in
    g

let frequency = function
  | [] -> invalid_arg "Gen.frequency: empty list"
  | xs ->
    let total = List.fold_left (fun acc (w, _) -> acc + w) 0 xs in
    let* n = choose_int (1, total) in
    let rec pick n = function
      | [] -> assert false
      | (k, g) :: rest -> if n <= k then g else pick (n - k) rest
    in
    pick n xs

(* variant: implements `rands r !! (v+1)` from goal.md.
   Performs v+1 right-splits to deterministically perturb the PRNG state.
   Used by coarbitrary to build function generators. *)
let variant v (Gen m) =
  Gen (fun n r ->
    let rec perturb r k =
      if k = 0 then r
      else
        let _, r' = Random.split r in
        perturb r' (k - 1)
    in
    m n (perturb r (v + 1)))

(* promote: direct translation of promote from goal.md.
   promote f = Gen (\n r -> \a -> let Gen m = f a in m n r) *)
let promote f =
  Gen (fun n r -> fun a ->
    let (Gen m) = f a in
    m n r)

let list_of ?(min = 0) gen =
  let* n = sized (fun size -> choose_int (min, max min size)) in
  Gen (fun _ r ->
    let rec go acc k r =
      if k = 0 then List.rev acc
      else
        let r1, r2 = Random.split r in
        go ((run gen ~size:0 ~rand:r1) :: acc) (k - 1) r2
    in
    go [] n r)

let option_of gen =
  Gen (fun _ r ->
    if Random.bool r then
      let _, r' = Random.split r in
      Some (run gen ~size:0 ~rand:r')
    else None)

let pair ga gb =
  let* a = ga in
  let* b = gb in
  return (a, b)

let sequence gens =
  List.fold_right
    (fun g acc ->
      let* v = g in
      let* vs = acc in
      return (v :: vs))
    gens (return [])
