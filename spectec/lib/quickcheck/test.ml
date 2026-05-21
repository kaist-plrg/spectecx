(* QuickCheck test runner *)

type config = {
  num_tests : int;
  max_size : int;
  seed : [ `Deterministic of int | `Nondeterministic ];
  verbose : bool;
}

let default_config =
  { num_tests = 300; max_size = 50; seed = `Deterministic 42; verbose = false }

type outcome =
  | Pass of { num_tests : int; stamps : (string * int) list }
  | Fail of { num_tests : int; counterexample : string list }
  | Gave_up of { num_tests : int }

(* Aggregates label frequencies and returns a sorted list *)
let count_stamps stamps =
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun s ->
      let n = try Hashtbl.find tbl s with Not_found -> 0 in
      Hashtbl.replace tbl s (n + 1))
    stamps;
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []
  |> List.sort (fun (_, a) (_, b) -> compare b a)

let rec shrink_loop (r : Property.Result.t) : Property.Result.t =
  let candidate_gens = r.Property.Result.shrink () in
  let failing =
    List.find_map
      (fun gen ->
        let r' = Gen.run gen ~size:0 ~rand:(Random.make 0) in
        if r'.Property.Result.ok = Some false then Some r' else None)
      candidate_gens
  in
  match failing with None -> r | Some r' -> shrink_loop r'

let rec generalize_loop (r : Property.Result.t) : Property.Result.t =
  let candidates = r.Property.Result.generalize () in
  let found =
    List.find_map
      (fun (_, gens) ->
        if gens = [] then None
        else
          let results =
            List.map (fun gen -> Gen.run gen ~size:3 ~rand:(Random.make 0)) gens
          in
          if
            List.exists (fun r' -> r'.Property.Result.ok = Some false) results
            && List.for_all
                 (fun r' -> r'.Property.Result.ok <> Some true)
                 results
          then Some (List.hd results)
          else None)
      candidates
  in
  match found with
  | None    -> r
  | Some r' ->
    if r'.Property.Result.arguments = r.Property.Result.arguments then r
    else generalize_loop r'

let check ?(config = default_config) prop =
  let rand =
    match config.seed with
    | `Deterministic n -> Random.make n
    | `Nondeterministic -> Random.make_self_init ()
  in
  let gen = Property.evaluate prop in
  (* Each trial derives an independent PRNG by splitting and advancing rand *)
  let rec loop i discarded all_stamps rand =
    if i >= config.num_tests then
      Pass { num_tests = i; stamps = count_stamps all_stamps }
    else if discarded > config.num_tests * 10 then Gave_up { num_tests = i }
    else
      let size =
        if config.max_size = 0 then 0
        else i * config.max_size / config.num_tests
      in
      let trial_rand, next_rand = Random.split rand in
      let result = Gen.run gen ~size ~rand:trial_rand in
      if config.verbose then
        Printf.printf "Test %d: %s\n%!" (i + 1)
          (match result.Property.Result.ok with
          | Some true -> "OK"
          | Some false -> "FAIL"
          | None -> "discarded");
      match result.Property.Result.ok with
      | Some false ->
          let minimal = shrink_loop result in
          let generalized = generalize_loop minimal in
          Fail
            {
              num_tests = i + 1;
              counterexample = generalized.Property.Result.arguments;
            }
      | Some true ->
          loop (i + 1) discarded
            (result.Property.Result.stamp @ all_stamps)
            next_rand
      | None -> loop i (discarded + 1) all_stamps next_rand
  in
  loop 0 0 [] rand

type opt = Prop | Gen

let print_outcome opt = function
  | Pass { num_tests; stamps } ->
      (match opt with
      | Prop -> Printf.printf "OK, passed %d samples.\n" num_tests
      | Gen -> Printf.printf "OK, generated %d tests.\n" num_tests);
      List.iter
        (fun (lbl, count) ->
          Printf.printf "%3d%% %s\n\n" (count * 100 / num_tests) lbl)
        stamps
  | Fail { num_tests; counterexample } -> (
      match opt with
      | Prop ->
          Printf.printf "Falsifiable, after %d tests:\n" num_tests;
          List.iter (fun s -> Printf.printf "  %s\n" s) counterexample
      | Gen -> ())
  | Gave_up { num_tests } ->
      Printf.printf "Gave up after %d tests (too many discarded).\n" num_tests
