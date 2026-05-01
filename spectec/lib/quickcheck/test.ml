(* QuickCheck 테스트 러너 *)

type config = {
  num_tests : int;
  max_size : int;
  seed : [ `Deterministic of int | `Nondeterministic ];
  verbose : bool;
}

let default_config = {
  num_tests = 1000;
  max_size = 20;
  seed = `Deterministic 43;
  verbose = false;
}

type outcome =
  | Pass of { num_tests : int; stamps : (string * int) list }
  | Fail of { num_tests : int; counterexample : string list }
  | Gave_up of { num_tests : int }

(* 레이블 빈도를 집계하여 정렬된 리스트로 반환 *)
let count_stamps stamps =
  let tbl = Hashtbl.create 16 in
  List.iter (fun s ->
    let n = try Hashtbl.find tbl s with Not_found -> 0 in
    Hashtbl.replace tbl s (n + 1))
    stamps;
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []
  |> List.sort (fun (_, a) (_, b) -> compare b a)

let check ?(config = default_config) prop =
  let rand =
    match config.seed with
    | `Deterministic n -> Random.make n
    | `Nondeterministic -> Random.make_self_init ()
  in
  let gen = Property.evaluate prop in
  (* 각 트라이얼마다 독립 PRNG를 split으로 파생하여 실행한다 *)
  let rec loop i discarded all_stamps =
    if i >= config.num_tests then
      Pass { num_tests = i; stamps = count_stamps all_stamps }
    else if discarded > config.num_tests * 10 then
      Gave_up { num_tests = i }
    else begin
      let size =
        if config.max_size = 0 then 0
        else (i * config.max_size) / config.num_tests
      in
      let _, trial_rand = Random.split rand in
      let result = Gen.run gen ~size ~rand:trial_rand in
      if config.verbose then
        Printf.printf "Test %d: %s\n%!" (i + 1)
          (match result.Property.Result.ok with
           | Some true -> "OK"
           | Some false -> "FAIL"
           | None -> "discarded");
      match result.Property.Result.ok with
      | Some false ->
        Fail { num_tests = i + 1;
               counterexample = result.Property.Result.arguments }
      | Some true ->
        loop (i + 1) discarded
          (result.Property.Result.stamp @ all_stamps)
      | None ->
        loop i (discarded + 1) all_stamps
    end
  in
  loop 0 0 []

let quickcheck ?(config = default_config) prop =
  match check ~config prop with
  | Pass { num_tests; stamps } ->
    Printf.printf "OK, passed %d tests.\n" num_tests;
    if stamps <> [] then
      List.iter (fun (lbl, count) ->
        Printf.printf "%3d%% %s\n"
          (count * 100 / num_tests) lbl)
        stamps
  | Fail { num_tests; counterexample } ->
    Printf.printf "Falsifiable, after %d tests:\n" num_tests;
    List.iter (fun s -> Printf.printf "  %s\n" s) counterexample;
    failwith "QuickCheck: property falsified"
  | Gave_up { num_tests } ->
    Printf.printf "Gave up after %d tests (too many discarded).\n" num_tests

let for_all ?(config = default_config) ~show gen pred =
  let prop =
    Property.for_all ~show gen
      (fun a -> Property.Bool_testable.property (pred a))
  in
  quickcheck ~config prop
