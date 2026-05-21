(* Property system — direct translation of Property/Result/Testable from goal.md *)

module Result = struct
  type t = {
    ok : bool option;
    stamp : string list;
    arguments : string list;
    shrink : unit -> t Gen.t list;
    generalize : unit -> (string * t Gen.t list) list;
  }

  let nothing =
    {
      ok = None;
      stamp = [];
      arguments = [];
      shrink = (fun () -> []);
      generalize = (fun () -> []);
    }

  let with_ok b = { nothing with ok = Some b }
  let add_argument s r = { r with arguments = s :: r.arguments }
  let add_stamp s r = { r with stamp = s :: r.stamp }
end

type t = Prop of Result.t Gen.t

(* Alias to refer to the outer t from within a nested module type *)
type prop = t

let of_result res = Prop (Gen.return res)
let evaluate (Prop gen) = gen

module type TESTABLE = sig
  type t

  val property : t -> prop
end

(* Bool instance: property b = result (nothing { ok = Just b }) *)
module Bool_testable = struct
  type t = bool

  let property b = of_result (Result.with_ok b)
end

(* Property instance: property prop = prop *)
module Prop_testable = struct
  type t = prop

  let property p = p
end

(* Function instance: property f = forAll arbitrary f *)
module Make_fun_testable (A : Arbitrary.ARBITRARY) (B : TESTABLE) = struct
  type t = A.t -> B.t

  let property f =
    (* for_all is defined below; build directly to avoid a forward reference *)
    Prop
      (let open Gen in
       let* a = A.arbitrary in
       let* res = evaluate (B.property (f a)) in
       return (Result.add_argument "<fun-arg>" res))
end

(* forAll: direct translation from goal.md
   forAll gen body = Prop $ do
     a <- gen; res <- evaluate (body a)
     return (res { arguments = show a : arguments res }) *)
let generalize_n = 10

let rec for_all ?(shrink = fun _ -> []) ?(generalize = fun _ -> []) ~show gen
    body =
  Prop
    (let open Gen in
     let* a = gen in
     let* res = evaluate (body a) in
     let base = Result.add_argument (show a) res in
     return
       {
         base with
         Result.shrink =
           (fun () ->
             List.map
               (fun a' ->
                 evaluate
                   (for_all ~shrink ~generalize ~show (Gen.return a') body))
               (shrink a));
         Result.generalize =
           (fun () ->
             List.map
               (fun (s, gen') ->
                 let samples =
                   List.init generalize_n (fun i ->
                       Gen.map
                         (fun r ->
                           {
                             r with
                             Result.arguments =
                               (match r.Result.arguments with
                               | _ :: rest -> s :: rest
                               | [] -> [ s ]);
                           })
                         (evaluate
                            (for_all ~shrink ~generalize ~show
                               (Gen.variant i gen') body)))
                 in
                 (s, samples))
               (generalize a));
       })

(* ==>: precondition filtering *)
let ( ==> ) cond prop = if cond then prop else of_result Result.nothing

(* label: adds a label to stamp *)
let label s (Prop gen) = Prop (Gen.map (Result.add_stamp s) gen)
let classify cond name prop = if cond then label name prop else prop
let collect ~show v prop = label (show v) prop
