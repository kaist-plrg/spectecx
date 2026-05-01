(* Property 시스템 — goal.md의 Property/Result/Testable 직역 *)

module Result = struct
  type t = {
    ok : bool option;
    stamp : string list;
    arguments : string list;
  }

  let nothing = { ok = None; stamp = []; arguments = [] }
  let with_ok b = { nothing with ok = Some b }
  let add_argument s r = { r with arguments = s :: r.arguments }
  let add_stamp s r = { r with stamp = s :: r.stamp }
end

type t = Prop of Result.t Gen.t

(* 중첩 모듈 타입에서 외부 t를 참조하기 위한 별칭 *)
type prop = t

let of_result res = Prop (Gen.return res)

let evaluate (Prop gen) = gen

module type TESTABLE = sig
  type t
  val property : t -> prop
end

(* Bool 인스턴스: property b = result (nothing { ok = Just b }) *)
module Bool_testable = struct
  type t = bool
  let property b = of_result (Result.with_ok b)
end

(* Property 인스턴스: property prop = prop *)
module Prop_testable = struct
  type t = prop
  let property p = p
end

(* 함수 인스턴스: property f = forAll arbitrary f *)
module Make_fun_testable (A : Arbitrary.ARBITRARY) (B : TESTABLE) = struct
  type t = A.t -> B.t
  let property f =
    (* for_all은 아래에 정의되므로 전방 참조를 피하기 위해 직접 구성 *)
    Prop (
      let open Gen in
      let* a = A.arbitrary in
      let* res = evaluate (B.property (f a)) in
      return (Result.add_argument "<fun-arg>" res))
end

(* forAll: goal.md 직역
   forAll gen body = Prop $ do
     a <- gen; res <- evaluate (body a)
     return (res { arguments = show a : arguments res }) *)
let for_all ~show gen body =
  Prop (
    let open Gen in
    let* a = gen in
    let* res = evaluate (body a) in
    return (Result.add_argument (show a) res))

(* ==>: 전제조건 필터링 *)
let ( ==> ) cond prop =
  if cond then prop else of_result Result.nothing

(* label: stamp에 레이블 추가 *)
let label s (Prop gen) = Prop (Gen.map (Result.add_stamp s) gen)

let classify cond name prop =
  if cond then label name prop else prop

let collect ~show v prop = label (show v) prop
