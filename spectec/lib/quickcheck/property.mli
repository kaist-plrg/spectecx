(** 속성 기반 테스트의 결과 타입과 Testable 인터페이스.

    goal.md의 Haskell 명세를 직역한다:
    - [Result] = [{ ok :: Maybe Bool; stamp :: [String]; arguments :: [String] }]
    - [Property] = [Prop (Gen Result)]
    - [TESTABLE] = [class Testable a where property :: a -> Property] *)

(** {2 테스트 결과} *)

module Result : sig
  type t = {
    ok : bool option;
    (** [None] = 판단 유보(neutral/discarded),
        [Some true] = 통과, [Some false] = 실패 *)
    stamp : string list;
    (** [label]/[classify]가 누적한 레이블. 통계 수집에 사용된다. *)
    arguments : string list;
    (** [for_all]이 기록한 반례 인자의 문자열 표현. *)
  }

  val nothing : t
  (** 기본 중립 결과: [{ ok = None; stamp = []; arguments = [] }]. *)

  val with_ok : bool -> t
  (** [with_ok b]는 [{ nothing with ok = Some b }]이다. *)

  val add_argument : string -> t -> t
  val add_stamp : string -> t -> t
end

(** {2 속성 타입} *)

type t = Prop of Result.t Gen.t
(** [Property]는 [Result]의 생성기이다. *)

(** 중첩 모듈 타입에서 외부 [t]를 참조하기 위한 별칭. *)
type prop = t

val of_result : Result.t -> t
(** 고정된 결과를 속성으로 올린다. *)

val evaluate : t -> Result.t Gen.t
(** 내부 생성기를 추출한다. *)

(** {2 Testable 타입클래스} *)

module type TESTABLE = sig
  type t
  val property : t -> prop
  (** [property x]는 [x]를 [Property]로 변환한다. *)
end

(** {2 원시 Testable 인스턴스} *)

module Bool_testable : TESTABLE with type t = bool
module Prop_testable : TESTABLE with type t = prop

(** {2 파생 Testable Functor} *)

module Make_fun_testable (A : Arbitrary.ARBITRARY) (B : TESTABLE) :
  TESTABLE with type t = A.t -> B.t
(** [Testable (a -> b)]를 [Arbitrary a]와 [Testable b]로부터 파생한다.
    Haskell의 [property f = forAll arbitrary f] 직역이다. *)

(** {2 속성 콤비네이터} *)

val for_all : show:('a -> string) -> 'a Gen.t -> ('a -> t) -> t
(** [for_all ~show gen body]는 [gen]으로 값을 생성하고, [body]에 공급하여
    결과의 [arguments]에 문자열 표현을 기록한다.
    goal.md의 [forAll] 직역이다. *)

val ( ==> ) : bool -> t -> t
(** [cond ==> prop]: [cond]가 [true]이면 [prop], 아니면 중립 결과를 반환한다. *)

val label : string -> t -> t
(** [label s prop]은 모든 결과의 [stamp]에 [s]를 추가한다. *)

val classify : bool -> string -> t -> t
(** [classify true name prop = label name prop],
    [classify false _ prop = prop]. *)

val collect : show:('a -> string) -> 'a -> t -> t
(** [collect ~show v prop = label (show v) prop]. *)
