(** Arbitrary / Coarbitrary 타입클래스 시뮬레이션.

    Haskell의 타입클래스를 OCaml 모듈 타입([ARBITRARY], [COARBITRARY])으로
    표현하고, Functor([Make_list], [Make_pair] 등)로 복합 인스턴스를 파생한다.
    프로젝트의 [Envs.Make] Functor 패턴과 동일한 관용구를 따른다. *)

(** {2 타입클래스 시그니처} *)

module type ARBITRARY = sig
  type t
  val arbitrary : t Gen.t
  (** [arbitrary]는 [t] 타입의 기본 생성기이다. *)
end

module type COARBITRARY = sig
  type t
  val coarbitrary : t -> 'b Gen.t -> 'b Gen.t
  (** [coarbitrary x gen]은 [x]의 구조에 따라 [gen]의 PRNG 상태를 교란한다.
      함수 생성기([arbitrary (a -> b)])를 만들기 위해 사용된다. *)
end

(** {2 원시 인스턴스} *)

module Bool : sig
  include ARBITRARY with type t = bool
  val coarbitrary : bool -> 'b Gen.t -> 'b Gen.t
end

module Nat : sig
  (** 음이 아닌 정수. 크기 파라미터로 상한이 제한된다. *)
  include ARBITRARY with type t = int
  val coarbitrary : int -> 'b Gen.t -> 'b Gen.t
end

module Int : sig
  (** 부호 있는 정수. [-size, size] 범위에서 생성된다. *)
  include ARBITRARY with type t = int
  val coarbitrary : int -> 'b Gen.t -> 'b Gen.t
end

module Char : sig
  include ARBITRARY with type t = char
  val coarbitrary : char -> 'b Gen.t -> 'b Gen.t
end

module Text : sig
  include ARBITRARY with type t = string
  val coarbitrary : string -> 'b Gen.t -> 'b Gen.t
end

(** {2 파생 인스턴스 (Functor)} *)

module Make_list (A : ARBITRARY) : ARBITRARY with type t = A.t list
(** 리스트 인스턴스를 파생한다. 길이는 크기 파라미터로 제한된다. *)

module Make_option (A : ARBITRARY) : ARBITRARY with type t = A.t option
(** 옵션 인스턴스를 파생한다. *)

module Make_pair (A : ARBITRARY) (B : ARBITRARY) :
  ARBITRARY with type t = A.t * B.t
(** 쌍 인스턴스를 파생한다. *)

module Make_fun (A : COARBITRARY) (B : ARBITRARY) :
  ARBITRARY with type t = A.t -> B.t
(** 함수 인스턴스를 파생한다.
    Haskell의 [arbitrary = promote (`coarbitrary` arbitrary)] 직역이다. *)

(** {2 1급 모듈 헬퍼} *)

type 'a arbitrary = (module ARBITRARY with type t = 'a)

val gen_of : 'a arbitrary -> 'a Gen.t
(** [(module M)] 에서 생성기를 추출한다. *)
