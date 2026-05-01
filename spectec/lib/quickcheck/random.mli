(** 분리 가능한(splittable) 순수 함수형 PRNG.

    Haskell QuickCheck의 [split :: StdGen -> (StdGen, StdGen)] 시맨틱을
    OCaml로 구현한다. Gen 모나드의 bind가 PRNG 상태를 두 독립 스트림으로
    분기할 수 있어야 하므로, split이 핵심 연산이다. *)

type t
(** 불변 PRNG 상태. 두 개의 int 씨드로 구성된다. *)

val make : int -> t
(** [make seed] deterministic PRNG를 seed로부터 초기화한다. *)

val make_self_init : unit -> t
(** [make_self_init ()] 비결정적(non-deterministic) PRNG를 만든다.
    OCaml stdlib의 [Random.State.make_self_init]를 내부적으로 사용한다. *)

val split : t -> t * t
(** [split r]은 [r]로부터 서로 독립적인 두 자식 스트림 [(r1, r2)]를 반환한다.
    Gen 모나드 bind의 핵심 연산: 왼쪽 계산에 r1, 오른쪽 계산에 r2를 공급한다. *)

val bool : t -> bool
(** 균등(uniform) 임의 bool을 반환한다. *)

val int : lo:int -> hi:int -> t -> int
(** [int ~lo ~hi r]은 [[lo, hi]] 범위의 균등 정수를 반환한다. *)

val float : lo:float -> hi:float -> t -> float
(** [float ~lo ~hi r]은 [[lo, hi]] 범위의 균등 부동소수점을 반환한다. *)

val char : t -> char
(** 출력 가능한 ASCII 문자(코드 32~126)를 균등하게 반환한다. *)
