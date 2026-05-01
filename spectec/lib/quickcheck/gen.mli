(** Gen 모나드: 크기 인식(size-aware) 임의 값 생성기.

    Haskell QuickCheck의 [newtype Gen a = Gen (Int -> Rand -> a)]를
    OCaml로 직역한 것이다. 크기 파라미터([int])는 생성되는 값의 복잡도 상한을
    제어하고, [Random.t]는 PRNG 상태를 운반한다.
    두 값 모두 bind를 통해 암묵적으로 전파된다. *)

type 'a t
(** ['a] 값의 생성기. 내부적으로 [int -> Random.t -> 'a] 함수로 표현된다. *)

(** {2 모나드 인터페이스} *)

val return : 'a -> 'a t
(** [return a]는 항상 [a]를 생성하는 상수 생성기이다. *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** 모나드 bind: PRNG 상태를 분리하여 계속 계산(continuation)에
    독립적인 스트림을 공급한다. goal.md의 [>>=] 직역이다. *)

val map : ('a -> 'b) -> 'a t -> 'b t

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
(** [let*]는 [bind]이다. 프로젝트의 [Common.Attempt] 컨벤션을 따른다. *)

val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
(** [let+]는 [map]이다. *)

val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
(** [and*]는 두 생성기를 PRNG 분리를 통해 독립적으로 실행하여 쌍을 만든다. *)

(** {2 실행} *)

val run : 'a t -> size:int -> rand:Random.t -> 'a
(** [run gen ~size ~rand]는 생성기를 실행한다. *)

val sample : 'a t -> 'a
(** [sample gen]은 기본 크기(30)와 자체 초기화된 PRNG로 실행한다.
    디버그 및 대화형 탐색에 유용하다. *)

val of_fun : (int -> Random.t -> 'a) -> 'a t
(** [of_fun f]는 함수 [f]를 생성기로 감싼다.
    내부 함수로 직접 생성기를 구성할 때 사용한다. *)

(** {2 핵심 콤비네이터} *)

val sized : (int -> 'a t) -> 'a t
(** [sized f]는 현재 크기 파라미터를 [f]에 전달한다. *)

val resize : int -> 'a t -> 'a t
(** [resize n gen]은 [gen]을 크기 [n]으로 고정하여 실행한다. *)

val scale : (int -> int) -> 'a t -> 'a t
(** [scale f gen]은 현재 크기를 [f]로 변환한 뒤 [gen]을 실행한다. *)

val choose_int : int * int -> int t
(** [choose_int (lo, hi)]는 [[lo, hi]] 범위의 균등 정수를 생성한다. *)

val elements : 'a list -> 'a t
(** [elements xs]는 [xs]에서 균등하게 하나를 선택한다.
    빈 리스트에 대해 [Invalid_argument]를 발생시킨다. *)

val oneof : 'a t list -> 'a t
(** [oneof gens]는 생성기 리스트에서 균등하게 하나를 선택하여 실행한다. *)

val frequency : (int * 'a t) list -> 'a t
(** [frequency weighted]는 가중치에 비례하여 생성기를 선택한다.
    goal.md의 [frequency] 직역이다. *)

val variant : int -> 'a t -> 'a t
(** [variant v gen]은 [gen] 실행 전에 PRNG 상태를 [v]에 따라 교란한다.
    [coarbitrary]가 함수 생성기를 만들기 위해 사용한다.
    goal.md의 [rands r !! (v+1)] 패턴 구현이다. *)

val promote : ('a -> 'b t) -> ('a -> 'b) t
(** [promote f]는 생성기를 반환하는 함수를 함수를 생성하는 생성기로 올린다.
    Haskell arbitrary(a -> b) 인스턴스 구현에 사용한다. *)

val list_of : ?min:int -> 'a t -> 'a list t
(** [list_of gen]은 크기 파라미터에 의해 길이가 제한된 리스트를 생성한다.
    [~min]은 최소 길이를 지정한다(기본값 0). *)

val option_of : 'a t -> 'a option t
(** [option_of gen]은 [None] 또는 [Some x]를 동등한 확률로 생성한다. *)

val pair : 'a t -> 'b t -> ('a * 'b) t
(** [pair ga gb]는 두 생성기를 독립적으로 실행하여 쌍을 만든다. *)

val sequence : 'a t list -> 'a list t
(** [sequence gens]는 생성기 리스트를 순서대로 실행하여 값 리스트를 만든다. *)
