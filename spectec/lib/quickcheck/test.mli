(** QuickCheck 테스트 러너.

    [check]는 속성을 실행하고 결과([outcome])를 반환한다.
    [quickcheck]는 결과를 출력하고 실패 시 예외를 발생시킨다. *)

(** {2 설정} *)

type config = {
  num_tests : int;
  (** 실행할 테스트 케이스 수. 기본값: 100. *)
  max_size : int;
  (** 최대 크기 파라미터. 0부터 max_size까지 점진적으로 증가한다. 기본값: 20. *)
  seed : [ `Deterministic of int | `Nondeterministic ];
  (** PRNG 씨드. 기본값: [`Deterministic 42] (재현 가능). *)
  verbose : bool;
  (** true이면 각 테스트 케이스를 출력한다. 기본값: false. *)
}

val default_config : config

(** {2 결과 타입} *)

type outcome =
  | Pass of { num_tests : int; stamps : (string * int) list }
  (** 모든 테스트 통과. [stamps]는 레이블 빈도를 담는다. *)
  | Fail of { num_tests : int; counterexample : string list }
  (** 반례 발견. [counterexample]은 [Result.arguments] 필드이다. *)
  | Gave_up of { num_tests : int }
  (** 중립 결과가 너무 많아 포기. *)

(** {2 러너} *)

val check : ?config:config -> Property.t -> outcome
(** [check prop]은 [prop]을 실행하고 [outcome]을 반환한다. *)

val quickcheck : ?config:config -> Property.t -> unit
(** [quickcheck prop]은 [check]를 실행하고 사람이 읽을 수 있는 보고서를 출력한다.
    [Fail]이면 [Failure] 예외를 발생시킨다. *)

(** {2 편의 진입점} *)

val for_all :
  ?config:config ->
  show:('a -> string) ->
  'a Gen.t ->
  ('a -> bool) ->
  unit
(** [for_all ~show gen pred]는 [gen]과 [pred]로부터 속성을 구성하고 실행한다. *)
