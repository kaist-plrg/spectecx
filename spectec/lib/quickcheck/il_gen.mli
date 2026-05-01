(** IL 타입 기반 임의 값 생성기.

    [Lang.Il.Types]의 [typ']와 [deftyp']로부터 [Il.Value.t Gen.t]를 파생한다.
    SpecTec spec에 정의된 타입들에 대해 임의 값을 생성하여
    속성 기반 테스트의 입력으로 사용한다. *)

open Lang.Il

(** {2 타입 기반 생성} *)

val gen_of_typ : spec -> typ -> Value.t Gen.t
(** [gen_of_typ spec typ]은 [typ]의 임의 값을 생성하는 생성기를 반환한다.

    타입별 생성 규칙:
    - [BoolT]          → [BoolV]
    - [NumT `NatT]     → [NumV (`Nat n)] (0 이상, 크기 파라미터 이하)
    - [NumT `IntT]     → [NumV (`Int n)] ([-size, size] 범위)
    - [TextT]          → [TextV s]
    - [TupleT typs]   → [TupleV vs] (각 타입 재귀 생성)
    - [IterT (t, Opt)] → [OptV v]
    - [IterT (t, List)]→ [ListV vs] (크기 제한)
    - [VarT (id, _)]  → spec에서 정의를 조회하여 재귀 생성
    - [FuncT]          → 예외 발생 (함수 값은 생성 불가) *)

val gen_of_deftyp : spec -> typ -> deftyp -> Value.t Gen.t
(** [gen_of_deftyp spec outer_typ deftyp]은 [PlainT], [StructT], [VariantT]를 처리한다.
    [outer_typ]는 생성된 값의 [vnote.typ] 어노테이션에 사용된다.
    [VariantT]의 경우 크기 파라미터를 줄여 재귀 타입의 무한 루프를 방지한다. *)
