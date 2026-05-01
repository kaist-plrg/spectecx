# QuickCheck 라이브러리 문서

spectec-core의 프로퍼티 기반 테스팅(Property-Based Testing) 라이브러리.  
Haskell QuickCheck에서 영감을 받아, SpecTec 명세 언어와 통합되어 있다.

---

## 목차

1. [개요](#개요)
2. [디렉토리 구조](#디렉토리-구조)
3. [아키텍처: 3단계 파이프라인](#아키텍처-3단계-파이프라인)
4. [모듈별 상세 설명](#모듈별-상세-설명)
5. [.quickcheck 파일 문법](#quickcheck-파일-문법)
6. [실행 흐름](#실행-흐름)
7. [CLI 사용법](#cli-사용법)
8. [예제](#예제)

---

## 개요

QuickCheck 라이브러리는 `.quickcheck` 파일에 정의된 속성(property)을 랜덤 입력값으로 자동 테스트한다.  
두 가지 모드를 지원한다:

| 모드 | 키워드 | 역할 |
|------|--------|------|
| **Property 테스트** | `quickcheck/prop` | 명제가 무작위 입력에 대해 항상 성립하는지 검증 |
| **Generator** | `quickcheck/gen` | 전제(premise)를 만족하는 값 샘플을 출력 |

---

## 디렉토리 구조

```
spectec/lib/quickcheck/
├── quickcheck.ml(i)   # 메인 진입점 및 명령 실행 디스패처
├── qc_ast.ml(i)       # .quickcheck 파일 파싱 결과 AST
├── qc_parse.ml(i)     # .quickcheck 파일 파서
├── qc_ir.ml(i)        # 정제된 중간 표현 (IR)
├── qc_elab.ml(i)      # AST → IR 정제(elaboration)
├── gen.ml(i)          # 크기 파라미터 기반 모나딕 생성기
├── arbitrary.ml(i)    # Arbitrary / Coarbitrary 타입클래스 시뮬레이션
├── property.ml(i)     # Property 시스템 및 Testable 인터페이스
├── test.ml(i)         # 테스트 러너 및 결과 출력
├── random.ml(i)       # XorShift 기반 분리 가능한(splittable) PRNG
├── il_gen.ml(i)       # IL 타입으로부터 임의 값 생성
└── dune               # 빌드 설정
```

---

## 아키텍처: 3단계 파이프라인

```
.quickcheck 파일
      │
      ▼
 [1] qc_parse       ── 텍스트를 파싱하여 AST 생성
      │                  (EL 타입, EL premise 재사용)
      ▼
 [2] qc_elab        ── AST → IR 정제
      │                  (EL 타입 → IL 타입, premise 바운드 변수 추출)
      ▼
 [3] quickcheck.ml  ── IR 명령 실행
           ├── QcProp: 랜덤 값 생성 → 전제 실행 → 목표 검증
           └── QcGen:  전제 만족하는 값 샘플링 → 출력
```

---

## 모듈별 상세 설명

### `random.ml` — PRNG

XorShift 알고리즘 기반의 순수 함수형 난수 생성기.

```ocaml
type t  (* 두 개의 int seed *)

val make           : int * int -> t
val make_self_init : unit -> t
val split          : t -> t * t   (* 독립적인 두 스트림으로 분리 *)
val bool  : t -> bool
val int   : int * int -> t -> int
val float : float * float -> t -> float
val char  : t -> char
```

핵심: `split`으로 PRNG를 두 개의 독립 스트림으로 나눌 수 있어서,  
모나딕 `bind`에서 부작용 없이 독립적인 난수를 공급할 수 있다.

---

### `gen.ml` — 모나딕 생성기

```ocaml
type 'a t = Gen of (int -> Random.t -> 'a)
(*                   ^^^    ^^^^^^^
*                  size    PRNG seed  *)
```

- `size`: 생성되는 값의 복잡도 제어 (리스트 길이, 재귀 깊이 등)
- `bind`가 PRNG를 `split`해서 독립 스트림을 각 하위 생성기에 공급

**주요 콤비네이터:**

| 함수 | 설명 |
|------|------|
| `sized (f : int -> 'a t) : 'a t` | 현재 size 값을 노출 |
| `choose_int (lo, hi) : int t` | `[lo, hi]` 구간 정수 |
| `elements : 'a list -> 'a t` | 리스트에서 균등 선택 |
| `oneof : 'a t list -> 'a t` | 여러 생성기 중 하나 선택 |
| `frequency : (int * 'a t) list -> 'a t` | 가중치 기반 선택 |
| `variant : int -> 'a t -> 'a t` | PRNG 상태를 n번 교란 |
| `list_of : ?min:int -> 'a t -> 'a list t` | 임의 길이 리스트 |
| `option_of : 'a t -> 'a option t` | Some/None 랜덤 선택 |
| `promote : ('a -> 'b t) -> ('a -> 'b) t` | 함수 생성기 |

---

### `arbitrary.ml` — Arbitrary / Coarbitrary

Haskell의 타입클래스를 OCaml 모듈로 구현한다.

```ocaml
module type ARBITRARY = sig
  type t
  val arbitrary : t Gen.t
end

module type COARBITRARY = sig
  type t
  val coarbitrary : t -> 'b Gen.t -> 'b Gen.t
  (* 값으로 PRNG를 교란하여 함수 생성기에 사용 *)
end
```

**기본 인스턴스:**

| 모듈 | 타입 |
|------|------|
| `Bool` | `bool` |
| `Nat` | `int (>= 0)` |
| `Int` | `int` |
| `Char` | `char` |
| `Text` | `string` |

**파생 펑터:**

| 펑터 | 역할 |
|------|------|
| `Make_list(A)` | `A.t list` |
| `Make_option(A)` | `A.t option` |
| `Make_pair(A)(B)` | `A.t * B.t` |
| `Make_fun(A)(B)` | `A.t -> B.t` (Coarbitrary + Arbitrary 조합) |

---

### `property.ml` — 프로퍼티 시스템

```ocaml
(* 테스트 결과 *)
module Result : sig
  type t = {
    ok        : bool option;   (* None=버림, Some true=통과, Some false=실패 *)
    stamp     : string list;   (* 레이블 (classify/collect용) *)
    arguments : string list;   (* 반례 출력용 *)
  }
end

(* 프로퍼티 = 결과를 생성하는 생성기 *)
type t = Prop of Result.t Gen.t

module type TESTABLE = sig
  type t
  val property : t -> Property.t
end
```

**콤비네이터:**

| 함수 | 설명 |
|------|------|
| `for_all : (show : 'a -> string) -> 'a Gen.t -> ('a -> t) -> t` | 전칭 속성 |
| `==>` | 조건부 속성 (`prem ==> prop`) |
| `label : string -> t -> t` | 테스트 케이스에 레이블 부착 |
| `classify : bool -> string -> t -> t` | 조건부 분류 |
| `collect : (show : 'a -> string) -> 'a -> t -> t` | 값 분포 수집 |

---

### `test.ml` — 테스트 러너

```ocaml
type config = {
  num_tests : int;   (* 테스트 횟수, 기본 100 *)
  max_size  : int;   (* 최대 size, 기본 20 *)
  seed      : int * int;  (* 초기 시드, 기본 (43, 0) — 재현 가능 *)
  verbose   : bool;
}

type outcome = Pass | Fail of string list | Gave_up

val check      : Property.t -> outcome
val quickcheck : Property.t -> unit  (* 결과를 사람이 읽기 좋게 출력 *)
```

---

### `il_gen.ml` — IL 타입 기반 생성기

IL의 타입 정보로부터 임의 값을 생성하는 핵심 모듈.  
정의된 spec를 통해 variant(대수적 타입) 생성자를 찾아 재귀적으로 값을 구성한다.

```ocaml
val gen_of_typ : Lang.Il.spec -> Lang.Il.typ -> Lang.Il.Value.t Gen.t
```

| IL 타입 | 생성 전략 |
|---------|----------|
| `BoolT` | `true` / `false` |
| `NumT` | 크기 제한 숫자 |
| `TextT` | 임의 문자열 |
| `TupleT ts` | 각 타입 재귀 생성 후 튜플 조합 |
| `IterT(t, Opt)` | `None` 또는 `Some(gen)` |
| `IterT(t, List)` | 임의 길이 리스트 |
| `VarT id` | spec에서 정의를 찾아 생성자 중 선택, size 감소로 종료 보장 |

---

### `qc_ast.ml` — .quickcheck 파일 AST

```ocaml
type ast_param = {
  p_id  : id;
  p_typ : plaintyp;   (* EL 타입 *)
}

type ast_block =
  | AB_Prop of {
      params : ast_param list;
      goal   : prem;        (* 검증할 목표 premise *)
      prems  : prem list;   (* -- 로 시작하는 전제들 *)
    }
  | AB_Gen of {
      params : ast_param list;
      prems  : prem list;
    }

type ast_file = ast_block list
```

---

### `qc_ir.ml` — 정제된 IR

```ocaml
type ir_var = {
  iv_id  : string;
  iv_typ : Lang.Il.typ;   (* IL 타입으로 변환 완료 *)
}

type qc_command =
  | QcProp of {
      free_vars     : ir_var list;   (* 생성해야 할 자유 변수들 *)
      all_var_names : id' list;      (* 환경에 바인딩할 이름 목록 *)
      goal          : prem;
      prems         : prem list;
    }
  | QcGen of {
      free_vars     : ir_var list;
      all_var_names : id' list;
      prems         : prem list;
    }
```

---

### `qc_elab.ml` — 정제(Elaboration)

AST → IR 변환 과정:

1. EL `plaintyp` → IL `typ` 변환 (`Elaborate.elab_typ` 재사용)
2. 각 param의 바운드 변수를 추출하여 `all_var_names` 구성
3. EL premise → IL premise 정제 (`Elaborate.elab_prems_in_spec` 재사용)
4. 첫 번째 non-premise 라인 = goal (QcProp), 없으면 QcGen

---

### `quickcheck.ml` — 메인 진입점

```ocaml
val quickcheck_file : Lang.Il.spec -> string -> unit
(*                    IL 명세      .quickcheck 파일 경로 *)
```

**QcProp 실행 흐름:**

```
free_vars 각각에 대해:
  il_gen.gen_of_typ으로 임의 값 생성
      │
      ▼
  Interp.run_prems(prems, env) 실행
      │
  ├── 실패 → 이 케이스 버림(discard), 다음 시도
  └── 성공 → Interp.run_prems([goal], env) 실행
                  │
              ├── 성공 → 테스트 통과
              └── 실패 → 반례 발견, 테스트 실패
```

**QcGen 실행 흐름:**

```
free_vars 각각에 대해:
  il_gen.gen_of_typ으로 임의 값 생성
      │
      ▼
  Interp.run_prems(prems, env) 실행
      │
  ├── 실패 → 버림, 다음 시도
  └── 성공 → env 바인딩 출력
```

---

## .quickcheck 파일 문법

```
quickcheck/prop
    (변수1 : 타입1) (변수2 : 타입2) ...
    목표_premise
    -- 전제_premise_1
    -- 전제_premise_2

quickcheck/gen
    (변수1 : 타입1) (변수2 : 타입2) ...
    -- 전제_premise_1
    -- 전제_premise_2
```

**규칙:**
- 파라미터: `(변수이름 : EL타입)` 형식, 공백으로 구분
- `quickcheck/prop`에서 `--` 없는 첫 줄 = 검증 목표
- `--` 로 시작하는 줄 = 전제(premise, 필터 조건)
- 타입과 premise는 명세 파일(.watsup)의 문법을 그대로 사용

---

## 실행 흐름

```
spectec quickcheck spec.watsup --target test.quickcheck
         │
         ▼
    1. spec.watsup 파싱 및 IL 정제
         │
         ▼
    2. test.quickcheck 파싱 (qc_parse)
         │
         ▼
    3. AST → IR 정제 (qc_elab)
         │
         ▼
    4. 각 명령 실행 (quickcheck.ml)
         │
         ├── QcProp: Test 모듈로 프로퍼티 검증
         └── QcGen:  샘플 값 출력
```

---

## CLI 사용법

```bash
# 기본 사용
spectec quickcheck <spec파일들...> --target <.quickcheck파일>

# 예시
spectec quickcheck tutorial.watsup --target test.quickcheck
```

---

## 예제

### `spectec/examples/tutorial/test.quickcheck`

```
# prog 타입이 타입 검사를 통과하면 실행도 성공해야 한다 (타입 안전성)
quickcheck/prop
    (prog : prog)
    Eval_prog: |- prog `=> _
    -- Check_prog: |- prog

# prog 타입이 실행 가능하면 항상 실행 성공 (무조건)
quickcheck/prop
    (prog : prog)
    Eval_prog: |- prog `=> _

# 타입 검사를 통과하는 prog 샘플 출력
quickcheck/gen
    (prog : prog)
    -- Check_prog: |- prog
```

**설명:**

| 블록 | 의미 |
|------|------|
| 첫 번째 `prop` | `Check_prog`(타입 검사)를 통과하는 `prog`만 걸러서, 그 중 `Eval_prog`(실행)도 성공하는지 확인 → 타입 안전성 검증 |
| 두 번째 `prop` | 모든 임의 `prog`에 대해 실행이 성공하는지 확인 |
| `gen` | 타입 검사를 통과하는 `prog` 샘플을 생성하여 출력 |

---

## 주요 설계 결정

### 1. 기존 인프라 최대 재사용
- EL/IL 타입 및 premise 문법을 그대로 사용 → .quickcheck 파일에서 명세와 동일한 문법 사용 가능
- `Interp.run_prems`를 전제 실행에 재사용
- `Elaborate.elab_prems_in_spec`을 정제에 재사용

### 2. 순수 함수형 PRNG
- `random.ml`의 `split` 덕분에 모나딕 바인딩이 독립적인 난수 스트림을 보장
- 결정적 시드(43, 0)로 기본 재현 가능

### 3. 크기 제어(Size Parameter)
- 재귀 타입 생성 시 size를 감소시켜 무한 루프 방지
- `Test.max_size`로 테스트 전반의 복잡도 제어

---

## 현재 상태 (2025-05)

- 브랜치: `quickcheck`
- 최근 커밋:
  - `36b68fd8` — 출력 형식 수정 (`Property]` → `Test]`)
  - `d69b4414` — `Qc_ir` 변수 추적 단순화
  - `2640ef7d` — 초기 구현 (전체 인프라)
- main과의 sync: 정기적으로 merge하여 최신 EL/IL 변경사항 반영 중
