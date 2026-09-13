# The Rust surface grammar and its IL constructors

This file is the contract between the target's parser/value converter
(`spectec/targets/rust/`) and the encoding's `2-syntax.spectec`
(`spectec/specs/rust/`, written by Task 38). Every production of
`spec/ch02-syntax.tex` Figs. 2.1 (items), 2.2 (bounds and program types) and 2.3
(terms) appears below with the IL constructor the converter emits and the
`syntax` sort it belongs to. Constructor tags are keyword atoms (`Xl.Atom.Keyword`)
and are taken from `spectec-pilot/1-syntax.watsup` wherever the pilot has one, so
that `2-syntax.spectec` can reuse the pilot's declarations verbatim.

Source of truth: `rust-type-semantics` at `v0.2`, `spec/ch02-syntax.tex`.

Conventions used below:
- `x*` is an IL list, `x?` an IL option, `text` an IL text value, `nat` an IL nat.
- "sugar" marks a production that is **not** in Figs. 2.1–2.3 but is listed in
  §2.9's desugaring table $\desug$; the parser keeps it as its own constructor so
  that $\desug$ has something to eliminate.
- "folded" marks a $\desug$ row the *parser* performs, because it is purely local
  (needs no knowledge of the program's declarations) and its result is a core
  production. Those rows are noted in §7 and are not separate constructors.
- Fig. 2.4's internal forms (`OPQ`, `CLOT`, `FNDEF`, `DYN`, `RPLACE`, `RVAR`),
  Fig. 2.5's predicates and Fig. 2.6's environments are **not** surface syntax and
  are not produced by the parser. They are declared by `2-syntax.spectec` in the
  same sorts (`typ`, `region`) and listed here only where a sort is shared.

## 0. Labels are not surface syntax

`\lbl` is assigned by $\lbl$ in Ch. 3, **after** $\desug$ and $\expand$
(`spec/ch02-syntax.tex`, "Alias expansion": *"Expansion precedes labelling"*).
The parser therefore never invents a label. Every labelled occurrence
($\mathit{path}^p$, $S^p$, $\hole^p$, $\kw{impl}^p$, $|\ov{ca}|^p$, $\kw{async}^p$,
$n^p$, $\kw{loop}^p$, $\kw{return}^p$) carries a **`label?`** that the parser emits
empty; $\lbl$ fills it. This is the one shape change against the pilot, whose
constructors carry a bare `label`.

`syntax label = nat`.

## 1. Identifiers (§2.1 "Names")

All are `text`, exactly as the pilot declares them:

| sort | metavariable | note |
|---|---|---|
| `tyid` | `T` | `Self` is the reserved type-parameter name |
| `rgid` | `'a`, `'s` | written without the leading quote |
| `adtid` | `S`, `E` | struct or enum name |
| `variantid` | `V` | |
| `traitid` | `D` | |
| `associd` | `A` | associated type / const / fn name in a path |
| `fnid` | `f` | |
| `constid` | `C` | |
| `varid` | `x` | term variable or field name (may be a numeral) |
| `aliasid` | `A` | transparent top-level alias |
| `cratename` | | |

## 2. Programs, crates, items (Fig. 2.1)

| surface form | sort | IL constructor | pilot |
|---|---|---|---|
| $\ov{\mathit{crate}}$ | `pgm` | `PGM crate*` | same |
| $c\,\{\ov{\mathit{item}}\}$ | `crate` | `CRATE crateid item*` | same |
| crate label $(\mathit{name},\mathit{loc},\mathit{ed})$ | `crateid` | record `{ CNAME cratename, CLOC locality, CED edition }` | same |
| local / foreign | `locality` | `LOCAL` \| `FOREIGN` | same |
| 2021 / 2024 | `edition` | `E2021` \| `E2024` | same |
| $\epsilon$ / $\kw{pub}$ | `vis` | `VPRIV` \| `VPUB` | same |
| $\epsilon$ / $\kw{async}^p$ | `aq` | `AQNONE` \| `AQASYNC label?` | `AQASYNC label` |

Generic parameters $\gamma$:

| surface | sort | IL constructor |
|---|---|---|
| $T$ | `gparam` | `GTY tyid` |
| $T : \ov{\beta}$ | `gparam` | `GTYB tyid bound*` |
| $\lt{r}$ | `gparam` | `GRG rgid` |
| $\lt{r} : \ov{\lt{s}}$ | `gparam` | `GRGB rgid rgid*` |

Written where-clause entries $\ba$ (bound assertions):

| surface | sort | IL constructor |
|---|---|---|
| $\ty : \ov{\beta}$ | `bassert` | `BATY typ bound*` |
| $\lt{r} : \ov{\lt{s}}$ | `bassert` | `BARG region region*` |
| $\kw{for}\gen{\ov{\lt{r}}}\;\ba$ | `bassert` | `BAFORALL rgid* bassert` — **gap, see §8** |

Function parameters $a$:

| surface | sort | IL constructor |
|---|---|---|
| $q : \ty$ | `fparam` | `APAT pat typ` |
| $\kw{self} : \ty$ | `fparam` | `ASELF typ` |

Items:

| surface | IL constructor (`item`) | pilot |
|---|---|---|
| $\mathit{vis}\;\kw{struct}\;S\gen{\ov\gamma}\;\kw{where}\;\ov\ba\;\{\ov{x:\ty}\}$ | `ISTRUCT vis adtid gparam* bassert* sfield*` | same |
| $\mathit{vis}\;\kw{struct}\;S\gen{\ov\gamma}(\ov\ty)\;\kw{where}\;\ov\ba\;;$ | `ITUPSTRUCT vis adtid gparam* bassert* typ*` | same |
| $\mathit{vis}\;\kw{enum}\;E\gen{\ov\gamma}\;\kw{where}\;\ov\ba\;\{\ov{V(\ov\ty)}\}$ | `IENUM vis adtid gparam* bassert* variant*` | same |
| $\mathit{vis}\;\kw{trait}\;D\gen{\ov\gamma}:\ov\beta\;\kw{where}\;\ov\ba\;\{\ov{\mathit{titem}}\}$ | `ITRAIT vis traitid gparam* bound* bassert* titem*` | same |
| $\mathit{vis}\;\kw{impl}\gen{\ov\gamma}\;D\gen{\ov\ty,\ov{\lt r}}\;\kw{for}\;\ty\;\kw{where}\;\ov\ba\;\{\ov{\mathit{iitem}}\}$ | `IIMPL vis gparam* traitref typ bassert* iitem*` | pilot also carries `implid` |
| $\mathit{vis}\;\mathit{aq}\;\kw{fn}\;f\gen{\ov\gamma}(\ov a)\to\ty\;\kw{where}\;\ov\ba\;\{e\}$ | `IFN vis aq fnid gparam* fparam* typ bassert* term` | same |
| $\mathit{vis}\;\kw{const}\;C:\ty = e\;;$ | `ICONST vis constid typ term` | same |
| $\mathit{vis}\;\kw{type}\;A\gen{\ov\gamma} = \ty\;;$ | `IALIAS vis aliasid gparam* typ` | same |

`implid` is **not** surface: Fig. 2.1 writes no index on an impl. The index that
Ch. 12/13 need is assigned when impls are collected into $\Theta$ (Ch. 11), so the
converter does not invent one.

Struct field, enum variant:

| surface | sort | IL constructor |
|---|---|---|
| $x : \ty$ | `sfield` | `SF varid typ` |
| $V(\ov\ty)$ | `variant` | `VAR variantid typ*` |

Trait items:

| surface | IL constructor (`titem`) |
|---|---|
| $\kw{type}\;A : \ov\beta\;\kw{where}\;\ov\ba\;;$ | `TITYPE associd bound* bassert*` |
| $\kw{const}\;C : \ty\;;$ | `TICONST constid typ` |
| $\kw{fn}\;f\gen{\ov\gamma}(\ov a)\to\ty\;\kw{where}\;\ov\ba\;;$ | `TIFN fnid gparam* fparam* typ bassert*` |

Impl items:

| surface | IL constructor (`iitem`) |
|---|---|
| $\kw{type}\;A = \ty\;\kw{where}\;\ov\ba\;;$ | `IITYPE associd typ bassert*` |
| $\kw{const}\;C : \ty = e\;;$ | `IICONST constid typ term` |
| $\kw{fn}\;f\gen{\ov\gamma}(\ov a)\to\ty\;\kw{where}\;\ov\ba\;\{e\}$ | `IIFN fnid gparam* fparam* typ bassert* term` |

## 3. Bounds, regions and program types (Fig. 2.2)

Associated-item constraints $\delta$:

| surface | sort | IL constructor |
|---|---|---|
| $A = \ty$ | `constr` | `CEQ associd typ` |
| $A : \ov\beta$ | `constr` | `CBND associd bound*` |

Bounds $\beta$:

| surface | sort | IL constructor |
|---|---|---|
| $D\gen{\ov\ty,\ov{\lt r},\ov\delta}$ | `bound` | `BTRAIT traitid typ* region* constr*` |
| $\lt r$ | `bound` | `BREGION region` |
| $\kw{?Sized}$ | `bound` | `BRELAX` |
| $\kw{for}\gen{\ov{\lt r}}\;\beta$ | `bound` | `BFORALL rgid* bound` |

Capture list $\mathit{cap}$:

| surface | sort | IL constructor |
|---|---|---|
| $\epsilon$ | `cap` | `CAPNONE` |
| $+\,\kw{use}\gen{\ov T,\ov{\lt r}}$ | `cap` | `CAPUSE tyid* rgid*` |

Regions $\lt r$ (Fig. 2.2's program-writable three; the rest of the sort belongs
to Fig. 2.4 and is never produced here):

| surface | sort | IL constructor |
|---|---|---|
| $\lt a$ | `region` | `RNAME rgid` |
| $\lt{static}$ | `region` | `RSTATIC` |
| $\lt\_$ | `region` | `RANON` |
| — (Fig. 2.4) | `region` | `RPLACE rgid`, `RVAR nat`, `ROBJ` |

Trait reference (used by `IIMPL` and by the qualified path):

| surface | sort | IL constructor |
|---|---|---|
| $D\gen{\ov\ty,\ov{\lt r}}$ | `traitref` | `TR traitid typ* region*` |

Program-writable types $\ty$:

| surface | IL constructor (`typ`) | pilot |
|---|---|---|
| $()$ | `UNIT` | same |
| $\kw{bool}$ | `BOOLT` | same |
| $\kw{i32}$ | `I32` | same |
| $\kw{u8}$ | `U8` | same |
| $\kw{usize}$ | `USIZE` | same |
| $\kw{str}$ | `STR` | same |
| $T$ | `TPARAM tyid` | same |
| $\&\lt r\,\ty$ | `REF region typ` | same |
| $\&\lt r\;\kw{mut}\;\ty$ | `REFMUT region typ` | same |
| $\kw{fn}\gen{\ov{\lt r}}(\ov\ty)\to\ty$ | `FNPTR rgid* typ* typ` | same |
| $S\gen{\ov\ty,\ov{\lt r}}$, $E\gen{\ov\ty,\ov{\lt r}}$ | `ADT adtid typ* region*` | same |
| $(\ty_1,\dots,\ty_m)$, $m\ge2$ | `TUP typ*` | same |
| $[\,\ty\,;\,n\,]$ | `ARR typ nat` | same |
| $\dyn{\ov\beta} + \lt r$ | `DYNW bound* region?` | pilot: `DYNW bound* region` |
| $\proj{\ty}{D\gen{\ov\ty,\ov{\lt r}}}{A}$ | `PROJ typ traitref associd` | same |
| $\kw{impl}^p\;\ov\beta\;\mathit{cap}$ | `IMPLT label? bound* cap` | pilot: `IMPLT label …` |
| $\hole^p$ | `HOLE label?` | pilot: `HOLE label` |
| $A\gen{\ov\ty,\ov{\lt r}}$ (alias use) | `ALIASU aliasid typ* region*` | same |
| $\nev$ (Fig. 2.4 only) | `NEVER` | same |

`DYNW`'s region is an **option** because Ch. 3's $\mathrm{mark}$ runs *before*
$\desug$ and "can no longer tell a `dyn` type written without a region bound from
one written with `'_`" (`spec/ch03-resolution.tex`, "mark precedes desug"). The
absent option is exactly "written without a region bound"; $\mathrm{mark}$ replaces
it with `ROBJ`.

Sugar type form (§2.9, "shorthand paths"):

| surface | sort | IL constructor |
|---|---|---|
| $T\!::\!A$, $\langle\ty\rangle\!::\!A$ | `typ` | `TSHORT typ associd` — sugar |

## 4. Terms (Fig. 2.3)

Patterns $q$:

| surface | sort | IL constructor |
|---|---|---|
| $x$ | `pat` | `QVAR varid` |
| $\hole$ | `pat` | `QWILD` |
| $E\!::\!V(\ov q)$ | `pat` | `QVARIANT adtid variantid pat*` |
| $S(\ov q)$ | `pat` | `QTUPSTRUCT adtid pat*` |

Literals $\mathit{lit}$:

| surface | sort | IL constructor | pilot |
|---|---|---|---|
| $n^p$ | `lit` | `LNUM label? nat` | `LNUM label nat` |
| $n_{\kw{i32}}$ | `lit` | `LI32 nat` | same |
| $n_{\kw{u8}}$ | `lit` | `LU8 nat` | same |
| $n_{\kw{usize}}$ | `lit` | `LUSIZE nat` | same |
| $\kw{true}$ / $\kw{false}$ | `lit` | `LTRUE` / `LFALSE` | same |
| $()$ | `lit` | `LUNIT` | pilot spells it `LLUNIT` (typo) |

Paths $\mathit{path}$:

| surface | sort | IL constructor |
|---|---|---|
| $f$ | `path` | `PFN fnid` |
| $S$ | `path` | `PSTRUCT adtid` |
| $E\!::\!V$ | `path` | `PVARIANT adtid variantid` |
| $C$ | `path` | `PCONST constid` |
| $\proj{\ty}{D\gen{\ov\ty,\ov{\lt r}}}{f}$ | `path` | `PQFN typ traitref fnid` |
| $\proj{\ty}{D\gen{\ov\ty,\ov{\lt r}}}{C}$ | `path` | `PQCONST typ traitref constid` |
| $V$ (bare prelude variant) | `path` | `PBVARIANT variantid` — sugar |
| $T\!::\!X$, $\langle\ty\rangle\!::\!X$ | `path` | `PSHORT typ associd` — sugar |
| $D\!::\!f$ | `path` | `PDSHORT traitid associd` — sugar |

Small sorts:

| surface | sort | IL constructor |
|---|---|---|
| $\epsilon$ / $::\gen{\ov\ty,\ov{\lt r}}$ | `targs` | `TANONE` / `TASOME typ* region*` |
| $\epsilon$ / $\kw{move}$ | `mv` | `MVNONE` / `MVMOVE` |
| $\epsilon$ / $\to\ty$ | `rett` | `RTNONE` / `RTSOME typ` |
| $q$ / $q : \ty$ | `cparam` | `CAPAT pat` / `CAPATT pat typ` |
| $x$ / $*e$ / $e.x$ | `lv` | `LVVAR varid` / `LVDEREF term` / `LVFIELD term varid` |
| $q \Rightarrow e$ | `arm` | `ARM pat term` |
| $x : e$ | `field` | `FLD varid term` |

Terms $e$:

| surface | IL constructor (`term`) | pilot |
|---|---|---|
| $x$ | `EVAR varid` | same |
| $\mathit{path}^p\,\mathit{ta}$ | `EPATH label? path targs` | `EPATH label …` |
| $\mathit{lit}$ | `ELIT lit` | same |
| $e(\ov e)$ | `ECALL term term*` | same |
| $(e_1,\dots,e_m)$, $m\ge2$ | `ETUP term*` | same |
| $S^p\,\mathit{ta}\,\{\ov{x:e}\}$ | `ESTRUCT label? adtid targs field*` | `ESTRUCT label …` |
| $[\,\ov e\,]$ | `EARRAY term*` | same |
| $[\,e\,;\,n\,]$ | `EREPEAT term nat` | same |
| $\&e$ | `EREF term` | same |
| $\&\,\kw{mut}\;e$ | `EREFMUT term` | same |
| $*e$ | `EDEREF term` | same |
| $e.x$ | `EFIELD term varid` | same |
| $\mathit{lv} = e$ | `EASSIGN lv term` | same |
| $e\;\kw{as}\;\ty$ | `ECAST term typ` | same |
| $e\,?$ | `ETRY term` | same |
| $\mathit{mv}\;\lvert\ov{\mathit{ca}}\rvert^p\;\mathit{ret}\;e$ | `ECLOSURE mv label? cparam* rett term` | `ECLOSURE mv label …` |
| $\kw{async}^p\;\mathit{mv}\;\{e\}$ | `EASYNC label? mv term` | `EASYNC label …` |
| $e.\kw{await}$ | `EAWAIT term` | same |
| $\kw{match}\;e\;\{\ov{q\Rightarrow e}\}$ | `EMATCH term arm*` | same |
| $\kw{loop}^p\;\{e\}$ | `ELOOP label? term` | `ELOOP label …` |
| $\kw{return}^p\;e$ | `ERETURN label? term` | `ERETURN label …` |
| $\kw{let}\;q:\ty = e\,;\,e$ | `ELET pat typ term term` | same |
| $e\,;\,e$ | `ESEQ term term` | same |

## 5. Crate directive (surface only, not a production)

A `.rs` file is one crate unless it says otherwise. The crate label is metadata,
not Rust syntax (`spec/ch02-syntax.tex`: *"not surface syntax, but a property the
build system fixes per crate"*), so the target carries it in a directive comment
that `rustc` ignores:

```
//@ crate <name> (local|foreign) (2021|2024)
```

Items following a directive belong to that crate; items before the first
directive belong to an implicit crate whose name is the file's basename, `local`,
of the edition given by `--edition` (default 2021). `unparse` always emits the
directive, so `parse -r` round-trips the crate label as well as the items.

## 6. Not surface syntax

`Opq_k`, `Clo_k`, `FnDef_f`, the internal `dyn`, region placeholders `!'r` and
region variables `?r`, predicates $\pi$, goals $G$, obligation sets $\Omega$,
environments, signatures, schemes, the `#[fundamental]` attribute (a fixed fact
about three types, not a production), and all labels `^p`. None is produced by
the parser.

## 7. $\desug$ rows the parser folds

These §2.9 rows are purely local rewrites into core productions; the parser
applies them and the resulting value is already in the core grammar, so
`2-syntax.spectec` needs no constructor for the left-hand side. `unparse` prints
the folded form, which re-parses to the same value.

| $\desug$ row | folded to |
|---|---|
| receivers `self`, `&'a self`, `&'a mut self` | `ASELF (TPARAM "Self")`, `ASELF (REF …)`, `ASELF (REFMUT …)` |
| parenthesized `Fn`: $D(\ov\ty)\to\ty$ | `BTRAIT D [tuple of args] [] [CEQ "Output" ty]` (0 args → `UNIT`, 1 arg → that type, $\ge2$ → `TUP`) — a **reading**, see below |
| elided regions in `&τ`, `&mut τ`, `S<τ̄>`, `D<τ̄,δ̄>`, `<τ as D<τ̄>>::A` | `RANON` in each omitted position (**not** in a `dyn` type — see §3) |
| elided returns | `-> ()` |
| fn-pointer binder $\kw{for}\gen{\ov{\lt r}}\;\kw{fn}(\ov\ty)\to\ty$ | `FNPTR r̄ τ̄ τ` |
| empty body `{}` | `{ () }`, i.e. `ELIT LUNIT` |
| unannotated `let` | `ELET q (HOLE ?()) e e'` |
| statement tail (a body ending in a statement) | `ESEQ … (ELIT LUNIT)` |
| module segments $c\!::\!m_1\!::\!\cdots\!::\!X$ | `X` (the segments are dropped) |

Two further rewrites the parser performs that §2.9 does not tabulate but
`spec/ch02-syntax.tex` §2.7 fixes in prose (*"the witnesses' `Box::new` and
`Box::leak` paths name exactly them"*):

| written | folded to |
|---|---|
| `Box::new` | `PFN "box_new"` |
| `Box::leak` | `PFN "box_leak"` |

The $\desug$ rows the parser does **not** fold, because they need the program's
declarations or are genuine surface forms, keep their own constructor (§3, §4):
unit-struct value `S` (already `PSTRUCT`), bare prelude variant `V`
(`PBVARIANT`), the three shorthand paths (`TSHORT`, `PSHORT`, `PDSHORT`),
inline bounds on $\gamma$ (`GTYB`/`GRGB`, which are core Fig. 2.1 productions),
and `pub` (`VPUB`, a core production).

### 7a. The one-argument parenthesized `Fn` is a reading, not a transcription

§2.9's row writes $D(\ov\ty)\to\ty \rightsquigarrow D\gen{(\ov\ty),\,\mathit{Output} = \ty}$
with $(\ov\ty)$ a **tuple**, but Fig. 2.2's tuple production is
$(\ty_1,\dots,\ty_m)$ with $m \geq 2$: there is no one-element tuple to build for
$|\ov\ty| = 1$. The parser reads the row as "the tuple when the grammar has one,
the bare type when it does not" (0 → `UNIT`, 1 → that type, $\ge 2$ → `TUP`).
`Box<dyn FnOnce(&T) -> …>` in `bugs/witnesses/118876.rs` and
`FnOnce() -> T` in `bugs/witnesses/141713.rs` are the cases that exercise it.
**Chapter 6's built-in `Fn`/`FnMut`/`FnOnce` impls must adopt the same reading**,
or a one-argument `Fn` bound will not solve.

**Settled (Task 39, correction C4-3).** The document now states the reading, in
one clause under §2.9's desug table: $(\ov\ty)$ is the tuple when
$|\ov\ty| \neq 1$ — the empty list giving $()$ — and the single type when
$|\ov\ty| = 1$. The encoding's `$fn_tuple` (`2-syntax.spectec`, §2.9) is that
reading, and `(ent-builtin-fn)`, `(norm-builtin-fn)` and `builtincand` all spell
the trait argument with it, so the parser's fold and the built-in impls agree.
Row 3 of `notes/phase4-open-questions.md` is closed; the correction is `C4-3` of
`notes/corrections.yaml`, verdict impact none.

### 7b. Name classification (`resolve.ml`)

Fig. 2.3 has separate productions for a term variable `x`, a fn item `f`, a
struct `S`, a const `C` and a variant `E::V`; Fig. 2.2 for a type parameter `T`,
an ADT and a transparent alias. The token stream does not say which a name is,
so `resolve.ml` decides it from the program's declarations plus Fig. 2.7's
prelude. Ordered tests, first match wins — the spec side needs the procedure to
read the converter's output:

| written | resolved to |
|---|---|
| a type name with no arguments, bound by an enclosing `γ` or by a trait/impl header (`Self`) | `TPARAM` |
| a type name declared `type A<γ̄> = τ;` | `ALIASU` |
| any other type name | `ADT` |
| a bare path name that is a prelude variant (`None`, `Some`, `Ok`, `Err`, `Break`, `Continue`) | `PBVARIANT` |
| a bare path name that is a declared or prelude `fn` | `PFN` |
| a bare path name that is a declared `const` | `PCONST` |
| a bare path name that is a declared or prelude `struct` | `PSTRUCT` |
| any other bare path name — the **capitalisation fallback** | `PSTRUCT` if it starts upper-case, else `PFN` |
| a bare name in *term* position in none of those four sets | `EVAR` |
| `Box::new`, `Box::leak` | `PFN "box_new"`, `PFN "box_leak"` |
| `A::B` with `A` a declared enum | `PVARIANT` |
| `A::B` with `A` a declared or prelude trait | `PDSHORT` |
| any other `A::B` | `PSHORT` over `A` read as a type |
| `<τ as D<..>>::x` with `D` declaring `const x` | `PQCONST` |
| any other `<τ as D<..>>::x` | `PQFN` |

Two limits, recorded rather than fixed:

- The **capitalisation fallback** is a guess, reached only for a name no
  declaration and no prelude entry introduces. Such a program is ill formed
  anyway (Ch. 11's collection has nothing to bind the name to), so the guess
  decides only which error a later chapter reports.
- A bare term name has **no local-binding scope**: the sets are top-level
  declarations, so a `let`, a closure parameter or a fn parameter that shadows a
  declared `fn`, `const`, `struct` or prelude-variant name would resolve to the
  path, not to `EVAR`. No program of the acceptance corpus does that — the one
  near-miss, `weird`, is a fn item in witness 96460 and a parameter in witness
  141713, in different files and so under different declaration sets. Fixing it
  means threading the binders of `ELET`, `ECLOSURE` and `fparam` through the
  pass; nothing needs it yet.

## 8. Gap against Fig. 2.1

`ba ::= τ : β̄ | r : s̄` has no binder, but four of the 39 oracle programs write a
`for<'r,…>` **on the where-clause entry itself**, with the entry's *subject*
mentioning the bound regions, which `τ : for<'r> β` cannot express:

| program | clause |
|---|---|
| `bugs/witnesses/100051.rs` | `for<'what, 'ever> &'what &'ever (): Trait` |
| `bugs/witnesses/114061.rs` | `for<'a> <T as WithAssoc<'a>>::Assoc: WhereBound` |
| `bugs/witnesses/98117.rs` | `for<'a> &'a T: Outlives<'a>` |
| `bugs/witnesses/105787.rs` | `for<'a> Ptr<T>: ToUnit<'a>` (subject region-free; expressible as `Ptr<T> : for<'a> ToUnit<'a>`) |

The document assumes the form exists: `spec/ch05-implied-bounds.tex` analyses
100051 as turning on "the `for<'what,'ever>` where clause of the witness", and
$\ibcf$'s fourth clause drops "a clause that quantifies over a region". Only
$\clsf$ (`spec/ch04-wf.tex` Fig. 4.x) is written for a binder on the *bound*.
The parser therefore emits `BAFORALL rgid* bassert`; the finding is recorded in
`notes/phase4-open-questions.md` as a proposed one-clause correction to Fig. 2.1
(add `\ba ::= \kw{for}\gen{\ov{\lt r}}\;\ba`, with
$\clsf(\kw{for}\gen{\ov{\lt r}}\,\ba) = \{\kw{for}\gen{\ov{\lt r}}\,\pi \mid \pi \in \clsf(\ba)\}$).
It changes no witness verdict; the controller rules on whether the document takes it.
