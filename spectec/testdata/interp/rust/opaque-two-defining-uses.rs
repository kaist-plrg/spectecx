// Probe: opaque-two-defining-uses.  Open question item 7 (notes/phase3-open-questions.md) --
//   "(op-agree) is satisfiable by construction in this model -- Delta is a function on labels
//   -- so it states an invariant rather than constraining a program".
// Chapter 11, owning rule (op-ok), Fig. 11.10 -- the rule that carried the (op-agree) premise
//   until this probe decided its fate (Phase 3 design section 2.5).
// Companion: bugs/probes/opaque-two-defining-uses-recursive.rs, the same program with the two
//   type arguments of the recursive call in the OTHER order, which the pin accepts.
//
// What it separates.  (op-agree) said that Delta is a FUNCTION on opaque names and that the
//   value Gamma carries is that one.  The question design section 2.5 poses is whether any
//   program of the Chapter 2 subset can make two defining uses of one opaque DISAGREE without
//   already failing typing -- which is what would make the premise a check rather than a
//   restatement.  This is the sharpest program there is: BOTH uses are defining uses (their
//   arguments are distinct generic parameters, so `opaque_type_has_defining_use_args` and the
//   model's def_use both accept them), and the two hidden types they give the opaque, once
//   remapped to its declaration parameters, are `(A, B)` and `(B, A)`.  It is the program
//   that reaches rustc's "concrete type differs" comparison, and it is the one the answer
//   turns on.
//
// Program.  Written to Fig. 2.3: the subset has no `if` and no binary operators, so the two
//   uses are a `return` and the body's tail, sequenced by `e ; e`.  Turbofish is
//   Fig. 2.3's `path^p ta`.  (The brief's `fn f(b: bool) -> impl Sized { if b { 1u8 } else
//   { "x" } }` is outside the grammar; compiled anyway, the pin answers E0308 "`if` and
//   `else` have incompatible types" -- the ARMS are unified with each other before the opaque
//   is consulted -- and the in-grammar `return 1u8; "x"` answers E0308 "return type resolved
//   to be `u8`", a plain coercion failure.  Neither reaches a second defining use.  This one
//   does.)
//
// Hand derivation (Ch. 11 section 11.4, Ch. 7 section 7.5).  (item-fn) forms
//   Delta = defs(chi, Gamma, k-bar) = (Opq_k := chi(k)) -- ONE definition, because chi is a
//   finite map on labels and k is a label -- and reveals it before typing the body.  Typing
//   emits two coercion obligations:
//     from (ty-return):      (A, B)       ~> Opq_k<A,B>
//     from (item-fn)'s tail: Opq_k<B,A>   ~> Opq_k<A,B>
//   Both go through (co-sub) and (co-sub-alias-l)/(co-sub-alias-r), which emit equality goals.
//   (norm-opaque-in-scope) reveals each occurrence: def_use holds of <A,B> and of <B,A> alike
//   -- both lists are distinct type parameters of the defining item, and both region lists are
//   empty -- so Opq_k<A,B> normalizes to chi(k) and Opq_k<B,A> to theta chi(k) with
//   theta = [B/A][A/B].  The first goal gives chi(k) = (A, B); the second then demands
//   (B, A) == (A, B).
// Last step: no chi satisfies both, |-STG has no derivation for any chi, (pgm-ok)'s
//   existential fails, and the model REJECTS -- in (item-discharge), through the equality goal
//   a COERCION emitted, at stage 3.  Delta is a one-element map throughout, and the deleted
//   (op-agree) would have accepted it: the second use is not a second definition but an
//   INSTANCE theta chi(k) of the one hidden type, and what fails is the instance's agreement
//   with its context, which is typing's business.  That is the measurement behind design
//   section 2.5's first branch, and Ch. 11 section 11.4's \implnote{abstraction} states it.
//
// rustc 1.98.1 (`rustc +1.98.1 --edition=2021 --crate-type=lib --emit=metadata -A warnings`):
//   REJECT.  "error: concrete type differs from previous defining opaque type use", with
//   "expected `(A, B)`, got `(B, A)`" and "note: previous use here".  That is
//   OpaqueHiddenTypeMismatch, built by DefinitionSiteHiddenType::build_mismatch_error and
//   emitted from writeback's hidden-type loop (rustc_hir_typeck/src/writeback.rs) and from
//   compute_definition_site_hidden_types' add_hidden_type
//   (rustc_borrowck/src/region_infer/opaque_types/mod.rs) -- rustc infers one hidden type per
//   defining use, remaps each to the declaration parameters, and compares.  The model has no
//   such comparison and needs none: it has one chi(k) and checks every use against it.
// Model and pin AGREE (`agree: yes`) on the verdict; they reach it by different routes, and
//   the difference is exactly what the demotion of (op-agree) records.
pub fn h<A, B>(a: A, b: B) -> impl Sized {
    return (a, b);
    h::<B, A>(b, a)
}
