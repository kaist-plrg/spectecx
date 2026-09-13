// Probe: opaque-two-defining-uses-recursive.  Open question item 7
//   (notes/phase3-open-questions.md).
// Chapter 11, owning rule (op-ok), Fig. 11.10.
// Companion: bugs/probes/opaque-two-defining-uses.rs, the rejecting half.  The two files
//   differ in three characters -- `h::<B, A>(b, a)` there, `g::<A, B>(a, b)` here -- and the
//   PAIR is what decides Phase 3 design section 2.5.  Read that file's header first.
//
// Program.  The recursive occurrence is a defining use at the IDENTITY, which is the shape
//   the open question was really about: a second defining use that agrees.  A model in which
//   Delta were a multimap would have to compare its two entries here and find them equal.
//
// Hand derivation (Ch. 11 section 11.4, Ch. 7 section 7.5).  Delta = (Opq_k := chi(k)) again,
//   one definition.  Typing emits
//     from (ty-return):      (A, B)      ~> Opq_k<A,B>   -- equality goal, chi(k) = (A, B)
//     from (item-fn)'s tail: Opq_k<A,B>  ~> Opq_k<A,B>   -- (co-refl), no goal at all
//   and the recursive call's own result type is the instantiated signature return Opq_k<A,B>,
//   whose in-scope occurrence satisfies def_use (distinct type parameters, empty region list).
// Last step: chi(k) = (A, B) is the unique solution; (op-define) checks it against `Sized` in
//   the defining item's environment -- FV((A,B)) = {A,B} = capt(k) -- and (op-bounds) checks
//   the bound itself, so |-OPQ derives and the model ACCEPTS.  Note what the second defining
//   use contributed: NOTHING to constrain chi(k).  Every in-scope use of Opq_k is theta chi(k)
//   by (norm-opaque-in-scope), so it is an INSTANCE of the one hidden type and can never be a
//   second, competing definition; when theta is the identity the obligation is discharged by
//   (co-refl) before any goal is emitted.  That is the invariant Ch. 11 section 11.4 now
//   states in place of (op-agree), and this row is the evidence that it is an invariant of the
//   subset and not merely of the rules.
//
// The `return` term's OWN type, in both files of this pair.  Since Phase 3 (Task 29)
//   (ty-return) concludes chi(p) and not `!`, where p is the return occurrence's label, so
//   `return (a, b);` is a fallback point (fbform(return^p e) = div, Ch. 2 section 2.9) and
//   both programs carry one.  It changes nothing here: the `return` stands in the statement
//   position of `e ; e`, whose first component's type is discarded, so no goal constrains
//   chi(p); (det-fallback-diverge) is then a preference that installs fbdef(Gamma, p) -- `()`
//   at edition 2021, `!` at 2024 -- and (pgm-ok)'s |-FB premise holds at that value in either
//   edition.  chi(p) and the opaque's chi(k) are different labels: what fixes chi(k) is the
//   coercion the RETURNED expression emits, walked above, and it is unaffected.
//
// rustc 1.98.1 (`rustc +1.98.1 --edition=2021 --crate-type=lib --emit=metadata -A warnings`):
//   ACCEPT, with no diagnostic.  The recursive use is a defining use whose arguments are the
//   declaration parameters, so remap_generic_params_to_declaration_params gives it the same
//   hidden type as the `return`, and add_hidden_type finds `prev.ty == hidden_ty.ty`.
// Model and pin AGREE (`agree: yes`).
pub fn g<A, B>(a: A, b: B) -> impl Sized {
    return (a, b);
    g::<A, B>(a, b)
}
