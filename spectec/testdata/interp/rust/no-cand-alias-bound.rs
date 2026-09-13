// Probe: no-cand-alias-bound.  Open question item 5 (notes/phase3-open-questions.md) --
//   "no_cand's coarseness is argued to be safe but is not measured".
// Chapter 13, owning rule (coh-overlap-bounds); the clause under test is the ALIAS-BOUND
//   clause of no_cand (Fig. 13.6), the one Section 13.4's \implnote{abstraction} calls the
//   instance of "coarseness is safe, OMISSION is not".
//
// What it separates.  refutable(Gamma, Theta, pi-bar) declares a conjunct of the intersection
//   unprovable by ANY crate, present or future, and (coh-overlap-bounds) then declares the two
//   impls DISJOINT.  A candidate family left out of no_cand makes no_cand true where a
//   candidate does apply, and two overlapping impls are then wrongly accepted.  This program is
//   the alias-bound family's instance, built to the exact shape Section 13.4 describes: a
//   GROUND conjunct `<u8 as Tr>::A : Marker` with `Marker` local and impl-less.
//
// The construction, and why each piece is needed.
//   * `Marker` has NO impl anywhere.  With one, no_cand's first clause would already answer
//     false -- unify's alias clause lets a projection unify with any type, so ANY `impl Marker
//     for ...` would be a candidate for `<u8 as Tr>::A : Marker` -- and the probe would measure
//     nothing.
//   * `type A : Marker + ?Sized` is the item bound.  `?Sized` is what lets an impl of `Tr`
//     exist at all without an impl of `Marker`: `impl Tr for u8 { type A = dyn Marker; }`
//     satisfies the declared bound through the built-in object impl (ent-object), not through
//     a user impl, so Theta stays free of `Marker` schemes.
//   * `impl Tr for u8` must exist, because the intersection's OTHER conjunct is `u8 : Tr`.
//     Without it that conjunct is refutable, the pair is disjoint for a reason that has nothing
//     to do with alias bounds, and rustc agrees: deleting the impl makes this file COMPILE at
//     the pin (measured).  With it, the alias conjunct is the only one left to decide.
//
// Hand derivation (Ch. 13 Section 13.4).  The two headers `W<T> : Foo` and `W<u8> : Foo` unify
//   at theta = [u8/T], so unifiable is true and (coh-overlap) does not apply.
//   (coh-overlap-bounds) then asks refutable of theta(pi-bar_1, pi-bar_2) =
//   { u8 : Tr, <u8 as Tr>::A : Marker, u8 : Sized }:
//     u8 : Sized           -- no_cand false by the built-in clause.
//     u8 : Tr              -- no_cand false by the first clause: `impl Tr for u8` is in Theta.
//     <u8 as Tr>::A : Marker -- knowable IS true (FV of the projection is empty, so
//                             uncovered = {} , and `Marker` is local), and this is the whole
//                             point: the conjunct is not shielded by knowable.  no_cand is
//                             false ONLY by the alias-bound clause -- the subject is the
//                             projection <u8 as Tr>::A, `Tr` declares `type A : Marker`, and
//                             cls(<u8 as Tr>::A, (Marker, ?Sized)) contains exactly this
//                             predicate.  Drop that clause and no_cand is TRUE.
// Last step: refutable is false on every conjunct, so (coh-overlap-bounds) does not apply
//   either; neither disjointness rule derives, the pair overlaps, (coh-ok) has no derivation
//   and the model REJECTS.  Without the alias-bound clause refutable would be true, the pair
//   would be declared disjoint and the model would ACCEPT a program the pin rejects -- an
//   over-permissiveness, the unsafe direction.
//
// rustc 1.98.1 (`rustc +1.98.1 --edition=2021 --crate-type=lib --emit=metadata -A warnings`):
//   REJECT.  "error[E0119]: conflicting implementations of trait `Foo` for type `W<u8>`".
//   impl_intersection_has_impossible_obligation finds no impossible obligation: it proves
//   `<u8 as Tr>::A : Marker` outright from `Tr`'s declared item bound (ent-alias-bound's
//   counterpart), exactly as the model's alias-bound clause says it can.
// Model and pin AGREE (`agree: yes`).
pub trait Marker {}
pub trait Tr {
    type A: Marker + ?Sized;
}
impl Tr for u8 {
    type A = dyn Marker;
}
pub trait Foo {}
pub struct W<T>(T);
impl<T: Tr> Foo for W<T> where <T as Tr>::A: Marker {}
impl Foo for W<u8> {}
