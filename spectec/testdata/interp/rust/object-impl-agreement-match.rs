// Probe: object-impl-agreement-match.  Open question item 6 (notes/phase3-open-questions.md).
// Chapter 13, owning rule (coh-overlap-object), Fig. 13.7.
// Companion: bugs/probes/object-impl-agreement.rs.  This file is the half that SEPARATES the
//   two readings of rust#57893; read the companion's header for the two readings.
//
// Program.  The same shape as the companion with one character changed: `Agr`'s supertrait
//   bound pins <Self as Fix>::Out to `()`, which is what the blanket impl supplies.  The
//   projection is Self-free and the two agree on it.
//
// Hand derivation (Ch. 13 Section 13.5).  obj_ty(Gamma, Agr) = dyn for<> { Agr,
//   <Fix |> Out> = (), eps } + 'static; sigma-bar' = { <Self as Fix>::Out == () }, so the one
//   downarrow goal is Gamma; Theta; eps |- <tau_d as Fix>::Out || ().  (norm-object) is
//   blocked by NOT impl_cand exactly as before and (norm-impl) answers `()`.
// Last step: `()` IS `()`, the premise holds, (coh-overlap-object) derives, and so does
//   (coh-ok): the model ACCEPTS.  `Fix` itself is discharged vacuously -- cls(Self, Fix)
//   elaborates to no equality member, so sigma-bar' is empty and the rule has no goal to ask.
//
// Why the pair is the measurement.  Under the LITERAL reading of #57893 -- object impl versus
//   blanket impl must be disjoint -- this program is rejected too, because
//   `impl<T: ?Sized> Fix for T` covers `dyn Agr`.  The model accepts it.  So the row below is
//   what distinguishes the reading this document took from the one it declined, and it is the
//   evidence for Section 13.5's claim that requiring disjointness "would reject
//   impl<T: ?Sized> D for T for every dyn-compatible D -- a large class of programs rustc
//   accepts and that nothing in the 30 issues indicts".  The companion shows the model is
//   still strict enough to catch the unsound case.
//
// rustc 1.98.1 (`rustc +1.98.1 --edition=2021 --crate-type=lib --emit=metadata -A warnings`):
//   ACCEPT, with no diagnostic.
// Model and pin AGREE (`agree: yes`).
pub trait Fix {
    type Out: ?Sized;
}
impl<T: ?Sized> Fix for T {
    type Out = ();
}
pub trait Agr: Fix<Out = ()> {}
