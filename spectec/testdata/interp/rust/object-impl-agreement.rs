// Probe: object-impl-agreement.  Open question item 6 (notes/phase3-open-questions.md) --
//   "(coh-overlap-object) is this document's reading of rust#57893, which is open upstream;
//   the types team has not ruled".
// Chapter 13, owning rule (coh-overlap-object), Fig. 13.7.
// Companion: bugs/probes/object-impl-agreement-match.rs.  The PAIR is the measurement; this
//   file alone measures only that the model is stricter than the pin.
//
// The two readings of #57893.  rustc's coherence enumerates a trait's IMPLS, and the object
//   impl is a BuiltinImplSource::Object, never a CandidateSource::Impl, so a blanket impl is
//   never compared with it (overlapping_trait_impls, local_trait_impls; check_object_overlap
//   guards only a `dyn` SELF type).  Two repairs are possible:
//     (literal)     treat the object impl as an impl and require DISJOINTNESS -- which rejects
//                   `impl<T: ?Sized> D for T` for every dyn-compatible D;
//     (agreement)   require only that where an impl applies to the object type it supplies the
//                   values that type already fixes -- which is (coh-overlap-object).
//   This file is a program the two readings agree to reject, for DIFFERENT premises.  The
//   companion is a program on which they DIFFER, and that is what makes the choice measurable
//   rather than argued.
//
// Program.  `Obj`'s supertrait bound pins <Self as Fix>::Out to `u8`, while the blanket impl
//   makes the same projection `()` at every type, the object type included.  The disagreeing
//   value `()` is Self-FREE: this is not #114389's mechanism (there the blanket impl's
//   `type This = T` disagrees by way of Self), which is why the probe is not that witness
//   again.  Nothing implements `Obj`, so the contradiction is invisible to every other check.
//
// Hand derivation (Ch. 13 Section 13.5).  (coh-ok)'s object premise ranges over traits(Gamma),
//   so it reaches `Obj` whether or not the program mentions `dyn Obj`.
//   obj_ty(Gamma, Obj): `Obj` is dyn compatible (no methods), assoc(Gamma, Obj) = {(Fix, Out)},
//     objval of that pair reads the elaborated supertrait clauses and answers `u8`, so
//     tau_d = dyn for<> { Obj, <Fix |> Out> = u8, eps } + 'static, which is not bottom --
//     (coh-object-none) does not apply.
//   sigma-bar' -- the EQUALITY members of elab(Gamma, cls(Self, Obj)) -- is
//     { <Self as Fix>::Out == u8 }, one member, so there is one downarrow goal:
//         Gamma; Theta; eps |- <tau_d as Fix>::Out  ||  u8.
//   Only (norm-object) and (norm-impl) apply to a projection whose self type is `dyn`.
//     (norm-object) carries the guard NOT impl_cand (Section 7.4: the impl wins) and
//     impl_cand IS true here, so (norm-object) is blocked; (norm-impl) matches the blanket
//     impl at T := tau_d and answers `()`.
// Last step: `()` is not `u8`, the downarrow premise fails, (coh-overlap-object) has no
//   derivation, neither does (coh-ok), and the model REJECTS.  Under the literal reading the
//   same program is rejected one premise earlier, by disjointness of the blanket impl and the
//   object impl.
//
// rustc 1.98.1 (`rustc +1.98.1 --edition=2021 --crate-type=lib --emit=metadata -A warnings`):
//   ACCEPT, with no diagnostic at all.  That is #57893 at the pin: the overlap is visible to
//   coherence and coherence does not look, and unsound_prefer_builtin_dyn_impl -- which does
//   know about it -- returns at once when typing_mode().is_coherence().
// Model and pin DISAGREE (`agree: no`); Ch. 13 Section 13.6 carries the \implnote{bug} on
//   #114389 and Ch. 15 Section 15.6 explains the row.
pub trait Fix {
    type Out: ?Sized;
}
impl<T: ?Sized> Fix for T {
    type Out = ();
}
pub trait Obj: Fix<Out = u8> {}
