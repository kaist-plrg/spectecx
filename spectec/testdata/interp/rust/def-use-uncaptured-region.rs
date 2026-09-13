// Probe (Phase 3 design section 3).  Not an issue: no row in bugs/issues.yaml, no \bug marker.
// Open question: item 12 of notes/phase3-open-questions.md -- "the lifetime half of the
//   defining-use restriction".  This probe measures the DOMAIN of the half that Task 31 added.
// Owning rule: (norm-opaque-in-scope), Ch. 7 Fig. 7.4.  Chapter: 7.
// Program: the smallest program on which the opaque's CAPTURE LIST and its DEFINING ITEM's
//   region parameters differ at an in-scope use.  Both parameters are early bound (each is
//   named by a clause, Ch. 3 section 3.2), so the turbofish may supply them; the where clause
//   is what makes `f::<'b,'b>` well formed.
//
// Why it separates the two readings.  The opaque of `impl Sized + 'a` is edition-2021 and its
//   bounds name `'a` alone, so Fig. 3.6 gives capt(p) = ('a): gamma-bar = ('a).  The call
//   `f::<'b,'b>(y)` instantiates the signature at 'a := 'b, so its result type is the in-scope
//   occurrence Opq_k<'b> -- a region argument that IS a region parameter of the defining item
//   `f` and is NOT one the opaque captures.
//   rustc accepts it: opaque_type_has_defining_use_args
//   (rustc_trait_selection/src/opaque_types.rs:89-95) tests only
//   `matches!(lt.kind(), ty::ReEarlyParam(_) | ty::ReLateParam(_))` at the captured positions --
//   iter_captured_args (rustc_type_ir/src/opaque_ty.rs:23) restricts which POSITIONS are
//   walked, not which regions may appear in them -- and `'b` is a ReEarlyParam.  So this is a
//   defining use at the pin, with hidden type ().
//
// Hand derivation (Ch. 7, then Chs. 11 and 14).
//   def_use(Gamma, k, epsilon, ('b)): the OPQ entry is (gamma-bar, beta-bar_k, X, rpit, tau_h)
//       with X = f; denv(Gamma, f) gives gamma-bar_X = ('a,'b); there is no type argument;
//       'b is a region parameter in gamma-bar_X and distinct('b) holds.  TRUE.
//       (Under the rejected reading -- 'b in gamma-bar, the capture list -- it would be FALSE,
//       the reveal would not fire, (norm-opaque-rigid) needs hid = bottom which is false inside
//       the scope, and the document would REJECT a program the pin accepts.  That is what this
//       probe measures.)
//   (norm-opaque-in-scope) then reveals theta tau_h with theta = ['b/'a], and the annotation
//       `let _z: () = ...` forces chi(k) = () (theta tau_h = () and theta touches regions only).
//   (op-define) is satisfied by it: FV(()) = {} is inside every capture list, () is well formed,
//       and cls((), {Sized, 'a}) holds -- () : Sized, and () : 'a by (ol-*) on a region-free type.
//   The tail `loop {}` is a fallback point (fbform = div).  (det-fallback-diverge): chi' with
//       chi'(p) = fbdef = () satisfies |-STG, because Opq_k<'a> normalizes to () inside the
//       defining scope and (co-refl) discharges `() ~> Opq_k<'a>`; so the rule IMPOSES
//       chi(p) = () under edition 2021.  (det-ok) asks for agreement off FB only, and the one
//       point off FB, k, is pinned to () by the annotation.
// Verdict: the model ACCEPTS, the pin ACCEPTS, agree: yes.  Ch. 7 section 7.7 carries the
//   derivation; Ch. 7 section 7.5 carries the domain argument.
// Feature triggers: rpit, implied

pub fn f<'a, 'b>(y: &'b u8) -> impl Sized + 'a
where
    'b: 'a,
{
    let _z: () = f::<'b, 'b>(y);
    loop {}
}
