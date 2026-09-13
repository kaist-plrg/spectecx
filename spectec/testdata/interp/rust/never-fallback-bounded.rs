// Probe (Phase 3 design section 3).  Not an issue: no row in bugs/issues.yaml, no \bug marker.
// Open question: item 8 of notes/phase3-open-questions.md -- "(det-fallback-diverge) is
//   unfalsifiable inside the model, and the model has no counterpart to
//   calculate_diverging_fallback's coercion graph".
// Owning rule: (det-fallback-diverge), Ch. 14 Fig. 14.1.  Chapter: 14.
// Program: bugs/witnesses/96927.rs with one line added -- `trait Parse` implemented for `()`
//   and for nothing else, and a `Parse` bound on `from_str`.  This is the experiment Ch. 14
//   section 14.3 describes ("give from_str a bound that () satisfies and ! does not"); it is
//   recorded here so that the claim is compiled rather than asserted.  `Other` is kept from
//   the witness and does NOT implement Parse, which is the whole difference.
//
// Hand derivation (Ch. 14, both editions).  Two labelled occurrences matter.
//   q = the `loop {}` that is from_str's body: fbform(loop^q{}) = div, so q is a fallback
//       point.  (ty-loop) gives it type chi(q), and (item-fn)'s discharge premise carries
//       chi(q) ~> Result<T, Error> with T from_str's own rigid parameter.
//       (det-fallback-diverge): chi' = chi[q |-> fbdef(Gamma,q)] is () under 2021 and ! under
//       2024.  Under 2021 the default is UNAVAILABLE -- chi' |-STG fails, since
//       `() ~> Result<T,Error>` has no derivation in Ch. 10 -- so the implication's antecedent
//       is false and the rule imposes nothing; chi(q) is left to the context, which admits
//       Result<T,Error> by (co-refl) and ! by (co-never).  Under 2024 the default IS available
//       -- `! ~> Result<T,Error>` is (co-never) -- so chi' |-STG succeeds and the rule IMPOSES
//       chi(q) = !, which also discharges from_str's body premise.  Either way an admissible
//       chi exists, and (pgm-ok)'s own |-FB premise is satisfied at q.
//   p = the call `from_str^p(&())`: a chi point, NOT a fallback point (Ch. 14 section 14.3's
//       ruling on 96927 -- FB is decided by the FORM of the occurrence).  (ty-path) emits
//       Omega = theta(pi-bar) for the path use, so the goal is chi(p)_1 : Parse.
//       `impl Parse for ()` is the only impl, so chi(p) = () is the UNIQUE solution -- the
//       bound does here what an annotation would do.
// Last step: (det-ok)'s uniqueness premise, which fails for the unbounded witness 96927, is
//   satisfied here, so |-DET holds and (pgm-ok) succeeds.  The model ACCEPTS, under both
//   editions.  Nothing in the model connects p to q: it has no coercion graph.
//
// rustc 1.98.1 (`rustc +1.98.1 --edition=EE --crate-type=lib --emit=metadata -A warnings`):
//   edition 2021: REJECT.  The deny-by-default future-incompatibility lint
//     `dependency_on_unit_never_type_fallback` fires as an ERROR (a deny-level lint is an
//     error, and `-A warnings` does not lower it): "error: this function depends on never type
//     fallback being `()`", with "note: in edition 2024, the requirement `!: Parse` will fail".
//   edition 2024: REJECT.  "error[E0277]: the trait bound `!: Parse` is not satisfied", with
//     "help: the trait `Parse` is implemented for `()`" and "required by a bound in `from_str`".
// Both diagnostics name the choice, which is what the probe measures.  Model and pin therefore
// DISAGREE on both rows (`agree: no`); Ch. 14 section 14.6's \implnote{abstraction} explains
// the two causes -- no lints at all under 2021, and no coercion graph under 2024.
pub struct Error;
pub struct Other;
pub trait Parse {}
impl Parse for () {}
pub fn from_str<T: Parse>(_s: &()) -> Result<T, Error> {
    loop {}
}
pub fn test() -> Result<(), Error> {
    let _v = from_str(&())?;
    Ok(())
}
