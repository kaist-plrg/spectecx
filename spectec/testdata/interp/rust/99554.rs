// Issue: https://github.com/rust-lang/rust/issues/99554
// Status in paper: fixed. Expected rustc 1.98.1 verdict: accept (future-incompat warning only).
// Rewrites from the original: the issue's two-crate setup (`impl<T> foreign::Trait2<B, T>
//   for <T as Id>::Assoc`, cf. rustc/tests/ui/coherence/orphan-check-alias.rs) is collapsed
//   into one crate by substituting the std trait `std::cmp::PartialEq` for the foreign
//   `foreign::Trait2`; since no stable std trait carries two type parameters, the local type
//   is made generic (`Local<T>`) so that `T` is still constrained by the impl's trait ref
//   (otherwise E0207 fires before the orphan check runs); `fn eq(&self, ..)` is written with
//   the explicit receiver `self: &Self`.
// Feature triggers: assoc
struct Local<T>(T);
trait Identity {
    type Output;
}
impl<T> Identity for T {
    type Output = T;
}
// `<T as Identity>::Output` is a projection, so it does not *cover* `T`; `T` therefore appears
// uncovered before the first local type `Local<T>`, and the orphan rule should reject this impl.
// rustc 1.98.1 only emits the `uncovered_param_in_projection` future-incompatibility warning
// (tracking issue #124559), so with `-A warnings` the crate still compiles.
impl<T> std::cmp::PartialEq<Local<T>> for <T as Identity>::Output {
    fn eq(self: &Self, other: &Local<T>) -> bool {
        false
    }
}
