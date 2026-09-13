// Issue: https://github.com/rust-lang/rust/issues/110925
// Status in paper: reported. Expected rustc 1.98.1 verdict: accept (bug present).
// Rewrites from the original: the ill-formed type argument `(str, str)` is replaced by
//   `NeedsCopy<NotCopy>`, which is ill-formed for the same reason (a struct applied to a
//   type argument that violates the struct's own parameter bound) but uses neither `str`
//   nor a tuple type, neither of which is in spec §3.1; the `?Sized` relaxations are then
//   unnecessary and are dropped; `fn main` and its `foo()` call are dropped (the unsound
//   accept is `foo`'s signature itself).
// Feature triggers: rpit
// `NeedsCopy<NotCopy>` is not well formed: `NotCopy: Copy` does not hold. Written anywhere
// else it is rejected with E0277 -- verified at the pin for `fn ctl(_: NeedsCopy<NotCopy>)`
// and for `fn ctl() where (): Test<NeedsCopy<NotCopy>>` -- but as a type argument of an
// RPIT's bound it is never WF-checked, and `foo` compiles.
pub trait Test<T> {}
impl<T, U> Test<U> for T {}
pub struct NeedsCopy<T: Copy>(T);
pub struct NotCopy;
pub fn foo() -> impl Test<NeedsCopy<NotCopy>> {
    ()
}
