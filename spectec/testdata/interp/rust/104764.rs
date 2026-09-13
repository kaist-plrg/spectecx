// Issue: https://github.com/rust-lang/rust/issues/104764
// Status in paper: fixed. Expected rustc 1.98.1 verdict: reject.
// Rewrites from the original: none (the issue body is already a free function plus
//   a blanket trait impl); `test` made `pub`.
// Feature triggers: assoc
trait Trait {
    type Ty;
}
impl<T> Trait for T {
    type Ty = ();
}
pub fn test<T>() {
    // The user annotation `<&'static T as Trait>::Ty` is normalized to `()` before
    // being WF-checked, so the missing `T: 'static` goes unnoticed.
    let _: <&'static T as Trait>::Ty = ();
}
