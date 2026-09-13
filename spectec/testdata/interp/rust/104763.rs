// Issue: https://github.com/rust-lang/rust/issues/104763
// Status in paper: fixed. Expected rustc 1.98.1 verdict: reject.
// Rewrites from the original: none (already free functions + trait impl).
// Feature triggers: const
trait Trait {
    const TRAIT: bool;
}
impl<T> Trait for &'static T {
    const TRAIT: bool = true;
}
pub fn test<T>() {
    let _ = <&'static T>::TRAIT; // needs T: 'static; must be rejected
}
