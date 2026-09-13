// Issue: https://github.com/rust-lang/rust/issues/139406
// Status in paper: fixed. Expected rustc 1.98.1 verdict: reject.
// Rewrites from the original: the crate attribute `#![allow(unconditional_recursion)]` is
//   dropped (the driver passes `-A warnings`); `fn print_return_type`, `fn main` and the
//   `println!`/`std::any::type_name` observation are dropped (they only read the hidden
//   type back out at run time); both functions made `pub`. The issue's third snippet
//   (`what3`, a triple recursion) is left out: measured at the pin, it is rejected with the
//   same E0720 that the pin now also produces for `what2` (the issue body reports E0792 for
//   `what3` on nightly 1.88, so the diagnostic changed between that nightly and the pin).
// Feature triggers: rpit, rec
// χ1: hidden type of `impl Sized` in `fn what1` := ()   χ2: hidden type of `impl Sized` in `fn what1` := T
// `what1`'s only defining use is `what1(x)`, whose type is the opaque type itself, so the
// constraint `Opaque<T> ≡ Opaque<T>` is vacuous and *every* type is a solution; `()` and
// `T` both compile when written as the return type instead of `impl Sized`. rustc picks
// `()` for `what1` but `T` for `what2` -- the same vacuous constraint, a different answer.
// The pin rejects `what2` with E0720 ("cannot resolve opaque type") while `what1` alone is
// still accepted, so the file's verdict is `reject`.
pub fn what1<T>(x: T) -> impl Sized {
    what1(x)
}
pub fn what2<T>(x: T) -> impl Sized {
    what2(what2(x))
}
