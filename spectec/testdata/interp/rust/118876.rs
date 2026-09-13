// Issue: https://github.com/rust-lang/rust/issues/118876
// Status in paper: fixed. Expected rustc 1.98.1 verdict: reject.
// Rewrites from the original: the `struct Bounded` declared inside each function is
//   hoisted to a single top-level item (the subset has top-level items only); the
//   `extend_mut` variant dropped (it is the same bug with `&mut`).
// Feature triggers: implied, tr-obj, hr, clo
// Foreign (std) items used: `Box`, `FnOnce`.
struct Bounded<'a, 'b: 'static, T>(&'a T, [&'b (); 0]);

// `Bounded<'static, '_, T>` as the (higher-ranked) return type of the boxed closure
// makes rustc implicitly assume `'x: 'static` for the anonymous input lifetime,
// turning `&'x T` into `&'static T`. Must be rejected.
pub fn extend<T>(input: &T) -> &'static T {
    let n: Box<dyn FnOnce(&T) -> Bounded<'static, '_, T>> = Box::new(|x| Bounded(x, []));
    n(input).0
}
