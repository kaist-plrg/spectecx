// Issue: https://github.com/rust-lang/rust/issues/141713
// Status in paper: confirmed. Expected rustc 1.98.1 verdict: accept (bug present).
// Rewrites from the original: `use std::any::Any` -> fully qualified path (no `use`
//   items in the subset); the `make_static_and_drop` laundering step, `main` and
//   `println!` dropped -- they only need `<dyn Any>::downcast_ref` / `Option::unwrap`
//   to observe the use-after-free, while the unsound accept is `write_incoherent_p2`.
// Feature triggers: assoc, hr, clo
// Foreign (std) items used: `std::any::Any`, `FnOnce`, `Box`.
pub trait Producer<T>: FnOnce() -> T {}
impl<T, F: FnOnce() -> T> Producer<T> for F {}

// `P: for<'a> Producer<&'a T>` makes `P::Output` resolve through the *supertrait*
// `FnOnce() -> &'a T` at an arbitrary, unconstrained `'a`, so `weird` may be a
// short-lived reference that is then stored as `&'static dyn Any`.
fn write_incoherent_p2<T, P: for<'a> Producer<&'a T>>(
    weird: P::Output,
    out: &mut &'static dyn std::any::Any,
) {
    *out = weird;
}

fn write_incoherent_p1<T, P: for<'a> Producer<&'a T>>(
    p: P,
    out: &mut &'static dyn std::any::Any,
) {
    write_incoherent_p2::<T, P>(p(), out)
}

pub fn construct_implementor<T>(not_static: T, out: &mut &'static dyn std::any::Any) {
    write_incoherent_p1::<T, _>(|| Box::leak(Box::new(not_static)), out);
}
