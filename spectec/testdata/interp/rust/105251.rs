// Issue: https://github.com/rust-lang/rust/issues/105251
// Status in paper: fixed. Expected rustc 1.98.1 verdict: reject.
// Rewrites from the original: `use`-free (`std::future::Future` written in full);
//   `test` made `pub`. The `async move { .. }` block is kept verbatim (spec §3.1
//   includes `async { e }` blocks).
// Feature triggers: assoc, rpit
// Foreign (std) items used: `std::future::Future`, `Sized`.

// Neither opaque lists `'s` among the lifetimes it may capture, yet the hidden type of
// the OUTER one -- the `async move` block -- mentions `'s`. Compiled at the pin the
// E0700 is reported on the outer opaque: "hidden type for `impl Future<Output = impl
// Sized>` captures lifetime that does not appear in bounds". Must be rejected.
pub fn test<'s: 's>(s: &'s str) -> impl std::future::Future<Output = impl Sized> {
    async move {
        let _s = s;
    }
}
