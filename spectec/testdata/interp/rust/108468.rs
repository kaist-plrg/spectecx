// Issue: https://github.com/rust-lang/rust/issues/108468
// Status in paper: reported. Expected rustc 1.98.1 verdict: accept (bug present).
// Rewrites from the original: none (`args` renamed `_args`); `test` made `pub`.
// Feature triggers: it, async
// Foreign (std) items used: `Iterator`.

// The elided lifetime in `Item = &str` is an anonymous lifetime inside `impl Trait`
// in argument position, which is gated behind the unstable
// `anonymous_lifetime_in_impl_trait` feature. Without `async` this is an error;
// with `async` rustc accepts it.
pub async fn test(_args: impl Iterator<Item = &str>) {}
