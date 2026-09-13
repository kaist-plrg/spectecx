// Issue: https://github.com/rust-lang/rust/issues/108468
// Status in paper: reported. Expected rustc 1.98.1 verdict: accept.
// Rewrites from the original: none; this is the edition-2024 twin of 108468.rs.
// Feature triggers: it, async
// Foreign (std) items used: `Iterator`.
// Purpose: the only edition-2024 witness. Exercises cap_all (Fig. 3.6, edition clause):
// an async fn's opaque captures all in-scope generics in both editions, and an elided
// region inside an argument-position `impl Trait` is accepted only because the fn is async.

pub async fn test(_args: impl Iterator<Item = &str>) {}
