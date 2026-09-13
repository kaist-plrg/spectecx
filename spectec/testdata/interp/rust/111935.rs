// Issue: https://github.com/rust-lang/rust/issues/111935
// Status in paper: fixed. Expected rustc 1.98.1 verdict: reject.
// Rewrites from the original: the crate attribute `#![allow(unconditional_recursion)]` is
//   dropped (the driver passes `-A warnings`); `fn main` dropped; both functions made
//   `pub`. The two functions are the issue body's first and third snippets, and together
//   they are the pin's own regression test tests/ui/impl-trait/rpit/non-defining-use.rs.
// Feature triggers: rpit, rec
// χ1 (unique solution): hidden type of `impl Sized` in `fn bar` := T; in `fn foo` := ()
// This is NOT a determinacy pair in the sense of spec §6.2 (two distinct χ that both let
// stages 3-4 succeed): `bar` has the unique solution `T`, and so does `foo` (`()`). It is an
// over-rejection case, and the mechanism at the pin is a *use-site* one. rustc's
// `opaque_type_has_defining_use_args` requires every in-scope use of the opaque to have
// non-lifetime arguments that are distinct generic parameters of the defining item (the
// lifetime half is checked later, under MirBorrowck); a use that is not is a *non-defining
// use* and an error ON THAT USE. So the pin reports two E0792 ("expected generic type
// parameter, found `u8`"), one on the call `foo::<u8>()` and one on the call `bar(0u8)`
// -- `foo` has no second defining use for anything to clash with. The version the issue was
// filed against behaved differently: it treated `bar(0u8)` as *defining*, generalized the
// single constraint `Opaque<u8> ≡ u8` -- from which, in the issue author's own words, "it is
// ambiguous whether `Opaque<T> = T` or `Opaque<T> = u8`" -- to `Opaque<T> := u8`, and then
// reported the clash with `val: T`. PR #112842 removed that generalization; the equivalent
// `type_alias_impl_trait` program was always rejected.
// The model reproduces the use-site condition: `(norm-opaque-in-scope)` (Ch. 7 §7.5) reveals
// an opaque only when the occurrence's non-lifetime arguments are distinct generic parameters
// of the defining item, so both calls -- which are at `T := u8` -- fail the guard, neither
// result type normalizes, and the `let` annotations that compare them have no derivation.
// The model therefore REJECTS this witness, on the use and for rustc's reason. The issue is
// Ch. 7's (it was Ch. 11's `(op-agree)` while the condition was unstated); Ch. 11 §11.4 says
// why the condition's home is the reveal, and Ch. 14 must not copy a χ pair from here.
// The lifetime analogue was still open when the issue was closed (per the
// issue body's 2023-10 note); at the pin the lifetime cases are rejected too -- see
// rustc/tests/ui/impl-trait/rpit/non-defining-use-lifetimes.rs, whose .stderr expects two
// E0792 ("expected generic lifetime parameter, found `'static`" / "found `'_`") plus one
// "non-defining opaque type use in defining scope", with no `known-bug` directive.
pub fn foo<T>() -> impl Sized {
    let _: () = foo::<u8>();
}
pub fn bar<T>(val: T) -> impl Sized {
    let _: u8 = bar(0u8);
    val
}
