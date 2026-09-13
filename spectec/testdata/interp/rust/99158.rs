// Issue: https://github.com/rust-lang/rust/issues/99158
// Status in paper: confirmed (duplicate of #25860). Expected rustc 1.98.1 verdict: accept (bug present).
// Rewrites from the original: `main` + `println!` replaced by the free function
//   `exploit`, which performs the same fn-item -> fn-pointer coercion; `T: Default`
//   spelled out (the issue body renders it as `DEFAULT`); an extra `_guard: &()`
//   parameter added to `extend_with_proof`. PR #129021 ("Check well-formedness of
//   the source type's signature in fn pointer casts") only *partly* closed this hole
//   -- the pinned rustc's own `tests/ui/implied-bounds/
//   implied-bounds-on-nested-references-plus-variance-2.rs` is `//@ check-pass` with
//   `//@ known-bug: #25860` and keeps the hole open by exactly this extra-parameter
//   trick, while the parameter-free shape is now caught
//   (`implied-bounds-on-nested-references-plus-variance.rs`, check-fail).
// Feature triggers: implied, fn-ptr
// Foreign (std) items used: `std::marker::PhantomData`, `std::default::Default`.

// Legitimate: the `&'static &'a ()` "proof" argument implies `'a: 'static`.
fn extend_with_proof<'a>(
    _proof: std::marker::PhantomData<&'static &'a ()>,
    x: &'a str,
    _guard: &(),
) -> &'static str {
    x
}

// Unsound: the implied bounds of the pointee signature are not enforced when the
// function is reached through a `fn` pointer, so `T` may be instantiated with a
// `PhantomData` that carries no proof at all.
fn call_with_fake_proof<'a, T: Default>(
    extend_with_proof_ptr: fn(T, &'a str, &()) -> &'static str,
    s: &'a str,
) -> &'static str {
    extend_with_proof_ptr(Default::default(), s, &())
}

pub fn exploit<'a>(s: &'a str) -> &'static str {
    call_with_fake_proof(extend_with_proof, s)
}
