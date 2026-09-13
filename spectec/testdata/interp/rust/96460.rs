// Issue: https://github.com/rust-lang/rust/issues/96460
// Status in paper: fixed. Expected rustc 1.98.1 verdict: reject.
// Rewrites from the original: `use core::marker::PhantomData` -> the fully qualified path
//   `std::marker::PhantomData` (no `use` items in the subset); `weird` made `pub`.
// Feature triggers: rpit
// Foreign (std) items used: `std::marker::PhantomData`.
// χ1: hidden type of `impl Sized` in `fn weird` := ()   χ2: hidden type of `impl Sized` in `fn weird` := u8
// Nothing in the body constrains the opaque type -- `PhantomData` is well typed whatever
// the hidden type is -- so `()` and `u8` are equally good solutions. Nightlies after
// PR #94081 silently chose `()` (visible as "`()` is not a future" when `Sized` is
// replaced by `Future`); PR #96516 turned that into E0282 "type annotations needed",
// which is the verdict the pin produces.
pub fn weird() -> std::marker::PhantomData<impl Sized> {
    std::marker::PhantomData
}
