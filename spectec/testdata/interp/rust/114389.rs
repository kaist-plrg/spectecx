// Issue: https://github.com/rust-lang/rust/issues/114389
// Status in paper: confirmed (duplicate of #57893). Expected rustc 1.98.1 verdict: accept (bug present).
// Rewrites from the original: the issue body's own reproduction, with the runtime exploit
//   removed — the default method bodies of `Cat::meow`/`Dog::bark` (both are just `println!`,
//   a macro, and §3.1's traits declare methods without default bodies) are dropped along with
//   the trait methods themselves, since only the coercion has to type-check; `fn main` becomes
//   `pub fn bad` returning the laundered `&'static dyn Dog` instead of calling `bark()` on it;
//   the `let` binding's elided lifetime and `bad`'s return type are written `'static` (the supertrait binding
//   `This = dyn Cat` means `dyn Cat + 'static`, so a non-'static reference would be rejected
//   by the borrow checker for an unrelated reason, E0521).
// Feature triggers: assoc, tr-obj
trait TypeEq {
    type This: ?Sized;
}
impl<T: ?Sized> TypeEq for T {
    type This = T;
}
pub fn identity<T: ?Sized>(t: &<T as TypeEq>::This) -> &T {
    t
}
trait Cat {}
struct SomeType;
impl Cat for SomeType {}
// `Dog`'s supertrait binding pins `<dyn Dog as TypeEq>::This` to `dyn Cat`, while the blanket
// impl above makes the same projection equal `dyn Dog`. `Dog` has no impls at all, so the
// bound is impossible, yet rustc 1.98.1 still accepts `identity` at `T = dyn Dog` and hands
// back a `&dyn Dog` whose vtable is in fact `dyn Cat`'s.
trait Dog: TypeEq<This = dyn Cat> {}
pub fn bad() -> &'static dyn Dog {
    let normal_coercion: &'static dyn Cat = &SomeType;
    identity(normal_coercion)
}
