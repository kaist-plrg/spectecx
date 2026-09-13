// Issue: https://github.com/rust-lang/rust/issues/136508
// Status in paper: reported. Expected rustc 1.98.1 verdict: accept (bug present).
// Rewrites from the original: `String` (not in the subset) is replaced by the local unit
//   struct `Payload`; the `#[repr(u8)]` attribute is dropped (verified at the pin: the cast
//   is accepted with and without it); the inherent `impl FooBar { fn discriminant }` with
//   its `unsafe` pointer cast is dropped, as are `fn main` and the `println!`s -- the
//   misleading accept is the cast expression itself, which is wrapped in `pub fn f`.
// Feature triggers: enum
// `FooBar::Foo` is not a discriminant here but the enum variant *constructor*, a fn item of
// type `fn(i32) -> FooBar`. Casting it to `u8` truncates the constructor's address, yet
// rustc accepts it silently, as if `FooBar` were a fieldless enum. Making any one variant
// fieldless makes the same cast an error.
pub struct Payload;
pub enum FooBar {
    Foo(i32),
    Bar(Payload),
}
pub fn f() -> u8 {
    FooBar::Foo as u8
}
