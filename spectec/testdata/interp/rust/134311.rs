// Issue: https://github.com/rust-lang/rust/issues/134311
// Status in paper: reported. Expected rustc 1.98.1 verdict: accept (bug present).
// Rewrites from the original: `fn main` is absent already; `demo0` made `pub` and the
//   shared definitions of the issue body are inlined above it. The type alias
//   `type DiscardT<T> = <T as Discard>::Output;` is *kept* although spec §3.1 has no type
//   aliases: it is what smuggles the `impl Trait` into a non-final path segment. Spelling
//   the projection out as `fn demo0() -> <impl ?Sized as Discard>::Output` is rejected by
//   the pin with E0562 ("`impl Trait` is not allowed in paths"), so the alias is
//   load-bearing. The issue's RPITIT variants and its TAIT/ATPIT comparisons are dropped.
// Feature triggers: assoc, rpit
// χ1: hidden type of `impl ?Sized` in `fn demo0` := ()   χ2: hidden type of `impl ?Sized` in `fn demo0` := Point
// `DiscardT<X>` normalizes to `Point` for every `X`, so the opaque type is discarded and
// nothing constrains it; every `?Sized` type is a solution. rustc infers `()`: the issue's
// sibling `fn demo1() -> DiscardT<impl NobodyImplsThis + ?Sized> { Point }` fails at the
// pin with "the trait bound `(): NobodyImplsThis` is not satisfied", naming the choice.
pub trait Discard {
    type Output;
}
impl<T: ?Sized> Discard for T {
    type Output = Point;
}
pub struct Point;
pub type DiscardT<T> = <T as Discard>::Output;
pub fn demo0() -> DiscardT<impl ?Sized> {
    Point
}
