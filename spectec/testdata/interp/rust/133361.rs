// Issue: https://github.com/rust-lang/rust/issues/133361
// Status in paper: fixed. Expected rustc 1.98.1 verdict: reject.
// Rewrites from the original: taken from the pin's regression test
//   rustc/tests/ui/traits/object/incomplete-multiple-super-projection.rs; `&'static str`
//   (the `str` type is not in the modeled subset) is replaced by `()` as the first impl's
//   associated type value — the two impls only have to disagree; `fn main` with its
//   `println!` runtime exploit is dropped and `fn call` is made `pub`.
// Feature triggers: assoc, tr-obj
trait Sup<T> {
    type Assoc;
}
impl<T> Sup<T> for () {
    type Assoc = T;
}
impl<T, U> Dyn<T, U> for () {}
// `dyn Dyn<A, B>` carries the two supertrait projections `<Self as Sup<A>>::Assoc ≡ A` and
// `<Self as Sup<B>>::Assoc ≡ B`; for `A = B = ()` they coincide, which coherence fails to see.
trait Dyn<A, B>: Sup<A, Assoc = A> + Sup<B, Assoc = B> {}

trait Trait {
    type Assoc;
}
impl Trait for dyn Dyn<(), ()> {
    type Assoc = ();
}
// Overlaps the impl above at `A = B = ()`. rustc 1.98.1 rejects (E0119).
impl<A, B> Trait for dyn Dyn<A, B> {
    type Assoc = usize;
}

pub fn call<A, B>(x: usize) -> <dyn Dyn<A, B> as Trait>::Assoc {
    x
}
