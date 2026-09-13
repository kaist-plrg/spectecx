// Issue: https://github.com/rust-lang/rust/issues/126079
// Status in paper: fixed. Expected rustc 1.98.1 verdict: reject.
// Rewrites from the original: taken from the pin's regression test
//   rustc/tests/ui/dyn-compatibility/almost-supertrait-associated-type.rs; the method call
//   `(&PhantomData::<T> as &dyn Foo<T, U>).transmute(t)` becomes the UFCS call
//   `<dyn Foo<T, U> as Foo<T, U>>::transmute(..)`; `use std::marker::PhantomData` is spelled
//   out as a fully qualified path; `fn main` with its `String`/`println!` runtime exploit is
//   dropped and `fn transmute` is made `pub`. The `&self` receivers are kept (written
//   `self: &Self`): a trait whose method has no receiver is dyn-incompatible for an unrelated
//   reason, which would destroy the witness. NO return-position `impl Trait` in a trait method
//   (RPITIT) is needed; see notes/extraction/witnesses.md.
// Feature triggers: assoc, tr-obj
struct ActuallySuper;
struct NotActuallySuper;
trait Super<Q> {
    type Assoc;
}
trait Dyn {
    type Out;
}
impl<T, U> Dyn for dyn Foo<T, U> + '_ {
    type Out = U;
}
impl<S: Dyn<Out = U> + ?Sized, U> Super<NotActuallySuper> for S {
    type Assoc = U;
}
// `transmute` returns `<Self as Super<NotActuallySuper>>::Assoc`. `Super<NotActuallySuper>` is
// *not* a supertrait of `Foo` (only `Super<ActuallySuper>` is), so the projection is not fixed
// by the `dyn Foo<T, U>` type and the vtable entry is unsound. rustc 1.98.1 rejects (E0038).
trait Foo<T, U>: Super<ActuallySuper, Assoc = T>
where
    <Self as Mirror>::Assoc: Super<NotActuallySuper>,
{
    fn transmute(self: &Self, t: T) -> <Self as Super<NotActuallySuper>>::Assoc;
}
trait Mirror {
    type Assoc: ?Sized;
}
impl<T: ?Sized> Mirror for T {
    type Assoc = T;
}
impl<T, U> Foo<T, U> for std::marker::PhantomData<T> {
    fn transmute(self: &Self, t: T) -> T {
        t
    }
}
impl<T> Super<ActuallySuper> for std::marker::PhantomData<T> {
    type Assoc = T;
}
impl<T> Super<NotActuallySuper> for std::marker::PhantomData<T> {
    type Assoc = T;
}
pub fn transmute<T, U>(t: T) -> U {
    <dyn Foo<T, U> as Foo<T, U>>::transmute(&std::marker::PhantomData::<T> as &dyn Foo<T, U>, t)
}
