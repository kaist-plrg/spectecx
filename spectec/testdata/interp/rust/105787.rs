// Issue: https://github.com/rust-lang/rust/issues/105787
// Status in paper: fixed. Expected rustc 1.98.1 verdict: reject.
// Rewrites from the original: taken from the pin's own regression test
//   rustc/tests/ui/coherence/occurs-check/associated-type.rs; the raw pointer type
//   `*const T` (not in the modeled subset) is replaced by the local wrapper struct
//   `Ptr<T>`; the type alias `type Assoc<'a, T> = <*const T as ToUnit<'a>>::Unit` is
//   inlined at its single use site (no type aliases in the subset); the `foo`/`main`
//   runtime exploit is dropped (only the coherence verdict is needed).
// Feature triggers: assoc, hr
trait ToUnit<'a> {
    type Unit;
}
struct LocalTy;
struct Ptr<T: ?Sized>(std::marker::PhantomData<T>);
impl<'a> ToUnit<'a> for Ptr<LocalTy> {
    type Unit = ();
}
impl<'a, T: Copy + ?Sized> ToUnit<'a> for Ptr<T> {
    type Unit = ();
}
trait Overlap<T> {
    type Assoc;
}
impl<T> Overlap<T> for T {
    type Assoc = usize;
}
// The higher-ranked projection `<Ptr<T> as ToUnit<'a>>::Unit` cannot be replaced by an
// inference variable, so the occurs check reports an error instead of ambiguity and the
// overlap with the blanket impl above is missed. rustc 1.98.1 rejects (E0119).
impl<T> Overlap<for<'a> fn(&'a (), <Ptr<T> as ToUnit<'a>>::Unit)> for T
where
    for<'a> Ptr<T>: ToUnit<'a>,
{
    type Assoc = Box<usize>;
}
