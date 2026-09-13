// Issue: https://github.com/rust-lang/rust/issues/112417
// Status in paper: fixed. Expected rustc 1.98.1 verdict: reject.
// Rewrites from the original: `panic!()` -> `loop {}` (no macros in the subset);
//   `fn subtype(self, ..)` -> `fn subtype(this: Self, ..)`; the `Func` blanket impl,
//   the generic `subtype` exploit driver, `main` and `println!` dropped -- the unsound
//   accept is `foo` itself.
// Feature triggers: implied, rpit
trait CallMeMaybe<'a, 'b> {
    fn mk() -> Self;
    fn subtype<T>(this: Self, x: &'b T) -> &'a T;
}

struct Foo<'a, 'b: 'a>(&'a (), &'b ());

impl<'a, 'b> CallMeMaybe<'a, 'b> for Foo<'a, 'b> {
    fn mk() -> Self {
        Foo(&(), &())
    }

    fn subtype<T>(_this: Self, x: &'b T) -> &'a T {
        x
    }
}

// The defining use `Foo(&(), &())` sits in dead code, so MIR typeck never checks
// the region constraint `'b: 'a` that `Foo<'a, 'b>` requires. Must be rejected.
pub fn foo<'a, 'b>() -> impl CallMeMaybe<'a, 'b> {
    loop {}
    Foo(&(), &())
}
