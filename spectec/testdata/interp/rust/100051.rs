// Issue: https://github.com/rust-lang/rust/issues/100051
// Status in paper: reported. Expected rustc 1.98.1 verdict: accept (bug present).
// Rewrites from the original: `fn extend(self, ..)` -> `fn extend(this: Self, ..)` in the
//   trait, written `_this` in the impl (no `self` receiver, no method-call syntax, and the
//   parameter is unused); `main` + `println!` dropped, since
//   the unsound accept is the `impl` header itself.
// Feature triggers: implied, assoc, hr
trait Trait {
    type Type;
}

impl<T> Trait for T {
    type Type = ();
}

trait Extend<'a, 'b> {
    fn extend(this: Self, s: &'a str) -> &'b str;
}

// The self type is the *unnormalized* projection `<&'b &'a () as Trait>::Type`,
// from which rustc wrongly implies `'a: 'b`; that lets `extend` launder `&'a str`
// into `&'b str` for unrelated `'a`, `'b`.
impl<'a, 'b> Extend<'a, 'b> for <&'b &'a () as Trait>::Type
where
    for<'what, 'ever> &'what &'ever (): Trait,
{
    fn extend(_this: Self, s: &'a str) -> &'b str {
        s
    }
}
