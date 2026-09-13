// Issue: https://github.com/rust-lang/rust/issues/98543
// Status in paper: fixed. Expected rustc 1.98.1 verdict: reject.
// Rewrites from the original: `main` + `drop`/`println!` replaced by the free
//   function `exploit`, which is the same `f(&x, ())` call in function form (the fix
//   of PR #99217 shows up at the *call* site: the definition of `f` still compiles,
//   and the issue's `main` now fails plain borrowck at `drop(x)`).
// Feature triggers: implied, assoc
trait Trait {
    type Type;
}

impl<T> Trait for T {
    type Type = ();
}

// The unnormalized argument type `<&'a &'b () as Trait>::Type` together with the
// `&'a &'b (): Trait` where-clause used to imply `'b: 'a` inside `f`.
fn f<'a, 'b>(s: &'b str, _: <&'a &'b () as Trait>::Type) -> &'a str
where
    &'a &'b (): Trait,
{
    s
}

// The implied bound is no longer handed to the caller, so turning `&'b str` into
// `&'a str` for unrelated `'a`, `'b` is rejected.
pub fn exploit<'a, 'b>(s: &'b str) -> &'a str {
    f(s, ())
}
