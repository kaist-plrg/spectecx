// Issue: https://github.com/rust-lang/rust/issues/104005
// Status in paper: fixed. Expected rustc 1.98.1 verdict: reject.
// Rewrites from the original: `use std::fmt::Display` -> fully qualified path (no
//   `use` items in the subset); `fn display(self) -> ..` -> `fn display(this: Self) -> ..`
//   and `self.0` -> `this.0`; `main` + `println!` replaced by the free function
//   `test_call`, mirroring `fn test_call` in the pinned rustc's own regression test
//   `tests/ui/borrowck/fn-item-check-type-params.rs` (the fix of PR #120019 lands at
//   the *use* site: `extend_lt` itself still compiles).
// Feature triggers: implied, tr-obj
// Foreign (std) items used: `std::fmt::Display` plays the role of the upstream crate.
trait Displayable {
    fn display(this: Self) -> Box<dyn std::fmt::Display>;
}

impl<T: std::fmt::Display> Displayable for (T, Option<&'static T>) {
    fn display(this: Self) -> Box<dyn std::fmt::Display> {
        Box::new(this.0)
    }
}

// `U` is never checked for well-formedness inside `extend_lt`.
fn extend_lt<T, U>(val: T) -> Box<dyn std::fmt::Display>
where
    (T, Option<U>): Displayable,
{
    Displayable::display((val, None))
}

// The caller must pick `U = &'static &'a str`, which needs `'a: 'static`;
// rustc 1.98.1 checks the fn item's type at the use site, so this is rejected.
pub fn test_call<'a>(val: &'a str) {
    extend_lt(val);
}
