// Issue: https://github.com/rust-lang/rust/issues/144045
// Status in paper: reported. Expected rustc 1.98.1 verdict: accept (bug present).
// Rewrites from the original: `use std::marker::PhantomData` -> fully qualified path
//   (no `use` items in the subset); items made `pub` (they already were).
// Feature triggers: arr
// Foreign (std) items used: `std::marker::PhantomData`.
pub struct Thing<'a> {
    _phantom: std::marker::PhantomData<fn(&'a ()) -> &'a ()>,
}

// `Thing<'a>` is invariant in `'a`, so `[x; 0]: [Thing<'b>; 0]` should require
// `'a == 'b`. The zero-length repeat expression drops the constraint entirely.
pub fn foo<'a, 'b>(x: Thing<'a>) -> [Thing<'b>; 0] {
    [x; 0]
}

pub fn lol<'a, 'b>(x: &'a i32) -> [&'b i32; 0] {
    [x; 0]
}
