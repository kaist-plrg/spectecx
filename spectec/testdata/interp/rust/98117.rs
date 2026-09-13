// Issue: https://github.com/rust-lang/rust/issues/98117
// Status in paper: fixed. Expected rustc 1.98.1 verdict: reject.
// Rewrites from the original: `main` + `println!` replaced by the free function
//   `exploit`, which performs the same `step1(&String)` call (the fix landed at the
//   *use* site, so the witness must contain a call; the bare `t_is_static` example
//   from the issue body still compiles on its own).
// Feature triggers: implied, tr-obj
// Foreign (std) items used: `AsRef`, `Box` (`Box::leak`, `Box::new`).
trait Outlives<'a>: 'a {}
impl<'a, T> Outlives<'a> for &'a T {}

// Only well-formed if `T: 'static`; that WF obligation was not checked.
fn step2<T>(t: T) -> &'static str
where
    &'static T: Outlives<'static>,
    T: AsRef<str>,
{
    AsRef::as_ref(Box::leak(Box::new(t) as Box<dyn AsRef<str> + 'static>))
}

fn step1<T>(t: T) -> &'static str
where
    for<'a> &'a T: Outlives<'a>,
    T: AsRef<str>,
{
    step2(t)
}

// Instantiating `T = &'x String` makes the supertrait obligation `&'a &'x String: 'a`
// collapse to `'x: 'static`. rustc 1.98.1 enforces it here (PR #124336), so this is
// rejected.
pub fn exploit<'x>(s: &'x String) -> &'static str {
    step1(s)
}
