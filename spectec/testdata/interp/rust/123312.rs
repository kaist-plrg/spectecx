// Issue: https://github.com/rust-lang/rust/issues/123312
// Status in paper: reported. Expected rustc 1.98.1 verdict: accept (bug present).
// Rewrites from the original: none.
// Feature triggers: clo
struct Static<D: 'static>(Option<D>);
pub fn test<D>() {
    let _ = || Static(None::<D>); // closure return type not WF-checked
}
