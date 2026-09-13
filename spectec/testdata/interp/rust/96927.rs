// Issue: https://github.com/rust-lang/rust/issues/96927
// Status in paper: confirmed. Expected rustc 1.98.1 verdict: accept (bug present).
// Rewrites from the original: the third-party `serde_json::from_str` is replaced by the
//   local generic `from_str`, whose type parameter is likewise unconstrained by its
//   argument (`serde_json::Result<T>` becomes `Result<T, Error>` with a local `Error`;
//   `&'a str` becomes `&()`, since `str` is not in the subset; the `Deserialize` bound is
//   dropped -- see the note below); `fn main` and `println!` replaced by `pub fn test`,
//   which keeps the `-> Result<(), Error>` signature that makes `?` well typed.
// Feature triggers: try
// χ1: type argument T of `from_str` at line 24 := ()   χ2: type argument T of `from_str` at line 24 := Other
// Nothing constrains `T`: `_v` is never used, so "type annotations needed" (E0282) is the
// expected verdict. Instead the variable is treated as diverging and never-type fallback
// resolves it to `!` -> `()` in edition 2021. `Other` type-checks just as well (verified by
// annotating `let _v: Other`). Keeping a `Deserialize`-like bound on `from_str` makes the
// pin reject the file outright, via the deny-by-default future-incompat lint
// `dependency_on_unit_never_type_fallback` ("in edition 2024, the requirement `!: Parse`
// will fail") -- which is also the evidence that the choice here is `()`.
pub struct Error;
pub struct Other;
pub fn from_str<T>(_s: &()) -> Result<T, Error> {
    loop {}
}
pub fn test() -> Result<(), Error> {
    let _v = from_str(&())?;
    Ok(())
}
