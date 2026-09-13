// Issue: https://github.com/rust-lang/rust/issues/106040
// Status in paper: reported. Expected rustc 1.98.1 verdict: accept (bug present).
// Rewrites from the original: `use std::marker::Unpin;` replaced by the fully qualified
//   path `std::marker::Unpin` (no `use` items in the subset); `fn main` replaced by the
//   `pub fn test` caller (the cycle is only entered from a use site).
// Feature triggers: assoc
pub fn is_unpin<T: std::marker::Unpin>() {}
trait OtherTrait {
    type Assoc
    where
        Self: std::marker::Unpin;
}
struct LocalTy;
// Proving `LocalTy: Unpin` needs `<LocalTy as OtherTrait>::Assoc ≡ LocalTy`, which needs
// `LocalTy: Unpin` again. `Unpin` is an auto trait, so the cycle looks coinductive to
// `evaluate`, but the projection obligation in it makes it inductive. rustc 1.98.1 accepts.
impl std::marker::Unpin for LocalTy where Self: OtherTrait<Assoc = LocalTy> {}
impl<T> OtherTrait for T {
    type Assoc = T
    where
        Self: std::marker::Unpin;
}
pub fn test() {
    is_unpin::<LocalTy>()
}
