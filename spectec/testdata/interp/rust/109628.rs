// Issue: https://github.com/rust-lang/rust/issues/109628
// Status in paper: fixed. Expected rustc 1.98.1 verdict: reject.
// Rewrites from the original: none (already top-level items); `fn_test` made `pub`.
// Feature triggers: assoc
trait Trait {
    type Assoc;
}

impl<T: 'static> Trait for Box<T> {
    type Assoc = ();
}

struct MyTy<U>(U)
where
    U: Trait,
    U::Assoc: Sized, // any predicate naming U::Assoc
;

// `MyTy<Box<T>>` is only well-formed if `Box<T>: Trait`, i.e. if `T: 'static`.
// The `T: 'static` bound of the *impl* was wrongly used as an implied bound of
// `fn_test`, so `fn_test` was accepted without declaring `T: 'static`.
pub fn fn_test<T>(_: MyTy<Box<T>>) {}
