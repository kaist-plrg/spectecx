// Issue: https://github.com/rust-lang/rust/issues/107268
// Status in paper: fixed. Expected rustc 1.98.1 verdict: reject.
// Rewrites from the original: the generic associated type `trait Trait { type Ty<'a>; }`
//   is replaced by a lifetime-parameterized trait with a plain associated type,
//   `trait Trait<'a> { type Ty; }` (GATs are excluded by spec §3.1), so `T::Ty<'_>` becomes
//   `<T as Trait<'_>>::Ty` and `T: Trait` becomes `T: for<'a> Trait<'a>`; the shorthand
//   projections `A::Ty<'static>` are written as qualified paths; the second type parameter
//   `B` is given the `where` clause `B: for<'a> Trait<'a, Ty = <A as Trait<'static>>::Ty>`
//   so that it is a *genuine* competing solution (with the issue's bare `B: Trait`,
//   `my_fn::<B>` does not type-check and χ2 below would be vacuous); items made `pub`.
// Feature triggers: assoc, fn-ptr
// χ1: type argument T of `my_fn` at line 28 := A   χ2: type argument T of `my_fn` at line 28 := B
// The coercion of the fn item `my_fn::<?T>` to `impl Fn(<A as Trait<'static>>::Ty)`
// instantiates the late-bound `'_` with a region variable, leaving the single constraint
// `<?T as Trait<'?r>>::Ty ≡ <A as Trait<'static>>::Ty`. Projections are not injective, so
// both `?T := A` and `?T := B` solve it; each compiles when written out explicitly.
// Pre-fix rustc related the two higher-ranked projections structurally and silently chose
// `A`; the pin reports E0283 "type annotations needed" instead.
pub trait Trait<'a> {
    type Ty;
}
pub fn my_fn<T: for<'a> Trait<'a>>(_: <T as Trait<'_>>::Ty) {}
pub fn test<A, B>() -> impl Fn(<A as Trait<'static>>::Ty)
where
    A: for<'a> Trait<'a>,
    B: for<'a> Trait<'a, Ty = <A as Trait<'static>>::Ty>,
{
    my_fn
}
