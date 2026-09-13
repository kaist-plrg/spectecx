// Issue: https://github.com/rust-lang/rust/issues/114061
// Status in paper: fixed. Expected rustc 1.98.1 verdict: reject.
// Rewrites from the original: none beyond dropping `#![crate_type = "lib"]` (the driver
//   already passes `--crate-type=lib`) and the comment sketching the downstream crate.
//   The issue's own reproduction is already a single crate of trait declarations and impls;
//   it is the pin's regression test
//   rustc/tests/ui/coherence/coherence-overlap-unnormalizable-projection-0.rs.
// Feature triggers: assoc, hr
trait WhereBound {}
impl WhereBound for () {}

pub trait WithAssoc<'a> {
    type Assoc;
}

pub trait Trait {}

// Coherence treats the unnormalizable projection `<T as WithAssoc<'a>>::Assoc: WhereBound`
// as not holding, so it fails to see that a downstream crate could satisfy it for
// `T = Box<Local>` and make these two impls overlap. rustc 1.98.1 rejects (E0119).
impl<T> Trait for T
where
    T: 'static,
    for<'a> T: WithAssoc<'a>,
    for<'a> <T as WithAssoc<'a>>::Assoc: WhereBound,
{
}

impl<T> Trait for Box<T> {}
