fn extend_with_proof<'a>(
    _: std::marker::PhantomData<&'static &'a ()>,
    x: &'a str, _: &(),
) -> &'static str { x }
fn call_with_fake_proof<'a, T: Default>(
    f: fn(T, &'a str, &()) -> &'static str,
    s: &'a str,
) -> &'static str { f(Default::default(), s, &()) }
fn exploit<'a>(s: &'a str) -> &'static str {
    call_with_fake_proof(extend_with_proof, s)
}
