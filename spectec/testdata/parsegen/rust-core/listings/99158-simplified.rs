fn foo<'a>(
    v: &'a i32,
    u: &'static &'a i32,
) -> &'static i32 {
    v
}

fn bar<'a>(v: &'a i32) -> &'static i32 {
    let f: fn(&'a i32, _) -> &'static i32 = foo;
    f(v, &&0)
}
