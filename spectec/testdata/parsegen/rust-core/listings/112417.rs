trait CallMeMaybe<'a, 'b> {
    fn mk() -> Self;
    fn subtype<T>(self, x: &'b T) -> &'a T;
}
struct Foo<'a, 'b: 'a>(&'a (), &'b ());

impl<'a, 'b> CallMeMaybe<'a, 'b> for Foo<'a, 'b> {
    fn mk() -> Self {
        Foo(&(), &())
    }
    fn subtype<T>(self, x: &'b T) -> &'a T {
        x
    }
}

fn foo<'a, 'b>() -> impl CallMeMaybe<'a, 'b> {
    panic!();
    Foo(&(), &())
}

trait Func {
    type RetTy;
}
impl<F: FnOnce() -> R, R> Func for F {
    type RetTy = R;
}

fn subtype<'a, 'b, F: FnOnce() -> R,
           R: CallMeMaybe<'a, 'b>, T>(
    _: F, x: &'b T,
) -> &'a T {
    let proof = R::mk();
    proof.subtype(x)
}

fn probe() {
    let y = subtype(foo, &String::from("temporary"));
    println!("{}", y);
}
