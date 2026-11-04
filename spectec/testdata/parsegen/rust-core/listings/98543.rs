trait Trait {
    type Type;
}
impl<T> Trait for T {
    type Type = ();
}

fn f<'a, 'b>(
    s: &'b str,
    _: <&'a &'b () as Trait>::Type,
) -> &'a str
where
    for<'what, 'ever> &'what &'ever (): Trait,
{
    s
}

fn probe() {
    let x = String::from("temporary");
    let y = f(&x, ());
    drop(x);
    println!("{}", y);
}
