trait Trait {
    type Type;
}
impl<T> Trait for T {
    type Type = ();
}

trait Extend<'a, 'b> {
    fn extend(self, s: &'a str) -> &'b str;
}

impl<'a, 'b> Extend<'a, 'b>
    for <&'b &'a () as Trait>::Type
where
    for<'what, 'ever> &'what &'ever (): Trait,
{
    fn extend(self, s: &'a str) -> &'b str {
        s
    }
}

fn probe() {
    let y = <() as Extend<'_, '_>>::extend(
        (), &String::from("temporary")
    );
    println!("{}", y);
}
