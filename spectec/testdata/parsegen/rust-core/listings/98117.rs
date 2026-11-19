use std::fmt::Display;

trait Static: 'static {}
impl<T> Static for &'static T {}

fn foo<S: Display>(x: S) -> Box<dyn Display>
where
    &'static S: Static,
{
    Box::new(x)
}

fn probe() {
    let s = foo(&String::from("temporary"));
    println!("{}", s);
}
