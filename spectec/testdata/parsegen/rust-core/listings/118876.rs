fn extend<T>(input: &T) -> &'static T {
    struct Bounded<'a, 'b: 'static, T>(
        &'a T, [&'b (); 0]
    );
    let n: Box<dyn FnOnce(&T)
        -> Bounded<'static, '_, T>> =
        Box::new(|x| Bounded(x, []));
    n(input).0
}

fn extend_mut<'a, T>(
    input: &'a mut T,
) -> &'static mut T {
    struct Bounded<'a, 'b: 'static, T>(
        &'a mut T, [&'b (); 0]
    );
    let mut n: Box<dyn FnMut(&mut T)
        -> Bounded<'static, '_, T>> =
        Box::new(|x| Bounded(x, []));
    n(input).0
}
