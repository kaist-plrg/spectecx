fn f<F>(x: F) where for<'a> F: Fn(&'a u8) {}
