Installing the Rust target package adds its command and the encoding it is checked against.

  $ grep -q 'target_plugins/rust/META' ../../../../spectec-target-rust.install

  $ spectec --help | grep '^  rust'
    rust                       . Rust commands

`parse -r` re-parses its own output and compares the IL values, so the printed program is
the surface form the grammar table of targets/rust/GRAMMAR.md fixes.  The crate label is a
directive comment, because an edition is per-crate metadata and not Rust syntax.

  $ spectec rust parse -p ../../../testdata/interp/rust/96460.rs -r --color never
  //@ crate 96460 local 2021
  pub fn weird() -> PhantomData<impl Sized> { PhantomData }

The sugar rows of ch02-syntax.tex section 2.9 that need no knowledge of the program are
folded by the parser: an elided region becomes '_, an omitted return type becomes -> (),
`Box::new` becomes the prelude fn item box_new, and a body ending in a statement gains its
unit tail.

  $ spectec rust parse -p ../../../testdata/interp/rust/118876.rs -r --color never
  //@ crate 118876 local 2021
  struct Bounded<'a, 'b: 'static, T>(&'a T, [&'b (); 0]) ;
  pub fn extend<T>(input: &'_ T) -> &'static T { let n: Box<dyn FnOnce<&'_ T, Output = Bounded<T, 'static, '_>>> = box_new(|x| { Bounded(x, []) }); n(input).0 }

Every program of the acceptance inventory round-trips.

  $ for f in ../../../testdata/interp/rust/*.rs; do
  >   spectec rust parse -p "$f" -r --color never > /dev/null || echo "FAIL $f"
  > done
  $ ls ../../../testdata/interp/rust/*.rs | wc -l | tr -d ' '
  39
