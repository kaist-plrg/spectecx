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

An edition is not written in the file, so `-e` supplies it and the crate label carries it
through the round trip.  Two rows of the inventory are edition 2024.

  $ spectec rust parse -p ../../../testdata/interp/rust/108468-2024.rs -e 2024 -r --color never
  //@ crate 108468-2024 local 2024
  pub async fn test(_args: impl Iterator<Item = &'_ str>) -> () { () }

  $ spectec rust parse -p ../../../testdata/interp/rust/108468-2024.rs -r --color never
  //@ crate 108468-2024 local 2021
  pub async fn test(_args: impl Iterator<Item = &'_ str>) -> () { () }

`struct S;` is the empty named-field list of section 2.2, and a where clause may stand
between the generics and the `;`.

  $ cat > unit.rs <<'EOF'
  > pub struct Unit;
  > pub struct Bounded<T> where T: Copy;
  > EOF
  $ spectec rust parse -p unit.rs -r --color never
  //@ crate unit local 2021
  pub struct Unit { }
  pub struct Bounded<T> where T: Copy { }

`expect.yaml` carries one entry per inventory ROW, so a program the inventory lists at both
editions is collected twice; the batch runner's ids are the rows.

  $ grep -c . ../../../testdata/interp/rust/expect.yaml
  46
  $ grep 'never-fallback-bounded' ../../../testdata/interp/rust/expect.yaml
  never-fallback-bounded.rs 2021: positive
  never-fallback-bounded.rs 2024: positive

Every program of the acceptance inventory round-trips.

  $ for f in ../../../testdata/interp/rust/*.rs; do
  >   spectec rust parse -p "$f" -r --color never > /dev/null || echo "FAIL $f"
  > done
  $ ls ../../../testdata/interp/rust/*.rs | wc -l | tr -d ' '
  39
