Installing the Rust target package adds its command and the encoding it is checked against.

  $ grep -q 'target_plugins/rust/META' ../../../../spectec-target-rust.install

  $ spectec --help | grep '^  rust'
    rust                       . Rust commands

The encoding has ONE correct load order and directory collection does not produce it: the
generated relation headers must precede the chapters whose rules use them, and
`Spec_files.collect` sorts `gen/` after every digit-prefixed file.  So every command below is
given `--spec` with the explicit list, which is what `tools/gen_spectec.py --print-files` in
`rust-type-semantics` prints and what `rust-spectec/README.md` documents under "Load order"
(open question row 4).  `parse` needs it as much as `typecheck` does, because it builds an IL
value of `pgm` and therefore reads the syntax.

  $ R=../../../specs/rust
  $ SPEC=
  $ for f in 0-stdlib 2-syntax gen/relations 3-resolution 4-wf 5-implied-bounds \
  >          6-trait-solving 7-normalization 8-regions 9-typing 10-coercion 11-items \
  >          12-dyn 13-coherence 14-determinacy 15-unify gen/congruence; do
  >   SPEC="$SPEC --spec $R/$f.spectec"
  > done

`parse -r` re-parses its own output and compares the IL values, so the printed program is
the surface form the grammar table of targets/rust/GRAMMAR.md fixes.  The crate label is a
directive comment, because an edition is per-crate metadata and not Rust syntax.

  $ spectec rust parse -p ../../../testdata/interp/rust/96460.rs -r --color never $SPEC
  //@ crate 96460 local 2021
  pub fn weird() -> PhantomData<impl Sized> { PhantomData }

The sugar rows of ch02-syntax.tex section 2.9 that need no knowledge of the program are
folded by the parser: an elided region becomes '_, an omitted return type becomes -> (),
`Box::new` becomes the prelude fn item box_new, and a body ending in a statement gains its
unit tail.

  $ spectec rust parse -p ../../../testdata/interp/rust/118876.rs -r --color never $SPEC
  //@ crate 118876 local 2021
  struct Bounded<'a, 'b: 'static, T>(&'a T, [&'b (); 0]) ;
  pub fn extend<T>(input: &'_ T) -> &'static T { let n: Box<dyn FnOnce<&'_ T, Output = Bounded<T, 'static, '_>>> = box_new(|x| { Bounded(x, []) }); n(input).0 }

An edition is not written in the file, so `-e` supplies it and the crate label carries it
through the round trip.  Two rows of the inventory are edition 2024.

  $ spectec rust parse -p ../../../testdata/interp/rust/108468-2024.rs -e 2024 -r --color never $SPEC
  //@ crate 108468-2024 local 2024
  pub async fn test(_args: impl Iterator<Item = &'_ str>) -> () { () }

  $ spectec rust parse -p ../../../testdata/interp/rust/108468-2024.rs -r --color never $SPEC
  //@ crate 108468-2024 local 2021
  pub async fn test(_args: impl Iterator<Item = &'_ str>) -> () { () }

`struct S;` is the empty named-field list of section 2.2, and a where clause may stand
between the generics and the `;`.

  $ cat > unit.rs <<'EOF'
  > pub struct Unit;
  > pub struct Bounded<T> where T: Copy;
  > EOF
  $ spectec rust parse -p unit.rs -r --color never $SPEC
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

Every program of the acceptance inventory round-trips.  This is acceptance milestone (1) of
Phase 4 design section 6.

  $ for f in ../../../testdata/interp/rust/*.rs; do
  >   spectec rust parse -p "$f" -r --color never $SPEC > /dev/null || echo "FAIL $f"
  > done
  $ ls ../../../testdata/interp/rust/*.rs | wc -l | tr -d ' '
  39

`typecheck` runs Chapter 11's `Pgm_ok` end to end.  A program the DOCUMENT accepts must
succeed and one it rejects must not; `expect.yaml` carries the document's verdict per row.

  $ spectec rust typecheck -p ../../../testdata/interp/rust/implicit-region-bound.rs --color never $SPEC
  Typecheck succeeded

  $ spectec rust typecheck -p ../../../testdata/interp/rust/141713.rs --color never $SPEC > /dev/null 2>&1 || echo rejected
  rejected

Every row of the acceptance inventory, against the document's verdict.  The two rows below are
the KNOWN DEVIATION `notes/deviations.yaml` records as D-23: Fig. 2.7's prelude declares the
`Try` and `FromResidual` traits and supplies no impl, so the `?` operator's two obligations are
underivable and the probe cannot be accepted for the reason section 14.6 gives.  The whole
run, with the blamed rule compared against the inventory as well, is `make encoding` in
`rust-type-semantics`; this checks the verdicts alone and from inside this repository.

  $ grep -v '^#' ../../../testdata/interp/rust/expect.yaml | while IFS= read -r line; do
  >   [ -n "$line" ] || continue
  >   f=${line%% *}; rest=${line#* }; e=${rest%%:*}; want=${rest#*: }
  >   if spectec rust typecheck -p ../../../testdata/interp/rust/$f -e $e --color never $SPEC \
  >        > /dev/null 2>&1
  >   then got=positive; else got=negative; fi
  >   [ "$got" = "$want" ] || echo "deviation: $f $e wants $want and the run gives $got"
  > done
  deviation: never-fallback-bounded.rs 2021 wants positive and the run gives negative
  deviation: never-fallback-bounded.rs 2024 wants positive and the run gives negative
