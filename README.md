# SpecTec-Core

A spec programming framework.
SpecTec was originally developed for WebAssembly (Wasm-SpecTec), then adapted/generalized for P4 (P4-SpecTec). SpecTec Core is a stripped down version of P4-SpecTec's algorithmic flavor, meant to serve as a base for adaptation to other languages or domains.

### Installation

* Install `opam` version 2.0.5 or higher.
  ```bash
  apt-get install opam
  opam init
  ```

* Create OCaml switch for version 5.1.0
  Install `dune` version 3.16.1, `bignum` version v0.17.0, `menhir` version 20240715, `core` version v0.17.1, `core_unix` version v0.17.0, and `bisect_ppx` version 2.8.3 via `opam`.
  ```bash
  opam switch create 5.1.0
  eval $(opam env)
  opam install dune bignum menhir core core_unix bisect_ppx
  ```

### Building the Project

```bash
make exe
```

This creates an executable `spectec-core` in the project root.

### Structure

SpecTec-Core currently consists of three main components.
* SpecTec EL is the surface language in which the spec is authored.
* SpecTec IL (internal language). EL -> IL conversion is called "elaboration". Elaboration makes the spec more algorithmic and unambiguous.
* SpecTec SL (structured language). IL -> SL conversion is called "structuring". Structuring groups related execution paths into explicit branching with over-approximation. This minimizes backtracking, making the SL interpreter much faster than the IL interpreter.
* Interpreter backends for IL/SL.
  * Needs to be coupled with a parser that converts an input file into a SpecTec IL value.

### Commands
```bash
# print out the IL representation of a SpecTec spec
./spectec-core elab spec/*.spectec
# print the SL representation of a SpecTec spec
./spectec-core struct spec/*.spectec

## P4-specific commands

# parse a P4 program to an IL value (-r to do a roundtrip test)
./spectec-core p4parse spec/*.spectec -i tests/interp/p4-tests/includes -p target/file.p4 [-r]

# run a P4 program based on SpecTec IL/SL
./spectec-core type-p4-il spec/*.spectec -i tests/interp/p4-tests/includes -p target/file.p4 [-dbg] [-profile]
./spectec-core type-p4-sl spec/*.spectec [-i tests/interp/p4-tests/includes -p target/file.p4 [-dbg] [-profile]

```
### Testing
```bash
make test
```

- Checks parsing, elaboration and structuring using the `examples/p4-concrete` spec corpus.
- Checks IL/SL interpreter coupled with the P4 parser using `tests/interp/p4-tests` files.

### Contributing

SpecTec-Core is an open-source project. Please feel free to contribute by opening issues or pull requests.

### License

SpecTec-Core is released under the [Apache 2.0 license](LICENSE).

### Credits

Most of the current codebase is derived from [P4-SpecTec](https://github.com/kaist-plrg/p4-spectec), which in turn is largely based on [Wasm-SpecTec](https://github.com/Wasm-DSL/spectec/tree/main).
