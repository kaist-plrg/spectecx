# SpecTecX: An executable subset of SpecTec

SpecTec is a spec programming framework, originally developed for WebAssembly (Wasm-SpecTec), then adapted/generalized for P4 (P4-SpecTec). SpecTecX is a stripped-down version of P4-SpecTec that decouples language semantics from target definition, meant to serve as an adaptation base for other languages or domains.

### Installation

* Install `opam` version 2.0.5 or higher.
  ```bash
  apt-get install opam
  opam init
  ```

* Create an OCaml switch:
  ```bash
  opam switch create spectecx 5.1.1
  eval $(opam env)
  ```

* Install the core libraries and target-independent `spectec` executable from the checkout:
  ```bash
  opam install ./spectec.opam
  ```

* Install the target packages you need:
  ```bash
  opam install ./spectec-target-p4.opam
  opam install ./spectec-target-miniml.opam
  opam install ./spectec-target-impty.opam
  opam install ./spectec-target-itpl.opam
  ```
  Each target package installs command plugins and default specifications. The ITPL (*Introduction to Programming Languages*) package includes the AE, VAE, F1VAE, FVAE, FAE, RFAE, BFAE, MFAE, LFAE, KFAE, TFAE, TRFAE, TVFAE, PTFAE, STFAE, and TIFAE targets. The `spectec` executable discovers installed target plugins at startup.

For development, install every package's pinned dependency versions without installing the packages themselves:

```bash
opam install . --deps-only --locked
```

The lockfile (`spectec.opam.locked`) records the exact transitive dependency set CI uses. The unlocked constraints live in `dune-project` and surface in the generated opam files.

### Building the Project

```bash
make exe
```

This creates an executable `spectecx` in the project root.

### Structure

The compiler processes a specification through EL -> IL -> AL -> SL.

* SpecTec EL is the surface language in which the spec is authored.
* SpecTec IL (internal language) holds the result of elaboration, which checks types and resolves source notation.
* SpecTec AL (algorithmic language) holds executable rules and clauses. The algo pass turns IL into AL by deciding variable bindings, adding checks and annotating iterations.
* SpecTec SL (structured language) groups related AL execution paths into explicit branches to reduce backtracking.

The default interpreter executes AL, and `--sl` selects the SL interpreter. The default mode is named IL in command-line help and test targets. Targets provide parsers that turn programs into IL values.

Repository layout:

```
spectec/lib/lang/        ASTs for el / il / al / sl / pl / xl
spectec/lib/pass/        parse, elaborate (EL->IL), algo (IL->AL), structure (AL->SL)
spectec/lib/interp/      interpreters, builtins, target interface
spectec/lib/cli/         reusable CLI machinery and target plugin loading
spectec/lib/spectec.ml   public facade (pipeline + eval + Error/Task/Target)
spectec/targets/<t>/     per-target code, CLI modules, and plugin registration
spectec/bin/             target-independent command-line entrypoint
spectec/test/            diff-based test drivers
spectec/testdata/        test inputs
```

### Commands

The target-specific examples require the corresponding target package.

```bash
# print out the IL representation of a SpecTec spec
./spectecx elab spectec/specs/impty/base/spec.spectec
# print the AL representation of a SpecTec spec
./spectecx algo spectec/specs/impty/base/spec.spectec
# print the SL representation of a SpecTec spec
./spectecx struct spectec/specs/impty/base/spec.spectec

## P4-specific commands

# parse a P4 program to an IL value (-r to do a roundtrip test)
./spectecx p4 parse spec/*.spectec -i spectec/testdata/interp/p4-tests/includes -p target/file.p4 [-r]

# run a P4 program with the default AL interpreter or SL
./spectecx p4 typecheck -i spectec/testdata/interp/p4-tests/includes -p target/file.p4
./spectecx p4 typecheck -i spectec/testdata/interp/p4-tests/includes -p target/file.p4 --sl

# reject multiple successful alternatives for the current input
./spectecx p4 typecheck -i spectec/testdata/interp/p4-tests/includes -p target/file.p4 --det

# instantiate a P4 program using eBPF extern constructors
./spectecx p4 instantiate -p target/file.p4 --sl

# run an eBPF program on the packets and table lines of an STF file
./spectecx p4 eval -i spectec/testdata/interp/p4/p4c/includes -p spectec/testdata/interp/p4/p4c/p4_16_samples/test_ebpf.p4 -stf spectec/testdata/interp/p4/p4c/p4_16_samples/test_ebpf.stf --sl

## ITPL commands

./spectecx ae eval -p spectec/testdata/interp/itpl/ae/arithmetic.ae
./spectecx vae eval -p spectec/testdata/interp/itpl/vae/shadowing.vae --sl
./spectecx f1vae eval -p spectec/testdata/interp/itpl/f1vae/twice.f1vae
./spectecx fvae eval -p spectec/testdata/interp/itpl/fvae/capture.fvae --sl
./spectecx fae eval -p spectec/testdata/interp/itpl/fae/sugar.fae
./spectecx rfae eval -p spectec/testdata/interp/itpl/rfae/sum.rfae --sl
./spectecx bfae eval -p spectec/testdata/interp/itpl/bfae/alias.bfae
./spectecx mfae eval -p spectec/testdata/interp/itpl/mfae/counter.mfae --sl
./spectecx lfae eval -p spectec/testdata/interp/itpl/lfae/unused.lfae
./spectecx kfae eval -p spectec/testdata/interp/itpl/kfae/escape.kfae --sl
./spectecx tfae typecheck -p spectec/testdata/interp/itpl/tfae/twice.tfae
./spectecx trfae eval -p spectec/testdata/interp/itpl/trfae/sum.trfae --sl
./spectecx tvfae typecheck -p spectec/testdata/interp/itpl/tvfae/variant.tvfae
./spectecx ptfae typecheck -p spectec/testdata/interp/itpl/ptfae/identity.ptfae
./spectecx stfae typecheck -p spectec/testdata/interp/itpl/stfae/width.stfae
./spectecx tifae typecheck -p spectec/testdata/interp/itpl/tifae/identity.tifae
```

The `p4 instantiate` command typechecks a P4 program and creates its initial objects and state. It supports `CounterArray`, whose counters start at zero, and the table implementations `array_table` and `hash_table`. It rejects other extern objects.

`p4 eval` processes packets and applies table updates from an STF file in order. It keeps the eBPF P4 program's counters and table entries between packets. A `packet` line supplies an input port and packet bytes in hexadecimal. The P4 program's parser and filter control decide whether to transmit each packet unchanged on its input port. The command prints each transmitted packet with its port.

`expect` lines list the packets the program must transmit on each port, in order. `*` matches any hexadecimal digit. A trailing `$` requires the whole packet to match. Without `$`, the expected bytes need only match the start of the packet. The run fails if the transmitted packets differ from these expectations.

An `add` line installs a table entry, and a `setdefault` line replaces a table's default action. STF names beginning with `pipe_` refer to names inside the P4 program's `main.filt` control. Table key values may be decimal, hexadecimal (`0x`), binary (`0b`) or prefixes written as `0x<digits>/<length>`. A `*` in a hexadecimal or binary key matches any value at that digit position. The command rejects other STF statements, such as `wait`.

`p4 eval` supports the one-argument `packet_in.extract` method, `packet_in.advance`, `packet_in.length`, `CounterArray.increment`, `CounterArray.add` and the extern function `verify`. A run fails on unsupported extern calls, including the C functions used by `ebpf_checksum_extern.p4`.

With `--batch-dir DIR`, `p4 eval` runs each eBPF program under `DIR` that has an STF file of the same name, using `DIR/includes` as the include path. You must supply `-p` and `-stf` for batch runs, although the command ignores their values.

The [ITPL specifications](spectec/specs/itpl/README.md) follow the languages in chapters 6 through 24 of *Introduction to Programming Languages* by Jaemin Hong and Sukyoung Ryu, the textbook for KAIST CS320.

`--det` is opt-in and works with IL, SL, and PL alike. It rejects an input as soon as more than one alternative succeeds for it, even when the alternatives agree on their output, and it only checks inputs actually supplied to a run, not every input the spec could ever see. Evaluating the extra alternatives can invoke host operations (e.g. a builtin with a side effect) that are not rolled back. A resumed checkpoint must have been saved with the same `--det` setting as the run resuming it.

### Editor support

Integrations for `.spectec` files live in `editors/`:

- **Syntax highlighting** for VS Code, Emacs, and Vim/Neovim, one per subdirectory.
- **Diagnostics**: `make lsp` builds `spectecx-lsp`, a language server that reports parse, elaboration and algo errors as you edit.

See [editors/README.md](editors/README.md) for installing a highlighter and turning on the language server.

### Testing
```bash
make test
```

- Checks parsing, elaboration, algo and structuring using the specs under `spectec/specs/p4` and `spectec/specs/p4-old`.
- Checks interpreter results with the P4 parser using files under `spectec/testdata/interp/p4`.
- Runs p4c's eBPF programs that have STF files with the `p4 eval` evaluator in IL, SL and PL, skipping files named in the [P4 exclusion lists](spectec/testdata/interp/p4/excludes/README.md). Use `make test-il-eval`, `make test-sl-eval` or `make test-pl-eval` to run one interpreter's evaluation suite.

### Adding a New Target

Targets live in `spectec/targets/<name>/`, separate from `spectec/lib/`. The reusable CLI infrastructure (`Target_cli`, `Task_cli`, `Subcommand` constructors) lives in `spectec/lib/cli/`. To add a target:

1. Implement `Spectec.Target.S` and one or more `Spectec.Task.S` in `spectec/targets/<name>/`.
2. Add target-specific built-ins under `spectec/targets/<name>/builtins/`.
3. For each task, implement a `Cli.Task_cli.S` module that parses command-line flags into the task's input.
4. Compose those task-CLIs into a `Cli : Cli.Target_cli.S` module using `Cli.Subcommand` constructors (`make_task`, `make_parse`, `make_batch`, `make_checkpoint`).
5. Add a plugin entry module that calls `Cli.Target_registry.register (module Your_target.Cli)` when loaded.
6. Declare a target package in `dune-project`, including any named installation directories for packaged specifications.
7. Add a Dune `plugin` stanza that installs the entry module in the core package's `target_plugins` directory. Use `generate_sites_module` when target code needs to locate packaged specifications.

The P4, Mini-ML, Impty, and ITPL targets under `spectec/targets/` are working examples. A package can install several plugins, each registering one target with the same name as its plugin. Adding a target does not require changing `spectec/bin/main.ml`.

### Contributing

Contributions are welcome through issues and pull requests. [CONTRIBUTING.md](CONTRIBUTING.md) describes what contributors are expected to do.

### License

SpecTecX is released under the [Apache 2.0 license](LICENSE).

### Credits

Most of the current codebase is derived from [P4-SpecTec](https://github.com/kaist-plrg/p4-spectec), which in turn is largely based on [Wasm-SpecTec](https://github.com/Wasm-DSL/spectec/tree/main).
