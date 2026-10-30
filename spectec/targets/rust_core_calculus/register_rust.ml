let () =
  Cli.Target_registry.register
    (module Targets_rust_core_calculus.Rust_cli.Cli)
