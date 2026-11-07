module Fresh = Fresh
module P4_values = P4_values
module Ebpf = Ebpf

let builtins =
  [
    Nats.builtins;
    Ints.builtins;
    Texts.builtins;
    Lists.builtins;
    Sets.builtins;
    Maps.builtins;
    Numerics.builtins;
    Fresh.builtins;
  ]
  |> List.concat
