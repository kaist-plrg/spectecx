module Fresh = Fresh
module Ebpf = Ebpf

let builtins =
  [
    Nats.builtins;
    Texts.builtins;
    Lists.builtins;
    Sets.builtins;
    Maps.builtins;
    Numerics.builtins;
    Fresh.builtins;
  ]
  |> List.concat
