module Fresh = Fresh

(* The two generators the document names as builtin (spec/ch02-syntax.tex section 2.9,
   "freshness is generated, not chosen"), plus the implementations of the sequence, set,
   map, text and nat declarations of `0-stdlib.spectec`.  The stdlib file is byte-identical
   to the P4 target's, so the five modules beside this one are its implementation, taken
   from `targets/p4/builtins/`; none of them is P4-specific.  Everything else the encoding
   needs it defines as a `def` in `spectec/specs/rust/`. *)
let builtins =
  [ Nats.builtins; Texts.builtins; Lists.builtins; Sets.builtins; Maps.builtins;
    Fresh.builtins ]
  |> List.concat
