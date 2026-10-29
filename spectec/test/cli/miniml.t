Mini-ML CLI: surface behavior.

  $ SPEC=../../specs/miniml/spec.spectec
  $ CORPUS=../../testdata/interp/miniml

The command group presents the language name and surface operations consistently:

  $ spectec miniml --help
  Mini-ML commands
  
    spectec miniml SUBCOMMAND
  
  === subcommands ===
  
    checkpoint                 . Checkpoint utilities
    eval                       . Run Mini-ML evaluator
    parse                      . Parse a Mini-ML program to an IL value
    help                       . explain a given subcommand (perhaps recursively)
  

The evaluator prints a Mini-ML surface value:

  $ spectec miniml eval --spec $SPEC -p $CORPUS/fix1.ml --color never
  55

Applications with a function-valued left operand round-trip without losing required parentheses:

  $ spectec miniml parse --spec $SPEC -p $CORPUS/beta1.ml --color never -r
  (fun x -> x) 42
  $ spectec miniml parse --spec $SPEC -p $CORPUS/beta2.ml --color never -r
  (fun x -> + (x, x)) 21

Operator atoms carry the regions of their source tokens:

  $ echo '+ - ifz fst snd' > operators.ml
  $ spectec miniml parse --spec $SPEC -p operators.ml --color never -t | sed -En '/atom (ADD|SUB|IFZ|FST|SND) /s/.*atom /atom /p'
  atom ADD  1:1-1:2
  atom SUB  1:3-1:4
  atom IFZ  1:5-1:8
  atom FST  1:9-1:12
  atom SND  1:13-1:16
