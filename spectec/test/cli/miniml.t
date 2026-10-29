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

Operator atoms carry their source regions, and applications include grouping delimiters:

  $ echo '(+) (-) (ifz) (fst) (snd)' > operators.ml
  $ spectec miniml parse --spec $SPEC -p operators.ml --color never -t | sed '/^$/d'
  expr Eapp % %  1:1-1:26
  ├─atom Eapp  (no region)
  ├─expr Eapp % %  1:1-1:20
  │ ├─atom Eapp  (no region)
  │ ├─expr Eapp % %  1:1-1:14
  │ │ ├─atom Eapp  (no region)
  │ │ ├─expr Eapp % %  1:1-1:8
  │ │ │ ├─atom Eapp  (no region)
  │ │ │ ├─expr Eop %  1:2-1:3
  │ │ │ │ ├─atom Eop  (no region)
  │ │ │ │ └─op ADD  1:2-1:3
  │ │ │ │   └─atom ADD  1:2-1:3
  │ │ │ └─expr Eop %  1:6-1:7
  │ │ │   ├─atom Eop  (no region)
  │ │ │   └─op SUB  1:6-1:7
  │ │ │     └─atom SUB  1:6-1:7
  │ │ └─expr Eop %  1:10-1:13
  │ │   ├─atom Eop  (no region)
  │ │   └─op IFZ  1:10-1:13
  │ │     └─atom IFZ  1:10-1:13
  │ └─expr Eop %  1:16-1:19
  │   ├─atom Eop  (no region)
  │   └─op FST  1:16-1:19
  │     └─atom FST  1:16-1:19
  └─expr Eop %  1:22-1:25
    ├─atom Eop  (no region)
    └─op SND  1:22-1:25
      └─atom SND  1:22-1:25
