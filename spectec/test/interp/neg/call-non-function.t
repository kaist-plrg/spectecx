Calling a non-function value fails the typechecker.

  $ ./main.exe ../../../specs/impty/closure call-non-function.imp
  error: invocation of relation Check_prog failed
    --> ../../../specs/impty/closure/spec.spectec:149:6
      |
  149 |   -- Check_command: eps |- command -| tenv
      |      ^^^^^^^^^^^^^
      |
      | source: il-interp
      |
      | trace:
      | application of rule Check_prog/ failed
      | └── ../../../specs/impty/closure/spec.spectec:149:6-149:19:
      |     invocation of relation Check_command failed
      |     ├── ../../../specs/impty/closure/spec.spectec:149:6-149:19:
      |     │   application of rule Check_command/skip failed
      |     │   └── ../../../specs/impty/closure/spec.spectec:114:11-114:15:
      |     │       condition command matches `SKIP` was not met
      |     ├── ../../../specs/impty/closure/spec.spectec:149:6-149:19:
      |     │   application of rule Check_command/decl failed
      |     │   └── ../../../specs/impty/closure/spec.spectec:117:12-117:23:
      |     │       condition command matches `% % = %` was not met
      |     ├── ../../../specs/impty/closure/spec.spectec:149:6-149:19:
      |     │   application of rule Check_command/assign failed
      |     │   └── ../../../specs/impty/closure/spec.spectec:121:12-121:18:
      |     │       condition command matches `% = %` was not met
      |     ├── ../../../specs/impty/closure/spec.spectec:149:6-149:19:
      |     │   application of rule Check_command/ite failed
      |     │   └── ../../../specs/impty/closure/spec.spectec:126:12-126:38:
      |     │       condition command matches `IF % THEN % ELSE % END` was not met
      |     ├── ../../../specs/impty/closure/spec.spectec:149:6-149:19:
      |     │   application of rule Check_command/while failed
      |     │   └── ../../../specs/impty/closure/spec.spectec:132:12-132:28:
      |     │       condition command matches `WHILE % DO % END` was not met
      |     └── ../../../specs/impty/closure/spec.spectec:149:6-149:19:
      |         application of rule Check_command/seq failed
      |         └── ../../../specs/impty/closure/spec.spectec:139:6-139:19:
      |             invocation of relation Check_command failed
      |             ├── ../../../specs/impty/closure/spec.spectec:139:6-139:19:
      |             │   application of rule Check_command/skip failed
      |             │   └── ../../../specs/impty/closure/spec.spectec:114:11-114:15:
      |             │       condition command matches `SKIP` was not met
      |             ├── ../../../specs/impty/closure/spec.spectec:139:6-139:19:
      |             │   application of rule Check_command/decl failed
      |             │   └── ../../../specs/impty/closure/spec.spectec:118:6-118:16:
      |             │       invocation of relation Check_expr failed
      |             │       ├── ../../../specs/impty/closure/spec.spectec:118:6-118:16:
      |             │       │   application of rule Check_expr/num failed
      |             │       │   └── ../../../specs/impty/closure/spec.spectec:69:12-69:18:
      |             │       │       condition expr <: literal was not met
      |             │       ├── ../../../specs/impty/closure/spec.spectec:118:6-118:16:
      |             │       │   application of rule Check_expr/boollit failed
      |             │       │   └── ../../../specs/impty/closure/spec.spectec:72:12-72:19:
      |             │       │       condition expr <: literal was not met
      |             │       ├── ../../../specs/impty/closure/spec.spectec:118:6-118:16:
      |             │       │   application of rule Check_expr/id failed
      |             │       │   └── ../../../specs/impty/closure/spec.spectec:75:11-75:12:
      |             │       │       condition expr <: id was not met
      |             │       ├── ../../../specs/impty/closure/spec.spectec:118:6-118:16:
      |             │       │   application of rule Check_expr/add failed
      |             │       │   └── ../../../specs/impty/closure/spec.spectec:79:11-79:21:
      |             │       │       condition expr matches `% + %` was not met
      |             │       ├── ../../../specs/impty/closure/spec.spectec:118:6-118:16:
      |             │       │   application of rule Check_expr/leq failed
      |             │       │   └── ../../../specs/impty/closure/spec.spectec:84:11-84:22:
      |             │       │       condition expr matches `% <= %` was not met
      |             │       ├── ../../../specs/impty/closure/spec.spectec:118:6-118:16:
      |             │       │   application of rule Check_expr/not failed
      |             │       │   └── ../../../specs/impty/closure/spec.spectec:89:11-89:15:
      |             │       │       condition expr matches `! %` was not met
      |             │       ├── ../../../specs/impty/closure/spec.spectec:118:6-118:16:
      |             │       │   application of rule Check_expr/and failed
      |             │       │   └── ../../../specs/impty/closure/spec.spectec:93:11-93:22:
      |             │       │       condition expr matches `% && %` was not met
      |             │       ├── ../../../specs/impty/closure/spec.spectec:118:6-118:16:
      |             │       │   application of rule Check_expr/fun failed
      |             │       │   └── ../../../specs/impty/closure/spec.spectec:98:11-98:53:
      |             │       │       condition expr matches `FUN (% %) -> % {%}` was not met
      |             │       └── ../../../specs/impty/closure/spec.spectec:118:6-118:16:
      |             │           application of rule Check_expr/call failed
      |             │           └── ../../../specs/impty/closure/spec.spectec:103:32-103:52:
      |             │               condition type matches `% -> %` was not met
      |             ├── ../../../specs/impty/closure/spec.spectec:139:6-139:19:
      |             │   application of rule Check_command/assign failed
      |             │   └── ../../../specs/impty/closure/spec.spectec:121:12-121:18:
      |             │       condition command matches `% = %` was not met
      |             ├── ../../../specs/impty/closure/spec.spectec:139:6-139:19:
      |             │   application of rule Check_command/ite failed
      |             │   └── ../../../specs/impty/closure/spec.spectec:126:12-126:38:
      |             │       condition command matches `IF % THEN % ELSE % END` was not met
      |             ├── ../../../specs/impty/closure/spec.spectec:139:6-139:19:
      |             │   application of rule Check_command/while failed
      |             │   └── ../../../specs/impty/closure/spec.spectec:132:12-132:28:
      |             │       condition command matches `WHILE % DO % END` was not met
      |             └── ../../../specs/impty/closure/spec.spectec:139:6-139:19:
      |                 application of rule Check_command/seq failed
      |                 └── ../../../specs/impty/closure/spec.spectec:137:12-137:22:
      |                     condition command matches `% ; %` was not met
  [1]
