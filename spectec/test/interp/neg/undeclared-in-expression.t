An initializer that reads a never-declared variable fails the typechecker.

  $ ./main.exe ../../../specs/impty/base undeclared-in-expression.imp
  error: invocation of relation Check_prog failed
    --> ../../../specs/impty/base/spec.spectec:143:6
      |
  143 |   -- Check_command: eps |- command -| TC
      |      ^^^^^^^^^^^^^
      |
      | source: il-interp
      |
      | trace:
      | application of rule Check_prog/ failed
      | └── ../../../specs/impty/base/spec.spectec:143:6-143:19:
      |     invocation of relation Check_command failed
      |     ├── ../../../specs/impty/base/spec.spectec:143:6-143:19:
      |     │   application of rule Check_command/skip failed
      |     │   └── ../../../specs/impty/base/spec.spectec:107:9-107:13:
      |     │       condition command matches `SKIP` was not met
      |     ├── ../../../specs/impty/base/spec.spectec:143:6-143:19:
      |     │   application of rule Check_command/decl failed
      |     │   └── ../../../specs/impty/base/spec.spectec:111:6-111:16:
      |     │       invocation of relation Check_expr failed
      |     │       ├── ../../../specs/impty/base/spec.spectec:111:6-111:16:
      |     │       │   application of rule Check_expr/num failed
      |     │       │   └── ../../../specs/impty/base/spec.spectec:70:10-70:16:
      |     │       │       condition expr <: literal was not met
      |     │       ├── ../../../specs/impty/base/spec.spectec:111:6-111:16:
      |     │       │   application of rule Check_expr/boollit failed
      |     │       │   └── ../../../specs/impty/base/spec.spectec:73:10-73:17:
      |     │       │       condition expr <: literal was not met
      |     │       ├── ../../../specs/impty/base/spec.spectec:111:6-111:16:
      |     │       │   application of rule Check_expr/id failed
      |     │       │   └── ../../../specs/impty/base/spec.spectec:77:37-77:41:
      |     │       │       condition type'?{type' <- type'?} matches (_) was not met
      |     │       ├── ../../../specs/impty/base/spec.spectec:111:6-111:16:
      |     │       │   application of rule Check_expr/add failed
      |     │       │   └── ../../../specs/impty/base/spec.spectec:80:9-80:19:
      |     │       │       condition expr matches `% + %` was not met
      |     │       ├── ../../../specs/impty/base/spec.spectec:111:6-111:16:
      |     │       │   application of rule Check_expr/leq failed
      |     │       │   └── ../../../specs/impty/base/spec.spectec:85:9-85:20:
      |     │       │       condition expr matches `% <= %` was not met
      |     │       ├── ../../../specs/impty/base/spec.spectec:111:6-111:16:
      |     │       │   application of rule Check_expr/not failed
      |     │       │   └── ../../../specs/impty/base/spec.spectec:90:9-90:13:
      |     │       │       condition expr matches `! %` was not met
      |     │       └── ../../../specs/impty/base/spec.spectec:111:6-111:16:
      |     │           application of rule Check_expr/and failed
      |     │           └── ../../../specs/impty/base/spec.spectec:94:9-94:20:
      |     │               condition expr matches `% && %` was not met
      |     ├── ../../../specs/impty/base/spec.spectec:143:6-143:19:
      |     │   application of rule Check_command/assign failed
      |     │   └── ../../../specs/impty/base/spec.spectec:114:10-114:16:
      |     │       condition command matches `% = %` was not met
      |     ├── ../../../specs/impty/base/spec.spectec:143:6-143:19:
      |     │   application of rule Check_command/ite failed
      |     │   └── ../../../specs/impty/base/spec.spectec:119:10-119:36:
      |     │       condition command matches `IF % THEN % ELSE % END` was not met
      |     ├── ../../../specs/impty/base/spec.spectec:143:6-143:19:
      |     │   application of rule Check_command/while failed
      |     │   └── ../../../specs/impty/base/spec.spectec:125:10-125:26:
      |     │       condition command matches `WHILE % DO % END` was not met
      |     └── ../../../specs/impty/base/spec.spectec:143:6-143:19:
      |         application of rule Check_command/seq failed
      |         └── ../../../specs/impty/base/spec.spectec:130:10-130:20:
      |             condition command matches `% ; %` was not met
  [1]
