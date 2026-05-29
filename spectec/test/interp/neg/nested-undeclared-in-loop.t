A loop body reading a never-declared variable fails the typechecker.

  $ ./main.exe ../../../specs/impty/base nested-undeclared-in-loop.imp
  error: tracing backtrack logs:
  invocation of relation Check_prog failed
  └── application of rule Check_prog/ failed
      └── ../../../specs/impty/base/spec.spectec:137:6-137:19:
          invocation of relation Check_command failed
          ├── 1. application of rule Check_command/skip failed
          │   └── ../../../specs/impty/base/spec.spectec:102:9-102:13:
          │       condition command matches `SKIP` was not met
          ├── 2. application of rule Check_command/decl failed
          │   └── ../../../specs/impty/base/spec.spectec:105:10-105:21:
          │       condition command matches `% % = %` was not met
          ├── 3. application of rule Check_command/assign failed
          │   └── ../../../specs/impty/base/spec.spectec:109:10-109:16:
          │       condition command matches `% = %` was not met
          ├── 4. application of rule Check_command/ite failed
          │   └── ../../../specs/impty/base/spec.spectec:114:10-114:36:
          │       condition command matches `IF % THEN % ELSE % END` was not met
          ├── 5. application of rule Check_command/while failed
          │   └── ../../../specs/impty/base/spec.spectec:120:10-120:26:
          │       condition command matches `WHILE % DO % END` was not met
          └── 6. application of rule Check_command/seq failed
              └── ../../../specs/impty/base/spec.spectec:127:6-127:19:
                  invocation of relation Check_command failed
                  ├── 1. application of rule Check_command/skip failed
                  │   └── ../../../specs/impty/base/spec.spectec:102:9-102:13:
                  │       condition command matches `SKIP` was not met
                  ├── 2. application of rule Check_command/decl failed
                  │   └── ../../../specs/impty/base/spec.spectec:105:10-105:21:
                  │       condition command matches `% % = %` was not met
                  ├── 3. application of rule Check_command/assign failed
                  │   └── ../../../specs/impty/base/spec.spectec:109:10-109:16:
                  │       condition command matches `% = %` was not met
                  ├── 4. application of rule Check_command/ite failed
                  │   └── ../../../specs/impty/base/spec.spectec:114:10-114:36:
                  │       condition command matches `IF % THEN % ELSE % END` was not met
                  ├── 5. application of rule Check_command/while failed
                  │   └── ../../../specs/impty/base/spec.spectec:122:6-122:19:
                  │       invocation of relation Check_command failed
                  │       ├── 1. application of rule Check_command/skip failed
                  │       │   └── ../../../specs/impty/base/spec.spectec:102:9-102:13:
                  │       │       condition command matches `SKIP` was not met
                  │       ├── 2. application of rule Check_command/decl failed
                  │       │   └── ../../../specs/impty/base/spec.spectec:105:10-105:21:
                  │       │       condition command matches `% % = %` was not met
                  │       ├── 3. application of rule Check_command/assign failed
                  │       │   └── ../../../specs/impty/base/spec.spectec:110:6-110:16:
                  │       │       invocation of relation Check_expr failed
                  │       │       ├── 1. application of rule Check_expr/num failed
                  │       │       │   └── ../../../specs/impty/base/spec.spectec:66:10-66:16:
                  │       │       │       condition expr <: literal was not met
                  │       │       ├── 2. application of rule Check_expr/boollit failed
                  │       │       │   └── ../../../specs/impty/base/spec.spectec:69:10-69:17:
                  │       │       │       condition expr <: literal was not met
                  │       │       ├── 3. application of rule Check_expr/id failed
                  │       │       │   └── ../../../specs/impty/base/spec.spectec:72:9-72:10:
                  │       │       │       condition expr <: id was not met
                  │       │       ├── 4. application of rule Check_expr/add failed
                  │       │       │   └── ../../../specs/impty/base/spec.spectec:78:6-78:16:
                  │       │       │       invocation of relation Check_expr failed
                  │       │       │       ├── 1. application of rule Check_expr/num failed
                  │       │       │       │   └── ../../../specs/impty/base/spec.spectec:66:10-66:16:
                  │       │       │       │       condition expr <: literal was not met
                  │       │       │       ├── 2. application of rule Check_expr/boollit failed
                  │       │       │       │   └── ../../../specs/impty/base/spec.spectec:69:10-69:17:
                  │       │       │       │       condition expr <: literal was not met
                  │       │       │       ├── 3. application of rule Check_expr/id failed
                  │       │       │       │   └── ../../../specs/impty/base/spec.spectec:73:37-73:41:
                  │       │       │       │       condition type'?{type' <- type'?} matches (_) was not met
                  │       │       │       ├── 4. application of rule Check_expr/add failed
                  │       │       │       │   └── ../../../specs/impty/base/spec.spectec:76:9-76:19:
                  │       │       │       │       condition expr matches `% + %` was not met
                  │       │       │       ├── 5. application of rule Check_expr/leq failed
                  │       │       │       │   └── ../../../specs/impty/base/spec.spectec:81:9-81:20:
                  │       │       │       │       condition expr matches `% <= %` was not met
                  │       │       │       ├── 6. application of rule Check_expr/not failed
                  │       │       │       │   └── ../../../specs/impty/base/spec.spectec:86:9-86:13:
                  │       │       │       │       condition expr matches `! %` was not met
                  │       │       │       └── 7. application of rule Check_expr/and failed
                  │       │       │           └── ../../../specs/impty/base/spec.spectec:90:9-90:20:
                  │       │       │               condition expr matches `% && %` was not met
                  │       │       ├── 5. application of rule Check_expr/leq failed
                  │       │       │   └── ../../../specs/impty/base/spec.spectec:81:9-81:20:
                  │       │       │       condition expr matches `% <= %` was not met
                  │       │       ├── 6. application of rule Check_expr/not failed
                  │       │       │   └── ../../../specs/impty/base/spec.spectec:86:9-86:13:
                  │       │       │       condition expr matches `! %` was not met
                  │       │       └── 7. application of rule Check_expr/and failed
                  │       │           └── ../../../specs/impty/base/spec.spectec:90:9-90:20:
                  │       │               condition expr matches `% && %` was not met
                  │       ├── 4. application of rule Check_command/ite failed
                  │       │   └── ../../../specs/impty/base/spec.spectec:114:10-114:36:
                  │       │       condition command matches `IF % THEN % ELSE % END` was not met
                  │       ├── 5. application of rule Check_command/while failed
                  │       │   └── ../../../specs/impty/base/spec.spectec:120:10-120:26:
                  │       │       condition command matches `WHILE % DO % END` was not met
                  │       └── 6. application of rule Check_command/seq failed
                  │           └── ../../../specs/impty/base/spec.spectec:125:10-125:20:
                  │               condition command matches `% ; %` was not met
                  └── 6. application of rule Check_command/seq failed
                      └── ../../../specs/impty/base/spec.spectec:125:10-125:20:
                          condition command matches `% ; %` was not met
  
  
    source: il-interp
  [1]
