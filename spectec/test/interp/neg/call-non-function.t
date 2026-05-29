Calling a non-function value fails the typechecker.

  $ ./main.exe ../../../specs/impty/closure call-non-function.imp
  error: tracing backtrack logs:
  invocation of relation Check_prog failed
  └── application of rule Check_prog/ failed
      └── ../../../specs/impty/closure/spec.spectec:151:6-151:19:
          invocation of relation Check_command failed
          ├── 1. application of rule Check_command/skip failed
          │   └── ../../../specs/impty/closure/spec.spectec:116:9-116:13:
          │       condition command matches `SKIP` was not met
          ├── 2. application of rule Check_command/decl failed
          │   └── ../../../specs/impty/closure/spec.spectec:119:10-119:21:
          │       condition command matches `% % = %` was not met
          ├── 3. application of rule Check_command/assign failed
          │   └── ../../../specs/impty/closure/spec.spectec:123:10-123:16:
          │       condition command matches `% = %` was not met
          ├── 4. application of rule Check_command/ite failed
          │   └── ../../../specs/impty/closure/spec.spectec:128:10-128:36:
          │       condition command matches `IF % THEN % ELSE % END` was not met
          ├── 5. application of rule Check_command/while failed
          │   └── ../../../specs/impty/closure/spec.spectec:134:10-134:26:
          │       condition command matches `WHILE % DO % END` was not met
          └── 6. application of rule Check_command/seq failed
              └── ../../../specs/impty/closure/spec.spectec:141:6-141:19:
                  invocation of relation Check_command failed
                  ├── 1. application of rule Check_command/skip failed
                  │   └── ../../../specs/impty/closure/spec.spectec:116:9-116:13:
                  │       condition command matches `SKIP` was not met
                  ├── 2. application of rule Check_command/decl failed
                  │   └── ../../../specs/impty/closure/spec.spectec:120:6-120:16:
                  │       invocation of relation Check_expr failed
                  │       ├── 1. application of rule Check_expr/num failed
                  │       │   └── ../../../specs/impty/closure/spec.spectec:71:10-71:16:
                  │       │       condition expr <: literal was not met
                  │       ├── 2. application of rule Check_expr/boollit failed
                  │       │   └── ../../../specs/impty/closure/spec.spectec:74:10-74:17:
                  │       │       condition expr <: literal was not met
                  │       ├── 3. application of rule Check_expr/id failed
                  │       │   └── ../../../specs/impty/closure/spec.spectec:77:9-77:10:
                  │       │       condition expr <: id was not met
                  │       ├── 4. application of rule Check_expr/add failed
                  │       │   └── ../../../specs/impty/closure/spec.spectec:81:9-81:19:
                  │       │       condition expr matches `% + %` was not met
                  │       ├── 5. application of rule Check_expr/leq failed
                  │       │   └── ../../../specs/impty/closure/spec.spectec:86:9-86:20:
                  │       │       condition expr matches `% <= %` was not met
                  │       ├── 6. application of rule Check_expr/not failed
                  │       │   └── ../../../specs/impty/closure/spec.spectec:91:9-91:13:
                  │       │       condition expr matches `! %` was not met
                  │       ├── 7. application of rule Check_expr/and failed
                  │       │   └── ../../../specs/impty/closure/spec.spectec:95:9-95:20:
                  │       │       condition expr matches `% && %` was not met
                  │       ├── 8. application of rule Check_expr/fun failed
                  │       │   └── ../../../specs/impty/closure/spec.spectec:100:9-100:51:
                  │       │       condition expr matches `FUN (% %) -> % {%}` was not met
                  │       └── 9. application of rule Check_expr/call failed
                  │           └── ../../../specs/impty/closure/spec.spectec:105:30-105:50:
                  │               condition type matches `% -> %` was not met
                  ├── 3. application of rule Check_command/assign failed
                  │   └── ../../../specs/impty/closure/spec.spectec:123:10-123:16:
                  │       condition command matches `% = %` was not met
                  ├── 4. application of rule Check_command/ite failed
                  │   └── ../../../specs/impty/closure/spec.spectec:128:10-128:36:
                  │       condition command matches `IF % THEN % ELSE % END` was not met
                  ├── 5. application of rule Check_command/while failed
                  │   └── ../../../specs/impty/closure/spec.spectec:134:10-134:26:
                  │       condition command matches `WHILE % DO % END` was not met
                  └── 6. application of rule Check_command/seq failed
                      └── ../../../specs/impty/closure/spec.spectec:139:10-139:20:
                          condition command matches `% ; %` was not met
  
  
    source: il-interp
  [1]
