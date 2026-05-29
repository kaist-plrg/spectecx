Assigning to a variable that was never declared fails the typechecker.

  $ ./main.exe ../../../specs/impty/base undeclared-variable.imp
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
          │   └── ../../../specs/impty/base/spec.spectec:111:9-111:41:
          │       condition ($lookup_<id, type>(TC, x) = ?(type)) was not met
          ├── 4. application of rule Check_command/ite failed
          │   └── ../../../specs/impty/base/spec.spectec:114:10-114:36:
          │       condition command matches `IF % THEN % ELSE % END` was not met
          ├── 5. application of rule Check_command/while failed
          │   └── ../../../specs/impty/base/spec.spectec:120:10-120:26:
          │       condition command matches `WHILE % DO % END` was not met
          └── 6. application of rule Check_command/seq failed
              └── ../../../specs/impty/base/spec.spectec:125:10-125:20:
                  condition command matches `% ; %` was not met
  
  
    source: il-interp
  [1]
