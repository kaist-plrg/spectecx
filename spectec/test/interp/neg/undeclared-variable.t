Assigning to a variable that was never declared fails the typechecker.

  $ ./main.exe ../../../specs/impty/base undeclared-variable.imp
  error: tracing backtrack logs:
  invocation of relation Check_prog failed
  └── application of rule Check_prog/ failed
      └── ../../../specs/impty/base/spec.spectec:143:6-143:19:
          invocation of relation Check_command failed
          ├── 1. application of rule Check_command/skip failed
          │   └── ../../../specs/impty/base/spec.spectec:107:9-107:13:
          │       condition command matches `SKIP` was not met
          ├── 2. application of rule Check_command/decl failed
          │   └── ../../../specs/impty/base/spec.spectec:110:10-110:21:
          │       condition command matches `% % = %` was not met
          ├── 3. application of rule Check_command/assign failed
          │   └── ../../../specs/impty/base/spec.spectec:116:9-116:41:
          │       condition ($lookup_<id, type>(TC, x) = ?(type)) was not met
          ├── 4. application of rule Check_command/ite failed
          │   └── ../../../specs/impty/base/spec.spectec:119:10-119:36:
          │       condition command matches `IF % THEN % ELSE % END` was not met
          ├── 5. application of rule Check_command/while failed
          │   └── ../../../specs/impty/base/spec.spectec:125:10-125:26:
          │       condition command matches `WHILE % DO % END` was not met
          └── 6. application of rule Check_command/seq failed
              └── ../../../specs/impty/base/spec.spectec:130:10-130:20:
                  condition command matches `% ; %` was not met
  
  
    source: il-interp
  [1]
