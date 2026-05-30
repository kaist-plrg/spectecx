An if-condition that is not a boolean fails the typechecker.

  $ ./main.exe ../../../specs/impty/base non-boolean-condition.imp
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
      |     │   └── ../../../specs/impty/base/spec.spectec:110:10-110:21:
      |     │       condition command matches `% % = %` was not met
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
      |         └── ../../../specs/impty/base/spec.spectec:132:6-132:19:
      |             invocation of relation Check_command failed
      |             ├── ../../../specs/impty/base/spec.spectec:132:6-132:19:
      |             │   application of rule Check_command/skip failed
      |             │   └── ../../../specs/impty/base/spec.spectec:107:9-107:13:
      |             │       condition command matches `SKIP` was not met
      |             ├── ../../../specs/impty/base/spec.spectec:132:6-132:19:
      |             │   application of rule Check_command/decl failed
      |             │   └── ../../../specs/impty/base/spec.spectec:110:10-110:21:
      |             │       condition command matches `% % = %` was not met
      |             ├── ../../../specs/impty/base/spec.spectec:132:6-132:19:
      |             │   application of rule Check_command/assign failed
      |             │   └── ../../../specs/impty/base/spec.spectec:114:10-114:16:
      |             │       condition command matches `% = %` was not met
      |             ├── ../../../specs/impty/base/spec.spectec:132:6-132:19:
      |             │   application of rule Check_command/ite failed
      |             │   └── ../../../specs/impty/base/spec.spectec:120:28-120:32:
      |             │       condition type matches `BOOL` was not met
      |             ├── ../../../specs/impty/base/spec.spectec:132:6-132:19:
      |             │   application of rule Check_command/while failed
      |             │   └── ../../../specs/impty/base/spec.spectec:125:10-125:26:
      |             │       condition command matches `WHILE % DO % END` was not met
      |             └── ../../../specs/impty/base/spec.spectec:132:6-132:19:
      |                 application of rule Check_command/seq failed
      |                 └── ../../../specs/impty/base/spec.spectec:130:10-130:20:
      |                     condition command matches `% ; %` was not met
  [1]
