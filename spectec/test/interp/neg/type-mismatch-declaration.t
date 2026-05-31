A declaration whose initializer type differs from the declared type fails.

  $ ./main.exe ../../../specs/impty/base type-mismatch-declaration.imp
  error: invocation of relation Check_prog failed
    --> ../../../specs/impty/base/spec.spectec:141:6
      |
  141 |   -- Check_command: eps |- command -| tenv
      |      ^^^^^^^^^^^^^
      |
      | source: il-interp
      |
      | trace:
      | application of rule Check_prog/ failed
      | └── ../../../specs/impty/base/spec.spectec:141:6-141:19:
      |     invocation of relation Check_command failed
      |     ├── ../../../specs/impty/base/spec.spectec:141:6-141:19:
      |     │   application of rule Check_command/skip failed
      |     │   └── ../../../specs/impty/base/spec.spectec:105:11-105:15:
      |     │       condition command matches `SKIP` was not met
      |     ├── ../../../specs/impty/base/spec.spectec:141:6-141:19:
      |     │   application of rule Check_command/decl failed
      |     │   └── ../../../specs/impty/base/spec.spectec:109:30-109:34:
      |     │       condition (type' = type) was not met
      |     ├── ../../../specs/impty/base/spec.spectec:141:6-141:19:
      |     │   application of rule Check_command/assign failed
      |     │   └── ../../../specs/impty/base/spec.spectec:112:12-112:18:
      |     │       condition command matches `% = %` was not met
      |     ├── ../../../specs/impty/base/spec.spectec:141:6-141:19:
      |     │   application of rule Check_command/ite failed
      |     │   └── ../../../specs/impty/base/spec.spectec:117:12-117:38:
      |     │       condition command matches `IF % THEN % ELSE % END` was not met
      |     ├── ../../../specs/impty/base/spec.spectec:141:6-141:19:
      |     │   application of rule Check_command/while failed
      |     │   └── ../../../specs/impty/base/spec.spectec:123:12-123:28:
      |     │       condition command matches `WHILE % DO % END` was not met
      |     └── ../../../specs/impty/base/spec.spectec:141:6-141:19:
      |         application of rule Check_command/seq failed
      |         └── ../../../specs/impty/base/spec.spectec:128:12-128:22:
      |             condition command matches `% ; %` was not met
  [1]
