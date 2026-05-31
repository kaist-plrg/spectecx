A loop body reading a never-declared variable fails the typechecker.

  $ ./main.exe ../../../specs/impty/base nested-undeclared-in-loop.imp
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
      |     │   └── ../../../specs/impty/base/spec.spectec:108:12-108:23:
      |     │       condition command matches `% % = %` was not met
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
      |         └── ../../../specs/impty/base/spec.spectec:130:6-130:19:
      |             invocation of relation Check_command failed
      |             ├── ../../../specs/impty/base/spec.spectec:130:6-130:19:
      |             │   application of rule Check_command/skip failed
      |             │   └── ../../../specs/impty/base/spec.spectec:105:11-105:15:
      |             │       condition command matches `SKIP` was not met
      |             ├── ../../../specs/impty/base/spec.spectec:130:6-130:19:
      |             │   application of rule Check_command/decl failed
      |             │   └── ../../../specs/impty/base/spec.spectec:108:12-108:23:
      |             │       condition command matches `% % = %` was not met
      |             ├── ../../../specs/impty/base/spec.spectec:130:6-130:19:
      |             │   application of rule Check_command/assign failed
      |             │   └── ../../../specs/impty/base/spec.spectec:112:12-112:18:
      |             │       condition command matches `% = %` was not met
      |             ├── ../../../specs/impty/base/spec.spectec:130:6-130:19:
      |             │   application of rule Check_command/ite failed
      |             │   └── ../../../specs/impty/base/spec.spectec:117:12-117:38:
      |             │       condition command matches `IF % THEN % ELSE % END` was not met
      |             ├── ../../../specs/impty/base/spec.spectec:130:6-130:19:
      |             │   application of rule Check_command/while failed
      |             │   └── ../../../specs/impty/base/spec.spectec:125:6-125:19:
      |             │       invocation of relation Check_command failed
      |             │       ├── ../../../specs/impty/base/spec.spectec:125:6-125:19:
      |             │       │   application of rule Check_command/skip failed
      |             │       │   └── ../../../specs/impty/base/spec.spectec:105:11-105:15:
      |             │       │       condition command matches `SKIP` was not met
      |             │       ├── ../../../specs/impty/base/spec.spectec:125:6-125:19:
      |             │       │   application of rule Check_command/decl failed
      |             │       │   └── ../../../specs/impty/base/spec.spectec:108:12-108:23:
      |             │       │       condition command matches `% % = %` was not met
      |             │       ├── ../../../specs/impty/base/spec.spectec:125:6-125:19:
      |             │       │   application of rule Check_command/assign failed
      |             │       │   └── ../../../specs/impty/base/spec.spectec:113:6-113:16:
      |             │       │       invocation of relation Check_expr failed
      |             │       │       ├── ../../../specs/impty/base/spec.spectec:113:6-113:16:
      |             │       │       │   application of rule Check_expr/num failed
      |             │       │       │   └── ../../../specs/impty/base/spec.spectec:68:12-68:18:
      |             │       │       │       condition expr <: literal was not met
      |             │       │       ├── ../../../specs/impty/base/spec.spectec:113:6-113:16:
      |             │       │       │   application of rule Check_expr/boollit failed
      |             │       │       │   └── ../../../specs/impty/base/spec.spectec:71:12-71:19:
      |             │       │       │       condition expr <: literal was not met
      |             │       │       ├── ../../../specs/impty/base/spec.spectec:113:6-113:16:
      |             │       │       │   application of rule Check_expr/id failed
      |             │       │       │   └── ../../../specs/impty/base/spec.spectec:74:11-74:12:
      |             │       │       │       condition expr <: id was not met
      |             │       │       ├── ../../../specs/impty/base/spec.spectec:113:6-113:16:
      |             │       │       │   application of rule Check_expr/add failed
      |             │       │       │   └── ../../../specs/impty/base/spec.spectec:80:6-80:16:
      |             │       │       │       invocation of relation Check_expr failed
      |             │       │       │       ├── ../../../specs/impty/base/spec.spectec:80:6-80:16:
      |             │       │       │       │   application of rule Check_expr/num failed
      |             │       │       │       │   └── ../../../specs/impty/base/spec.spectec:68:12-68:18:
      |             │       │       │       │       condition expr <: literal was not met
      |             │       │       │       ├── ../../../specs/impty/base/spec.spectec:80:6-80:16:
      |             │       │       │       │   application of rule Check_expr/boollit failed
      |             │       │       │       │   └── ../../../specs/impty/base/spec.spectec:71:12-71:19:
      |             │       │       │       │       condition expr <: literal was not met
      |             │       │       │       ├── ../../../specs/impty/base/spec.spectec:80:6-80:16:
      |             │       │       │       │   application of rule Check_expr/id failed
      |             │       │       │       │   └── ../../../specs/impty/base/spec.spectec:75:39-75:43:
      |             │       │       │       │       condition type'?{type' <- type'?} matches (_) was not met
      |             │       │       │       ├── ../../../specs/impty/base/spec.spectec:80:6-80:16:
      |             │       │       │       │   application of rule Check_expr/add failed
      |             │       │       │       │   └── ../../../specs/impty/base/spec.spectec:78:11-78:21:
      |             │       │       │       │       condition expr matches `% + %` was not met
      |             │       │       │       ├── ../../../specs/impty/base/spec.spectec:80:6-80:16:
      |             │       │       │       │   application of rule Check_expr/leq failed
      |             │       │       │       │   └── ../../../specs/impty/base/spec.spectec:83:11-83:22:
      |             │       │       │       │       condition expr matches `% <= %` was not met
      |             │       │       │       ├── ../../../specs/impty/base/spec.spectec:80:6-80:16:
      |             │       │       │       │   application of rule Check_expr/not failed
      |             │       │       │       │   └── ../../../specs/impty/base/spec.spectec:88:11-88:15:
      |             │       │       │       │       condition expr matches `! %` was not met
      |             │       │       │       └── ../../../specs/impty/base/spec.spectec:80:6-80:16:
      |             │       │       │           application of rule Check_expr/and failed
      |             │       │       │           └── ../../../specs/impty/base/spec.spectec:92:11-92:22:
      |             │       │       │               condition expr matches `% && %` was not met
      |             │       │       ├── ../../../specs/impty/base/spec.spectec:113:6-113:16:
      |             │       │       │   application of rule Check_expr/leq failed
      |             │       │       │   └── ../../../specs/impty/base/spec.spectec:83:11-83:22:
      |             │       │       │       condition expr matches `% <= %` was not met
      |             │       │       ├── ../../../specs/impty/base/spec.spectec:113:6-113:16:
      |             │       │       │   application of rule Check_expr/not failed
      |             │       │       │   └── ../../../specs/impty/base/spec.spectec:88:11-88:15:
      |             │       │       │       condition expr matches `! %` was not met
      |             │       │       └── ../../../specs/impty/base/spec.spectec:113:6-113:16:
      |             │       │           application of rule Check_expr/and failed
      |             │       │           └── ../../../specs/impty/base/spec.spectec:92:11-92:22:
      |             │       │               condition expr matches `% && %` was not met
      |             │       ├── ../../../specs/impty/base/spec.spectec:125:6-125:19:
      |             │       │   application of rule Check_command/ite failed
      |             │       │   └── ../../../specs/impty/base/spec.spectec:117:12-117:38:
      |             │       │       condition command matches `IF % THEN % ELSE % END` was not met
      |             │       ├── ../../../specs/impty/base/spec.spectec:125:6-125:19:
      |             │       │   application of rule Check_command/while failed
      |             │       │   └── ../../../specs/impty/base/spec.spectec:123:12-123:28:
      |             │       │       condition command matches `WHILE % DO % END` was not met
      |             │       └── ../../../specs/impty/base/spec.spectec:125:6-125:19:
      |             │           application of rule Check_command/seq failed
      |             │           └── ../../../specs/impty/base/spec.spectec:128:12-128:22:
      |             │               condition command matches `% ; %` was not met
      |             └── ../../../specs/impty/base/spec.spectec:130:6-130:19:
      |                 application of rule Check_command/seq failed
      |                 └── ../../../specs/impty/base/spec.spectec:128:12-128:22:
      |                     condition command matches `% ; %` was not met
  [1]
