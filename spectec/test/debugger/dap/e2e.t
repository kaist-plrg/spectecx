DAP frontend on Mini-ML: scripted sessions. Each request is one JSON line,
framed by framing.exe; each message the adapter writes is printed as a line. The
run's own output goes to stderr, shown after the session.

  $ SPEC=../../../specs/miniml/spec.spectec
  $ PROG=../../../testdata/interp/miniml/distr_pair.ml
  $ session() {
  >   ./framing.exe frame \
  >   | spectec miniml debug eval --dap --spec $SPEC -p $PROG --color never \
  >       2> stderr \
  >   | ./framing.exe unframe
  >   echo "stderr:"; cat stderr
  > }

With no breakpoints, the run goes from configurationDone to the end:

  $ session <<'EOF2'
  > {"seq":1,"type":"request","command":"initialize","arguments":{"adapterID":"spectec"}}
  > {"seq":2,"type":"request","command":"launch"}
  > {"seq":3,"type":"request","command":"setBreakpoints","arguments":{"source":{"path":"spec.spectec"},"breakpoints":[{"line":143}]}}
  > {"seq":4,"type":"request","command":"continue","arguments":{"threadId":1}}
  > {"seq":5,"type":"request","command":"evaluate","arguments":{"expression":"x"}}
  > {"seq":6,"type":"request","command":"configurationDone"}
  > EOF2
  {"seq":1,"type":"response","request_seq":1,"success":true,"command":"initialize","body":{"supportsConfigurationDoneRequest":true,"supportsFunctionBreakpoints":true,"supportsTerminateRequest":true}}
  {"seq":2,"type":"event","event":"initialized"}
  {"seq":3,"type":"response","request_seq":2,"success":true,"command":"launch"}
  {"seq":4,"type":"response","request_seq":3,"success":true,"command":"setBreakpoints","body":{"breakpoints":[{"verified":false,"message":"Line breakpoints are not supported yet."}]}}
  {"seq":5,"type":"response","request_seq":4,"success":false,"command":"continue","message":"The run hasn't started."}
  {"seq":6,"type":"response","request_seq":5,"success":false,"command":"evaluate","message":"Unsupported request `evaluate`."}
  {"seq":7,"type":"response","request_seq":6,"success":true,"command":"configurationDone"}
  {"seq":8,"type":"event","event":"terminated"}
  stderr:
  42

A function breakpoint stops the run; the client can then inspect the stack and
step. Names that do not match are answered unverified:

  $ session <<'EOF2'
  > {"seq":1,"type":"request","command":"initialize","arguments":{"adapterID":"spectec"}}
  > {"seq":2,"type":"request","command":"setFunctionBreakpoints","arguments":{"breakpoints":[{"name":"Eval_expr/let"},{"name":"Nope"},{"name":"a/b/c"}]}}
  > {"seq":3,"type":"request","command":"configurationDone"}
  > {"seq":4,"type":"request","command":"threads"}
  > {"seq":5,"type":"request","command":"stackTrace","arguments":{"threadId":1}}
  > {"seq":6,"type":"request","command":"next","arguments":{"threadId":1}}
  > {"seq":7,"type":"request","command":"stepIn","arguments":{"threadId":1}}
  > {"seq":8,"type":"request","command":"stackTrace","arguments":{"threadId":1,"startFrame":0,"levels":1}}
  > {"seq":9,"type":"request","command":"stepOut","arguments":{"threadId":1}}
  > {"seq":10,"type":"request","command":"stackTrace","arguments":{"threadId":1}}
  > {"seq":11,"type":"request","command":"configurationDone"}
  > {"seq":12,"type":"request","command":"continue","arguments":{"threadId":1}}
  > EOF2
  {"seq":1,"type":"response","request_seq":1,"success":true,"command":"initialize","body":{"supportsConfigurationDoneRequest":true,"supportsFunctionBreakpoints":true,"supportsTerminateRequest":true}}
  {"seq":2,"type":"event","event":"initialized"}
  {"seq":3,"type":"response","request_seq":2,"success":true,"command":"setFunctionBreakpoints","body":{"breakpoints":[{"id":1,"verified":true},{"verified":false,"message":"No definition matches `Nope`."},{"verified":false,"message":"Cannot read `a/b/c` as Rel or Rel/rule."}]}}
  {"seq":4,"type":"response","request_seq":3,"success":true,"command":"configurationDone"}
  {"seq":5,"type":"event","event":"stopped","body":{"reason":"breakpoint","threadId":1,"allThreadsStopped":true,"hitBreakpointIds":[1]}}
  {"seq":6,"type":"response","request_seq":4,"success":true,"command":"threads","body":{"threads":[{"id":1,"name":"main"}]}}
  {"seq":7,"type":"response","request_seq":5,"success":true,"command":"stackTrace","body":{"stackFrames":[{"id":0,"name":"Eval_expr/let","source":{"name":"spec.spectec","path":"$TESTCASE_ROOT/../../../specs/miniml/spec.spectec"},"line":143,"column":6,"endLine":143,"endColumn":26}],"totalFrames":1}}
  {"seq":8,"type":"response","request_seq":6,"success":true,"command":"next"}
  {"seq":9,"type":"event","event":"stopped","body":{"reason":"step","threadId":1,"allThreadsStopped":true}}
  {"seq":10,"type":"response","request_seq":7,"success":true,"command":"stepIn"}
  {"seq":11,"type":"event","event":"stopped","body":{"reason":"step","threadId":1,"allThreadsStopped":true}}
  {"seq":12,"type":"response","request_seq":8,"success":true,"command":"stackTrace","body":{"stackFrames":[{"id":0,"name":"$subst_var","source":{"name":"spec.spectec","path":"$TESTCASE_ROOT/../../../specs/miniml/spec.spectec"},"line":44,"column":6,"endLine":44,"endColumn":14}],"totalFrames":6}}
  {"seq":13,"type":"response","request_seq":9,"success":true,"command":"stepOut"}
  {"seq":14,"type":"event","event":"stopped","body":{"reason":"step","threadId":1,"allThreadsStopped":true}}
  {"seq":15,"type":"response","request_seq":10,"success":true,"command":"stackTrace","body":{"stackFrames":[{"id":0,"name":"$subst","source":{"name":"spec.spectec","path":"$TESTCASE_ROOT/../../../specs/miniml/spec.spectec"},"line":56,"column":1,"endLine":56,"endColumn":47},{"id":1,"name":"$subst","source":{"name":"spec.spectec","path":"$TESTCASE_ROOT/../../../specs/miniml/spec.spectec"},"line":57,"column":1,"endLine":57,"endColumn":70},{"id":2,"name":"$subst","source":{"name":"spec.spectec","path":"$TESTCASE_ROOT/../../../specs/miniml/spec.spectec"},"line":57,"column":1,"endLine":57,"endColumn":70},{"id":3,"name":"$subst","source":{"name":"spec.spectec","path":"$TESTCASE_ROOT/../../../specs/miniml/spec.spectec"},"line":57,"column":1,"endLine":57,"endColumn":70},{"id":4,"name":"Eval_expr/let","source":{"name":"spec.spectec","path":"$TESTCASE_ROOT/../../../specs/miniml/spec.spectec"},"line":144,"column":6,"endLine":144,"endColumn":30}],"totalFrames":5}}
  {"seq":16,"type":"response","request_seq":11,"success":false,"command":"configurationDone","message":"The run is already in progress."}
  {"seq":17,"type":"response","request_seq":12,"success":true,"command":"continue"}
  {"seq":18,"type":"event","event":"terminated"}
  stderr:
  42
