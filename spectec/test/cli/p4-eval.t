Run a P4 program on STF packets with the eBPF architecture.

  $ spectec p4 eval --spec-dir ../../specs/p4 -i ../../testdata/interp/p4/p4c/includes -p ../../testdata/interp/p4/p4c/p4_16_samples/valid_ebpf.p4 -stf ../../testdata/interp/p4/p4c/p4_16_samples/valid_ebpf.stf --color never 2>/dev/null
  Transmitted (0) 001B17000130B88198B7AEB70800450000344A6F4000400653920A0198453212C86ACF2C01BBD0FA585C4CCCB2AC80100353C31400000101080A0192463911A0C06F
  Transmitted (0) 001B17000130B88198B7AEB70800450000344A6F4000400653920A0198453212C86ACF2C01BBD0FA585C4CCCB2AC80100353C31400000101080A0192463911A0C06E

Run every eBPF program that has an STF file in a directory. The batch run
requires -p and -stf but does not use their values, and it indents each
transmitted packet under its program.

  $ SAMPLES=../../testdata/interp/p4/p4c/p4_16_samples
  $ mkdir -p corpus/includes
  $ cp ../../testdata/interp/p4/p4c/includes/core.p4 ../../testdata/interp/p4/p4c/includes/ebpf_model.p4 corpus/includes
  $ cp $SAMPLES/ebpf_headers.p4 $SAMPLES/valid_ebpf.p4 $SAMPLES/valid_ebpf.stf $SAMPLES/verify_ebpf.p4 $SAMPLES/verify_ebpf.stf corpus
  $ spectec p4 eval --spec-dir ../../specs/p4 -p corpus/valid_ebpf.p4 -stf corpus/valid_ebpf.stf --batch-dir corpus --color never 2>/dev/null
  >>> Running evaluator on corpus/valid_ebpf.p4
  Passed: corpus/valid_ebpf.p4
    Transmitted (0) 001B17000130B88198B7AEB70800450000344A6F4000400653920A0198453212C86ACF2C01BBD0FA585C4CCCB2AC80100353C31400000101080A0192463911A0C06F
    Transmitted (0) 001B17000130B88198B7AEB70800450000344A6F4000400653920A0198453212C86ACF2C01BBD0FA585C4CCCB2AC80100353C31400000101080A0192463911A0C06E
  
  >>> Running evaluator on corpus/verify_ebpf.p4
  Passed: corpus/verify_ebpf.p4
    Transmitted (0) 0100000000000000000000000000000000000000ABCDEF01
  
  
  Test Results: 2/2 passed, 0 failed

The parser rejects a packet shorter than its first header, so no packet is
transmitted.

  $ echo 'packet 0 00' > short.stf
  $ spectec p4 eval --spec-dir ../../specs/p4 -i ../../testdata/interp/p4/p4c/includes -p $SAMPLES/valid_ebpf.p4 -stf short.stf --color never 2>/dev/null
  No packets transmitted

STF statements other than packet, expect, add and setdefault are rejected.

  $ echo 'wait' > wait.stf
  $ spectec p4 eval --spec-dir ../../specs/p4 -i ../../testdata/interp/p4/p4c/includes -p $SAMPLES/valid_ebpf.p4 -stf wait.stf --color never 2>&1 | grep -A1 '^error'
  error: unsupported STF statement: wait
    --> wait.stf

An add line without an action is rejected as malformed.

  $ echo 'add pipe_Check_src_ip 0 key.field0:0x0a019845' > add-malformed.stf
  $ spectec p4 eval --spec-dir ../../specs/p4 -i ../../testdata/interp/p4/p4c/includes -p $SAMPLES/test_ebpf.p4 -stf add-malformed.stf --color never 2>&1 | grep -A1 '^error'
  error: malformed add statement
    --> add-malformed.stf

An add line that names a table the program lacks is rejected.

  $ echo 'add pipe_Missing 0 key.field0:0x0a019845 pipe_Reject()' > add-unknown-table.stf
  $ spectec p4 eval --spec-dir ../../specs/p4 -i ../../testdata/interp/p4/p4c/includes -p $SAMPLES/test_ebpf.p4 -stf add-unknown-table.stf --color never 2>&1 | grep '^error'
  error: unknown table main.filt.Missing
