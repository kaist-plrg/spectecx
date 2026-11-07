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
