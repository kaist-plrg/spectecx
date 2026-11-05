Run a P4 program on STF packets with the eBPF architecture.

  $ spectec p4 sim --spec-dir ../../specs/p4 -i ../../testdata/interp/p4/p4c/includes -p ../../testdata/interp/p4/p4c/p4_16_samples/valid_ebpf.p4 -stf ../../testdata/interp/p4/p4c/p4_16_samples/valid_ebpf.stf --color never 2>/dev/null
  Transmitted (0) 001B17000130B88198B7AEB70800450000344A6F4000400653920A0198453212C86ACF2C01BBD0FA585C4CCCB2AC80100353C31400000101080A0192463911A0C06F
  Transmitted (0) 001B17000130B88198B7AEB70800450000344A6F4000400653920A0198453212C86ACF2C01BBD0FA585C4CCCB2AC80100353C31400000101080A0192463911A0C06E
