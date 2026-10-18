extern CounterArray {
    CounterArray(bit<32> max_index, bool sparse);
}

parser Reader()(CounterArray counters, bool enabled = true) {
    state start { transition accept; }
}

control Filter()(CounterArray counters, bool enabled = true) {
    apply { }
}

package Pipeline<P, F>(P reader, F filter);

Pipeline(Reader(CounterArray(5, false)), Filter(CounterArray(6, true))) main;
