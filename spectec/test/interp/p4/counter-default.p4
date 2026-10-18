extern CounterArray {
    CounterArray(bool sparse = false, bit<32> max_index = 2,
                 @optional bit<32> unused);
}

CounterArray(max_index = 2) defaults;
CounterArray(max_index = 1, sparse = true) named_defaults;
CounterArray(unused = 7) all_defaults;
