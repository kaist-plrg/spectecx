extern CounterArray {
    CounterArray(bit<32> max_index, bool sparse);
    void increment(in bit<32> index);
    void add(in bit<32> index, in bit<32> value);
}

CounterArray(3, false) positional;
CounterArray(sparse = true, max_index = 4) named;
CounterArray(0, false) empty;
