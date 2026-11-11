#include <ebpf_model.p4>
#include <core.p4>

// The first byte of a packet selects an operation. The filter accepts the
// packet when the operation gives the expected result.

header Test_h {
    bit<8> kind;
    int<8> signed_value;
    bit<8> bits;
}

header Item_h {
    bit<8> value;
}

struct Headers_t {
    Test_h    test;
    Item_h[2] items;
}

parser prs(packet_in p, out Headers_t headers) {
    state start {
        p.extract(headers.test);
        verify(headers.test.kind != 8w0xFF, error.ParserInvalidArgument);
        transition select(headers.test.kind) {
            8w3     : stack;
            default : accept;
        }
    }
    state stack {
        headers.items.push_front(1);
        p.extract(headers.items.next);
        transition accept;
    }
}

control pipe(inout Headers_t headers, out bool pass) {
    CounterArray(32w4, false) counters;

    apply {
        counters.add(32w0, 32w1);
        pass = false;
        if (headers.test.kind == 8w1) {
            pass = headers.test.signed_value < 0;
        } else if (headers.test.kind == 8w2) {
            headers.test.bits[7:4] = 4w0xA;
            pass = headers.test.bits == 8w0xA5;
        } else if (headers.test.kind == 8w3) {
            pass = !headers.items[0].isValid()
                && headers.items[1].isValid()
                && headers.items[1].value == headers.test.bits;
        }
    }
}

ebpfFilter(prs(), pipe()) main;
