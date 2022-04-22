// Should be similar to: vendors/tezos-ligo/src/proto_alpha/lib_protocol/script_comparable.ml

int8_t __compare(int * item_a, int * item_b) {
  if (item_a < item_b) {
    return -1;
  } else if (item_a > item_b) {
    return 1;
  } else {
    return 0;
  }
}