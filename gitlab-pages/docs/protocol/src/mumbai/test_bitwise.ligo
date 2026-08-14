const test_bytes_bitwise_ops =
  block {
    const b_and         = Bitwise.and         (0x0005, 0x0106);
    const b_or          = Bitwise.or          (0x0005, 0x0106);
    const b_xor         = Bitwise.xor         (0x0005, 0x0106);
    const b_shift_left  = Bitwise.shift_left  (0x06,   8n);
    const b_shift_right = Bitwise.shift_right (0x0006, 1n);
  } with assert (b_and         = 0x0004
             and b_or          = 0x0107
             and b_xor         = 0x0103
             and b_shift_left  = 0x0600
             and b_shift_right = 0x0003)