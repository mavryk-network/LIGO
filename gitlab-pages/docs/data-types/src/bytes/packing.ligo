function id_string (const p : string) : option (string) is
  block {
    const packed : bytes = Bytes.pack (p)
  } with (Bytes.unpack (packed) : option (string))