function check_hash_key (const (kh1, k2) : key_hash * key) : bool * key_hash is
  block {
    var kh2 : key_hash := Crypto.hash_key (k2);
  } with ((kh1 = kh2), kh2)
