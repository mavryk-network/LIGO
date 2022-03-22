function check_signature (const (pk, signed, msg) : key * signature * bytes) : bool
is Crypto.check (pk, signed, msg)
