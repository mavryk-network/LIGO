module H=Helpers
open Ast_typed
open H


let mixed = [
  ("blake2b" , e_a_raw_code "{ BLAKE2B  }" (t_function (t_bytes ()) (t_bytes    ()) ())) ;
  ("sha256"  , e_a_raw_code "{ SHA256   }" (t_function (t_bytes ()) (t_bytes    ()) ())) ;
  ("sha512"  , e_a_raw_code "{ SHA512   }" (t_function (t_bytes ()) (t_bytes    ()) ())) ;
  ("sha3"    , e_a_raw_code "{ SHA3     }" (t_function (t_bytes ()) (t_bytes    ()) ())) ;
  ("keccak"  , e_a_raw_code "{ KECCAK   }" (t_function (t_bytes ()) (t_bytes    ()) ())) ;
  ("hash_key", e_a_raw_code "{ HASH_KEY }" (t_function (t_key   ()) (t_key_hash ()) ())) ;
]

let uncurried = [
  ("check"   , e_a_raw_code "{ UNPAIR ; UNPAIR ; CHECK_SIGNATURE }" (t_function (t_triplet (t_key ()) (t_signature ()) (t_bytes ())) (t_bool ()) ()))
]

let curried = [
  ("check"   , curryfy3 (e_a_raw_code "{ UNPAIR ; UNPAIR ; CHECK_SIGNATURE }") (t_bool ()) (t_key (),t_signature (), t_bytes ()))
]

let module_ e ~curry = 
  make_module e "Crypto"
  (match curry with 
    true -> mixed @ curried
  | false -> mixed @ uncurried)

