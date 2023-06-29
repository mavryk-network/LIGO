
type param = int

(* --------------------------------------------------------------------------- *)
(* no metadata here *)
type storage =
  { data     : int
  }
let entry_no_metadata (_p, s : param * storage) : operation list * storage =
  [], s

(* --------------------------------------------------------------------------- *)
(* metadata with incorrect format *)
type storage =
  { data     : int
  ; metadata : nat
  }
let entry_invalid_metadata_1 (_p, s : param * storage) : operation list * storage =
  [], s

(* --------------------------------------------------------------------------- *)
(* metadata with incorrect format (it's a big_map but params are reversed) *)
type storage =
  { data     : int
  ; metadata : (bytes, string) big_map
  }
let entry_invalid_metadata_2 (_p, s : param * storage) : operation list * storage =
  [], s

(* --------------------------------------------------------------------------- *)
(* metadata with incorrect format (as the last one, but using annotation) *)
type storage =
  { data     : int
  ; [@annot metadata] notdata : (bytes, string) big_map
  }
let entry_invalid_metadata_3 (_p, s : param * storage) : operation list * storage =
  [], s

(* --------------------------------------------------------------------------- *)
(* metadata with correct format *)
type storage =
  { data     : int
  ; metadata : (string, bytes) big_map
  }
let entry_valid_metadata (_p, s : param * storage) : operation list * storage =
  [], s

let good_storage : storage =
   { data = 42
   ; metadata = Big_map.literal
     [ ("", Bytes.concat [%bytes "tezos-storage:hello%2F"] [%bytes "world"])
     ; ("hello/world", [%bytes "JSON?"])
     ]
   }

let bad_storage0 : storage =
   { data = 42
   ; metadata = Big_map.literal
     [ ("", Bytes.concat [%bytes "tezos-storage:hello/"] [%bytes "invalid_not_http"])
     ; ("hello/world", [%bytes "JSON?"])
     ; ("invalid_not_http", [%bytes "https://www.example.com"])
     ; ("invalid_trailing_slash", [%bytes "ipfs://QmWqi3uBhBQ5KU6sR1LpLqJTr4GxuPfEK7UDyv6Gcc3fHL/"])
     ; ("invalid_wrong_hash", [%bytes "ipfs://invalid-hash"])
     ]
   }

let bad_storage1 : storage =
   { data = 42
   ; metadata = Big_map.literal
     [ ("", [%bytes "haha"])
     ; ("hello/world", [%bytes "http://www.example.com"])
     ]
   }
