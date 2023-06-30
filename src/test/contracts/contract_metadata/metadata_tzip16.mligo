
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

let json = [%bytes
  {| {
  "name":"FA2 NFT Marketplace",
  "description":"Example of FA2 implementation",
  "version":"0.0.1",
  "license":{"name":"MIT"},
  "authors":["Marigold<contact@marigold.dev>"],
  "homepage":"https://marigold.dev",
  "source":{
  "tools":["Ligo"],
  "location":"https://github.com/ligolang/contract-catalogue/tree/main/lib/fa2"},
  "interfaces":["TZIP-012"],
  "errors": [],
  "views": []
  }
  |}]

let good_storage : storage =
   { data = 42
   ; metadata = Big_map.literal
     [ ("", Bytes.concat [%bytes "tezos-storage:hello%2F"] [%bytes "world"])
     ; ("hello/world", json)
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

let bad_storage2 : storage =
   { data = 42
   ; metadata = Big_map.literal
     [ ("", [%bytes "tezos-storage:haha"])
     ; ("hello/world", [%bytes "http://www.example.com"])
     ]
   }

let bad_storage3 : storage =
   { data = 42
   ; metadata = Big_map.literal
     [ ("", [%bytes "tezos-storage:haha"])
     ; ("haha", [%bytes "nojson!"])
     ]
   }
