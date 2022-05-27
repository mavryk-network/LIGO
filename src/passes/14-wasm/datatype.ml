type t = 
  Int
| Nat
| Mutez
| Timestamp
| Bool
| ListItem
| Tuple
| Set
| Map
| Operations 
| String
| Bytes
| Address

let int32_of_datatype = function
  Int        -> 0l
| Nat        -> 1l
| Mutez      -> 2l
| Timestamp  -> 3l
| Bool       -> 4l
| ListItem   -> 5l
| Tuple      -> 6l
| Set        -> 7l
| Map        -> 8l
| Operations -> 9l
| String     -> 10l
| Bytes      -> 11l
| Address    -> 12l