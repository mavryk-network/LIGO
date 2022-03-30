let lib (s : Syntax_types.t) =
  match s with
  | PascaLIGO _ | ReasonLIGO | JsLIGO ->"
module Option = struct
   [@inline] let unopt (type a) (o : a option) : a = [%Michelson ({| { IF_NONE { PUSH string \"option is None\" ; FAILWITH } {} } |} : a option -> a)] o
   [@inline] let unopt_with_error (type a) ((o, s) : a option * string) : a = [%Michelson ({| { UNPAIR ; IF_NONE { FAILWITH } { SWAP ; DROP } } |} : (a option * string) -> a)] (o, s)
end
module Big_map = struct
   [@inline] let find_opt (type k v) ((k, m) : k * (k, v) big_map) : v option = [%Michelson ({| { UNPAIR ; GET } |} : k * (k, v) big_map -> v option)] (k, m)
end
module Map = struct
   [@inline] let find_opt (type k v) ((k, m) : k * (k, v) map) : v option = [%Michelson ({| { UNPAIR ; GET } |} : k * (k, v) map -> v option)] (k, m)
   [@inline] let size (type k v) (m : (k, v) map) : nat = [%Michelson ({| { SIZE } |} : (k, v) map -> nat)] m
end
module Set = struct
   [@inline] let cardinal (type a) (xs : a set) : nat = [%Michelson ({| { SIZE } |} : a set -> nat)] xs
   [@inline] let size (type a) (xs : a set) : nat = [%Michelson ({| { SIZE } |} : a set -> nat)] xs
end
module String = struct
   [@inline] let length (s : string) : nat = [%Michelson ({| { SIZE } |} : string -> nat)] s
   [@inline] let size (s : string) : nat = [%Michelson ({| { SIZE } |} : string -> nat)] s
   [@inline] let sub (sli : nat * nat * string) : string = [%Michelson ({| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} } |} : nat * nat * string -> string)] sli
   [@inline] let slice (sli : nat * nat * string) : string = [%Michelson ({| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} } |} : nat * nat * string -> string)] sli
   [@inline] let concat (p : string * string) : string = [%Michelson ({| { UNPAIR ; CONCAT } |} : string * string -> string)] p
end
module Crypto = struct
   [@inline] let blake2b (b : bytes) : bytes = [%Michelson ({| { BLAKE2B } |} : bytes -> bytes)] b
   [@inline] let sha256 (b : bytes) : bytes = [%Michelson ({| { SHA256 } |} : bytes -> bytes)] b
   [@inline] let sha512 (b : bytes) : bytes = [%Michelson ({| { SHA512 } |} : bytes -> bytes)] b
   [@inline] let sha3 (b : bytes) : bytes = [%Michelson ({| { SHA3 } |} : bytes -> bytes)] b
   [@inline] let keccak (b : bytes) : bytes = [%Michelson ({| { KECCAK } |} : bytes -> bytes)] b
   [@inline] let hash_key (k : key) : key_hash = [%Michelson ({| { HASH_KEY } |} : key -> key_hash)] k
   [@inline] let check ((k, s, b) : key * signature * bytes) : bool = [%Michelson ({| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |} : key * signature * bytes -> bool)] (k, s, b)
end
module Bytes = struct
   [@inline] let concat (p : bytes * bytes) : bytes = [%Michelson ({| { UNPAIR ; CONCAT } |} : bytes * bytes -> bytes)] p
   [@inline] let sub (sli : nat * nat * bytes) : bytes = [%Michelson ({| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} } |} : nat * nat * bytes -> bytes)] sli
   [@inline] let slice (sli : nat * nat * bytes) : bytes = [%Michelson ({| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} } |} : nat * nat * bytes -> bytes)] sli
   [@inline] let pack (type a) (x : a) : bytes = [%Michelson ({| { PACK } |} : a -> bytes)] x
   [@inline] let length (b : bytes) : nat = [%Michelson ({| { SIZE } |} : bytes -> nat)] b
   (* let unpack (type a) (b : bytes) : a option = [%Michelson ({| { UNPACK } |} : bytes -> a option)] b *)
end
module List = struct
   [@inline] let length (type a) (xs : a list) : nat = [%Michelson ({| { SIZE } |} : a list -> nat)] xs
   [@inline] let size (type a) (xs : a list) : nat = length xs
   [@inline] let head_opt (type a) (xs : a list) : a option =
     match xs with
     | [] -> None
     | x :: _ -> Some x
   [@inline] let tail_opt (type a) (xs : a list) : (a list) option =
     match xs with
     | [] -> None
     | _ :: xs -> Some xs
end
"
  | CameLIGO -> "
module Option = struct
   [@inline] let unopt (type a) (o : a option) : a = [%Michelson ({| { IF_NONE { PUSH string \"option is None\" ; FAILWITH } {} } |} : a option -> a)] o
   [@inline] let unopt_with_error (type a) (o : a option) (s : string) : a = [%Michelson ({| { UNPAIR ; IF_NONE { FAILWITH } { SWAP ; DROP } } |} : (a option * string) -> a)] (o, s)
end
module Big_map = struct
   [@inline] let find_opt (type k v) (k : k) (m : (k, v) big_map) : v option = [%Michelson ({| { UNPAIR ; GET } |} : k * (k, v) big_map -> v option)] (k, m)
end
module Map = struct
   [@inline] let find_opt (type k v) (k : k) (m : (k, v) map) : v option = [%Michelson ({| { UNPAIR ; GET } |} : k * (k, v) map -> v option)] (k, m)
   [@inline] let size (type k v) (m : (k, v) map) : nat = [%Michelson ({| { SIZE } |} : (k, v) map -> nat)] m
end
module Set = struct
   [@inline] let cardinal (type a) (xs : a set) : nat = [%Michelson ({| { SIZE } |} : a set -> nat)] xs
   [@inline] let size (type a) (xs : a set) : nat = [%Michelson ({| { SIZE } |} : a set -> nat)] xs
end
module String = struct
   [@inline] let length (s : string) : nat = [%Michelson ({| { SIZE } |} : string -> nat)] s
   [@inline] let size (s : string) : nat = [%Michelson ({| { SIZE } |} : string -> nat)] s
   [@inline] let sub (sli : nat * nat * string) : string = [%Michelson ({| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} } |} : nat * nat * string -> string)] sli
   [@inline] let slice (sli : nat * nat * string) : string = [%Michelson ({| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} } |} : nat * nat * string -> string)] sli
   [@inline] let sub (start : nat) (length : nat) (input : string) : string = [%Michelson ({| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} } |} : nat * nat * string -> string)] (start, length, input)
   [@inline] let concat (b : string) (c : string) : string = [%Michelson ({| { UNPAIR ; CONCAT } |} : string * string -> string)] (b, c)
end
module Crypto = struct
   [@inline] let blake2b (b : bytes) : bytes = [%Michelson ({| { BLAKE2B } |} : bytes -> bytes)] b
   [@inline] let sha256 (b : bytes) : bytes = [%Michelson ({| { SHA256 } |} : bytes -> bytes)] b
   [@inline] let sha512 (b : bytes) : bytes = [%Michelson ({| { SHA512 } |} : bytes -> bytes)] b
   [@inline] let sha3 (b : bytes) : bytes = [%Michelson ({| { SHA3 } |} : bytes -> bytes)] b
   [@inline] let keccak (b : bytes) : bytes = [%Michelson ({| { KECCAK } |} : bytes -> bytes)] b
   [@inline] let hash_key (k : key) : key_hash = [%Michelson ({| { HASH_KEY } |} : key -> key_hash)] k
   [@inline] let check (k : key) (s : signature) (b : bytes) : bool = [%Michelson ({| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |} : key * signature * bytes -> bool)] (k, s, b)
end
module Bytes = struct
   [@inline] let concat (b : bytes) (c : bytes) : bytes = [%Michelson ({| { UNPAIR ; CONCAT } |} : bytes * bytes -> bytes)] (b, c)
   [@inline] let sub (start : nat) (length : nat) (input : bytes) : bytes = [%Michelson ({| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} } |} : nat * nat * bytes -> bytes)] (start, length, input)
   [@inline] let slice (start : nat) (length : nat) (input : bytes) : bytes = [%Michelson ({| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} } |} : nat * nat * bytes -> bytes)] (start, length, input)
   [@inline] let pack (type a) (x : a) : bytes = [%Michelson ({| { PACK } |} : a -> bytes)] x
   [@inline] let length (b : bytes) : nat = [%Michelson ({| { SIZE } |} : bytes -> nat)] b
   (* let unpack (type a) (b : bytes) : a option = [%Michelson ({| { UNPACK } |} : bytes -> a option)] b *)
end
module List = struct
   [@inline] let length (type a) (xs : a list) : nat = [%Michelson ({| { SIZE } |} : a list -> nat)] xs
   [@inline] let size (type a) (xs : a list) : nat = length xs
   [@inline] let head_opt (type a) (xs : a list) : a option =
     match xs with
     | [] -> None
     | x :: _ -> Some x
   [@inline] let tail_opt (type a) (xs : a list) : (a list) option =
     match xs with
     | [] -> None
     | _ :: xs -> Some xs
end
"

let internalize_typed (ds : Ast_typed.program) =
  let open Ast_typed in
  let f (d : _) = match d with
    | Declaration_module { module_binder ; module_ ; module_attr = _ } ->
       let module_attr = { public = false } in
       Declaration_module { module_binder ; module_ ; module_attr }
    | _ -> d in
  let f (d : _ Ast_typed.location_wrap) = Simple_utils.Location.map f d in
  List.map ~f ds

let internalize_core (ds : Ast_core.module_) =
  let open Ast_core in
  let f (d : _) = match d with
    | Declaration_module { module_binder ; module_ ; module_attr = _ } ->
       let module_attr = { public = false } in
       Declaration_module { module_binder ; module_ ; module_attr }
    | _ -> d in
  let f (d : _ Ast_core.location_wrap) = Simple_utils.Location.map f d in
  List.map ~f ds

let stdlib ~options syntax =
  match Simple_utils.Trace.to_stdlib_result @@
          Ligo_compile.Utils.type_contract_string ~add_warning:(fun _ -> ()) ~options CameLIGO (lib syntax) with
  | Ok s -> s
  | _ -> failwith "Error compiling the stdlib"

let stdlib_typed ~options syntax =
  internalize_typed @@fst @@ stdlib ~options syntax

let stdlib_core ~options syntax =
  internalize_core @@snd @@ stdlib ~options syntax
