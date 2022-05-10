open Types
(* type constants *)
type t =
    Bool
  | String
  | Bytes
  | Int
  | Operation
  | Nat
  | Tez
  | Unit
  | Address
  | Signature
  | Key
  | Key_hash
  | Timestamp
  | Chain_id
  | List
  | Map
  | Big_map
  | Set
  | Contract
  | Michelson_or
  | Michelson_pair
  | Baker_hash
  | Pvss_key
  | Sapling_transaction
  | Sapling_state
  | Baker_operation
  | Bls12_381_g1
  | Bls12_381_g2
  | Bls12_381_fr
  | Never
  | Ticket
  | Michelson_program
  | Typed_address
  | Mutation
  | Chest
  | Chest_key
  | Chest_opening_result
  | External of string
  [@@deriving ord, eq]

let to_string = function
  | Bool                 -> "bool"
  | String               -> "string"
  | Bytes                -> "bytes"
  | Int                  -> "int"
  | Operation            -> "operation"
  | Nat                  -> "nat"
  | Tez                  -> "tez"
  | Unit                 -> "unit"
  | Address              -> "address"
  | Signature            -> "signature"
  | Key                  -> "key"
  | Key_hash             -> "key_hash"
  | Timestamp            -> "timestamp"
  | Chain_id             -> "chain_id"
  | List                 -> "list"
  | Map                  -> "map"
  | Big_map              -> "big_map"
  | Set                  -> "set"
  | Contract             -> "contract"
  | Michelson_or         -> "michelson_or"
  | Michelson_pair       -> "michelson_pair"
  | Baker_hash           -> "baker_hash"
  | Pvss_key             -> "pvss_key"
  | Sapling_transaction  -> "sapling_transaction"
  | Sapling_state        -> "sapling_state"
  | Baker_operation      -> "baker_operation"
  | Bls12_381_g1         -> "bls12_381_g1"
  | Bls12_381_g2         -> "bls12_381_g2"
  | Bls12_381_fr         -> "bls12_381_fr"
  | Never                -> "never"
  | Ticket               -> "ticket"
  | Michelson_program    -> "michelson_program"
  | Typed_address        -> "typed_address"
  | Mutation             -> "mutation"
  | Chest                -> "chest"
  | Chest_key            -> "chest_key"
  | Chest_opening_result -> "chest_opening_result"
  | External s           -> "external_" ^ s

  let of_string = function
  | "bool"                 -> Bool
  | "string"               -> String
  | "bytes"                -> Bytes
  | "int"                  -> Int
  | "operation"            -> Operation
  | "nat"                  -> Nat
  | "tez"                  -> Tez
  | "unit"                 -> Unit
  | "address"              -> Address
  | "signature"            -> Signature
  | "key"                  -> Key
  | "key_hash"             -> Key_hash
  | "timestamp"            -> Timestamp
  | "chain_id"             -> Chain_id
  | "list"                 -> List
  | "map"                  -> Map
  | "big_map"              -> Big_map
  | "set"                  -> Set
  | "contract"             -> Contract
  | "michelson_or"         -> Michelson_or
  | "michelson_pair"       -> Michelson_pair
  | "baker_hash"           -> Baker_hash
  | "pvss_key"             -> Pvss_key
  | "sapling_transaction"  -> Sapling_transaction
  | "sapling_state"        -> Sapling_state
  | "baker_operation"      -> Baker_operation
  | "bls12_381_g1"         -> Bls12_381_g1
  | "bls12_381_g2"         -> Bls12_381_g2
  | "bls12_381_fr"         -> Bls12_381_fr
  | "never"                -> Never
  | "ticket"               -> Ticket
  | "michelson_program"    -> Michelson_program
  | "typed_address"        -> Typed_address
  | "mutation"             -> Mutation
  | "chest"                -> Chest
  | "chest_key"            -> Chest_key
  | "chest_opening_result" -> Chest_opening_result
  | "external_failwith"    -> External "failwith"
  | "external_int"         -> External "int"
  | "external_ediv"        -> External "ediv"
  | "external_u_ediv"      -> External "u_ediv"
  | "external_add"         -> External "add"
  | "external_u_add"       -> External "u_add"
  | "external_polymorphic_add"   -> External "polymorphic_add"
  | "external_u_polymorphic_add" -> External "u_polymorphic_add"
  | "external_sub"         -> External "sub"
  | "external_u_sub"       -> External "u_sub"
  | "external_polymorphic_sub"   -> External "polymorphic_sub"
  | "external_u_polymorphic_sub" -> External "u_polymorphic_sub"
  | "external_mul"         -> External "mul"
  | "external_u_mul"       -> External "u_mul"
  | "external_div"         -> External "div"
  | "external_u_div"       -> External "u_div"
  | "external_mod"         -> External "mod"
  | "external_u_mod"       -> External "u_mod"
  | "external_and"         -> External "and"
  | "external_u_and"       -> External "u_and"
  | _ -> failwith "Forgot to add constant name in constant.ml?"

let bool                 = Bool
let string               = String
let bytes                = Bytes
let int                  = Int
let operation            = Operation
let nat                  = Nat
let tez                  = Tez
let unit                 = Unit
let address              = Address
let signature            = Signature
let key                  = Key
let key_hash             = Key_hash
let timestamp            = Timestamp
let chain_id             = Chain_id
let list                 = List
let map                  = Map
let big_map              = Big_map
let set                  = Set
let contract             = Contract
let michelson_or         = Michelson_or
let michelson_pair       = Michelson_pair
let baker_hash           = Baker_hash
let pvss_key             = Pvss_key
let sapling_transaction  = Sapling_transaction
let sapling_state        = Sapling_state
let baker_operation      = Baker_operation
let bls12_381_g1         = Bls12_381_g1
let bls12_381_g2         = Bls12_381_g2
let bls12_381_fr         = Bls12_381_fr
let never                = Never
let ticket               = Ticket
let michelson_program    = Michelson_program
let typed_address        = Typed_address
let mutation             = Mutation
let chest                = Chest
let chest_key            = Chest_key
let chest_opening_result = Chest_opening_result
let external_failwith    = External "failwith"
let external_int         = External "int"
let external_ediv        = External "ediv"
let external_u_ediv      = External "u_ediv"
let external_add         = External "add"
let external_u_add       = External "u_add"
let external_polymorphic_add   = External "polymorphic_add"
let external_u_polymorphic_add = External "u_polymorphic_add"
let external_sub         = External "sub"
let external_u_sub       = External "u_sub"
let external_polymorphic_sub   = External "polymorphic_sub"
let external_u_polymorphic_sub = External "u_polymorphic_sub"
let external_mul         = External "mul"
let external_u_mul       = External "u_mul"
let external_div         = External "div"
let external_u_div       = External "u_div"
let external_mod         = External "mod"
let external_u_mod       = External "u_mod"
let external_and         = External "and"
let external_u_and       = External "u_and"

let v_bool                 : type_variable = TypeVar.of_input_var (to_string Bool)
let v_string               : type_variable = TypeVar.of_input_var (to_string String)
let v_bytes                : type_variable = TypeVar.of_input_var (to_string Bytes)
let v_int                  : type_variable = TypeVar.of_input_var (to_string Int)
let v_operation            : type_variable = TypeVar.of_input_var (to_string Operation)
let v_nat                  : type_variable = TypeVar.of_input_var (to_string Nat)
let v_tez                  : type_variable = TypeVar.of_input_var (to_string Tez)
let v_unit                 : type_variable = TypeVar.of_input_var (to_string Unit)
let v_address              : type_variable = TypeVar.of_input_var (to_string Address)
let v_signature            : type_variable = TypeVar.of_input_var (to_string Signature)
let v_key                  : type_variable = TypeVar.of_input_var (to_string Key)
let v_key_hash             : type_variable = TypeVar.of_input_var (to_string Key_hash)
let v_timestamp            : type_variable = TypeVar.of_input_var (to_string Timestamp)
let v_chain_id             : type_variable = TypeVar.of_input_var (to_string Chain_id)
let v_option               : type_variable = TypeVar.of_input_var ("option")
let v_list                 : type_variable = TypeVar.of_input_var (to_string List)
let v_map                  : type_variable = TypeVar.of_input_var (to_string Map)
let v_big_map              : type_variable = TypeVar.of_input_var (to_string Big_map)
let v_set                  : type_variable = TypeVar.of_input_var (to_string Set)
let v_contract             : type_variable = TypeVar.of_input_var (to_string Contract)
let v_michelson_or         : type_variable = TypeVar.of_input_var (to_string Michelson_or)
let v_michelson_pair       : type_variable = TypeVar.of_input_var (to_string Michelson_pair)
let v_baker_hash           : type_variable = TypeVar.of_input_var (to_string Baker_hash)
let v_pvss_key             : type_variable = TypeVar.of_input_var (to_string Pvss_key)
let v_sapling_trasaction   : type_variable = TypeVar.of_input_var (to_string Sapling_transaction)
let v_sapling_state        : type_variable = TypeVar.of_input_var (to_string Sapling_state)
let v_baker_operation      : type_variable = TypeVar.of_input_var (to_string Baker_operation)
let v_bls12_381_g1         : type_variable = TypeVar.of_input_var (to_string Bls12_381_g1)
let v_bls12_381_g2         : type_variable = TypeVar.of_input_var (to_string Bls12_381_g2)
let v_bls12_381_fr         : type_variable = TypeVar.of_input_var (to_string Bls12_381_fr)
let v_never                : type_variable = TypeVar.of_input_var (to_string Never)
let v_ticket               : type_variable = TypeVar.of_input_var (to_string Ticket)
let v_test_michelson       : type_variable = TypeVar.of_input_var (to_string Michelson_program)
let v_typed_address        : type_variable = TypeVar.of_input_var (to_string Typed_address)
let v_mutation             : type_variable = TypeVar.of_input_var (to_string Mutation)
let v_chest                : type_variable = TypeVar.of_input_var (to_string Chest)
let v_chest_key            : type_variable = TypeVar.of_input_var (to_string Chest_key)
let v_chest_opening_result : type_variable = TypeVar.of_input_var (to_string Chest_opening_result)
let v_external_failwith    : type_variable = TypeVar.of_input_var (to_string @@ External "failwith")
let v_external_int         : type_variable = TypeVar.of_input_var (to_string @@ External "int")
let v_external_ediv        : type_variable = TypeVar.of_input_var (to_string @@ External "ediv")
let v_external_u_ediv      : type_variable = TypeVar.of_input_var (to_string @@ External "u_ediv")
let v_external_add         : type_variable = TypeVar.of_input_var (to_string @@ External "add")
let v_external_u_add       : type_variable = TypeVar.of_input_var (to_string @@ External "u_add")
let v_external_polymorphic_add   : type_variable = TypeVar.of_input_var (to_string @@ External "polymorphic_add")
let v_external_u_polymorphic_add : type_variable = TypeVar.of_input_var (to_string @@ External "u_polymorphic_add")
let v_external_sub         : type_variable = TypeVar.of_input_var (to_string @@ External "sub")
let v_external_u_sub       : type_variable = TypeVar.of_input_var (to_string @@ External "u_sub")
let v_external_polymorphic_sub   : type_variable = TypeVar.of_input_var (to_string @@ External "polymorphic_sub")
let v_external_u_polymorphic_sub : type_variable = TypeVar.of_input_var (to_string @@ External "u_polymorphic_sub")
let v_external_mul         : type_variable = TypeVar.of_input_var (to_string @@ External "mul")
let v_external_u_mul       : type_variable = TypeVar.of_input_var (to_string @@ External "u_mul")
let v_external_div         : type_variable = TypeVar.of_input_var (to_string @@ External "div")
let v_external_u_div       : type_variable = TypeVar.of_input_var (to_string @@ External "u_div")
let v_external_mod         : type_variable = TypeVar.of_input_var (to_string @@ External "mod")
let v_external_u_mod       : type_variable = TypeVar.of_input_var (to_string @@ External "u_mod")
let v_external_and         : type_variable = TypeVar.of_input_var (to_string @@ External "and")
let v_external_u_and       : type_variable = TypeVar.of_input_var (to_string @@ External "u_and")
