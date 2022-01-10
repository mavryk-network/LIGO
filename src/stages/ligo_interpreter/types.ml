include Ast_aggregated.Types

module Tezos_protocol = Tezos_protocol_011_PtHangz2
module Tezos_raw_protocol = Tezos_raw_protocol_011_PtHangz2

module Tez = Proto_alpha_utils.Memory_proto_alpha.Protocol.Alpha_context.Tez
module Timestamp = Memory_proto_alpha.Protocol.Alpha_context.Script_timestamp

type mcode = unit Tezos_utils.Michelson.michelson
type mcontract = Tezos_protocol.Protocol.Alpha_context.Contract.t

type mutation = Location.t * Ast_aggregated.expression

type env_item =
  | Expression of { name: expression_variable ; item: value_expr ; no_mutation : bool }

and env = env_item list

and func_val = {
    rec_name : expression_variable option ;
    orig_lambda : Ast_aggregated.expression ;
    arg_binder : expression_variable ;
    body : Ast_aggregated.expression ;
    env : env ;
  }

and typed_michelson_code = { code_ty : mcode ; code : mcode; ast_ty : Ast_aggregated.type_expression }

and michelson_code =
  | Contract of mcode
  | Ty_code of typed_michelson_code

and contract =
  { address : mcontract;
    entrypoint: string option }

and constant_val =
  | C_unit
  | C_bool of bool
  | C_int of Z.t
  | C_nat of Z.t
  | C_timestamp of Z.t
  | C_string of string
  | C_bytes of bytes
  | C_mutez of Z.t
  | C_address of mcontract (*should be represented as michelson data ? not convenient *)
  | C_contract of contract
  | C_key_hash of Tezos_protocol.Protocol.Alpha_context.public_key_hash
  | C_key of Tezos_protocol.Protocol.Alpha_context.public_key
  | C_signature of Tezos_protocol.Protocol.Alpha_context.signature
  | C_bls12_381_g1 of Bls12_381.G1.t
  | C_bls12_381_g2 of Bls12_381.G2.t
  | C_bls12_381_fr of Bls12_381.Fr.t

and micheline_value = (unit, string) Tezos_micheline.Micheline.node *
                        (unit, string) Tezos_micheline.Micheline.node

and value_expr = { ast_type : Ast_aggregated.type_expression ;
                   eval_term : value }
and value =
  | V_Ct of constant_val
  | V_List of value list
  | V_Record of value label_map
  | V_Map of (value * value) list
  | V_Set of value list
  | V_Construct of (string * value)
  | V_Michelson of michelson_code
  | V_Ligo of (string * string)
  | V_Mutation of mutation
  | V_Failure of exception_type
  | V_Func_val of func_val

and fail_reason = Val of value | Reason of string

and calltrace = Location.t list

and exception_type =
  Object_lang_ex of { location: Location.t ; errors: Tezos_error_monad.TzCore.error list ; calltrace : calltrace }
| Meta_lang_ex of { location : Location.t ; reason : fail_reason ; calltrace : calltrace }

type bigmap_state = (value * value) list
type bigmap_data = {
      key_type : Tezos_raw_protocol.Script_repr.expr;
      value_type : Tezos_raw_protocol.Script_repr.expr;
      version : bigmap_state }
type bigmap = int * bigmap_data
type bigmaps = bigmap list
