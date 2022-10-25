(*
  This file is used throughout the pipeline. Its idea is to add a unique place
  that you have to modify when you add a new operator/constant to the language.

  This file mirrors the LIGO pipeline, starting with Simplify, then Typer and
  ending with Stacking. Usually, when adding a new operator, you'll have to add
  a new constructor at all those places.
*)

module Ligo_string = Simple_utils.Ligo_string

module Tree_abstraction = struct

  let pseudo_module_to_string (c : Ligo_prim.Constant.constant') = match c with
    (* Operator module *)
    | C_NEG  -> "Operator.neg"
    | C_ADD  -> "Operator.add"
    | C_SUB  -> "Operator.sub"
    | C_SUB_MUTEZ -> "Operator.sub_mutez"
    | C_POLYMORPHIC_SUB -> "Operator.sub"
    | C_MUL  -> "Operator.times"
    | C_DIV  -> "Operator.div"
    | C_MOD  -> "Operator.modulus"
    | C_EQ   -> "Operator.eq"
    | C_NOT  -> "Operator.not"
    | C_AND  -> "Operator.and"
    | C_OR   -> "Operator.or"
    | C_GT   -> "Operator.gt"
    | C_GE   -> "Operator.ge"
    | C_LT   -> "Operator.lt"
    | C_LE   -> "Operator.le"
    | C_CONS -> "Operator.cons"
    | C_NEQ  -> "Operator.neq"

    (* Map module *)
    | C_MAP_ADD      -> "Map.add"
    | C_MAP_REMOVE   -> "Map.remove"

    (* Bitwise module *)
    | C_XOR -> "Bitwise.xor"
    | C_LSL -> "Bitwise.shift_left"
    | C_LSR -> "Bitwise.shift_right"

    | _ as c -> failwith @@ Format.asprintf "Constant not handled : %a" Ligo_prim.Constant.pp_constant' c


  let constant_to_string = function
      | Ligo_prim.Constant.Const x -> pseudo_module_to_string x
end

module Michelson = struct
  (*
    Most constants pass through the Spilling unchanged. So they need to be
    compiled down to Michelson. This is the last step.

    When compiling the constant, we need to provide its arity (through the type
    predicate, defined in `Helpers.Michelson`, and its michelson code.
    In the case of an n-ary constant, we assume that the stack has the form:
    `x1 :: x2 :: x3 ... :: xn :: _`.

    This step requires knowledge of Michelson. Knowledge of
    `Tezos_utils.Michelson` will help too, so that no Michelson has to actually
    be written by hand.
   *)
  type protocol_type = Environment.Protocols.t
  include Helpers.Michelson
  open Tezos_utils.Michelson
  open Ligo_prim.Constant

  let get_operators (protocol_version: protocol_type) c : predicate option =
    match c , protocol_version with
    | C_ADD                , _   -> Some ( simple_binary @@ prim "ADD")
    | C_SUB                , _   -> Some ( simple_binary @@ prim "SUB")
    | C_SUB_MUTEZ          , _   -> Some ( simple_binary @@ prim "SUB_MUTEZ")
    | C_MUL                , _   -> Some ( simple_binary @@ prim "MUL")
    | C_DIV                , _   -> Some ( simple_binary @@ seq [prim "EDIV" ; i_assert_some_msg (i_push_string "DIV by 0") ; i_car])
    | C_MOD                , _   -> Some ( simple_binary @@ seq [prim "EDIV" ; i_assert_some_msg (i_push_string "MOD by 0") ; i_cdr])
    | C_NEG                , _   -> Some ( simple_unary @@ prim "NEG")
    | C_OR                 , _   -> Some ( simple_binary @@ prim "OR")
    | C_AND                , _   -> Some ( simple_binary @@ prim "AND")
    | C_XOR                , _   -> Some ( simple_binary @@ prim "XOR")
    | C_LSL                , _   -> Some ( simple_binary @@ prim "LSL")
    | C_LSR                , _   -> Some ( simple_binary @@ prim "LSR")
    | C_NOT                , _   -> Some ( simple_unary @@ prim "NOT")
    | C_PAIR               , _   -> Some ( simple_binary @@ prim "PAIR")
    | C_CAR                , _   -> Some ( simple_unary @@ prim "CAR")
    | C_CDR                , _   -> Some ( simple_unary @@ prim "CDR")
    | C_TRUE               , _   -> Some ( simple_constant @@ i_push (prim "bool") (prim "True"))
    | C_FALSE              , _   -> Some ( simple_constant @@ i_push (prim "bool") (prim "False"))
    | C_EQ                 , _   -> Some ( simple_binary @@ seq [prim "COMPARE" ; prim "EQ"])
    | C_NEQ                , _   -> Some ( simple_binary @@ seq [prim "COMPARE" ; prim "NEQ"])
    | C_LT                 , _   -> Some ( simple_binary @@ seq [prim "COMPARE" ; prim "LT"])
    | C_LE                 , _   -> Some ( simple_binary @@ seq [prim "COMPARE" ; prim "LE"])
    | C_GT                 , _   -> Some ( simple_binary @@ seq [prim "COMPARE" ; prim "GT"])
    | C_GE                 , _   -> Some ( simple_binary @@ seq [prim "COMPARE" ; prim "GE"])
    | C_UPDATE             , _   -> Some ( simple_ternary @@ prim "UPDATE")
    | C_SOME               , _   -> Some ( simple_unary  @@ prim "SOME")
    | C_MAP_FIND           , _   -> Some ( simple_binary @@ seq [prim "GET" ; i_assert_some_msg (i_push_string "MAP FIND")])
    | C_MAP_FIND_OPT       , _   -> Some ( simple_binary @@ prim "GET")
    | C_MAP_ADD            , _   -> Some ( simple_ternary @@ seq [dip (i_some) ; prim "UPDATE"])
    | C_MAP_UPDATE         , _   -> Some ( simple_ternary @@ prim "UPDATE")
    | (C_MAP_GET_AND_UPDATE|C_BIG_MAP_GET_AND_UPDATE) , _ ->
      Some (simple_ternary @@ seq [prim "GET_AND_UPDATE"; prim "PAIR"])
    | C_CONS               , _   -> Some ( simple_binary @@ prim "CONS")
    | C_UNIT               , _   -> Some ( simple_constant @@ prim "UNIT")
    | C_SET_MEM            , _   -> Some ( simple_binary @@ prim "MEM")
    | C_SET_ADD            , _   -> Some ( simple_binary @@ seq [dip (i_push (prim "bool") (prim "True")) ; prim "UPDATE"])
    | C_SET_REMOVE         , _   -> Some ( simple_binary @@ seq [dip (i_push (prim "bool") (prim "False")) ; prim "UPDATE"])
    | C_SET_UPDATE         , _   -> Some ( simple_ternary @@ prim "UPDATE" )
    | C_CONCAT             , _   -> Some ( simple_binary @@ prim "CONCAT")
    | C_SLICE              , _   -> Some ( simple_ternary @@ seq [prim "SLICE" ; i_assert_some_msg (i_push_string "SLICE")])
    | C_NONE               , _   -> Some (trivial_special "NONE")
    | C_NIL                , _   -> Some (trivial_special "NIL")
    | C_LOOP_CONTINUE      , _   -> Some (trivial_special "LEFT")
    | C_LOOP_STOP          , _   -> Some (trivial_special "RIGHT")
    | C_LIST_EMPTY         , _   -> Some (trivial_special "NIL")
    | C_SET_EMPTY          , _   -> Some (trivial_special "EMPTY_SET")
    | C_MAP_EMPTY          , _   -> Some (trivial_special "EMPTY_MAP")
    | C_BIG_MAP_EMPTY      , _   -> Some (trivial_special "EMPTY_BIG_MAP")
    | C_LIST_SIZE          , _   -> Some (trivial_special "SIZE")
    | C_SET_SIZE           , _   -> Some (trivial_special "SIZE")
    | C_MAP_SIZE           , _   -> Some (trivial_special "SIZE")
    | C_SIZE               , _   -> Some (trivial_special "SIZE")
    | C_MAP_MEM            , _   -> Some (simple_binary @@ prim "MEM")
    | C_MAP_REMOVE         , _   -> Some (special (fun with_args -> seq [dip (with_args "NONE"); prim "UPDATE"]))
    | C_LEFT               , _   -> Some (trivial_special "LEFT")
    | C_RIGHT              , _   -> Some (trivial_special "RIGHT")
    | C_CREATE_CONTRACT , _ ->
      Some (special
              (fun with_args ->
                 seq [with_args "CREATE_CONTRACT";
                      i_pair]))
    | C_GLOBAL_CONSTANT , _ ->
      Some (special
        (fun with_args ->  with_args "PUSH")
        )
    | _ -> None

  (* true if the name names a pure constant -- i.e. if uses will be pure
     assuming arguments are pure *)
  let is_pure_constant : constant' -> bool =
    function
    | C_UNIT
    | C_CAR | C_CDR | C_PAIR
    | C_NIL | C_CONS
    | C_NEG | C_OR | C_AND | C_XOR | C_NOT
    | C_EQ  | C_NEQ | C_LT | C_LE | C_GT | C_GE
    | C_NONE | C_SOME
    | C_LEFT | C_RIGHT
    | C_TRUE | C_FALSE
    | C_UPDATE | C_MAP_FIND_OPT | C_MAP_ADD | C_MAP_UPDATE
    | C_ADDRESS
    | C_CONCAT
    | C_SET_MEM | C_SET_ADD | C_SET_REMOVE | C_SET_UPDATE
    | C_LOOP_CONTINUE | C_LOOP_STOP
    | C_SUB_MUTEZ
    | C_BYTES_UNPACK
    | C_SET_EMPTY | C_SET_LITERAL
    | C_LIST_EMPTY | C_LIST_LITERAL
    | C_MAP_EMPTY | C_MAP_LITERAL
    | C_MAP_GET | C_MAP_REMOVE
    | C_MAP_GET_AND_UPDATE | C_BIG_MAP_GET_AND_UPDATE
    | C_BIG_MAP_EMPTY
    | C_SAPLING_EMPTY_STATE
    | C_SAPLING_VERIFY_UPDATE
    | C_OPEN_CHEST
    | C_GLOBAL_CONSTANT (* pure because restricted to PUSH *)
    | C_EMIT_EVENT
      -> true
    (* unfortunately impure: *)
    | C_ADD | C_SUB |C_MUL|C_DIV|C_MOD | C_LSL | C_LSR
    | C_POLYMORPHIC_ADD | C_POLYMORPHIC_SUB
    (* impure: *)
    | C_UNOPT
    | C_UNOPT_WITH_ERROR
    | C_OPTION_MAP
    | C_MAP_FIND
    | C_CALL
    | C_ITER
    | C_LOOP_LEFT
    | C_FOLD
    | C_FOLD_LEFT
    | C_FOLD_RIGHT
    | C_SET_ITER
    | C_SET_FOLD
    | C_SET_FOLD_DESC
    | C_LIST_ITER
    | C_LIST_MAP
    | C_LIST_FOLD
    | C_LIST_FOLD_LEFT
    | C_LIST_FOLD_RIGHT
    | C_MAP_GET_FORCE
    | C_MAP_ITER
    | C_MAP_MAP
    | C_MAP_FOLD
    | C_SET_DELEGATE
    | C_CREATE_CONTRACT
    (* TODO? *)
    | C_MAP
    | C_BIG_MAP
    | C_BIG_MAP_LITERAL
    | C_CONTRACT
    | C_CONTRACT_WITH_ERROR
    | C_CONTRACT_OPT
    | C_CONTRACT_ENTRYPOINT
    | C_CONTRACT_ENTRYPOINT_OPT
    | C_SELF
    | C_SELF_ADDRESS
    | C_IMPLICIT_ACCOUNT
    | C_VIEW
    (* Test - ligo interpreter, should never end up here *)
    | C_TEST_SIZE
    | C_TEST_ORIGINATE
    | C_TEST_GET_STORAGE_OF_ADDRESS
    | C_TEST_GET_BALANCE
    | C_TEST_SET_SOURCE
    | C_TEST_SET_BAKER
    | C_TEST_EXTERNAL_CALL_TO_ADDRESS
    | C_TEST_EXTERNAL_CALL_TO_ADDRESS_EXN
    | C_TEST_GET_NTH_BS
    | C_TEST_PRINT
    | C_TEST_TO_STRING
    | C_TEST_UNESCAPE_STRING
    | C_TEST_STATE_RESET
    | C_TEST_BOOTSTRAP_CONTRACT
    | C_TEST_NTH_BOOTSTRAP_CONTRACT
    | C_TEST_LAST_ORIGINATIONS
    | C_TEST_MUTATE_CONTRACT
    | C_TEST_MUTATE_VALUE
    | C_TEST_SAVE_MUTATION
    | C_TEST_RUN
    | C_TEST_COMPILE_CONTRACT
    | C_TEST_DECOMPILE
    | C_TEST_TO_CONTRACT
    | C_TEST_TO_ENTRYPOINT
    | C_TEST_TO_TYPED_ADDRESS
    | C_TEST_RANDOM
    | C_TEST_GENERATOR_EVAL
    | C_TEST_NTH_BOOTSTRAP_TYPED_ADDRESS
    | C_TEST_COMPILE_CONTRACT_FROM_FILE
    | C_TEST_COMPILE_AST_CONTRACT
    | C_TEST_SET_BIG_MAP
    | C_TEST_CAST_ADDRESS
    | C_TEST_CREATE_CHEST
    | C_TEST_CREATE_CHEST_KEY
    | C_TEST_ADD_ACCOUNT
    | C_TEST_NEW_ACCOUNT
    | C_TEST_BAKER_ACCOUNT
    | C_TEST_REGISTER_DELEGATE
    | C_TEST_BAKE_UNTIL_N_CYCLE_END
    | C_TEST_GET_VOTING_POWER
    | C_TEST_GET_TOTAL_VOTING_POWER
    | C_TEST_REGISTER_CONSTANT
    | C_TEST_CONSTANT_TO_MICHELSON
    | C_TEST_REGISTER_FILE_CONSTANTS
    | C_TEST_PUSH_CONTEXT
    | C_TEST_POP_CONTEXT
    | C_TEST_DROP_CONTEXT
    | C_TEST_FAILWITH
    | C_TEST_READ_CONTRACT_FROM_FILE
    | C_TEST_SIGN
    | C_TEST_GET_ENTRYPOINT
    | C_TEST_INT64_OF_INT
    | C_TEST_INT64_TO_INT
    | C_TEST_LAST_EVENTS
    | C_TEST_TRY_WITH
    | C_TEST_ABS
    | C_TEST_INT
    | C_TEST_SLICE
      -> false

end
