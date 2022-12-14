open Simple_utils.Trace
open Ast_unified
open Pass_type
open Errors

(*
  notes:
    - in the future (), we could imagine those mapping to be defined in the stdlib
*)
let mapping_binop ~syntax : (Operators.op * Ligo_prim.Constant.constant') list =
  let open Syntax_types in
  match syntax with
  | CameLIGO ->
    [ PLUS, C_POLYMORPHIC_ADD
    ; MINUS, C_POLYMORPHIC_SUB
    ; STAR, C_MUL
    ; SLASH, C_DIV
    ; PRCENT, C_MOD
    ; DPIPE, C_OR
    ; DAMPERSAND, C_AND
    ; LT, C_LT
    ; GT, C_GT
    ; GE, C_GE
    ; LE, C_LE
    ; SEQ, C_EQ
    ; LTGT, C_NEQ
    ; WORD_LSL, C_LSL
    ; WORD_LSR, C_LSR
    ; WORD_LXOR, C_XOR
    ; WORD_LAND, C_AND
    ; DCOLON, C_CONS
    ]
  | JsLIGO ->
    [ PLUS, C_POLYMORPHIC_ADD
    ; MINUS, C_POLYMORPHIC_SUB
    ; STAR, C_MUL
    ; SLASH, C_DIV
    ; PRCENT, C_MOD
    ; DPIPE, C_OR
    ; DAMPERSAND, C_AND
    ; LT, C_LT
    ; GT, C_GT
    ; GE, C_GE
    ; LE, C_LE
    ; DEQ, C_EQ
    ; EQ_SLASH_EQ, C_NEQ
    ]
  | PascaLIGO ->
    [ SHARP, C_CONS
    ; CARET, C_CONCAT
    ; PLUS, C_ADD
    ; MINUS, C_POLYMORPHIC_SUB
    ; STAR, C_MUL
    ; SLASH, C_DIV
    ; WORD_MOD, C_MOD
    ; WORD_OR, C_OR
    ; WORD_AND, C_AND
    ; LT, C_LT
    ; GT, C_GT
    ; GE, C_GE
    ; LE, C_LE
    ; SEQ, C_EQ
    ; EQ_SLASH_EQ, C_NEQ
    ]
  | ReasonLIGO -> failwith "deprecated reason"


let mapping_unop ~syntax : (Operators.op * Ligo_prim.Constant.constant') list =
  let open Syntax_types in
  match syntax with
  | CameLIGO -> [ MINUS, C_NEG; WORD_NOT, C_NOT ]
  | JsLIGO -> [ MINUS, C_NEG; EX_MARK, C_NOT ]
  | PascaLIGO -> [ MINUS, C_NEG; WORD_NOT, C_NOT ]
  | _ -> [] (* :) *)


let get_constant_of_operator mapping ~syntax k : Ligo_prim.Constant.constant' option =
  let m = mapping ~syntax in
  List.Assoc.find m ~equal:Operators.equal_op k


let get_operator_of_constant mapping ~syntax k : Operators.op option =
  let m = List.map ~f:(fun (x, y) -> y, x) (mapping ~syntax) in
  List.Assoc.find m ~equal:Ligo_prim.Constant.equal_constant' k


let compile ~syntax =
  let pass_expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    (* we are unfortunately throwing operator locations away *)
    match Location.unwrap e with
    | E_Binary_op { operator; left; right } ->
      (match
         get_constant_of_operator mapping_binop ~syntax (Location.unwrap operator)
       with
      | Some cons_name -> e_constant ~loc { cons_name; arguments = [ left; right ] }
      | None -> make_e ~loc e.wrap_content)
    | E_Unary_op { operator; arg } ->
      (match
         get_constant_of_operator mapping_binop ~syntax (Location.unwrap operator)
       with
      | Some cons_name -> e_constant ~loc { cons_name; arguments = [ arg ] }
      | None -> make_e ~loc e.wrap_content)
    | x -> make_e ~loc x
  in
  `Cata { idle_cata_pass with expr = pass_expr }


let reduction ~raise =
  let fail () = raise.error (wrong_reduction __MODULE__) in
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_Binary_op _ | E_Unary_op _; _ } -> fail ()
      | _ -> ())
  }


let decompile ~syntax =
  let pass_expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_constant { cons_name; arguments = [ left; right ] } ->
      (match get_operator_of_constant mapping_binop ~syntax cons_name with
      | Some c -> e_binary_op ~loc { operator = Location.wrap ~loc c; left; right }
      | None -> make_e ~loc e.wrap_content)
    | E_constant { cons_name; arguments = [ arg ] } ->
      (match get_operator_of_constant mapping_unop ~syntax cons_name with
      | Some c -> e_unary_op ~loc { operator = Location.wrap ~loc c; arg }
      | None -> make_e ~loc e.wrap_content)
    | x -> make_e ~loc x
  in
  `Cata { idle_cata_pass with expr = pass_expr }


let pass ~raise ~syntax =
  cata_morph
    ~name:__MODULE__
    ~compile:(compile ~syntax)
    ~decompile:(decompile ~syntax)
    ~reduction_check:(reduction ~raise)


open Unit_test_helpers

let%expect_test "compile" =
  {|
  ((P_Declaration
    (D_Const (
      (pattern (P_var x))
      (let_rhs
        (E_Binary_op ((operator SLASH) (left (E_variable x)) (right (E_variable y)))))))))
  |}
  |-> pass ~raise ~syntax:(PascaLIGO);
  [%expect
    {|
    ((P_Declaration
       (D_Const
         ((pattern (P_var x))
           (let_rhs
             (E_constant
               ((cons_name C_DIV) (arguments ((E_variable x) (E_variable y))))))))))
    |}]

let%expect_test "decompile" =
  {|
  ((P_Declaration
  (D_Const
    ((pattern (P_var x))
      (let_rhs
        (E_constant
          ((cons_name C_DIV) (arguments ((E_variable x) (E_variable y)))))))))) 
  |}
  <-| pass ~raise ~syntax:(PascaLIGO);
  [%expect
    {|
    ((P_Declaration
       (D_Const
         ((pattern (P_var x))
           (let_rhs
             (E_Binary_op
               ((operator SLASH) (left (E_variable x)) (right (E_variable y)))))))))
    |}]