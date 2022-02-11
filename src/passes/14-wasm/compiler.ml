(* use https://github.com/SanderSpies/ocaml/blob/manual_gc/asmcomp/wasm32/emit.mlp for inspiration *)

[@@@warning "-33-27"]
open Trace
open Errors 

module I = Mini_c.Types
module W = WasmObjectFile  
module A = W.Ast
module S = W.Source
module Z = Z
module Var = Stage_common.Var

let no_region = S.no_region

let rec expression ~raise : I.expression -> A.instr list = fun e ->
  print_endline "wasm compiler: expression -> ...";
  match e.content with 
  | E_literal (Literal_unit) -> failwith "not supported yet 1"
  | E_literal (Literal_int _z) -> failwith "not supported yet 2"
  | E_literal (Literal_nat _z) -> failwith "not supported yet 3"
  | E_literal (Literal_timestamp _z) -> failwith "not supported yet 4"
  | E_literal (Literal_mutez _z) -> failwith "not supported yet 5"
  | E_literal (Literal_string _s) -> failwith "not supported yet 6"
  | E_literal (Literal_bytes _b) -> failwith "not supported yet 7"
  | E_literal (Literal_address _b) -> failwith "not supported yet 8"
  | E_literal (Literal_signature _b) -> failwith "not supported yet 9"
  | E_literal (Literal_key _b) -> failwith "not supported yet 10"
  | E_literal (Literal_key_hash _b) -> failwith "not supported yet 11"
  | E_literal (Literal_chain_id _b) -> failwith "not supported yet 12"
  | E_literal (Literal_operation _b) -> failwith "not supported yet 13"
  | E_closure {binder; body} -> failwith "not supported yet 14"
  | E_constant _ -> failwith "not supported yet 15"
  | E_application _ -> failwith "not supported yet 16"
  | E_variable _ -> failwith "not supported yet 17"
  | E_iterator _ -> failwith "not supported yet 18"
  | E_fold     _ -> failwith "not supported yet 19"
  | E_fold_right _ -> failwith "not supported yet 20"
  | E_if_bool  _ -> failwith "not supported yet 21"
  | E_if_none  _ -> failwith "not supported yet 22"
  | E_if_cons  _ -> failwith "not supported yet 23"
  | E_if_left  _ -> failwith "not supported yet 24"
  | E_let_in ({content = E_closure e}, _inline, ((name, _type), e2)) -> 
    print_endline "---CLOSURE---";
    print_endline (Var.to_name_exn name);
    (*
      function information in data:
      - function name
      - no of arguments
    *)

    (* let _ = expression  ~raise e1 in *)
    let _ = expression  ~raise e2 in
    []
  | E_let_in (e1, _inline, ((name, _type), e2)) -> 
    print_endline "------";
    print_endline (Var.to_name_exn name);
    

    (* let _ = expression  ~raise e1 in *)
    let _ = expression  ~raise e2 in
    []
    (* failwith "not supported yet 25" *)
  | E_tuple _ -> failwith "not supported yet 26"
  | E_let_tuple _ -> failwith "not supported yet 27"
  (* E_proj (record, index, field_count): we use the field_count to
     know whether the index is the last field or not, since Michelson
     treats the last element of a comb differently than the rest. We
     could alternatively put `unit` at the end of all our combs, but
     that would break compatibility and is not a standard Michelson
     convention... *)
  | E_proj _ -> failwith "not supported yet 28"
  (* E_update (record, index, update, field_count): field_count as for E_proj *)
  | E_update _ -> failwith "not supported yet 29"  
  | E_raw_michelson _ -> raise.raise @@ michelson_insertion Location.dummy
  | _ -> []
  (* | E_closure of anon_function
  | E_constant of constant
  | E_application of (expression * expression)
  | E_variable of var_name
  | E_iterator of constant' * ((var_name * type_expression) * expression) * expression
  | E_fold     of (((var_name * typex _expression) * expression) * expression * expression)
  | E_fold_right of (((var_name * type_expression) * expression) * (expression * type_expression) * expression)
  | E_if_bool  of (expression * expression * expression)
  | E_if_none  of expression * expression * ((var_name * type_expression) * expression)
  | E_if_cons  of expression * expression * (((var_name * type_expression) * (var_name * type_expression)) * expression)
  | E_if_left  of expression * ((var_name * type_expression) * expression) * ((var_name * type_expression) * expression)
  | E_let_in   of expression * inline * ((var_name * type_expression) * expression)
  | E_tuple of expression list
  | E_let_tuple of expression * (((var_name * type_expression) list) * expression)
  (* E_proj (record, index, field_count): we use the field_count to
     know whether the index is the last field or not, since Michelson
     treats the last element of a comb differently than the rest. We
     could alternatively put `unit` at the end of all our combs, but
     that would break compatibility and is not a standard Michelson
     convention... *)
  | E_proj of expression * int * int
  (* E_update (record, index, update, field_count): field_count as for E_proj *)
  | E_update of expression * int * expression * int
  | E_raw_michelson _ -> failwith "not supported" *)

(* let func_type: I.expression -> W.AST.type_ = fun e ->
  let type_expression = e.type_expression in
  let type_content = type_expression.type_content in  
  let _location = type_expression.location in
  match type_content with  *)
  

  (* | T_tuple of type_expression annotated list
  | T_or of (type_expression annotated * type_expression annotated)
  | T_function of (type_expression * type_expression)
  | T_base of type_base
  | T_map of (type_expression * type_expression)
  | T_big_map of (type_expression * type_expression)
  | T_list of type_expression
  | T_set of type_expression
  | T_contract of type_expression
  | T_ticket of type_expression
  | T_sapling_state of Z.t
  | T_sapling_transaction of Z.t
  | T_option of type_expression *)

let global_offset = ref 0l

let convert_to_memory: string -> Z.t -> A.data_part A.segment list * A.sym_info list = fun name z -> 
  let no_of_bits = Z.of_int (Z.log2up z) in        (* the no of bits that's required *)
  let size =  Z.cdiv no_of_bits (Z.of_int 32) in (* assuming 32 bit*)
  let _mp_alloc = size in
  let _mp_size = if Z.lt z Z.zero then 
    Z.sub Z.zero size
  else
    size
  in
  (* 
    figure out how to get the limbs...  
  *)
  (* let limbs = Z.c_shift_right... *)

  let data = A.[
  S.{
    it = {
      index = {it = 0l; at = no_region};
      offset = {
        it = [
          { it = Const {it = I32 0l; at = no_region}; at = no_region}
        ]; at = no_region};
      init = {
        name = name;
        detail = [
          Int32 (Z.to_int32 _mp_alloc);
          Int32 (Z.to_int32 _mp_size);
          Symbol (name ^ "__ligo_internal_limbs")
        ]
      }
    };
    at = no_region
  };
  { 
    it = {
      index = {it = 0l; at = no_region};
      offset = {
        it = [
          { it = Const {
              it = I32 Int32.(!global_offset + 3l); 
              at = no_region
            }; 
            at = no_region
          }
        ]; 
        at = no_region
      };
      init = {
        name = name ^ "__ligo_internal_limbs";
        detail = [
          String (Z.to_bits z)
        ]
      }
    };
    at = no_region;
  }
  
  ]
  in 
  let symbols = [
    S.{
      it = A.{
        name;
        details = Data {
          index = {it = 0l; at = no_region};
          relocation_offset =  {it = 0l; at = no_region};
          size = { it = 3l; at = no_region};
          offset = { it = !global_offset; at = no_region}
        }
      };
      at = no_region
    };
    {
      it = {
        name = name ^ "__ligo_internal_limbs";
        details = Data {
          index = {it = 0l; at = no_region};
          relocation_offset = {it = 0l; at = no_region};
          size = {it = Z.to_int32 _mp_size; at = no_region};
          offset = {it = Int32.(!global_offset + 3l); at = no_region}
        }
      };
      at = no_region
    }
  ]
  in 
  global_offset := Int32.(!global_offset + 4l);
  data, symbols

let rec toplevel_bindings ~raise : I.expression -> W.Ast.module_' -> W.Ast.module_' = fun e w ->
  match e.content with
    E_let_in ({content = E_closure e; _}, _inline, ((name, _type), e2)) -> 
      (* 
        - use __stack_pointer I guess...
          - gmpz_init:
            - allocate pointer on stack
            - call mpz_init with allocated pointer
            - how do we set the value?
            - can't we do this more directly?
    

        - toplevel main function
          - set toplevel __gmpz_init_set stuff

        - allocate memory
        - we give __gmpz_init a memory location (pointer)

        - stack_pointer: not important for now.
        - Every argument is a pointer.
        - the pointer should point to a value that libgmp understands.
        - use gmp integer functions for now, no need to overcomplicate things with lower-level functions
        - create a test program...


        Memory:
        - list of values to which bindings can point
        - 


      *)


    print_endline "---CLOSURE---";
    print_endline (Var.to_name_exn name);
    toplevel_bindings  ~raise e2 w
  | E_let_in ({content = E_literal (Literal_int z); _}, _inline, ((name, _type), e2)) -> 
    let name, hash = Var.internal_get_name_and_counter name in
    let name = match hash with 
      Some s -> name ^ "#" ^ (string_of_int s)
    | None -> name
    in 
    let data, symbols = convert_to_memory name z in
    toplevel_bindings ~raise e2 {w with data = w.data @ data; symbols = w.symbols @ symbols }
    (* | Literal_unit *)
    (* | Literal_nat of z
    | Literal_timestamp of z
    | Literal_mutez of z *)
    (* | Literal_string of ligo_string
    | Literal_bytes of bytes
    | Literal_address of string
    | Literal_signature of string
    | Literal_key of string
    | Literal_key_hash of string
    | Literal_chain_id of string
    | Literal_operation of bytes
    | Literal_bls12_381_g1 of bytes
    | Literal_bls12_381_g2 of bytes
    | Literal_bls12_381_fr of bytes *)
  | E_let_in (e1, _inline, ((name, _type), e2)) -> 
    (* 
      These should be pointers.
      - create a new number: should always be allocated somewhere.
    *)
    print_endline "------";
    print_endline (Var.to_name_exn name);
    let _ = toplevel_bindings  ~raise e2 w in
    w
  | E_variable _ ->
    print_endline "ehm";
    w
  | _ -> failwith "Instruction not supported at the toplevel."

let compile ~raise : I.expression -> W.Ast.module_ = fun e -> 
  let w = W.Ast.empty_module in

  (* 
    First block of memory will be the GMP values apparently. 

    TODO: 1. how much memory is required for storage? Put at __data_start (or something...).
          2. allocate the memory
          3. read from storage (fd_read "<contract_name>_storage" for now)

          4. set parameter by reading cli argument (how?)
          5. do contract logic

          6. compress storage?
             - easy way, will do for now but lame: allocate new memory block
          7. write changed storage (fd_write "<contract_name>_storage" for now)
  *)

  let w = {
    w with memories = [
      {
        it = {
          mtype = MemoryType {min = 100l; max = Some 100l}
        };
        at = no_region
      }
    ]
  }
  in
  S.{ 
    it = toplevel_bindings ~raise e w;
    at = no_region
  }