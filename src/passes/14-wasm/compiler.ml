(* use https://github.com/SanderSpies/ocaml/blob/manual_gc/asmcomp/wasm32/emit.mlp for inspiration *)

[@@@warning "-33-27-26"]
open Trace
open Errors 

module I = Mini_c.Types
module W = WasmObjectFile  
module A = W.Ast
module T = W.Types
module S = W.Source
module Z = Z
module Var = Stage_common.Var
module Location = Simple_utils.Location

(**
 * Converts LIGO's location.t to WasmObjectFile's Source.region.
 *)
let location_to_region (l: Location.t) : S.region = 
  match l with  
    File l ->
      {
        left = {
          file   = l#file;
          line   = l#start#line;
          column = l#start#column `Byte;
        };
        right = {
          file   = l#file;
          line   = l#stop#line;
          column = l#stop#column `Byte;
        }
      }
  | Virtual _ -> S.no_region

(* Convert a variable to a string which we can use for symbols *)
let var_to_string name =  
  let name, hash = Var.internal_get_name_and_counter name in
  match hash with 
    Some s -> name ^ "#" ^ (string_of_int s)
  | None -> name

(* The data offset. This indicates where a block of data should be placed in the linear memory. *)
let global_offset = ref Default_env.offset

(**
 * Convert a Zarith value to WASM's linear memory for use with GMP.
 *
 * See also GMP internals (TODO:add link here)
 **)
let convert_to_memory: string -> S.region -> Z.t -> A.data_part A.segment list * A.sym_info list = fun name at z -> 
  let no_of_bits = Z.of_int (Z.log2up z) in      (* the no of bits that's required *)
  let size =  Z.cdiv no_of_bits (Z.of_int 32) in (* assuming 32 bit*)
  let _mp_alloc = size in
  let _mp_size = if Z.lt z Z.zero then 
    Z.sub Z.zero size
  else
    size
  in
  let data = A.[
    S.{
      it = {
        index = {it = 0l; at};
        offset = {
          it = [
            { it = Const {it = I32 !global_offset; at}; at}
          ]; at};
        init = {
          name = name;
          detail = [
            Int32 (Z.to_int32 _mp_alloc);
            Int32 (Z.to_int32 _mp_size);
            Symbol (name ^ "__ligo_internal_limbs")
          ]
        }
      };
      at
    };
    { 
      it = {
        index = {it = 0l; at};
        offset = {
          it = [
            { it = Const {
                it = I32 Int32.(!global_offset + 3l); 
                at
              }; 
              at
            }
          ]; 
          at
        };
        init = {
          name = name ^ "__ligo_internal_limbs";
          detail = [
            String (Z.to_bits z)
          ]
        }
      };
      at;
    }
  ]
  in 
  let symbols = [
    S.{
      it = A.{
        name;
        details = Data {
          index = {it = 0l; at};
          relocation_offset =  {it = 0l; at};
          size = { it = 3l; at};
          offset = { it = !global_offset; at}
        }
      };
      at
    };
    {
      it = {
        name = name ^ "__ligo_internal_limbs";
        details = Data {
          index = {it = 0l; at};
          relocation_offset = {it = 0l; at};
          size = {it = Z.to_int32 _mp_size; at};
          offset = {it = Int32.(!global_offset + 3l); at}
        }
      };
      at
    }
  ]
  in 
  global_offset := Int32.(!global_offset + 4l); (* TODO: get proper size for limbs... *)
  data, symbols

type locals = (string * T.value_type) list

let name s =
  try W.Utf8.decode s with W.Utf8.Utf8 ->
    failwith "invalid UTF-8 encoding"

let rec expression ~raise : A.module_' -> locals -> I.expression -> A.module_' * locals * A.instr list = fun w l e ->
  let at = location_to_region e.location in
  match e.content with 
  | E_literal (Literal_unit) ->
    w, l, [{it = Nop; at}]
  | E_literal (Literal_int z) -> 
    let unique_name = Var.fresh () in
    let name = var_to_string unique_name in
    let data, symbols = convert_to_memory name at z in
    let w = {w with data = w.data @ data; symbols = w.symbols @ symbols } in
    w, l, [{it = A.DataSymbol name; at}]
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
  | E_constant {cons_name = C_LIST_EMPTY; arguments = []} -> 
    
    w, l, [{it = DataSymbol "C_LIST_EMPTY"; at}]
  | E_constant {cons_name = C_PAIR; arguments = [e1; e2]} -> 
    let malloc_local = var_to_string (Var.fresh ~name:"malloc" ()) in
    let open A in
    let open S in
    let allocation = [
      { it = Const { it = I32 8l; at}; at };
      { it = Call "malloc"; at };
      { it = LocalSet malloc_local; at }
    ] in
    let w, l, e1 = expression ~raise w l e1 in
    let e1 = [
      { it = LocalGet malloc_local; at };
    ] 
    @ 
    e1 
    @ 
    [{ it = Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at }]
    in
    let w, l, e2 = expression ~raise w l e2 in
    let e2 = [
      { it = LocalGet malloc_local; at };
      { it = Const { it = I32 4l; at}; at };
      { it = Binary (I32 Add); at }
    ]
    @ 
    e2 
    @ 
    [{ it = Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at }]
    in
    w, (malloc_local, I32Type) :: l, allocation @ e1 @ e2
    
  | E_constant {cons_name = C_ADD; arguments = [e1; e2]} -> 
    let w, l, e1 = expression ~raise w l e1 in
    let w, l, e2 = expression ~raise w l e2 in
    w, l, e1 @ e2 @ [{it = A.Call "__gmpz_add"; at}]
  | E_constant {cons_name; arguments} -> failwith "not supported yet 15"
  | E_application _ -> 
    let rec aux result expr = 
      (match expr.I.content with 
        E_application (func, e) -> 
          aux (e :: result) func
      | E_variable v -> 
        let name = var_to_string v in
        name, List.rev result
      | _ -> failwith "Not supported yet x1")
    in
    let name, args = aux [] e in
    let w, l, args = List.fold ~f:(fun (w, l, a) f -> 
      let w, l, c = expression ~raise w l f in 
      w, l,  a @ c
    ) ~init:(w, l, []) args in
    let args = List.rev args in
    w, l, args @ [S.{it = A.Call name; at}]
  | E_variable name ->
    let name = var_to_string name in
    (match List.find ~f:(fun (n, _) -> String.equal n name) l with 
     Some _ -> w, l, [{it = LocalGet name; at}]
    | None ->  w, l, [{it = DataSymbol name; at}])
  | E_iterator _ -> failwith "not supported yet 18"
  | E_fold     _ -> failwith "not supported yet 19"
  | E_fold_right _ -> failwith "not supported yet 20"
  | E_if_bool  _ -> failwith "not supported yet 21"
  | E_if_none  _ -> failwith "not supported yet 22"
  | E_if_cons  _ -> failwith "not supported yet 23"
  | E_if_left  _ -> failwith "not supported yet 24"
  | E_let_in ({content = E_closure {binder; body}}, _inline, ((name, _type), e2)) -> failwith "should not happen..."
  | E_let_in (e1, _inline, ((name, typex), e2)) -> 
    let name = var_to_string name in
    let w, l, e1 = expression ~raise w l e1 in
    let l = (name, T.I32Type) :: l in
    let w, l, e2 = expression ~raise w l e2 in
    w, l, e1 @ [S.{it = A.LocalSet name; at}] @ e2
  | E_tuple _ -> failwith "not supported yet 26"
  | E_let_tuple (tuple, (values, rhs)) -> 
    let w, l, tuple = expression ~raise w l tuple in
    let tuple_name = var_to_string (Var.fresh ~name:"let_tuple" ()) in
    let t = tuple @ [
      S.{it = A.LocalSet tuple_name; at}
    ]
    in
    let l = (tuple_name, T.I32Type) :: l in
    let l, e = List.foldi ~f:(fun i (l, all) (name, _) -> 
      let name = var_to_string name in
      (name, T.I32Type) :: l, all 
      @
      [
        S.{ it = A.LocalGet tuple_name; at};
        { it = Const {it = I32 (Int32.(4l * (Int32.of_int_exn i))); at}; at };
        { it = Binary (I32 Add); at };
        { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
        { it = LocalSet name; at }
      ]
    ) ~init:(l, []) values
    in

    let w, l, e2 = expression ~raise w l rhs in 
    w, l, t @ e @ e2

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
  | _ -> failwith "not supported x"
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

let func I.{binder; body} =
  let rec aux arguments body =
    match body.I.content with 
      E_closure {binder; body} -> 
        aux (binder :: arguments) body
    | _ -> List.rev arguments, body
  in
  aux [binder] body

let rec toplevel_bindings ~raise : I.expression -> W.Ast.module_' -> W.Ast.module_' = fun e w ->
  let at = location_to_region e.location in
  match e.content with
    E_let_in ({content = E_closure c; _}, _inline, ((name, type_), e2)) -> 
      let name = var_to_string name in
      let arguments, body = func c in
      let locals = List.map ~f:(fun a -> (var_to_string a, T.I32Type)) arguments in
      let w, locals, body = expression ~raise w locals body in
      let type_arg = List.map ~f:(fun _ -> T.I32Type) arguments in
      let return_type = (match type_.type_content with 
         I.T_function (_, {type_content = I.T_base TB_unit; _}) -> []
       |  _ -> [T.I32Type]
      ) in
      let w = {w with 
        symbols = w.symbols @ [{
          it = {
            name = name;
            details = Function
          };
          at
        }];
        types = w.types @ [{
          it = {
            tname = name ^ "_type";
            tdetails = FuncType (type_arg, return_type)
          };
          at
        }];
        funcs = w.funcs @ [{
          it = {
            name = name;
            ftype = name ^ "_type";
            locals;
            body
          };
          at
        }]
      }
      in
      (* type is always int32 (aka pointers) *)

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
    toplevel_bindings ~raise e2 w
  | E_let_in ({content = E_literal (Literal_int z); _}, _inline, ((name, _type), e2)) -> 
    (* we convert these to in memory values *)
    let name = var_to_string name in
    let data, symbols = convert_to_memory name at z in
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
  | E_variable _ ->
    (* The variable at the end, nothing should happen here. *)
    w
  | _ -> failwith "Instruction not supported at the toplevel."

let compile ~raise : I.expression -> W.Ast.module_ = fun e -> 
  let w = Default_env.env in
  let at = location_to_region e.location in
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


    Memory representation of data types:
    - int: pointer
    
    Later:
    - bool:
    - string
    - variant
    - record
    - list: 
    - tuple:
      value, next_item or nothing
    - map:
    - set:
    - big map:
  *)

  (* let w = {
    w.it with memories = [
      {
        it = {
          mtype = MemoryType {min = 100l; max = Some 100l}
        };
        at
      }
    ]
  }
  in *)
  S.{ 
    it = toplevel_bindings ~raise e w.it;
    at = w.at
  }