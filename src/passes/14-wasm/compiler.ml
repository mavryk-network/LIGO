(* use https://github.com/SanderSpies/ocaml/blob/manual_gc/asmcomp/wasm32/emit.mlp for inspiration *)

[@@@warning "-33-27-26-39"]
open Trace
open Errors 

module I = Mini_c.Types
module W = WasmObjectFile  
module A = W.Ast
module T = W.Types
module S = W.Source
module Z = Z
module ValueVar = Stage_common.Types.ValueVar
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

(** 
 * Convert a variable to a string which we can use for symbols 
 *)
let var_to_string name =  
  let name, hash = ValueVar.internal_get_name_and_counter name in
  name ^ "#" ^ (string_of_int hash)
  (* | None -> name *)

(* The data offset. This indicates where a block of data should be placed in the linear memory. *)
let global_offset = ref 0l

(**
 * Convert a Zarith value to WASM's linear memory for use with GMP.
 *
 * See also [GMP internals](https://gmplib.org/manual/Internals)
 **)
let convert_to_memory: string -> S.region -> Z.t -> A.data_part A.segment list * A.sym_info list = fun name at z -> 
  let no_of_bits = Z.of_int (Z.log2up z) in      (* the no of bits that's required *)
  let no_of_bits = if Z.equal no_of_bits Z.zero then Z.of_int 1 else no_of_bits in (* TODO: this looks fishy - see if we can improve on this *)
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
                it = I32 Int32.(!global_offset + 12l); 
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
          size = { it = 12l; at};
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
          size = {it = Int32.(Z.to_int32 _mp_size * 4l); at};
          offset = {it = Int32.(!global_offset + 3l); at}
        }
      };
      at
    }
  ]
  in 
  global_offset := Int32.(!global_offset + Z.to_int32 _mp_size * 4l + 12l); (* TODO: get proper size for limbs... *)
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
    let unique_name = ValueVar.fresh () in
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
  | E_closure {binder; body} -> 
    w, l, [
      S.{ it = A.Nop; at}
    ]
  | E_constant {cons_name = C_LIST_EMPTY; arguments = []} -> w, l, [
    { it = DataSymbol "C_LIST_EMPTY"; at };
    { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at }
  ]
  | E_constant {cons_name = C_PAIR; arguments = [e1; e2]} -> 
    let malloc_local = var_to_string (ValueVar.fresh ~name:"malloc" ()) in
    let open A in
    let open S in

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
    [{ it = Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
      { it = LocalGet malloc_local; at };
    ]
    in
    let allocation = [
      { it = Const { it = I32 8l; at}; at };
      { it = Call "malloc"; at };
      { it = LocalTee malloc_local; at };
      { it = Const { it = I32 0l; at}; at };
      { it = Compare (I32 Eq); at };
      { it = If (
        ValBlockType (Some I32Type), 
        [
          { it = Const {it = I32 0l; at}; at};
          { it = Const {it = I32 4l; at}; at};
          { it = Call "__wasi_proc_exit"; at};
        ],
        e1 @ e2);
        at
      }
    ] in
    w, l @ [(malloc_local, I32Type)], allocation
    
  | E_constant {cons_name = C_ADD; arguments = [e1; e2]} -> 
    let new_value = var_to_string (ValueVar.fresh ~name:"C_ADD" ()) in
    let mpz_init = [
      S.{ it = A.Const { it = I32 8l; at}; at };
      { it = A.Call "malloc"; at };
      (* TODO: if malloc == 0 then it's an error! *)
      { it = A.LocalTee new_value; at };
      { it = A.Call "__gmpz_init"; at };   
      { it = A.LocalGet new_value; at };   
    ]
    in
    let l =  l @ [(new_value, T.I32Type)] in
    let w, l, e1 = expression ~raise w l e1 in
    let w, l, e2 = expression ~raise w l e2 in
    w, l, mpz_init @ e1 @ e2 @ [{it = A.Call "__gmpz_add"; at}; {it = A.LocalGet new_value; at}]
  | E_constant {cons_name = C_CAR; arguments = [cons]} -> 
    let w, l, cons = expression ~raise w l cons in
    w, l, cons @ [{ it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at }]
  | E_constant {cons_name = C_CDR; arguments = [cons]} -> 
    let w, l, cons = expression ~raise w l cons in
    w, l, cons @ [
       { it = A.Const { it = I32 4l; at}; at };
       { it = Binary (I32 Add); at };
       { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
    ]
    
  | E_constant {cons_name = C_CONS; arguments = [l1; l2]} ->
    let cons = var_to_string (ValueVar.fresh ~name:"C_CONS" ()) in
    let w, l, l1 = expression ~raise w l l1 in
    let w, l, l2 = expression ~raise w l l2 in
    w, (l @ [(cons, I32Type)]),
    [
    S.{ it = A.Const { it = I32 8l; at}; at };
      { it = A.Call "malloc"; at }; (* check if not 0 *)
      { it = LocalTee cons; at};
    ]
    @ 
    l1
    @
    [
      S.{ it = A.Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
      { it = LocalGet cons; at};
      { it = Const {it = I32 4l; at}; at };
      { it = Binary (I32 Add); at };
    ]
    @ 
    l2    
    @
    [
      { it = A.Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
      { it = LocalGet cons; at}
    ]
  | E_constant {cons_name = C_LIST_LITERAL; arguments = [l1]} -> failwith "not supported yet 15a2"
  | E_constant {cons_name; arguments} -> failwith "not supported yet 15b"
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
    (* let args = List.rev args in *)
    w, l, args @ [S.{it = A.Call name; at}]
  | E_variable name ->
    let name = var_to_string name in
    print_endline ("get variable:" ^ name);
    (match List.find ~f:(fun (n, _) -> String.equal n name) l with 
     Some _ -> w, l, [{it = LocalGet name; at}; ]
    | None ->  w, l, [{it = DataSymbol name; at}; ])
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
    let l = l @ [(name, T.I32Type)] in
    let w, l, e2 = expression ~raise w l e2 in
    w, l, e1 @ [S.{it = A.LocalSet name; at}] @ e2
  | E_tuple _ -> failwith "not supported yet 26"
  | E_let_tuple (tuple, (values, rhs)) -> 
    let w, l, tuple = expression ~raise w l tuple in
    let tuple_name = var_to_string (ValueVar.fresh ~name:"let_tuple" ()) in
    let t = tuple @ [
      S.{it = A.LocalSet tuple_name; at}
    ]
    in
    let l = l @ [(tuple_name, T.I32Type)] in
    let l, e = List.foldi ~f:(fun i (l, all) (name, _) -> 
      let name = var_to_string name in
      l @ [(name, T.I32Type)], all 
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
            locals ;
            body
          };
          at
        }]
      }
      in      
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
  | E_variable entrypoint ->
    let actual_name = var_to_string entrypoint in
    let name = "entrypoint" in
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
            tdetails = FuncType ([I32Type; I32Type], [I32Type])
          };
          at
        }];
        (*
/**
 * A region of memory for scatter/gather writes.
 */
typedef struct __wasi_ciovec_t {
    /**
     * The address of the buffer to be written.
     */
    const uint8_t * buf;

    /**
     * The length of the buffer to be written.
     */
    __wasi_size_t buf_len;

} __wasi_ciovec_t;        
        *)
        
        funcs = w.funcs @ [{
          it = {
            name = name;
            ftype = name ^ "_type";
            locals = [("entrypoint_tuple", I32Type); ("return_tuple", I32Type)];
            body = [
              {
                it = LocalGet "return_tuple";
                at
              };
              {
                it = LocalGet "entrypoint_tuple";
                at
              };
              {
                it = Call actual_name; 
                at
              };
              {
                it = Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at
              };
              {
                it = Const { it = I32 0l; at };
                at
              }
            ]
          };
          at
        }]
    }
    in
    w
    (* in *)
    (* { w with types = w.types @ type_; funcs = w.funcs @ func; symbols = w.symbols @ symbol } *)
  | _ -> failwith "Instruction not supported at the toplevel."


(* 
  - place everything at once in memory
  - however the pointers will be incorrect, the following code corrects this
  - possible improvement: - in storage, store the offset and use that to store it.
                          - if offset matches, it can be just a copy paste in memory...
*)
let rec generate_storage_loader: I.type_expression -> string -> (A.instr list * locals) = fun t offset ->
  let at = S.no_region in
  match t.type_content with 
    T_list t -> 
      let addr = var_to_string (ValueVar.fresh ~name:"addr" ()) in
      let next_item = var_to_string (ValueVar.fresh ~name:"next_item" ()) in
      let (list_item_loader, locals) = generate_storage_loader t offset in
      [
        S.{ it = A.LocalSet addr; at};
        {
          it = Loop (
            ValBlockType None, 
            [              
               (* change the first part of the list item *)
               S.{ it = A.LocalGet addr; at};
               { it = LocalGet addr; at};
               { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
               { it = LocalGet offset; at };
               { it = Binary (I32 Add); at };
               { it = Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at };

               (* check if we should change the second part of the list item *)
               { it = LocalGet addr; at};
               { it = Const {it = I32 4l; at}; at };
                { it = Binary (I32 Add); at };
                { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
                { it = DataSymbol "C_LIST_EMPTY"; at};
                { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
              
                { it = Compare (I32 Ne); at };
                { it = If (
                  ValBlockType None, 
                  [
                    (* change the second part of the list item  *)
                    { it = LocalGet addr; at};
                    { it = Const {it = I32 4l; at}; at };
                    { it = Binary (I32 Add); at };
                    
                    { it = LocalGet addr; at};
                    { it = Const {it = I32 4l; at}; at };
                    { it = Binary (I32 Add); at };

                    { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
                    { it = LocalGet offset; at };
                    { it = Binary (I32 Add); at };
                    { it = Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
                  ],
                  [
      
                  ]
                ); at 
              };
              

               (* change the list item value *)
              { it = LocalGet addr; at};
              { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
            ]
            @
            list_item_loader
            @
            [
              (* set the next list item *)
             
             
              { it = LocalGet addr; at};
              { it = Const {it = I32 4l; at}; at };
              { it = Binary (I32 Add); at };
              { it = LocalTee next_item; at};
              { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
              { it = DataSymbol "C_LIST_EMPTY"; at};
              { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
              
              (* { it = Const {it = I32 0l; at}; at }; *)
              { it = Compare (I32 Ne); at };
              { it = If (
                ValBlockType None, 
                [
                  { it = LocalGet next_item; at};
                  { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
                  { it = LocalSet addr; at};
                  { it = Br {it = 1l; at}; at}
                ],
                [
    
                ]
              ); at 
              }
            ]
          );
          at
        };
      ], ((addr, I32Type) :: (next_item, I32Type) :: locals)
  | T_tuple [(_, item1); (_, item2)] ->
    (* ignore annotations for now *)
    let addr = var_to_string (ValueVar.fresh ~name:"addr" ()) in
    let a1, locals1 =  generate_storage_loader item1 offset in
    let a2, locals2 =  generate_storage_loader item2 offset in
    ([
      S.{ it = A.LocalTee addr; at };
      
      { it = LocalGet addr; at };
      { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
      { it = LocalGet offset; at };
      { it = Binary (I32 Add); at };
      { it = Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at };  

      { it = LocalGet addr; at };
      { it = Const {it = I32 4l; at}; at };
      { it = Binary (I32 Add); at };
      
      { it = LocalGet addr; at };
      { it = Const {it = I32 4l; at}; at };
      { it = Binary (I32 Add); at };
      { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
      { it = LocalGet offset; at };
      { it = Binary (I32 Add); at };

      { it = Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at };  
      
      { it = A.LocalGet addr; at};
      { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
    ]
    @ 
    a1
    @
    [
      S.{ it = A.LocalGet addr; at};
      { it = Const {it = I32 4l; at}; at };
      { it = Binary (I32 Add); at };
      { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
    ]
    @ 
    a2,
    ((addr, T.I32Type) :: locals1) @ locals2)
  | T_base TB_int -> 
    let addr     = var_to_string (ValueVar.fresh ~name:"addr" ()) in
    [
      S.{ it = A.LocalTee addr; at };
      { it = Const {it = I32 8l; at}; at};
      { it = Binary (I32 Add); at };
      { it = LocalGet addr; at };
      { it = Const {it = I32 8l; at}; at};
      { it = Binary (I32 Add); at };
      { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
      { it = LocalGet offset; at };
      { it = Binary (I32 Add); at };
      { it = Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
    ], ((addr, I32Type) :: [])    
  | T_or ((annot_a, a), (annot_b, b)) ->
    let addr       = var_to_string (ValueVar.fresh ~name:"addr" ()) in
    let item_a     = var_to_string (ValueVar.fresh ~name:"item_a" ()) in
    let item_b     = var_to_string (ValueVar.fresh ~name:"item_b" ()) in
    let item_a_loader, locals_a = generate_storage_loader a offset in
    let item_b_loader, locals_b = generate_storage_loader b offset in
    [
      S.{ it = A.LocalTee addr; at };
      { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
      { it = LocalTee item_a; at };
    ]
    @
    item_a_loader
    @ 
    [
      S.{ it = A.LocalGet addr; at };
      { it = Const {it = I32 4l; at}; at };
      { it = Binary (I32 Add); at };
      { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
      { it = LocalTee item_b; at };
    ]
    @
    item_b_loader
    @
    [
      { it = LocalGet addr; at };
      { it = LocalGet item_a; at };
      { it = LocalGet offset; at };
      { it = Binary (I32 Add); at };

      { it = LocalGet addr; at };
      { it = Const {it = I32 4l; at}; at };
      { it = Binary (I32 Add); at };
      { it = LocalGet item_b; at };
      { it = LocalGet offset; at };
      { it = Binary (I32 Add); at };
    ],
    (((addr, T.I32Type) :: (item_a, I32Type) :: (item_b, I32Type) :: locals_a) @ locals_b)
  | T_set ty ->
    (* a set is expected to be: 
       - size, items of the set.
       - there should not be any interruptions in the set, otherwise: oops.
      *)
    let addr     = var_to_string (ValueVar.fresh ~name:"addr" ()) in
    let size     = var_to_string (ValueVar.fresh ~name:"size" ()) in
    let counter  = var_to_string (ValueVar.fresh ~name:"counter" ()) in
    let left     = var_to_string (ValueVar.fresh ~name:"left" ()) in
    let item     = var_to_string (ValueVar.fresh ~name:"item" ()) in
    let right    = var_to_string (ValueVar.fresh ~name:"right" ()) in
    let set_item_loader, locals = generate_storage_loader ty offset in
    [
      S.{ it = A.LocalTee addr; at };
      { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
      { it = LocalSet size; at };
      { it = LocalGet addr; at };
      { it = Const {it = I32 4l; at}; at };
      { it = Binary (I32 Add); at };
      { it = LocalSet addr; at };
      { it = Const {it = I32 0l; at}; at };
      { it = LocalSet counter; at };
      { it = Loop (
        ValBlockType (Some I32Type), 
        [
          (* left *)
          S.{ it = A.LocalGet addr; at };
          { it = LocalGet addr; at };
          { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
          { it = LocalTee left; at };
          { it = LocalGet offset; at };
          { it = Binary (I32 Add); at };
          { it = Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at };

          (* item *)
          { it = LocalGet addr; at };
          { it = Const {it = I32 4l; at}; at };
          { it = Binary (I32 Add); at };
          { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
          { it = LocalTee item; at };
        ]
        @ 
        set_item_loader
        @
        [
          { it = LocalGet addr; at };
          { it = Const {it = I32 4l; at}; at };
          { it = Binary (I32 Add); at };
          { it = LocalGet item; at };
          { it = LocalGet offset; at };
          { it = Binary (I32 Add); at };
          { it = Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at };

          (* do nothing with depth *)

          (* right *)
          { it = LocalGet addr; at };
          { it = Const {it = I32 12l; at}; at };
          { it = Binary (I32 Add); at };
          { it = LocalGet addr; at };
          { it = Const {it = I32 12l; at}; at };
          { it = Binary (I32 Add); at };
          { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
          { it = LocalTee right; at };
          { it = LocalGet offset; at };
          { it = Binary (I32 Add); at };
          { it = Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at };

         (* counter *)
          { it = LocalGet counter; at };
          { it = Const {it = I32 1l; at}; at };
          { it = LocalTee counter; at };
          { it = LocalGet size; at };
          { it = Compare (I32 LtU); at };
          { it = If 
            (ValBlockType None,
            [
              { it = LocalGet addr; at };
              { it = Const {it = I32 16l; at}; at };
              { it = Binary (I32 Add); at };
              { it = LocalSet addr; at }
            ],
            [
              {it = Br {it = 1l; at}; at };
            ]
            );
            at 
          }
        ]); at 
      }
    ], ((addr, I32Type) :: (size, I32Type) :: (counter, I32Type) :: (left, I32Type) :: (item, I32Type) :: (right, I32Type) :: locals)
  | T_function _ -> 
    print_endline "- function oh hai...";
    ([], [])
  | _ -> 
    print_endline "- Do nothing apparently...";
    ([], [])

(*
  Calculate the required storage that needs to be allocated for saving the storage in a compressed way.
*)
let rec calculate_storage_size: I.type_expression -> string -> string -> (A.instr list * locals) = fun t src_addr old_addr ->  
  let at = S.no_region in
  match t.type_content with 
  | T_tuple [(_, item1); (_, item2)] ->
      let tuple_addr     = var_to_string (ValueVar.fresh ~name:"tuple_addr" ()) in
      let a1, locals1 = calculate_storage_size item1 src_addr old_addr in
      let a2, locals2 = calculate_storage_size item2 src_addr old_addr in
      ([
        S.{ it = A.LocalGet src_addr; at};
        { it = A.LocalTee tuple_addr; at};
        { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
        { it = LocalSet src_addr; at };
      ]
      @
      a1
      @ 
      [
        S.{ it = A.LocalGet tuple_addr; at};
        { it = Const {it = I32 4l; at}; at}; 
        { it = Binary (I32 Add); at };
        { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
        { it = LocalSet src_addr; at };
      ]
      @
      a2
      @
      [{ it = Const {it = I32 8l; at}; at}; 
        { it = Binary (I32 Add); at };
        { it = Binary (I32 Add); at };
      ], ((tuple_addr, T.I32Type) :: locals1) @ locals2)
      
  | T_base TB_int ->    
    ([
      S.{ it = A.LocalGet src_addr; at };
      { it = Const {it = I32 4l; at}; at};
      { it = Binary (I32 Add); at };
      { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at }; (* get the size of the limbs *)
      { it = Const {it = I32 4l; at}; at};
      { it = Binary (I32 Mul); at };
      { it = Const {it = I32 12l; at}; at};
      { it = Binary (I32 Add); at };
    ], [])
  | T_list l ->
    let list_addr     = var_to_string (ValueVar.fresh ~name:"list_addr" ()) in
    let counter       = var_to_string (ValueVar.fresh ~name:"counter" ()) in
    let next_addr     = var_to_string (ValueVar.fresh ~name:"next_addr" ()) in
    let l, locals = calculate_storage_size l src_addr old_addr in
    ([
      S.{ it = A.LocalGet src_addr; at };
      { it = LocalSet list_addr; at };
      { it = Const {it = I32 0l; at}; at};
      { it = LocalSet counter; at };
      { it = Loop (
        ValBlockType None,
        [
          S.{ it = A.LocalGet list_addr; at };
          { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
          { it = LocalSet src_addr; at };
        ]
        @ 
        l
        @
        [
          S.{ it = A.Const {it = I32 8l; at}; at};
          { it = Binary (I32 Add); at };
          { it = LocalGet counter; at };
          { it = Binary (I32 Add); at };
          { it = LocalSet counter; at };
          { it = LocalGet list_addr; at};
          { it = Const {it = I32 4l; at}; at};
          { it = Binary (I32 Add); at };
          { it = A.LocalTee next_addr; at};
          { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
          { it = DataSymbol "C_LIST_EMPTY"; at};
          { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
          { it = Compare (I32 Ne); at };
          { it = If (
            ValBlockType None, 
            [
              { it = LocalGet next_addr; at};
              { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
              { it = LocalSet list_addr; at};
              { it = Br {it = 1l; at}; at}
            ],
            [

            ]
          );
          at};
          
        ]); 
        at 
      };
      { it = LocalGet counter; at };
    ], (counter, I32Type) :: (next_addr, I32Type) :: (list_addr, I32Type) :: locals)
  | _ -> 
    print_endline "uhhhhxxx";
    [], []

let rec generate_storage_saver: I.type_expression -> string -> string -> string -> (A.instr list * locals) = fun t src_addr target_addr offset ->
  let at = S.no_region in
  match t.type_content with 
  | T_base TB_int ->
    let mp_size = var_to_string (ValueVar.fresh ~name:"mp_size" ()) in
    let counter = var_to_string (ValueVar.fresh ~name:"counter" ()) in
    let limbs   = var_to_string (ValueVar.fresh ~name:"limbs" ()) in
    [
      S.
      (* _mp_alloc *)
      { it = LocalGet target_addr; at };
      { it = LocalGet offset; at};
      { it = Binary (I32 Add); at };
      { it = LocalGet src_addr; at };
      { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
      { it = Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at };

      (* _mp_size *)
      { it = A.LocalGet target_addr; at };
      { it = A.LocalGet offset; at };
      { it = Binary (I32 Add); at };
      { it = Const {it = I32 4l; at}; at};
      { it = Binary (I32 Add); at };
      { it = LocalGet src_addr; at };
      { it = Const {it = I32 4l; at}; at};
      { it = Binary (I32 Add); at };
      { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
      { it = LocalTee mp_size; at };
      { it = Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at };

      (* pointer to limbs *)
      { it = LocalGet target_addr; at };
      { it = A.LocalGet offset; at };
      { it = Binary (I32 Add); at };
      { it = Const {it = I32 8l; at}; at};
      { it = Binary (I32 Add); at };
      { it = A.LocalGet offset; at };
      { it = Const {it = I32 12l; at}; at};
      { it = Binary (I32 Add); at };
      { it = Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at };

      (* the limbs *)
      { it = LocalGet src_addr; at };
      { it = Const {it = I32 8l; at}; at};
      { it = Binary (I32 Add); at };
      { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
      { it = LocalSet limbs; at};

      { it = Const {it = I32 0l; at}; at};
      { it = LocalSet counter; at };
      { it = Loop (ValBlockType None,
          [
            S.{ it = A.LocalGet target_addr; at };
            { it = LocalGet offset; at};
            { it = Binary (I32 Add); at };
            { it = Const {it = I32 12l; at}; at};
            { it = Binary (I32 Add); at };
            { it = LocalGet counter; at};
            { it = Const {it = I32 4l; at}; at};
            { it = Binary (I32 Mul); at };
            { it = Binary (I32 Add); at };

            { it = LocalGet limbs; at };
            { it = LocalGet counter; at};
            { it = Const {it = I32 4l; at}; at};
            { it = Binary (I32 Mul); at };
            { it = Binary (I32 Add); at };
            { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
            { it = Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at };

            { it = LocalGet counter; at};
            { it = LocalGet mp_size; at};
            { it = Compare (I32 LtU); at };
            { it = If (
              ValBlockType None, 
              [
                { it = LocalGet counter; at};
                { it = Const {it = I32 1l; at}; at};
                { it = Binary (I32 Add); at };
                { it = LocalSet counter; at};
                { it = Br {it = 1l; at}; at}
              ],
              [

              ]
            );
            at}
          ];
        );
        at
      };
      { it = LocalGet offset; at };
      { it = Const {it = I32 12l; at}; at};
      { it = Binary (I32 Add); at };
      { it = LocalGet mp_size; at };
      { it = Const {it = I32 4l; at}; at};
      { it = Binary (I32 Mul); at };
      { it = Binary (I32 Add); at };
      { it = LocalSet offset; at };
    ], ((mp_size, I32Type) :: (counter, I32Type) :: (limbs, I32Type) :: [])
  | T_tuple [(_, item1); (_, item2)] -> 
    let tuple_offset = var_to_string (ValueVar.fresh ~name:"tuple_offset" ()) in
    let tuple_src_addr = var_to_string (ValueVar.fresh ~name:"tuple_src_addr" ()) in

    let a1, locals1 = generate_storage_saver item1 src_addr target_addr offset in
    let a2, locals2 = generate_storage_saver item2 src_addr target_addr offset in
    (([
      S.{ it = A.LocalGet offset; at };
      { it = LocalSet tuple_offset; at};
      { it = LocalGet src_addr; at };
      { it = LocalSet tuple_src_addr; at };
      { it = A.LocalGet offset; at };
      { it = Const {it = I32 8l; at}; at};
      { it = Binary (I32 Add); at };
      { it = LocalSet offset; at };

      { it = LocalGet target_addr; at};
      { it = LocalGet tuple_offset; at};
      { it = Binary (I32 Add); at };
      { it = LocalGet offset; at};
      { it = Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
    
      { it = LocalGet tuple_src_addr; at };
      { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
      { it = LocalSet src_addr; at };
    ] 
    @
    a1
    @
    [
      S.{ it = A.LocalGet target_addr; at};
      { it = LocalGet tuple_offset; at};
      { it = Binary (I32 Add); at };
      { it = Const {it = I32 4l; at}; at};
      { it = Binary (I32 Add); at };
      { it = LocalGet offset; at};
      { it = Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
      { it = LocalGet tuple_src_addr; at };
      { it = Const {it = I32 4l; at}; at};
      { it = Binary (I32 Add); at };
      { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
      { it = LocalSet src_addr; at };
    ]
    @
    a2
    ), ((tuple_src_addr, T.I32Type) :: (tuple_offset, I32Type) :: locals1) @ locals2)
  | T_list l ->
    let list_addr     = var_to_string (ValueVar.fresh ~name:"list_addr" ()) in
    let counter       = var_to_string (ValueVar.fresh ~name:"counter" ()) in
    let next_addr     = var_to_string (ValueVar.fresh ~name:"next_addr" ()) in
    let old_offset    = var_to_string (ValueVar.fresh ~name:"old_offset" ()) in
    let l, locals = generate_storage_saver l src_addr target_addr offset in
    ([
      S.{ it = A.LocalGet src_addr; at };
      { it = LocalSet list_addr; at };
      { it = Loop (
        ValBlockType None,
        [
          (* take 8 bytes *)
          (* child = right after *)          
          (* DO CHILD *)
          (* next = after child or null *)
          S.{ it = A.LocalGet target_addr; at };
          { it = LocalGet offset; at };
          { it = Binary (I32 Add); at };
          
          (* { it = A.LocalGet target_addr; at }; *)
          { it = LocalGet offset; at };
          (* { it = Binary (I32 Add); at }; *)
          { it = A.Const {it = I32 8l; at}; at};
          { it = Binary (I32 Add); at };

          { it = Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at };

          { it = LocalGet list_addr; at };
          { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
          { it = LocalSet src_addr; at };
          { it = LocalGet offset; at};
          { it = LocalTee old_offset; at};

          { it = A.Const {it = I32 8l; at}; at};
          { it = Binary (I32 Add); at };
          { it = LocalSet offset; at};
        ]
        @ 
        l
        @
        [
          S.
          { it = LocalGet list_addr; at};
          { it = Const {it = I32 4l; at}; at};
          { it = Binary (I32 Add); at };          
          { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
          { it = A.LocalTee next_addr; at};
          { it = DataSymbol "C_LIST_EMPTY"; at};
          { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
          { it = Compare (I32 Ne); at };
          { it = If (
            ValBlockType None, 
            [
              (* store the next item here *)
              { it = A.LocalGet target_addr; at };
              { it = LocalGet old_offset; at };
              { it = Binary (I32 Add); at };
              { it = Const {it = I32 4l; at}; at};
              { it = Binary (I32 Add); at };
              { it = LocalGet offset; at };
              { it = Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at };

              { it = LocalGet next_addr; at};
              
              { it = LocalSet list_addr; at};
              { it = Br {it = 1l; at}; at}
            ],
            [
              (* store null here *)
              { it = A.LocalGet target_addr; at };
              { it = LocalGet old_offset; at };
              { it = Binary (I32 Add); at };
              { it = Const {it = I32 4l; at}; at};
              { it = Binary (I32 Add); at };
              { it = DataSymbol "C_LIST_EMPTY"; at};
              { it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
              { it = Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
            ]
          );
          at};
          
        ]); 
        at 
      };
      
      (* { it = LocalGet counter; at }; *)
    ], (next_addr, I32Type) :: (list_addr, I32Type) :: (old_offset, I32Type) :: locals)  
  | _ -> 
    print_endline "- Do nothing apparently...";
    ([], [])

let compile ~raise : I.expression -> string -> string -> W.Ast.module_ = fun e filename entrypoint -> 
  let w = Default_env.env in
  let at = S.no_region in
  let input, output = match e.type_expression.type_content with 
   T_function (left, right) -> left, right
  | _ -> failwith "should not happen, I think..."
  in
  let parameter_type, storage_type_input = match input.type_content with 
    T_tuple [(_, a); (_,b)] -> a, b
  | _ -> failwith "should not happen, I think..."
  in
  let _operations_type, storage_type_output = match output.type_content with 
    T_tuple [(_, a); (_,b)] -> a, b
  | _ -> failwith "should not happen, I think..."
  in
  let offset          = var_to_string (ValueVar.fresh ~name:"offset" ()) in
  let addr            = var_to_string (ValueVar.fresh ~name:"addr" ()) in
  let body, locals    = generate_storage_loader storage_type_input offset in
  
  let target_addr     = var_to_string (ValueVar.fresh ~name:"target_addr" ()) in
  let src_addr        = var_to_string (ValueVar.fresh ~name:"src_addr" ()) in
  let src_addr_backup        = var_to_string (ValueVar.fresh ~name:"src_addr_backup" ()) in
  let old_addr        = var_to_string (ValueVar.fresh ~name:"old_addr" ()) in
  let storage_size    = var_to_string (ValueVar.fresh ~name:"storage_size" ()) in
  let result_with_size    = var_to_string (ValueVar.fresh ~name:"result_with_size" ()) in
  let body_calc, locals_calc = calculate_storage_size storage_type_output src_addr old_addr in
  let body_save, locals_save = generate_storage_saver storage_type_output src_addr target_addr offset in
  let w = {w with it = {
    w.it with funcs = w.it.funcs @ [
      { 
        it = {
          name = "__load";
          ftype = "__load_type";
          locals = (offset, I32Type) :: locals;
          body = {it = LocalGet offset; at = S.no_region} :: body;
        }; 
        at = S.no_region
      };
      { 
        it = {
          name = "__save";
          ftype = "__save_type";
          locals =
            ((src_addr, T.I32Type) ::            
            (result_with_size, T.I32Type) :: 
            (target_addr, I32Type) :: 
            (storage_size, I32Type) :: 
            (offset, I32Type) ::
            (old_addr, I32Type) ::
            (src_addr_backup, T.I32Type) ::
            locals_save) @ locals_calc;
          body = 
            [
              S.{it = A.LocalGet src_addr; at };
              {it = LocalSet src_addr_backup; at };
            ]
            @
            body_calc 
            @  
            [
              S.{ it = A.LocalTee storage_size; at };
              { it = Call "malloc"; at};
              { it = LocalSet target_addr; at};
              { it = LocalGet src_addr_backup; at };
              { it = LocalSet src_addr; at };
              { it = Const {it = I32 0l;  at}; at};
              { it = LocalSet offset; at};
            ] @
            body_save
            @ 
            (* Here we return the compressed storage and the size of the compressed 
               storage (which can directly used via `__wasi_ciovec_t` in C) *)
            [
              S.
              { it = A.LocalGet result_with_size; at };
              { it = LocalGet target_addr; at};
              { it = Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
              { it = LocalGet result_with_size; at };
              { it = Const { it = I32 4l; at}; at };
              { it = Binary (I32 Add); at };
              { it = A.LocalGet storage_size; at };
              { it = Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at };
            ]
        }; 
        at = S.no_region
      };
    ]   
    }
  }
  in
  let at = location_to_region e.location in
  global_offset := Default_env.offset; 
  S.{ 
    it = toplevel_bindings ~raise e w.it;
    at
  }