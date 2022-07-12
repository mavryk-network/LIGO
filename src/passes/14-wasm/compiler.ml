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
let location_to_region (l : Location.t) : S.region =
  match l with
  | File l ->
    {
      left = {file = l#file; line = l#start#line; column = l#start#column `Byte};
      right = {file = l#file; line = l#stop#line; column = l#stop#column `Byte};
    }
  | Virtual _ -> S.no_region

(** 
 * Convert a variable to a string which we can use for symbols 
 *)
let var_to_string name =
  let name, hash = ValueVar.internal_get_name_and_counter name in
  name ^ "#" ^ string_of_int hash

(* The data offset. This indicates where a block of data should be placed in the linear memory. *)
let global_offset = ref 0l

(**
 * Convert a Zarith number (used by LIGO and Tezos) to a num_bigint number (a Rust crate for big numbers).
 *)
let convert_to_memory :
    string -> S.region -> Z.t -> A.data_part A.segment list * A.sym_info list =
 fun name at z ->
  print_endline ("a number:" ^ name);
  let z = if Z.lt z Z.zero then Z.neg z else z in
  let no_of_bits = Z.of_int (Z.log2up z) in
  let no_of_bits =
    if Z.equal no_of_bits Z.zero then Z.of_int 1 else no_of_bits
  in
  let size = Int32.(Z.to_int32 (Z.cdiv no_of_bits (Z.of_int 8))) in
  let capacity = size in
  let length = size in
  let open Mem_helpers in
  let open Helpers in
  let open Int32 in
  let data =
    A.
      [
        data ~offset:!global_offset
          ~init:
            {
              name;
              detail =
                [
                  Int32 (Datatype.int32_of_datatype Datatype.Int);
                  Symbol (name ^ "__int_data");
                ];
            };
        data ~offset:(!global_offset + 8l)
          ~init:
            {
              name = name ^ "__int_data";
              detail =
                [Symbol (name ^ "__int_data2"); Int32 capacity; Int32 length];
            };
        data
          ~offset:Int32.(!global_offset + 20l)
          ~init:{name = name ^ "__int_data2"; detail = [String (Z.to_bits z)]};
      ]
  in
  let symbols =
    [
      symbol_data ~name ~index:0l ~size:8l ~offset:!global_offset;
      symbol_data ~name:(name ^ "__int_data") ~index:0l ~size:12l
        ~offset:(!global_offset + 8l);
      symbol_data ~name:(name ^ "__int_data2") ~index:0l ~size:(size * 4l)
        ~offset:(!global_offset + 20l);
    ]
  in
  global_offset := !global_offset + (size * 4l) + 20l;
  (data, symbols)

type locals = (string * T.value_type) list

let name s =
  try W.Utf8.decode s with W.Utf8.Utf8 -> failwith "invalid UTF-8 encoding"

let rec expression ~raise :
    A.module_' -> locals -> I.expression -> A.module_' * locals * A.instr list =
 fun w l e ->
  let at = location_to_region e.location in
  let host_call :
      fn:string ->
      response_size:int32 ->
      instructions:I.expression list ->
      A.module_' * locals * A.instr list =
   fun ~fn ~response_size ~instructions ->
    let new_value = var_to_string (ValueVar.fresh ~name:fn ()) in
    let w, l, e =
      List.fold_left
        ~f:(fun all (a : I.expression) ->
          let w, l, e = all in
          let w, l, e2 = expression ~raise w l a in
          (w, l, e @ e2))
        ~init:(w, l, []) instructions
    in
    ( w,
      l @ [(new_value, T.I32Type)],
      S.
        [
          {it = A.Const {it = I32 response_size; at}; at};
          {it = Call "malloc"; at};
          {it = LocalTee new_value; at};
        ]
      @ e
      @ S.[{it = A.Call fn; at}; {it = LocalGet new_value; at}] )
  in
  match e.content with
  | E_literal Literal_unit -> (w, l, [{it = Nop; at}])
  | E_literal (Literal_int z) ->
    let unique_name = ValueVar.fresh ~name:"Literal_int" () in
    let name = var_to_string unique_name in
    let data, symbols = convert_to_memory name at z in
    let w = {w with data = w.data @ data; symbols = w.symbols @ symbols} in
    (w, l, [{it = A.DataSymbol name; at}])
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
  | E_closure {binder; body} -> (w, l, [S.{it = A.Nop; at}])
  | E_constant {cons_name = C_LIST_EMPTY; arguments = []} -> (
    match e.type_expression.type_content with
    | I.T_list {type_content = I.T_base TB_operation} ->
      let open Mem_helpers in
      let o = Operations None in
      let o_locals, o_instr = store_datatype o in
      (w, l @ o_locals, o_instr)
    | _ -> (w, l, []))
  | E_constant {cons_name = C_PAIR; arguments = [e1; e2]} ->
    print_endline "C_PAIR";
    let w, l, e1 = expression ~raise w l e1 in
    let w, l, e2 = expression ~raise w l e2 in
    let open Mem_helpers in
    let t =
      Tuple
        {
          data =
            {
              value = {data = Instructions e1};
              next =
                Some {data = {value = {data = Instructions e2}; next = None}};
            };
        }
    in
    let d_locals, e = store_datatype t in
    (w, l @ d_locals, e)
  (* MATH *)
  | E_constant {cons_name = C_NEG; arguments} ->
    host_call ~fn:"c_neg" ~response_size:4l ~instructions:arguments
  | E_constant {cons_name = C_ADD; arguments = [e1; e2]} ->
    host_call ~fn:"c_add" ~response_size:4l ~instructions:[e1; e2]
  | E_constant {cons_name = C_SUB; arguments = [e1; e2]} ->
    host_call ~fn:"c_sub" ~response_size:4l ~instructions:[e1; e2]
  | E_constant {cons_name = C_MUL; arguments = [e1; e2]} ->
    host_call ~fn:"c_mul" ~response_size:4l ~instructions:[e1; e2]
  | E_constant {cons_name = C_DIV; arguments = [e1; e2]} ->
    host_call ~fn:"c_div" ~response_size:4l ~instructions:[e1; e2]
  | E_constant {cons_name = C_MOD; arguments = [e1; e2]} ->
    host_call ~fn:"c_mod" ~response_size:4l ~instructions:[e1; e2]
  (* LOGIC *)
  | E_constant {cons_name = C_NOT; arguments = [e1; e2]} ->
    host_call ~fn:"c_not" ~response_size:4l ~instructions:[e1; e2]
  | E_constant {cons_name = C_AND; arguments = [e1; e2]} ->
    host_call ~fn:"c_and" ~response_size:4l ~instructions:[e1; e2]
  | E_constant {cons_name = C_OR; arguments = [e1; e2]} ->
    host_call ~fn:"c_or" ~response_size:4l ~instructions:[e1; e2]
  | E_constant {cons_name = C_XOR; arguments = [e1; e2]} ->
    host_call ~fn:"c_xor" ~response_size:4l ~instructions:[e1; e2]
  | E_constant {cons_name = C_LSL; arguments = [e1; e2]} ->
    host_call ~fn:"c_lsl" ~response_size:4l ~instructions:[e1; e2]
  | E_constant {cons_name = C_LSR; arguments = [e1; e2]} ->
    host_call ~fn:"c_lsr" ~response_size:4l ~instructions:[e1; e2]
  | E_constant {cons_name = C_CAR; arguments = [cons]} ->
    let w, l, cons = expression ~raise w l cons in
    ( w,
      l,
      cons @ [{it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at}]
    )
  | E_constant {cons_name = C_CDR; arguments = [cons]} ->
    let w, l, cons = expression ~raise w l cons in
    ( w,
      l,
      cons
      @ [
          {it = A.Const {it = I32 4l; at}; at};
          {it = Binary (I32 Add); at};
          {it = Load {ty = I32Type; align = 0; offset = 0l; sz = None}; at};
        ] )
  | E_constant {cons_name = C_CONS; arguments = [l1; l2]} ->
    let cons = var_to_string (ValueVar.fresh ~name:"C_CONS" ()) in
    let w, l, l1 = expression ~raise w l l1 in
    let w, l, l2 = expression ~raise w l l2 in
    ( w,
      l @ [(cons, I32Type)],
      [
        S.{it = A.Const {it = I32 8l; at}; at};
        {it = A.Call "malloc"; at};
        (* check if not 0 *)
        {it = LocalTee cons; at};
      ]
      @ l1
      @ [
          S.{it = A.Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at};
          {it = LocalGet cons; at};
          {it = Const {it = I32 4l; at}; at};
          {it = Binary (I32 Add); at};
        ]
      @ l2
      @ [
          {it = A.Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at};
          {it = LocalGet cons; at};
        ] )
  | E_constant {cons_name = C_LIST_LITERAL; arguments = [l1]} ->
    failwith "not supported yet 15a2"
    (* | E_constant {cons_name = C_SET_ADD; arguments = [a; b]} ->
       let w, l, a = expression ~raise w l a in
       let w, l, b = expression ~raise w l b in
       a @
       b @

       [S.{ it = A.Call "insertNode"; at }] *)
  | E_constant {cons_name = C_LIST_ITER; arguments = [l1]} ->
    failwith "not supported yet 15a4"
  | E_constant {cons_name; arguments} -> failwith "not supported yet 15b"
  | E_application _ ->
    let rec aux result expr =
      match expr.I.content with
      | E_application (func, e) -> aux (e :: result) func
      | E_variable v ->
        let name = var_to_string v in
        (name, List.rev result)
      | _ -> failwith "Not supported yet x1"
    in
    let name, args = aux [] e in
    let w, l, args =
      List.fold
        ~f:(fun (w, l, a) f ->
          let w, l, c = expression ~raise w l f in
          (w, l, a @ c))
        ~init:(w, l, []) args
    in
    (* let args = List.rev args in *)
    (w, l, args @ [S.{it = A.Call name; at}])
  | E_variable name -> (
    let name = var_to_string name in
    print_endline ("get variable:" ^ name);
    match List.find ~f:(fun (n, _) -> String.equal n name) l with
    | Some _ -> (w, l, [{it = LocalGet name; at}])
    | None -> (w, l, [{it = DataSymbol name; at}]))
  | E_iterator (b, ((name, _), body), expr) -> failwith "not supported yet 18"
  | E_fold _ -> failwith "not supported yet 19"
  | E_fold_right _ -> failwith "not supported yet 20"
  | E_if_bool _ -> failwith "not supported yet 21"
  | E_if_none _ -> failwith "not supported yet 22"
  | E_if_cons _ -> failwith "not supported yet 23"
  | E_if_left _ -> failwith "not supported yet 24"
  | E_let_in
      ( {content = E_closure {binder; body}},
        _inline,
        _thunk,
        ((name, _type), e2) ) ->
    failwith "should not happen..."
  | E_let_in (e1, _inline, _thunk, ((name, typex), e2)) ->
    print_endline ("assignment: " ^ var_to_string name);
    let name = var_to_string name in
    let w, l, e1 = expression ~raise w l e1 in
    let l = l @ [(name, T.I32Type)] in
    let w, l, e2 = expression ~raise w l e2 in
    (w, l, e1 @ [S.{it = A.LocalSet name; at}] @ e2)
  | E_tuple _ -> failwith "not supported yet 26"
  | E_let_tuple (tuple, (values, rhs)) ->
    (* let a_node: Node = load_datatype_tuple location in

       (* how to ensure some form of type safety: result must be something more...?! *)

       print_endline "before 1";


       let m = new DataTypeLoader location in
       let t = m#get_tuple in

       let l = t#value() in

       let n = t#next() in *)

    (*
      - load a tuple -> 
          get a tuple back, but we don't want to load everything... soooo..

          TupleLoader {
            value (),
            next ()
          }

      - 
    
    *)

    (* let wrapper_node = load_datatype_tuple ~alloc in
       let node = load_wrapper wrapper_node in
       let fst = load ...  in *)
    (* a, b *)
    let w, l, tuple = expression ~raise w l tuple in
    print_endline "before 2";
    let tuple_name = var_to_string (ValueVar.fresh ~name:"let_tuple" ()) in
    let t = tuple @ [S.{it = A.LocalSet tuple_name; at}] in
    let l = l @ [(tuple_name, T.I32Type)] in
    let l, e =
      List.foldi
        ~f:(fun i (l, all) (name, _) ->
          let name = var_to_string name in
          ( l @ [(name, T.I32Type)],
            all
            @ [
                S.{it = A.LocalGet tuple_name; at};
                {it = Const {it = I32 Int32.(4l * Int32.of_int_exn i); at}; at};
                {it = Binary (I32 Add); at};
                {
                  it = Load {ty = I32Type; align = 0; offset = 0l; sz = None};
                  at;
                };
                {it = LocalSet name; at};
              ] ))
        ~init:(l, []) values
    in
    let w, l, e2 = expression ~raise w l rhs in
    (w, l, t @ e @ e2)
  (* E_proj (record, index, field_count): we use the field_count to
     know whether the index is the last field or not, since Michelson
     treats the last element of a comb differently than the rest. We
     could alternatively put `unit` at the end of all our combs, but
     that would break compatibility and is not a standard Michelson
     convention... *)
  | E_proj _ -> failwith "not supported yet 28"
  (* E_update (record, index, update, field_count): field_count as for E_proj *)
  | E_update _ -> failwith "not supported yet 29"
  | E_raw_michelson _ ->
    failwith "TOODO: fix raw michelson error"
    (* raise.raise @@ michelson_insertion Location.dummy *)
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
   match type_content with *)

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
    | E_closure {binder; body} -> aux (binder :: arguments) body
    | _ -> (List.rev arguments, body)
  in
  aux [binder] body

let rec toplevel_bindings ~raise :
    I.expression -> W.Ast.module_' -> W.Ast.module_' =
 fun e w ->
  let at = location_to_region e.location in
  match e.content with
  | E_let_in ({content = E_closure c; _}, _inline, _thunk, ((name, type_), e2))
    ->
    let name = var_to_string name in
    let arguments, body = func c in
    let locals =
      List.map ~f:(fun a -> (var_to_string a, T.I32Type)) arguments
    in
    let w, locals, body = expression ~raise w locals body in
    let type_arg = List.map ~f:(fun _ -> T.I32Type) arguments in
    let return_type =
      match type_.type_content with
      | I.T_function (_, {type_content = I.T_base TB_unit; _}) -> []
      | _ -> [T.I32Type]
    in
    let w =
      {
        w with
        symbols = w.symbols @ [{it = {name; details = Function}; at}];
        types =
          w.types
          @ [
              {
                it =
                  {
                    tname = name ^ "_type";
                    tdetails = FuncType (type_arg, return_type);
                  };
                at;
              };
            ];
        funcs =
          w.funcs @ [{it = {name; ftype = name ^ "_type"; locals; body}; at}];
      }
    in
    toplevel_bindings ~raise e2 w
  | E_let_in
      ( {content = E_literal (Literal_int z); _},
        _inline,
        _thunk,
        ((name, _type), e2) ) ->
    (* we convert these to in memory values *)
    let name = var_to_string name in
    let data, symbols = convert_to_memory name at z in
    toplevel_bindings ~raise e2
      {w with data = w.data @ data; symbols = w.symbols @ symbols}
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
    let w =
      {
        w with
        symbols = w.symbols @ [{it = {name; details = Function}; at}];
        types =
          w.types
          @ [
              {
                it =
                  {
                    tname = name ^ "_type";
                    tdetails = FuncType ([I32Type], [I32Type]);
                  };
                at;
              };
            ];
        funcs =
          w.funcs
          @ [
              {
                it =
                  {
                    name;
                    ftype = name ^ "_type";
                    locals = [("entrypoint_tuple", I32Type)];
                    body =
                      [
                        {it = LocalGet "entrypoint_tuple"; at};
                        {it = Call actual_name; at};
                      ];
                  };
                at;
              };
            ];
      }
    in
    w
    (* in *)
    (* { w with types = w.types @ type_; funcs = w.funcs @ func; symbols = w.symbols @ symbol } *)
  | _ -> failwith "Instruction not supported at the toplevel."

let compile ~raise : I.expression -> string -> string -> W.Ast.module_ =
 fun e filename entrypoint ->
  let w = Default_env.env in
  let at = location_to_region e.location in
  global_offset := Default_env.offset;
  S.{it = toplevel_bindings ~raise e w.it; at}
