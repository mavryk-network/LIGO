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

open Helpers

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
  let z = Z.to_int32 z in
  let open Int32 in
  let data = A.[data ~offset:!global_offset ~init:{name; detail = [Int32 z]}] in
  let symbols =
    A.[symbol_data ~name ~index:0l ~size:4l ~offset:!global_offset]
  in
  global_offset := !global_offset + 4l;
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
           const response_size at;
           call "malloc" at;
           local_tee new_value at;
         ]
      @
      e 
      @ S.[ 
          call fn at; 
          store at; 
          local_get new_value at
        ] )
  in
  match e.content with
  | E_literal Literal_unit -> (w, l, [{it = Nop; at}])
  | E_literal (Literal_int z) ->
    let unique_name = ValueVar.fresh ~name:"Literal_int" () in
    let name = var_to_string unique_name in
    let data, symbols = convert_to_memory name at z in
    let w = {w with data = w.data @ data; symbols = w.symbols @ symbols} in
    (w, l, [{it = A.DataSymbol name; at}])
  | E_literal (Literal_nat _z) -> failwith "E_literal (Literal_nat) not supported"
  | E_literal (Literal_timestamp _z) -> failwith "E_literal (Literal_timestamp) not supported"
  | E_literal (Literal_mutez _z) -> failwith "E_literal (Literal_mutez) not supported"
  | E_literal (Literal_string _s) -> failwith "E_literal (Literal_string) not supported"
  | E_literal (Literal_bytes _b) -> failwith "E_literal (Literal_bytes) not supported"
  | E_literal (Literal_address _b) -> failwith "E_literal (Literal_address) not supported"
  | E_literal (Literal_signature _b) -> failwith "E_literal (Literal_signature) not supported"
  | E_literal (Literal_key _b) -> failwith "E_literal (Literal_key) not supported"
  | E_literal (Literal_key_hash _b) -> failwith "E_literal (Literal_key_hash) not supported"
  | E_literal (Literal_chain_id _b) -> failwith "E_literal (Literal_chain_id) not supported"
  | E_literal (Literal_operation _b) -> failwith "E_literal (Literal_operation) not supported"
  | E_closure {binder; body} -> (w, l, [S.{it = A.Nop; at}])
  | E_constant {cons_name = C_LIST_EMPTY; arguments = []} ->
    (w, l, [data_symbol "C_LIST_EMPTY" at])
  | E_constant {cons_name = C_PAIR; arguments = [e1; e2]} ->
    let pair = var_to_string (ValueVar.fresh ~name:"C_PAIR" ()) in
    let w, l, e1 = expression ~raise w l e1 in
    let w, l, e2 = expression ~raise w l e2 in
    let e =
      S.[const 8l at; call "malloc" at; local_set pair at; local_get pair at]
      @ e1
      @ S.[store at; local_get pair at; const 4l at; i32_add at]
      @ e2
      @ S.[store at; local_get pair at]
    in
    (w, l @ [(pair, I32Type)], e)
  (* MATH *)
  | E_constant {cons_name = C_NEG; arguments} ->
    host_call ~fn:"c_neg" ~response_size:4l ~instructions:arguments
  | E_constant {cons_name = C_ADD; arguments = [e1; e2]} ->
    host_call ~fn:"c_add_i32" ~response_size:4l ~instructions:[e1; e2]
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
    failwith "E_constant (C_LIST_LITERAL) not supported"
  | E_constant {cons_name = C_LIST_ITER; arguments = [l1]} ->
    failwith "E_constant (C_LIST_ITER) not supported"
  | E_constant {cons_name; arguments} -> failwith "E_constant (..) not supported"
  | E_application _ ->
    let rec aux result expr =
      match expr.I.content with
      | E_application (func, e) -> aux (e :: result) func
      | E_variable v ->
        let name = var_to_string v in
        (name, List.rev result)
      | E_raw_wasm _code -> failwith "E_application (E_raw_wasm) not supported"
      | E_constant _ -> failwith "e_constant x1"
      | E_closure _ -> failwith "e_closure x1"
      | E_literal _ -> failwith "e_literal x1"
      | E_raw_michelson _ -> failwith "e_raw_michelson not supported x1"
      | _ -> failwith "E_application (..) not supported"
    in
    let name, args = aux [] e in
    let w, l, args =
      List.fold
        ~f:(fun (w, l, a) f ->
          let w, l, c = expression ~raise w l f in
          (w, l, a @ c))
        ~init:(w, l, []) args
    in
    (w, l, args @ [S.{it = A.Call name; at}])
  | E_variable name -> (
    let name = var_to_string name in
    match List.find ~f:(fun (n, _) -> String.equal n name) l with
    | Some _ -> (w, l, [{it = LocalGet name; at}])
    | None -> (w, l, [{it = DataSymbol name; at}]))
  | E_iterator (b, ((name, _), body), expr) -> failwith "E_iterator not supported"
  | E_fold _ -> failwith "E_fold not supported"
  | E_fold_right _ -> failwith "E_fold_right not supported"
  | E_if_bool _ -> failwith "E_if_bool not supported"
  | E_if_none _ -> failwith "E_if_none not supported"
  | E_if_cons _ -> failwith "E_if_cons not supported"
  | E_if_left _ -> failwith "E_if_left not supported"
  | E_let_in
      ( {content = E_closure {binder; body}},
        _inline,
        _thunk,
        ((name, _type), e2) ) ->
    failwith "should not happen..."
  | E_let_in (e1, _inline, _thunk, ((name, typex), e2)) ->
    let name = var_to_string name in
    let w, l, e1 = expression ~raise w l e1 in
    let l = l @ [(name, T.I32Type)] in
    let w, l, e2 = expression ~raise w l e2 in
    (w, l, e1 @ [S.{it = A.LocalSet name; at}] @ e2)
  | E_tuple _ -> failwith "E_tuple not supported"
  | E_let_tuple (tuple, (values, rhs)) ->
    let w, l, tuple = expression ~raise w l tuple in
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
  (* | E_raw_wasm _ -> *)

  | _ -> failwith "Instruction not supported"

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
  | E_variable entrypoint ->
    let actual_name = var_to_string entrypoint in
    let name = "entrypoint" in
    let w =
      {
        w with
        symbols = w.symbols @ [
          symbol ~name ~details:Function   
        ];
        types =
          w.types
          @ [
            type_ ~name:(name ^ "_type") ~typedef:(FuncType ([I32Type; I32Type], [I32Type]));
            ];
        funcs =
          w.funcs
          @ [
              {
                it =
                  {
                    name;
                    ftype = name ^ "_type";
                    locals = [("parameter", I32Type); ("storage", I32Type); ("entrypoint_tuple", I32Type)];
                    body =
                      [
                        const 8l at;
                        call "malloc" at;
                        local_set "entrypoint_tuple" at;
                        local_get "entrypoint_tuple" at;
                        local_get "parameter" at;
                        store at;

                        local_get "entrypoint_tuple" at;
                        const 4l at;
                        i32_add at;
                        local_get "storage" at;
                        store at;

                        local_get "entrypoint_tuple" at;
                        call actual_name at;
                      ];
                  };
                at;
              };
            ];
      }
    in
    w
  | _ -> failwith "Instruction not supported at the toplevel."

let compile ~raise : I.expression -> string -> string -> W.Ast.module_ =
 fun e filename entrypoint ->
  let w = Default_env.env in
  let at = location_to_region e.location in
  global_offset := Default_env.offset;
  S.{it = toplevel_bindings ~raise e w.it; at}
