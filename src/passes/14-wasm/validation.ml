[@@@warning "-27"]
module I = Mini_c.Types
module W = WasmObjectFile
module A = W.Ast
module T = W.Types
module S = W.Source
module Z = Z
module Value_var = Ligo_prim.Value_var
module Location = Simple_utils.Location

let find_local_type (env: Env.t) name = 
  let l = List.find ~f:(fun (n, _) -> String.equal n name) env.locals in
  match l with 
    Some (_, t) -> t
  | None -> failwith "Not expected"

let find_type w symbol = 
  let t = List.find ~f:(fun f -> match f.it with TypeSymbol {tname;_ } -> String.equal tname symbol | _ -> false ) w.A.types in
  match t with 
    Some t -> Some t.it
  | None -> None

let name s =
  try W.Utf8.decode s with W.Utf8.Utf8 -> failwith "invalid UTF-8 encoding"
  
let find_import_type w symbol = 
  match List.find ~f:(fun f -> Poly.equal f.it.item_name (name symbol)) w.A.imports with 
    Some ({it = {idesc = {it = FuncImport_symbol f; _}; _}; _}) -> find_type w f
  | _ -> failwith "not found"

let func_type w symbol = 
  match List.find ~f:(fun f -> match f.it with FuncSymbol {name; _} -> String.equal name symbol | _ -> false ) w.A.funcs with 
  | Some {it = FuncSymbol {ftype; _}; _} -> 
    let t = find_type w ftype in
    (match t with 
      Some t -> Some t
    | None -> find_type w symbol)
  | _ -> find_import_type w symbol

let rec count_stack c = function 
    Some (_, Env.Next n) -> count_stack (c + 1) n
  | None -> c

let stack_push (env: Env.t) (v: T.value_type) =
  {env with operand_stack = Some (v, Next env.operand_stack)}

let stack_pop (env: Env.t) (v2: T.value_type) = 
  match env.operand_stack with 
    Some (v1, Next operand_stack) when (Poly.equal v1 v2) ->
      {env with operand_stack}
  | Some (_, _) ->
      failwith "Bug for the developers. The item popped from the operand stack does not match the expected type, which should not happen."
  | None -> failwith "Bug for the developers. There's nothing on the operand stack, which should not happen."

let stack_pop_any (env: Env.t) = 
  match env.operand_stack with 
    Some (_, Next operand_stack) ->
      {env with operand_stack}
  | None -> failwith "Bug for the developers. There's nothing on the operand stack, which should not happen."

let block_push (env: Env.t) b =
  {env with blocks = b :: env.blocks }

let block_pop (env: Env.t) =
  match env.blocks with 
    hd :: tl -> {env with blocks = tl}
  | [] -> failwith "Bug for developers. Trying to remove a block that's not present. "

let block_n (env: Env.t) n = 
  match List.nth env.blocks n with 
    Some n -> n
  | None -> failwith "Bug for developers. Not present block."

let rec check w env e : Env.t =
  match e with 
  | S.{it = A.Unreachable; _} :: rest   -> check w env rest
  | {it = Nop; _} :: rest             -> check w env rest
  | {it = Drop; _} :: rest            -> let env = stack_pop_any env in check w env rest      
  | {it = Select _; _} :: rest          -> 
    let env = stack_pop env (T.NumType I32Type) in
    let env = stack_pop_any env in
    let env = stack_pop_any env in
    let env = stack_push env (T.NumType I32Type) in
    check w env rest
  | {it = Block (bt, il); _} :: rest
  | {it = Loop (bt, il); _} :: rest ->
    let env = block_push env bt in
    let _ = check w env il in
    let env = (match bt with 
      VarBlockType _ -> failwith "not supported yet"
    | ValBlockType (Some s) -> stack_push env s
    | ValBlockType (None) -> env)
    in
    let env = block_pop env in
    check w env rest
  | {it = If (bt, il1, il2); _} :: rest ->
    let env = stack_pop env (T.NumType I32Type) in
    let _ = check w env il1 in
    let _ = check w env il2 in
    let env = (match bt with 
      VarBlockType _ -> failwith "not supported yet"
    | ValBlockType (Some s) -> stack_push env s
    | ValBlockType (None) -> env)
    in
    check w env rest
  | {it = Br b; _} :: rest -> check w env rest
  | {it = BrIf b; _} :: rest -> check w env rest
  | {it = BrTable (bt, d); _} :: rest -> check w env rest
  | {it = Return; _} :: rest ->  check w env rest
  | {it = Call v; _} :: rest -> check w env rest
  | {it = CallIndirect (v, v2); _} :: rest -> check w env rest
  | {it = LocalGet v; _} :: rest -> check w env rest
  | {it = LocalSet v; _} :: rest -> check w env rest
  | {it = LocalTee v; _} :: rest -> check w env rest
  | {it = GlobalGet _; _} :: rest -> check w env rest
  | {it = GlobalSet _; _} :: rest -> check w env rest

  | {it = Call_symbol symbol; _} :: rest ->
    let env = (match (func_type w symbol) with 
    | Some (TypeSymbol {tdetails = FuncType (input, output); _}) -> 
      let env = List.fold_left ~f:(fun env i -> stack_pop env i) ~init:env input in
      let env = List.fold_left ~f:(fun env i -> stack_push env i) ~init:env output in
      env
    | _ -> failwith ("Could not find:" ^ symbol))
    in
    check w env rest
  | {it = CallIndirect_symbol symbol; _} :: rest -> 
    let env = (match (find_type w symbol) with 
      Some (TypeSymbol {tdetails = FuncType (input, output); _}) ->
        let env = List.fold_left ~f:(fun env i -> stack_pop env i) ~init:env input in
        let env = List.fold_left ~f:(fun env i -> stack_push env i) ~init:env output in
        env
    | _ -> failwith "Errr..")
    in
    check w env rest
  | {it = LocalGet_symbol symbol; _} :: rest -> 
    let l = find_local_type env symbol in
    let env = stack_push env l in
    check w env rest
  | {it = LocalSet_symbol symbol; _} :: rest -> 
    let l = find_local_type env symbol in
    let env = stack_pop env l in
    check w env rest
  | {it = Const {it = I32 _; _}; _} :: rest -> 
    let env = stack_push env (T.NumType I32Type) in
    check w env rest
  | {it = Const {it = I64 _; _}; _} :: rest -> 
    let env = stack_push env (T.NumType I64Type) in
    check w env rest
  | {it = Const {it = F32 _; _}; _} :: rest -> 
    let env = stack_push env (T.NumType F32Type) in
    check w env rest
  | {it = Const {it = F64 _; _}; _} :: rest -> 
    let env = stack_push env (T.NumType F64Type) in
    check w env rest
  | {it = DataSymbol _; _} :: rest -> 
    let env = stack_push env (T.NumType I32Type) in
    check w env rest
  | {it = FuncSymbol _; _} :: rest -> 
    let env = stack_push env (T.NumType I32Type) in
    check w env rest
  | {it = Test (I32 _); _} :: rest -> 
    let env = stack_pop env (T.NumType I32Type) in
    let env = stack_push env (T.NumType I32Type) in
    check w env rest
  | {it = Test (I64 _); _} :: rest -> 
    let env = stack_pop env (T.NumType I64Type) in
    let env = stack_push env (T.NumType I64Type) in
    check w env rest
  | {it = Compare (I32 _); _} :: rest -> 
    let env = stack_pop env (T.NumType I32Type) in
    let env = stack_pop env (T.NumType I32Type) in
    let env = stack_push env (T.NumType I32Type) in
    check w env rest
  | {it = Unary (I32 _); _} :: rest -> 
    let env = stack_pop env (T.NumType I32Type) in
    let env = stack_push env (T.NumType I32Type) in
    check w env rest
  | {it = Unary (I64 _); _} :: rest -> 
    let env = stack_pop env (T.NumType I64Type) in
    let env = stack_push env (T.NumType I64Type) in
    check w env rest
  | {it = Unary (F32 _); _} :: rest -> 
    let env = stack_pop env (T.NumType F32Type) in
    let env = stack_push env (T.NumType F32Type) in
    check w env rest
  | {it = Unary (F64 _); _} :: rest -> 
    let env = stack_pop env (T.NumType F64Type) in
    let env = stack_push env (T.NumType F64Type) in
    check w env rest
  | {it = Binary (I32 _); _} :: rest -> 
    let env = stack_pop env (T.NumType I32Type) in
    let env = stack_pop env (T.NumType I32Type) in
    let env = stack_push env (T.NumType I32Type) in
    check w env rest
  | {it = Binary (I64 _); _} :: rest -> 
    let env = stack_pop env (T.NumType I64Type) in
    let env = stack_pop env (T.NumType I64Type) in
    let env = stack_push env (T.NumType I64Type) in
    check w env rest
  | {it = Binary (F32 _); _} :: rest -> 
    let env = stack_pop env (T.NumType F32Type) in
    let env = stack_pop env (T.NumType F32Type) in
    let env = stack_push env (T.NumType F32Type) in
    check w env rest
  | {it = Binary (F64 _); _} :: rest -> 
    let env = stack_pop env (T.NumType F64Type) in
    let env = stack_pop env (T.NumType F64Type) in
    let env = stack_push env (T.NumType F64Type) in
    check w env rest
  | {it = Load {ty = n; _} ; _} :: rest -> 
    let env = stack_pop env (T.NumType I32Type) in
    let env = stack_push env (T.NumType n) in
    check w env rest
  | {it = Store {ty; _}; _} :: rest -> 
    let env = stack_pop env (T.NumType I32Type) in
    let env = stack_pop env (T.NumType ty) in
    check w env rest
  | {it = Convert _; _} :: rest -> check w env rest
  | {it = LocalTee_symbol symbol; _} :: rest -> check w env rest
  | {it = GlobalGet_symbol symbol; _} :: rest -> check w env rest
  | {it = GlobalSet_symbol symbol; _} :: rest -> check w env rest
  
  (*| {it = TableGet _; _} :: rest -> check w env rest
  | {it = TableSet _; _} :: rest -> check w env rest
  | {it = TableSize _; _} :: rest -> check w env rest
  | {it = TableGrow _; _} :: rest -> check w env rest
  | {it = TableFill _; _} :: rest -> check w env rest
  | {it = TableCopy _; _} :: rest -> check w env rest
  | {it = TableInit _; _} :: rest -> check w env rest
  | {it = ElemDrop _; _} :: rest -> check w env rest
  
  | {it = VecLoad _; _} :: rest -> check w env rest
  | {it = VecStore _; _} :: rest -> check w env rest
  | {it = VecLoadLane _; _} :: rest -> check w env rest
  | {it = VecStoreLane _; _} :: rest -> check w env rest
  | {it = MemorySize; _} :: rest -> check w env rest
  | {it = MemoryGrow; _} :: rest -> check w env rest
  | {it = MemoryFill; _} :: rest -> check w env rest
  | {it = MemoryCopy; _} :: rest -> check w env rest
  | {it = MemoryInit _; _} :: rest -> check w env rest
  | {it = DataDrop _; _} :: rest -> check w env rest
  | {it = RefNull _; _} :: rest -> check w env rest
  | {it = RefFunc _; _} :: rest -> check w env rest
  | {it = RefIsNull; _} :: rest -> check w env rest
  
  | {it = VecConst _; _} :: rest -> check w env rest
  | {it = VecTest _; _} :: rest -> check w env rest
  | {it = VecCompare _; _} :: rest -> check w env rest
  | {it = VecUnary _; _} :: rest -> check w env rest
  | {it = VecBinary _; _} :: rest -> check w env rest
  | {it = VecConvert _; _} :: rest -> check w env rest
  | {it = VecShift _; _} :: rest -> check w env rest
  | {it = VecBitmask _; _} :: rest -> check w env rest
  | {it = VecTestBits _; _} :: rest -> check w env rest
  | {it = VecUnaryBits _; _} :: rest -> check w env rest
  | {it = VecBinaryBits _; _} :: rest -> check w env rest
  | {it = VecTernaryBits _; _} :: rest -> check w env rest
  | {it = VecSplat _; _} :: rest -> check w env rest
  | {it = VecExtract _; _} :: rest -> check w env rest
  | {it = VecReplace _; _} :: rest -> check w env rest *)
  | [] -> env
  | _ -> failwith "not yet implemented" 