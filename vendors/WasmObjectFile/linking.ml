open Ast

let data_index (data: data_part segment list) symbol = 
  let rec iter_data (data: data_part segment list) count = 
    match data with
    | Source.{it = {init = {name; _}; _}; _} :: remaining when name = symbol -> count
    | _ :: remaining -> iter_data remaining (Int32.add count 1l) 
    | [] -> (-1l)
  in iter_data data 0l

let func_index (funcs: Ast.func list) (imports: Ast.import list) symbol = 
  let rec find_import imports count = 
    match imports with
    | Source.{it = {item_name; _};_} :: remaining when (Ast.string_of_name item_name) = symbol -> count
    | _ :: remaining -> find_import remaining (Int32.add count 1l) 
    | [] -> (-1l)
  in
  let result = find_import imports 0l in
  if result = (-1l) then 
    let rec find_func funcs count = 
      match funcs with
      | Source.{it = {name; ftype; _}; _} :: remaining when name = symbol -> count
      | _ :: remaining -> find_func remaining (Int32.add count 1l)
      | [] -> failwith ("Could not find: " ^ symbol)
    in
    find_func funcs (Int32.of_int (List.length imports))
  else 
    result

let find_type types x = 
  let rec iter result = function
    | Source.{it = {tname; _}; _} :: remaining when tname = x -> result
    | {it = {tname; _}; _} :: remaining -> iter (Int32.add result 1l) remaining
    | [] -> result
  in
  iter 0l types 

let find_global_index symbols at index_ =
  let result = ref (-1l) in
  List.iteri (fun i s -> match s.Source.it.details with
  | Global {index; _} when s.Source.it.name = index_ -> (
      result := index.it
    )
  | _ -> ()) symbols;
  if !result = (-1l) then (        
    failwith ("Could not find global: " ^ index_)
  ) else 
    Source.{
      it = !result;
      at
    }

let find_symbol_index symbols func =
  let rec aux index = function
    hd :: tl when func hd -> 
      (hd, Int32.of_int index)
  | _ :: tl ->
    aux (index + 1) tl
  | [] ->
    failwith "not found"
  in
  aux 0 symbols


(*type code_relocation =
  | R_WASM_FUNCTION_INDEX_LEB of int32 * string
  | R_WASM_MEMORY_ADDR_LEB of int32 * Ast.var  
  | R_WASM_TYPE_INDEX_LEB of int32 * Ast.var
  | R_WASM_GLOBAL_INDEX_LEB of int32 * string
  | R_WASM_MEMORY_ADDR_SLEB of int32 * string
  | R_WASM_TABLE_INDEX_SLEB  of int32 * string

type data_relocation =
  | R_WASM_TABLE_INDEX_I32 of int32 * string
  | R_WASM_MEMORY_ADDR_I32 of int32 * string



let func_symbol_index symbols symbol = 
  let rec f symbol symbols result = 
    match symbols with 
    | {name; details = Function} :: remaining
    | {name; details = Import _} :: remaining when name = symbol -> result
    | _ :: remaining -> f symbol remaining (Int32.add result 1l)
    | [] -> print_endline ("could not find:" ^ symbol); assert false
  in
  f symbol symbols 0l

let func_index funcs imports symbol = 
  let rec find_import imports count = 
    match imports with
    | {item_name} :: remaining when (Ast.string_of_name item_name) = symbol -> count
    | _ :: remaining -> find_import remaining (Int32.add count 1l) 
    | [] -> (-1l)
  in
  let result = find_import imports 0l in
  if result = (-1l) then 
    let rec find_func funcs count = 
      match funcs with
      | {name; _} :: remaining when name = symbol -> count
      | _ :: remaining -> find_func remaining (Int32.add count 1l)
      | [] -> failwith ("Could not find: " ^ symbol)
    in
    find_func funcs (Int32.of_int (List.length imports))
  else 
    result

let data_index data symbol = 
  let rec iter_data (data: data_part segment list) count = 
    match data with
    | {init = {name}} :: remaining when name = symbol -> count
    | _ :: remaining -> iter_data remaining (Int32.add count 1l) 
    | [] -> (-1l)
  in iter_data data 0l

 *)