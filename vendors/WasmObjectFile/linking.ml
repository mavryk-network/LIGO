open Ast

let data_index (data: data_segment list) symbol = 
  let rec iter_data (data: data_segment list) count = 
    match data with
    | Source.{it = {dinit = {name; _}; _}; _} :: remaining when name = symbol -> count
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
      | Source.{it = FuncSymbol {name; ftype; _}; _} :: remaining when name = symbol -> count
      | _ :: remaining -> find_func remaining (Int32.add count 1l)
      | [] -> failwith ("Could not find function: " ^ symbol)
    in
    find_func funcs (Int32.of_int (List.length imports))
  else 
  result

let find_type types x = 
  let rec iter result = function
    | Source.{it = TypeSymbol {tname; _}; _} :: remaining when tname = x -> result
    | _ :: remaining -> iter (Int32.add result 1l) remaining
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
    failwith "not found symbol"
  in
  aux 0 symbols