module Value_var = Ligo_prim.Value_var
module W = WasmObjectFile
module S = W.Source
open W.Source
open W.Ast
open W.Types

let at = no_region

(** 
 * Convert a variable to a string which we can use for symbols 
 *)
let var_to_string name =
  let name, hash = Value_var.internal_get_name_and_counter name in
  name ^ "#" ^ string_of_int hash


let unique_name name =
  let unique_name = Value_var.fresh ~name () in
  let name = var_to_string unique_name in
  name


(**
 * Converts LIGO's location.t to WasmObjectFile's Source.region.
 *)
let location_to_region (l : Location.t) : S.region =
  match l with
  | File l ->
    { left = { file = l#file; line = l#start#line; column = l#start#column `Byte }
    ; right = { file = l#file; line = l#stop#line; column = l#stop#column `Byte }
    }
  | Virtual _ -> S.no_region


let cover_region (a : instr list) (b : instr list) =
  match a, List.rev b with
  | hd :: _, tl :: _ -> S.{ left = hd.at.left; right = tl.at.right }
  | _ -> S.no_region


let xname s =
  try W.Utf8.decode s with
  | W.Utf8.Utf8 -> failwith "invalid UTF-8 encoding"


let data ~offset ~init =
  { it =
      { dinit = init
      ; dmode =
          { it =
              Active
                { index = { it = 0l; at }
                ; offset = { it = [ { it = Const { it = I32 offset; at }; at } ]; at }
                }
          ; at
          }
      }
  ; at
  }


let type_ ~name ~typedef = { it = TypeSymbol { tname = name; tdetails = typedef }; at }

let import_m ?module_name ~item ~desc () =
  { it =
      { module_name =
          (match module_name with
           | Some n -> xname n
           | None -> xname "env")
      ; item_name = xname item
      ; idesc = { it = desc; at }
      }
  ; at
  }


let import ~item ~desc = import_m ~item ~desc ()
let export ~name ~desc = { it = { name = xname name; edesc = { it = desc; at } }; at }
let symbol ~name ~details = { it = { name; details }; at }

let symbol_data ~name ~index ~size ~offset =
  symbol
    ~name
    ~details:
      (Data
         { index = { it = index; at }
         ; relocation_offset = { it = 0l; at }
         ; size = { it = size; at }
         ; offset = { it = offset; at }
         })


let const at i = { it = Const { it = I32 i; at }; at }
let call_s at name = { it = Call_symbol name; at }
let call_indirect_s at name = { it = CallIndirect_symbol name; at }
let local_set at index = { it = LocalSet { it = index; at }; at }
let local_set_s at name = { it = LocalSet_symbol name; at }
let local_get_s at name = { it = LocalGet_symbol name; at }
let local_get at index = { it = LocalGet { it = index; at }; at }
let local_tee_s at name = { it = LocalTee_symbol name; at }
let load at = { it = Load { ty = I32Type; align = 0; offset = 0l; pack = None }; at }
let store at = { it = Store { ty = I32Type; align = 0; offset = 0l; pack = None }; at }

let store8 at =
  { it = Store { ty = I32Type; align = 0; offset = 0l; pack = Some Pack8 }; at }


let i32_add at = { it = Binary (I32 Add); at }
let i32_sub at = { it = Binary (I32 Sub); at }
let i32_mul at = { it = Binary (I32 Mul); at }
let i32_div at = { it = Binary (I32 DivS); at }
let i32_and at = { it = Binary (I32 And); at }
let i32_or at = { it = Binary (I32 Or); at }
let i32_xor at = { it = Binary (I32 Xor); at }
let i32_lsl at = { it = Binary (I32 Shl); at }
let i32_lsr at = { it = Binary (I32 ShrS); at }
let i32_eq at = { it = Compare (I32 Eq); at }
let i32_eqz at = { it = Test (I32 Eqz); at }
let i32_ne at = { it = Compare (I32 Ne); at }
let i32_lt at = { it = Compare (I32 LtS); at }
let i32_gt at = { it = Compare (I32 GtS); at }
let i32_le at = { it = Compare (I32 LeS); at }
let i32_ge at = { it = Compare (I32 GeS); at }
let unreachable at = { it = Unreachable; at }
let data_symbol at symbol = { it = DataSymbol symbol; at }
let func_symbol at symbol : instr = { it = FuncSymbol symbol; at }
let memory_copy at = { it = MemoryCopy; at }

let elem at i =
  { it =
      { etype = FuncRefType
      ; einit = []
      ; emode =
          { it =
              Active
                { index = { it = 0l; at }
                ; offset = { it = [ const at (Int32.of_int_exn i) ]; at }
                }
          ; at
          }
      }
  ; at
  }


let compare_eq at = { it = Compare (I32 I32Op.Eq); at }
let if_ at bt t e = { it = If (bt, t, e); at }
let br at index = { it = Br { it = index; at }; at }
let br_if at index = { it = BrIf { it = index; at }; at }
let loop at b il = { it = Loop (b, il); at }
let nop at = { it = Nop; at }
let drop at = { it = Drop; at }

type missing_env =
  { arguments : symbol list
  ; locals : symbol list
  ; functions : symbol list
  ; missing_arguments : symbol list
  ; missing_locals : symbol list
  ; missing_functions : symbol list
  }

let find_missing e =
  let function_exists env v =
    match List.find ~f:(fun func -> String.equal v func) env.functions with
    | Some _ -> true
    | None ->
      (match List.find ~f:(fun func -> String.equal v func) env.missing_functions with
       | Some _ -> true
       | None -> false)
  in
  let argument_exists env v =
    match List.find ~f:(fun var -> String.equal v var) env.arguments with
    | Some _ -> true
    | None ->
      (match List.find ~f:(fun func -> String.equal v func) env.missing_arguments with
       | Some _ -> true
       | None -> false)
  in
  let local_exists env v =
    match List.find ~f:(fun var -> String.equal v var) env.locals with
    | Some _ -> true
    | None ->
      (match List.find ~f:(fun func -> String.equal v func) env.missing_locals with
       | Some _ -> true
       | None -> argument_exists env v)
  in
  let rec aux env e =
    match e with
    | { it = Block (_, bl); _ } :: remaining | { it = Loop (_, bl); _ } :: remaining ->
      let env = aux env bl in
      aux env remaining
    | { it = If (_, th, el); _ } :: remaining ->
      let env = aux env th in
      let env = aux env el in
      aux env remaining
    | { it = Call_symbol s; _ } :: remaining
    | { it = CallIndirect_symbol s; _ } :: remaining ->
      aux
        { env with
          missing_functions =
            (if function_exists env s
            then env.missing_functions
            else s :: env.missing_functions)
        }
        remaining
    | { it = LocalGet_symbol s; _ } :: remaining ->
      let env =
        if local_exists env s
        then env
        else
          { env with
            missing_arguments =
              (if argument_exists env s
              then env.missing_arguments
              else s :: env.missing_arguments)
          }
      in
      aux env remaining
    | { it = LocalSet_symbol s; _ } :: remaining
    | { it = LocalTee_symbol s; _ } :: remaining ->
      aux
        { env with
          missing_locals =
            (if local_exists env s then env.missing_locals else s :: env.missing_locals)
        }
        remaining
    | _ :: remaining -> aux env remaining
    | [] -> env
  in
  aux
    { missing_arguments = []
    ; missing_locals = []
    ; missing_functions = []
    ; arguments = []
    ; locals = []
    ; functions = []
    }
    e


let add_import_m w ~module_name ~name ~typedef =
  let t = type_ ~name:(name ^ "_type") ~typedef:(FuncType (fst typedef, snd typedef)) in
  let i =
    import_m ~module_name ~item:name ~desc:(FuncImport_symbol (name ^ "_type")) ()
  in
  let s = symbol ~name ~details:(Import (fst typedef, snd typedef)) in
  { w with
    it =
      { w.it with
        types = w.it.types @ [ t ]
      ; imports = w.it.imports @ [ i ]
      ; symbols = w.it.symbols @ [ s ]
      }
  }


let add_import w ~name ~typedef = add_import_m w ~module_name:"env" ~name ~typedef

let add_imports w (i : (string * string * (result_type * result_type)) list) =
  List.fold_left
    ~f:(fun w (module_name, name, typedef) -> add_import_m w ~module_name ~name ~typedef)
    ~init:w
    i


let add_function w helper_fn_name f_body =
  let local_get_s = local_get_s at in
  let required = find_missing (f_body []) in
  let required_arguments =
    List.fold_left
      ~f:(fun a s -> a @ [ local_get_s s ])
      ~init:[]
      required.missing_arguments
  in
  let f_body = f_body required_arguments in
  let required_locals =
    List.map ~f:(fun f -> f, NumType I32Type) required.missing_arguments
    @ List.map ~f:(fun f -> f, NumType I32Type) required.missing_locals
  in
  let f =
    FuncSymbol
      { name = helper_fn_name
      ; ftype = helper_fn_name ^ "_type"
      ; locals = required_locals
      ; body = f_body
      }
  in
  let f = S.{ it = f; at } in
  let t =
    TypeSymbol
      { tname = helper_fn_name ^ "_type"
      ; tdetails =
          FuncType
            ( List.map ~f:(fun _ -> NumType I32Type) required.missing_arguments
            , [ NumType I32Type ] )
      }
  in
  let s = { name = helper_fn_name; details = Function } in
  let s = S.{ it = s; at } in
  let t = S.{ it = t; at } in
  let w =
    { w with types = t :: w.types; funcs = f :: w.funcs; symbols = s :: w.symbols }
  in
  w, required_arguments
