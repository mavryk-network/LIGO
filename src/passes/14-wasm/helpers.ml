module Value_var = Ligo_prim.Value_var

module W = WasmObjectFile
module S = W.Source

open W.Source
open W.Ast

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
    {
      left = {file = l#file; line = l#start#line; column = l#start#column `Byte};
      right = {file = l#file; line = l#stop#line; column = l#stop#column `Byte};
    }
  | Virtual _ -> S.no_region

let cover_region (a: instr list) (b: instr list) =
  match a, List.rev b with 
    hd:: _, tl :: _ -> S.{ left = hd.at.left; right = tl.at.right }
  | _ -> S.no_region
    
let xname s =
  try W.Utf8.decode s with W.Utf8.Utf8 -> failwith "invalid UTF-8 encoding"

let data ~offset ~init =
  {
    it =
      {
        dinit = init;
        dmode = {
          it = Active {
            index = { it = 0l; at };
            offset = {
              it = [{it = Const {it = I32 offset; at}; at}];
              at
            }
          };
          at
        }
      };
    at;
  }

let type_ ~name ~typedef = {it = TypeSymbol {tname = name; tdetails = typedef}; at}

let import_m ?module_name ~item ~desc () =
  {
    it =
      {
        module_name = (match module_name with Some n -> xname n | None ->  xname "env");
        item_name = xname item;
        idesc = {it = desc; at};
      };
    at;
  }

let import ~item ~desc =
  import_m ~item ~desc ()


let symbol ~name ~details = {it = {name; details}; at}

let symbol_data ~name ~index ~size ~offset =
  symbol ~name
    ~details:
      (Data
         {
           index = {it = index; at};
           relocation_offset = {it = 0l; at};
           size = {it = size; at};
           offset = {it = offset; at};
         })

let const at i = {it = Const {it = I32 i; at}; at}

let call_s at name = {it = Call_symbol name; at}

let call_indirect_s at name = {it = CallIndirect_symbol name; at}

let local_set_s at name = {it = LocalSet_symbol name; at}

let local_get_s at name = {it = LocalGet_symbol name; at}

let local_tee_s at name = {it = LocalTee_symbol name; at}

let load at = 
  {it = Load {ty = I32Type; align = 0; offset = 0l; pack = None}; at}

let store at =
  {it = Store {ty = I32Type; align = 0; offset = 0l; pack = None}; at}

let i32_add at = {it = Binary (I32 Add); at}

let i32_sub at = {it = Binary (I32 Sub); at}

let i32_mul at = {it = Binary (I32 Mul); at}

let i32_div at = {it = Binary (I32 DivS); at}

let i32_and at = {it = Binary (I32 And); at}
let i32_or at = {it = Binary (I32 Or); at}
let i32_xor at = {it = Binary (I32 Xor); at}
let i32_lsl at = {it = Binary (I32 Shl); at}
let i32_lsr at = {it = Binary (I32 ShrS); at}

let i32_eq at = {it = Compare (I32 Eq); at}
let i32_ne at = {it = Compare (I32 Ne); at}
let i32_lt at = {it = Compare (I32 LtS); at}
let i32_gt at = {it = Compare (I32 GtS); at}
let i32_le at = {it = Compare (I32 LeS); at}
let i32_ge at = {it = Compare (I32 GeS); at}

let data_symbol at symbol = {it = DataSymbol symbol; at}

let func_symbol at symbol = ({it = FuncSymbol symbol; at}: instr)

let elem at i = {it = 
  { 
    etype = FuncRefType;
    einit = [];
    emode = {it = (Active {
      index = { it = 0l; at };
      offset = {it = [const at (Int32.of_int_exn i)]; at }
    }); at };
  }; 
  at} 

let compare_eq at = {it = Compare (I32 I32Op.Eq); at }

let if_ at bt t e = 
  {it = If (bt, t, e); at}

let br at index =
  {it = Br {it = index; at}; at }
  
let br_if at index =
  {it = BrIf {it = index; at}; at }
  
let loop at b il = 
  {it = Loop (b, il); at}

let nop at = 
  {it = Nop; at}

let drop at = 
  {it = Drop; at}