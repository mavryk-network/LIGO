module W = WasmObjectFile
open W.Source
open W.Ast

let at = no_region

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

let import ~item ~desc =
  {
    it =
      {
        module_name = xname "env";
        item_name = xname item;
        idesc = {it = desc; at};
      };
    at;
  }

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

let i32_mul at = {it = Binary (I32 Mul); at}

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

let br_if at index =
  {it = BrIf {it = index; at}; at }
  
let loop at b il = 
  {it = Loop (b, il); at}

let nop at = 
  {it = Nop; at}