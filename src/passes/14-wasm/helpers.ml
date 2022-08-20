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

let const i at = {it = Const {it = I32 i; at}; at}

let call_s name at = {it = Call_symbol name; at}

let call_indirect_s name at = {it = CallIndirect_symbol name; at}

let local_set_s name at = {it = LocalSet_symbol name; at}

let local_get_s name at = {it = LocalGet_symbol name; at}

let local_tee_s name at = {it = LocalTee_symbol name; at}

let load at = 
  {it = Load {ty = I32Type; align = 0; offset = 0l; pack = None}; at}

let store at =
  {it = Store {ty = I32Type; align = 0; offset = 0l; pack = None}; at}

let i32_add at = {it = Binary (I32 Add); at}

let data_symbol symbol at = {it = DataSymbol symbol; at}

let elem i at = {it = 
  { 
    etype = FuncRefType;
    einit = [];
    emode = {it = (Active {
      index = { it = 0l; at };
      offset = {it = [const (Int32.of_int_exn i) at]; at }
    }); at };
  }; 
  at} 