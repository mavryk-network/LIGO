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
        index = {it = 0l; at};
        offset = {it = [{it = Const {it = I32 offset; at}; at}]; at};
        init;
      };
    at;
  }

let type_ ~name ~typedef = {it = {tname = name; tdetails = typedef}; at}

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

let call name at = {it = Call name; at}

let local_set name at = {it = LocalSet name; at}

let local_get name at = {it = LocalGet name; at}

let local_tee name at = {it = LocalTee name; at}

let store at =
  {it = Store {ty = I32Type; align = 0; offset = 0l; sz = None}; at}

let i32_add at = {it = Binary (I32 Add); at}

let data_symbol symbol at = {it = DataSymbol symbol; at}
