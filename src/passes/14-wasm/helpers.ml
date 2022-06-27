module W = WasmObjectFile
open W.Source
open W.Ast

let at = no_region

let name s =
  try W.Utf8.decode s with W.Utf8.Utf8 -> failwith "invalid UTF-8 encoding"

let data ~offset ~init = 
  {
    it =
      {
        index = {it = 0l; at};
        offset = {it = [{it = Const {it = I32 offset; at}; at}]; at};
        init
      };
    at;
  }

let type_ ~name ~typedef = {
  it =
    {
      tname = name;
      tdetails = typedef;
    };
  at;
}

let import ~item ~desc =  {
  it =
    {
      module_name = name "env";
      item_name = name item;
      idesc =
        {
          it = desc;
          at;
        };
    };
  at;
}

let symbol ~name ~details =  {
  it =
    {
      name;
      details
    };
  at;
}

let symbol_data ~name ~index ~size ~offset = 
  symbol ~name ~details: (Data {
    index = {it = index; at};
    relocation_offset = {it = 0l; at};
    size = {it = size; at};
    offset = {it = offset; at};
  })
      