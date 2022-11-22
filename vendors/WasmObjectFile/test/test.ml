module Source = WasmObjectFile.Source
module Ast = WasmObjectFile.Ast
module Values = WasmObjectFile.Values
module Encode = WasmObjectFile.Encode

open Source
open Ast
open Values

let pos = {file = "dummy"; line = -1; column = -1}
let region = {left = pos; right = pos}
let r it = {
  it;
  at = region
}

let () = 
  let m = r {
    types = [
      r (TypeSymbol {
        tname = "some_type";
        tdetails = FuncType ([], [])
      });
      r (TypeSymbol {
        tname = "some_type2";
        tdetails = FuncType ([], [NumType I32Type])
      });
      
    ];
    globals = [];
    tables = [];
    memories = [];
    funcs = [
      r (FuncSymbol {
        name = "hello_world";
        ftype = "some_type2";
        locals = [];
        body = [
          r (Const (r (I32 10l)))
        ]
      });
      r (FuncSymbol {
        name = "entry";
        ftype = "some_type2";
        locals = [];
        body = [
          r (Call_symbol "hello_world")
        ]
      });
      r (FuncSymbol {
        name = "_start";
        ftype = "some_type2";
        locals = [];
        body = [
          r (Call_symbol "entry");
        ]
      })
    ];
    start = None;
    elems = [];
    datas  = [];
    imports = [];
    exports = [];
    symbols = [
      r {
        name = "hello_world";
        details = Function
      };
      r {
        name = "entry";
        details = Function
      };
      r {
        name = "_start";
        details = Function
      }

    ];
  }
  in
  let r = Encode.encode m in
  let oc = open_out_bin "testml.wasm" in
  output_string oc r;
  close_out oc;
  