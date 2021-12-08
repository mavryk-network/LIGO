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
      r {
        tname = "some_type";
        tdetails = FuncType ([], [])
      };
      r {
        tname = "some_type2";
        tdetails = FuncType ([], [I32Type])
      };
      
    ];
    globals = [];
    tables = [];
    memories = [];
    funcs = [
      r {
        name = "hello_world";
        ftype = r 1l;
        locals = [];
        body = [
          r (Const (r (I32 10l)))
        ]
      };
      r {
        name = "entry";
        ftype = r 1l;
        locals = [];
        body = [
          r (Call "hello_world")
        ]
      };
      r {
        name = "_start";
        ftype = r 0l;
        locals = [];
        body = [
          r (Call "entry");
          r Drop
        ]
      }
    ];
    start = None;
    elems = [];
    data  = [];
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
  