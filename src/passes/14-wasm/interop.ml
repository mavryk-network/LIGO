(* Do not change, this file is generated by the Rust `expose` macro. *)
module W = WasmObjectFile
open W.Source
open W.Ast

let at = no_region

let name s =
  try W.Utf8.decode s with W.Utf8.Utf8 ->
    failwith "invalid UTF-8 encoding"

let env: module_ = {
  it = {
    empty_module with 
      types = [
         {
            it = {
              tname = "int_neg";
              tdetails = FuncType([I32Type; I32Type; ], []);
            };
            at
        }; {
            it = {
              tname = "int_add";
              tdetails = FuncType([I32Type; I32Type; I32Type; ], []);
            };
            at
        }; {
            it = {
              tname = "int_sub";
              tdetails = FuncType([I32Type; I32Type; I32Type; ], []);
            };
            at
        };
      ];
      imports = [
         {
        it = {
            module_name = name "env";
            item_name   = name "int_neg";
            idesc = {
              it = FuncImport "int_neg";
              at
            };
        };
        at
      };
       {
        it = {
            module_name = name "env";
            item_name   = name "int_add";
            idesc = {
              it = FuncImport "int_add";
              at
            };
        };
        at
      };
       {
        it = {
            module_name = name "env";
            item_name   = name "int_sub";
            idesc = {
              it = FuncImport "int_sub";
              at
            };
        };
        at
      };
      
      ];
      symbols = [
         {
        it = {
          name = "int_neg";
          details = Import([I32Type; I32Type; ], []);
        };
        at
      }; {
        it = {
          name = "int_add";
          details = Import([I32Type; I32Type; I32Type; ], []);
        };
        at
      }; {
        it = {
          name = "int_sub";
          details = Import([I32Type; I32Type; I32Type; ], []);
        };
        at
      };
      ]
  };
  at
}