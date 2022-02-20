(**
  The default environment that we expect to have for a LIGO program to run in.

  Added:
  - wasi functions for file input / output
  - gmp functions that are expected to be used
  - ligo empty values
  - gmp's malloc for allocation

  Expected to be added later:
  - LIGO specific functions (basically the constants present in enum.ml)
**)

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
    data = [
      {
        it = {
          index = {it = 0l; at};
          offset = {it = [
            { it = Const {it = I32 0l; at}; at}
          ]; at};
          init = {
            name = "C_SET_EMPTY";
            detail = [Int32 0l]
          }
        };
        at
      };
      {
        it = {
          index = {it = 0l; at};
          offset = {it = [
            { it = Const {it = I32 4l; at}; at}
          ]; at};
          init = {
            name = "C_LIST_EMPTY";
            detail = [Int32 0l]
          }
        };
        at
      };
      {
        it = {
          index = {it = 0l; at};
          offset = {it = [
            { it = Const {it = I32 8l; at}; at}
          ]; at};
          init = {
            name = "C_MAP_EMPTY";
            detail = [Int32 0l]
          }
        };
        at
      };
      {
        it = {
          index = {it = 0l; at};
          offset = {it = [
            { it = Const {it = I32 12l; at}; at}
          ]; at};
          init = {
            name = "C_BIG_MAP_EMPTY";
            detail = [Int32 0l]
          }
        };
        at
      };
      {
        it = {
          index = {it = 0l; at};
          offset = {it = [
            { it = Const {it = I32 16l; at}; at}
          ]; at};
          init = {
            name = "ENTRYPOINT_TUPLE";
            detail = [
              Int32 0l; (* pointer to parameter info *)
              Int32 0l  (* pointer to storage *)
            ]
          }
        };
        at
      };
      {
        it = {
          index = {it = 0l; at};
          offset = {it = [
            { it = Const {it = I32 24l; at}; at}
          ]; at};
          init = {
            name = "STORAGE_FILE_STAT";
            detail = [
              Int64 0L; (* 0: device *)
              Int64 0L; (* 8: inode *)
              Int64 0L; (* 16: filetype *)
              Int64 0L; (* 24: linkcount *)
              Int64 0L; (* 32: filesize in bytes *)
              Int64 0L; (* 40: timestamp - last date accessed *)
              Int64 0L; (* 48: timestamp - last modification date *)
              Int64 0L; (* 56: timestamp - Last file status change timestamp. *)
            ]
          }
        };
        at
      };
      {
        it = {
          (* CONTRACT_RESULT *)
          index = {it = 0l; at};
          offset = {it = [
            { it = Const {it = I32 88l; at}; at}
          ]; at};
          init = {
            name = "CONTRACT_RESULT";
            detail = [
              Int32 0l; (* pointer to operations *)
              Int32 0l  (* pointer to modified storage *)
            ]
          }
        };
        at
      };
    ]; 
    types = [
      {
        it = {
          tname    = "malloc_type";
          tdetails = FuncType ([I32Type], [I32Type])
        };
        at
      };
      {
        it = {
          tname    = "fd_write_type";
          tdetails = FuncType ([I32Type; I32Type; I32Type; I32Type], [I32Type])
        };
        at
      };
      {
        it = {
          tname    = "fd_close_type";
          tdetails = FuncType ([I32Type], [I32Type])
        };
        at
      };
      {
        it = {
          tname    = "__wasi_path_filestat_get_type";
          tdetails = FuncType ([I32Type; I32Type; I32Type; I32Type], [I32Type])
        };
        at
      };
      {
        it = {
          tname    = "__wasi_fd_read_type";
          tdetails = FuncType ([I32Type; I32Type; I32Type; I32Type], [I32Type])
        };
        at
      };
      {
        it = {
          tname    = "__gmpz_init_type";
          tdetails = FuncType ([I32Type], [])
        };
        at
      };
      {
        it = {
          tname    = "__gmpz_add_type";
          tdetails = FuncType ([I32Type; I32Type; I32Type], [])
        };
        at
      };
      {
        it = {
          tname    = "__gmpz_sub_type";
          tdetails = FuncType ([I32Type; I32Type; I32Type], [])
        };
        at
      };
      {
        it = {
          tname    = "__gmpz_mul_type";
          tdetails = FuncType ([I32Type; I32Type; I32Type], [])
        };
        at
      };
      {
        it = {
          tname    = "__gmpz_tdiv_qr_type";
          tdetails = FuncType ([I32Type; I32Type; I32Type; I32Type], [])
        };
        at
      };
      {
        it = {
          tname    = "__gmpz_abs_type";
          tdetails = FuncType ([I32Type; I32Type], [])
        };
        at
      };
      {
        it = {
          tname    = "__gmpz_and_type";
          tdetails = FuncType ([I32Type; I32Type; I32Type], [])
        };
        at
      };   
      {
        it = {
          tname    = "__gmpz_neg_type";
          tdetails = FuncType ([I32Type; I32Type], [])
        };
        at
      };     
    ];
    imports = [
      {
        it = {
          module_name = name "env";
          item_name   = name "__linear_memory";
          idesc       = {
            it = MemoryImport (MemoryType {min = 0l; max = None});
            at 
          }
        };
        at
      };
      {
        it = {
          module_name = name "env";
          item_name   = name "__stack_pointer";
          idesc       = { 
            it = GlobalImport (GlobalType (I32Type, Mutable));
            at
          }
        };
        at
      };
      {
        it = {
          module_name = name "env";
          item_name   = name "malloc";
          idesc       = {
            it = FuncImport "malloc_type";
            at
          }
        };
        at
      };
      {
        it = {
          module_name = name "wasi_snapshot_preview1";
          item_name   = name "fd_write";
          idesc       = {
            it = FuncImport "fd_write_type" ;
            at
            }
        };
        at
      };
      {
        it = {
          module_name = name "wasi_snapshot_preview1";
          item_name   = name "fd_close";
          idesc       = {
            it = FuncImport "fd_close_type";
            at 
          }
        };
        at
      };
      {
        it = {
          module_name = name "env";
          item_name   = name "__wasi_path_filestat_get";
          idesc       = {
            it = FuncImport "__wasi_path_filestat_get_type";
            at
          }
        };
        at
      };
      {
        it = {
          module_name = name "env";
          item_name   = name "__wasi_fd_read";
          idesc       = {
            it = FuncImport "__wasi_fd_read_type";
            at
          }
        };
        at
      };
      {
        it = {
          module_name = name "env";
          item_name   = name "__gmpz_init";
          idesc       = {
            it = FuncImport "__gmpz_init_type";
            at
          }
        };
        at = no_region
      };
      {
        it = {
          module_name = name "env";
          item_name   = name "__gmpz_add";
          idesc       = {
            it = FuncImport "__gmpz_add_type";
            at
          }
        };
        at = no_region
      };
      {
        it = {
          module_name = name "env";
          item_name   = name "__gmpz_sub";
          idesc       = {
            it = FuncImport "__gmpz_sub_type";
            at 
          }
        };
        at = no_region
      };
      {
        it = {
          module_name = name "env";
          item_name   = name "__gmpz_mul";
          idesc       = {
            it = FuncImport "__gmpz_mul_type";
            at 
          }
        };
        at
      };
      {
        it = {
          module_name = name "env";
          item_name   = name "__gmpz_tdiv_qr";
          idesc       = {
            it = FuncImport "__gmpz_tdiv_qr_type";
            at 
          }
        };
        at
      };
      {
        it = {
          module_name = name "env";
          item_name   = name "__gmpz_abs";
          idesc       = {
            it = FuncImport "__gmpz_abs_type";
            at 
          }
        };
        at
      };
      {
        it = {
          module_name = name "env";
          item_name   = name "__gmpz_and";
          idesc       = {
            it = FuncImport "__gmpz_and_type";
            at
          }
        };
        at
      };
      {
        it = {
          module_name = name "env";
          item_name   = name "__gmpz_neg";
          idesc       = {
            it = FuncImport "__gmpz_neg_type";
            at
          }
        };
        at
      };
      (* Missing logarithmic functions, need to add those. *)
    ];
    symbols = [
      {
        it = {
          name    = "C_SET_EMPTY";
          details = Data {
            index = {it = 0l; at};
            relocation_offset = {it = 0l; at};
            size = {it = 4l; at};
            offset = {it = 0l; at}
          }
        };
        at
      };
      {
        it = {
          name    = "C_LIST_EMPTY";
          details = Data {
            index = {it = 0l; at};
            relocation_offset = {it = 0l; at};
            size = {it = 4l; at};
            offset = {it = 4l; at}
          }
        };
        at
      };
      {
        it = {
          name    = "C_MAP_EMPTY";
          details = Data {
            index = {it = 0l; at};
            relocation_offset = {it = 0l; at};
            size = {it = 4l; at};
            offset = {it = 8l; at}
          }
        };
        at
      };
      {
        it = {
          name    = "C_BIG_MAP_EMPTY";
          details = Data {
            index = {it = 0l; at};
            relocation_offset = {it = 0l; at};
            size = {it = 4l; at};
            offset = {it = 12l; at}
          }
        };
        at
      };
      {
        it = {
          name = "ENTRYPOINT_TUPLE";
          details = Data {
            index = {it = 0l; at};
            relocation_offset =  {it = 0l; at};
            size = { it = 8l; at};
            offset = { it = 16l; at}
          }
        };
        at
      };
      {
        it = {
          name = "STORAGE_FILE_STAT";
          details = Data {
            index = {it = 0l; at};
            relocation_offset =  {it = 0l; at};
            size = { it = 64l; at};
            offset = { it = 24l; at}
          }
        };
        at
      };
      {
        it = {
          name = "CONTRACT_RESULT";
          details = Data {
            index = {it = 0l; at};
            relocation_offset =  {it = 0l; at};
            size = { it = 8l; at};
            offset = { it = 88l; at}
          }
        };
        at
      };
      {
        it = {
          name    = "malloc";
          details = Import ([I32Type], [I32Type])
        };
        at
      };
      {
        it = {
          name    = "fd_write";
          details = Import ([I32Type; I32Type; I32Type; I32Type], [I32Type]);
        };
        at
      };
      {
        it = {
          name    = "fd_close";
          details = Import ([I32Type], [I32Type]);
        };
        at
      };
      {
        it = {
          name    = "__wasi_path_filestat_get";
          details = Import ([I32Type; I32Type; I32Type; I32Type], [I32Type]);
        };
        at
      };
      {
        it = {
          name    = "__wasi_fd_read";
          details = Import ([I32Type; I32Type; I32Type; I32Type], [I32Type]);
        };
        at
      };
      {
        it = {
          name    = "__gmpz_init";
          details = Import ([I32Type], [])
        };
        at
      };
      {
        it = {
          name    = "__gmpz_add";
          details = Import ([I32Type; I32Type; I32Type], [])
        };
        at
      };
      {
        it = {
          name    = "__gmpz_sub";
          details = Import ([I32Type; I32Type; I32Type], [])
        };
        at
      };
      {
        it = {
          name    = "__gmpz_mul";
          details = Import ([I32Type; I32Type; I32Type], [])
        };
        at
      };
      {
        it = {
          name    = "__gmpz_tdiv_qr";
          details = Import ([I32Type; I32Type; I32Type; I32Type], [])
        };
        at
      };
      {
        it = {
          name    = "__gmpz_abs";
          details = Import ([I32Type; I32Type], [])
        };
        at
      };
      {
        it = {
          name    = "__gmpz_and";
          details = Import ([I32Type; I32Type; I32Type], [])
        };
        at
      };
      {
        it = {
          name = "__gmpz_neg";
          details = Import ([I32Type; I32Type], [])
        };
        at
      }
    ];
  };
  at
}

let offset = 96l (* this needs to be made more robust *)