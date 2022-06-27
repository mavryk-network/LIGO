(**
  TODO: move to Rust.
**)

module W = WasmObjectFile
open W.Source
open W.Ast
open Helpers

let at = no_region


let interop_env = Interop.env.it

let env : module_ =
  {
    it =
      {
        empty_module with
        data =
          interop_env.data
          @ [
              data ~offset:0l ~init:{name = "C_SET_EMPTY"; detail = [
                Int32 0l
              ]};
              data ~offset:4l ~init:{name = "C_LIST_EMPTY"; detail = [
                Int32 (Datatype.int32_of_datatype Datatype.ListItem); 
                Int32 0l
              ]};
              data ~offset:8l ~init:{name = "C_MAP_EMPTY"; detail = [
                Int32 0l
              ]};
              data ~offset:12l ~init:{name = "C_BIG_MAP_EMPTY"; detail = [
                Int32 0l
              ]}
            ];
        types =
          interop_env.types
          @ [
            type_ ~name:"malloc_type" ~typedef:(FuncType ([I32Type], [I32Type]));
            type_ ~name:"__wasi_fd_write_type" ~typedef:(FuncType ([I32Type; I32Type; I32Type; I32Type], [I32Type]));
            type_ ~name:"__wasi_fd_close_type" ~typedef:(FuncType ([I32Type], [I32Type]));
            type_ ~name:"__wasi_path_open_type" ~typedef: (FuncType
            ( [
                I32Type;
                I32Type;
                I32Type;
                I32Type;
                I64Type;
                I64Type;
                I32Type;
                I32Type;
              ],
              [I32Type] ));
              type_ ~name:"__wasi_path_filestat_get_type" ~typedef: (FuncType ([I32Type; I32Type; I32Type; I32Type], [I32Type]));
              type_ ~name:"__wasi_fd_read_type" ~typedef: (FuncType ([I32Type; I32Type; I32Type; I32Type], [I32Type]));
              type_ ~name:"__wasi_proc_exit_type" ~typedef: (FuncType ([I32Type], []));
            ];
        imports =
          interop_env.imports
          @ [
              import ~item:"__linear_memory" ~desc: (MemoryImport (MemoryType {min = 0l; max = None}));
              import ~item:"__stack_pointer" ~desc: (GlobalImport (GlobalType (I32Type, Mutable)));
              import ~item:"malloc" ~desc: (FuncImport "malloc_type");
              import ~item:"__wasi_fd_write" ~desc: (FuncImport "__wasi_fd_write_type");
              import ~item:"__wasi_fd_close" ~desc: (FuncImport "__wasi_fd_close_type");
              import ~item:"__wasi_path_open" ~desc: (FuncImport "__wasi_path_open");
              import ~item:"__wasi_path_filestat_get" ~desc: (FuncImport "__wasi_path_filestat_get_type");
              import ~item:"__wasi_fd_read" ~desc: (FuncImport "__wasi_fd_read_type");
              import ~item:"__wasi_proc_exit" ~desc: (FuncImport "__wasi_proc_exit_type");
            ];
        symbols =
          interop_env.symbols
          @ [
            symbol_data ~name:"C_SET_EMPTY"  ~index:0l ~size:4l ~offset:0l;
            symbol_data ~name:"C_LIST_EMPTY" ~index:1l ~size:4l ~offset:4l;
            symbol_data ~name:"C_MAP_EMPTY"  ~index:2l ~size:4l ~offset:8l;
            symbol_data ~name:"C_BIG_MAP_EMPTY"  ~index:3l ~size:4l ~offset:12l;
            symbol_data ~name:"__heap_base"  ~index:4l ~size:4l ~offset:16l;
            symbol ~name: "malloc" ~details:(Import ([I32Type], [I32Type]));
            symbol ~name: "__wasi_fd_write" ~details:(Import ([I32Type; I32Type; I32Type; I32Type], [I32Type]));
            symbol ~name: "__wasi_fd_close" ~details:(Import ([I32Type], [I32Type]));
            symbol ~name: "__wasi_path_open" ~details:(Import
            ( [
                I32Type;
                I32Type;
                I32Type;
                I32Type;
                I64Type;
                I64Type;
                I32Type;
                I32Type;
              ],
              [I32Type] ));
              symbol ~name: "__wasi_path_filestat_get" ~details:(Import ([I32Type; I32Type; I32Type; I32Type], [I32Type]));
              symbol ~name: "__wasi_fd_read" ~details:(Import ([I32Type; I32Type; I32Type; I32Type], [I32Type]));
              symbol ~name: "__wasi_proc_exit" ~details:(Import ([I32Type], []))
            ];
      };
    at;
  }

let offset = 20l (* TODO: this needs to be made more robust *)
