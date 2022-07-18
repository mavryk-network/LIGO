(**
  TODO: move to Rust.
**)

module W = WasmObjectFile
open W.Source
open W.Ast
open Helpers

let at = no_region

let env : module_ =
  {
    it =
      {
        empty_module with
        data =
          [
            data ~offset:0l ~init:{name = "C_SET_EMPTY"; detail = [Int32 0l]};
            data ~offset:4l ~init:{name = "C_LIST_EMPTY"; detail = [Int32 0l]};
            data ~offset:8l ~init:{name = "C_MAP_EMPTY"; detail = [Int32 0l]};
            data ~offset:12l
              ~init:{name = "C_BIG_MAP_EMPTY"; detail = [Int32 0l]};
          ];
        types =
          [
            type_ ~name:"malloc_type" ~typedef:(FuncType ([I32Type], [I32Type]));
            type_ ~name:"c_add_i32_type"
              ~typedef:(FuncType ([I32Type; I32Type], [I32Type]));
          ];
        imports =
          [
            import ~item:"__linear_memory"
              ~desc:(MemoryImport (MemoryType {min = 0l; max = None}));
            import ~item:"__stack_pointer"
              ~desc:(GlobalImport (GlobalType (I32Type, Mutable)));
            import ~item:"malloc" ~desc:(FuncImport "malloc_type");
            import ~item:"c_add_i32" ~desc:(FuncImport "c_add_i32_type");
          ];
        symbols =
          [
            symbol_data ~name:"C_SET_EMPTY" ~index:0l ~size:4l ~offset:0l;
            symbol_data ~name:"C_LIST_EMPTY" ~index:1l ~size:4l ~offset:4l;
            symbol_data ~name:"C_MAP_EMPTY" ~index:2l ~size:4l ~offset:8l;
            symbol_data ~name:"C_BIG_MAP_EMPTY" ~index:3l ~size:4l ~offset:12l;
            symbol_data ~name:"__heap_base" ~index:4l ~size:4l ~offset:16l;
            symbol ~name:"malloc" ~details:(Import ([I32Type], [I32Type]));
            symbol ~name:"c_add_i32"
              ~details:(Import ([I32Type; I32Type], [I32Type]));
          ];
      };

      
    at;
  }

let offset = 20l (* TODO: this needs to be made more robust *)
