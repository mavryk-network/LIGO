module W = WasmObjectFile
open W.Source
open W.Ast
open Helpers

let at = no_region

let mod_ : module_ =
  {
    it =
      {
        empty_module with
        datas =
          [
            data ~offset:0l ~init:{name = "C_SET_EMPTY"; detail = [Int32 0l]};
            data ~offset:4l ~init:{name = "C_LIST_EMPTY"; detail = [Int32 0l]};
            data ~offset:8l ~init:{name = "C_MAP_EMPTY"; detail = [Int32 0l]};
            data ~offset:12l
              ~init:{name = "C_BIG_MAP_EMPTY"; detail = [Int32 0l]};
          ];
        imports =
          [
            import ~item:"__linear_memory"
              ~desc:(MemoryImport (MemoryType {min = 0l; max = None}));
            import ~item:"__stack_pointer"
              ~desc:(GlobalImport (GlobalType (NumType I32Type, Mutable)));
          ];
        symbols =
          [
            symbol_data ~name:"C_SET_EMPTY" ~index:0l ~size:4l ~offset:0l;
            symbol_data ~name:"C_LIST_EMPTY" ~index:1l ~size:4l ~offset:4l;
            symbol_data ~name:"C_MAP_EMPTY" ~index:2l ~size:4l ~offset:8l;
            symbol_data ~name:"C_BIG_MAP_EMPTY" ~index:3l ~size:4l ~offset:12l;
            symbol_data ~name:"__heap_base" ~index:4l ~size:4l ~offset:16l;
          ];
      };
    at;
  }

let mod_ = add_imports mod_ [
  (* general *)
  ("env", "malloc",                           ([NumType I32Type],                                                    [NumType I32Type]));

  (* toplevel *)
  ("env", "to_int",                           ([NumType I32Type], [NumType I32Type]));

  (* for testing *)
  ("host", "print",                           ([NumType I32Type],                                                    []));

  (* wasi *)
  ("wasi_unstable", "fd_write",               ([NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type], [NumType I32Type]));

  (* set *)
  ("env", "__ligo_internal__set_size",        ([NumType I32Type],                                                                      [NumType I32Type]));
  ("env", "__ligo_internal__set_remove",      ([NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type],                   [NumType I32Type]));  
  ("env", "__ligo_internal__set_iter",        ([NumType I32Type; NumType I32Type; NumType I32Type],                                    [NumType I32Type]));
  ("env", "__ligo_internal__set_mem",         ([NumType I32Type; NumType I32Type; NumType I32Type],                                    [NumType I32Type]));
  ("env", "__ligo_internal__set_fold",        ([NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type],                   [NumType I32Type]));
  ("env", "__ligo_internal__set_fold_right",  ([NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type],                   [NumType I32Type]));
  ("env", "__ligo_internal__set_add",         ([NumType I32Type; NumType I32Type; NumType I32Type],                                    [NumType I32Type]));            
  ("env", "__ligo_internal__set_update",      ([NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type;], [NumType I32Type]));          

  (* map *)
  ("env", "__ligo_internal__map_iter",        ([NumType I32Type; NumType I32Type; NumType I32Type],                  [NumType I32Type]));
  ("env", "__ligo_internal__map_add",    ([NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type], [NumType I32Type]));

  (* list *)
  ("env", "__ligo_internal__list_map",        ([NumType I32Type; NumType I32Type; NumType I32Type],                  [NumType I32Type]));
  ("env", "__ligo_internal__list_size",       ([NumType I32Type; NumType I32Type],                                   [NumType I32Type]));
  ("env", "__ligo_internal__list_fold",       ([NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type], [NumType I32Type]));
  ("env", "__ligo_internal__list_fold_right", ([NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type], [NumType I32Type]));
  ("env", "__ligo_internal__list_iter",       ([NumType I32Type; NumType I32Type; NumType I32Type],                  [NumType I32Type]));
  
  (* string *)
  ("env", "__ligo_internal__string_concat",   ([NumType I32Type; NumType I32Type],                                   [NumType I32Type]));
  ("env", "__ligo_internal__string_slice",    ([NumType I32Type; NumType I32Type; NumType I32Type],                  [NumType I32Type]));
         
]

let offset = 20l (* TODO: this needs to be made more robust *)
