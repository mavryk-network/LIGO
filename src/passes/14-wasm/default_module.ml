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
        types =
          [
            type_ ~name:"malloc_type" ~typedef:(FuncType ([NumType I32Type], [NumType I32Type]));
            type_ ~name:"c_add_i32_type"
              ~typedef:(FuncType ([NumType I32Type; NumType I32Type], [NumType I32Type]));


            (* type_ ~name:"write_debug_type" ~typedef:[NumType I32Type; NumType I32Type], [NumType I32Type] *)

            (* for testing *)
            type_ ~name:"print_type"
              ~typedef:(FuncType ([NumType I32Type], []));

            (* wasi *)
            type_ ~name:"fd_write_type"
              ~typedef:(FuncType ([NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type], [NumType I32Type]));

            (* deku specific *)
            type_ ~name:"ffi_read_ticket_type"
              ~typedef:(FuncType ([NumType I32Type], [NumType I32Type]));
            type_ ~name:"ffi_split_ticket_type"
              ~typedef:(FuncType ([NumType I32Type; NumType I64Type; NumType I64Type], [NumType I32Type]));
            type_ ~name:"ffi_join_tickets_type"
              ~typedef:(FuncType ([NumType I32Type; NumType I32Type], [NumType I32Type]));
            type_ ~name:"ffi_drop_ticket_type"
              ~typedef:(FuncType ([NumType I32Type], []));
            type_ ~name:"ffi_own_ticket_type"
              ~typedef:(FuncType ([NumType I32Type], [NumType I32Type]));
            type_ ~name:"ffi_mint_ticket_type"
              ~typedef:(FuncType ([NumType I32Type; NumType I64Type], [NumType I32Type]));
            type_ ~name:"ffi_self__type"
              ~typedef:(FuncType ([], [NumType I32Type]));
            type_ ~name:"ffi_source_type"
              ~typedef:(FuncType ([], [NumType I32Type]));
            type_ ~name:"ffi_sender_type"
              ~typedef:(FuncType ([], [NumType I32Type]));


            (* helpers *)
            type_ ~name:"right_rotate_type"
              ~typedef:(FuncType ([NumType I32Type], [NumType I32Type]));
            type_ ~name:"c_set_add_insert_value_type"
              ~typedef:(FuncType ([NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type], [NumType I32Type]));
            type_ ~name:"c_set_left_child_type"
              ~typedef:(FuncType ([NumType I32Type], [NumType I32Type]));
            type_ ~name:"to_int_type"
              ~typedef:(FuncType ([NumType I32Type], [NumType I32Type]));
            type_ ~name:"__ligo_internal__set_size_type"
              ~typedef:(FuncType ([NumType I32Type], [NumType I32Type]));
            type_ ~name:"__ligo_internal__set_remove_type"
              ~typedef:(FuncType ([NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type;], [NumType I32Type]));
            type_ ~name:"__ligo_internal__string_concat_type"
              ~typedef:(FuncType ([NumType I32Type; NumType I32Type], [NumType I32Type]));
            type_ ~name:"__ligo_internal__string_slice_type"
              ~typedef:(FuncType ([NumType I32Type; NumType I32Type; NumType I32Type], [NumType I32Type]));
            type_ ~name:"__ligo_internal__set_iter_type"
              ~typedef:(FuncType ([NumType I32Type; NumType I32Type; NumType I32Type], [NumType I32Type]));
            type_ ~name:"__ligo_internal__set_mem_type"
              ~typedef:(FuncType ([NumType I32Type; NumType I32Type; NumType I32Type], [NumType I32Type]));
            type_ ~name:"__ligo_internal__set_fold_type"
              ~typedef:(FuncType ([NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type], [NumType I32Type]));
            type_ ~name:"__ligo_internal__set_fold_right_type"
              ~typedef:(FuncType ([NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type], [NumType I32Type]));
            type_ ~name:"__ligo_internal__map_iter_type"
              ~typedef:(FuncType ([NumType I32Type; NumType I32Type; NumType I32Type], [NumType I32Type]));
            type_ ~name:"__ligo_internal__set_add_type"
              ~typedef:(FuncType ([NumType I32Type; NumType I32Type; NumType I32Type], [NumType I32Type]));
            type_ ~name:"__ligo_internal__map_add_type"
              ~typedef:(FuncType ([NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type], [NumType I32Type]));
            type_ ~name:"__ligo_internal__set_update_type"
              ~typedef:(FuncType ([NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type;], [NumType I32Type]));
            
              
              
          ];
        imports =
          [
            import ~item:"__linear_memory"
              ~desc:(MemoryImport (MemoryType {min = 0l; max = None}));
            import ~item:"__stack_pointer"
              ~desc:(GlobalImport (GlobalType (NumType I32Type, Mutable)));
            import ~item:"malloc" ~desc:(FuncImport_symbol "malloc_type");
            import ~item:"c_add_i32" ~desc:(FuncImport_symbol "c_add_i32_type");

            (* for testing *)
            import_m ~module_name:"host" ~item:"print" ~desc:(FuncImport_symbol "print_type") ();

            (* wasi *)
            import_m ~module_name:"wasi_unstable" ~item:"fd_write" ~desc:(FuncImport_symbol "fd_write_type") ();

            (* deku specific *)
            import ~item:"ffi_read_ticket" ~desc:(FuncImport_symbol "ffi_read_ticket_type");
            import ~item:"ffi_split_ticket" ~desc:(FuncImport_symbol "ffi_split_ticket_type");
            import ~item:"ffi_join_tickets" ~desc:(FuncImport_symbol "ffi_join_tickets_type");
            import ~item:"ffi_drop_ticket" ~desc:(FuncImport_symbol "ffi_drop_ticket_type");
            import ~item:"ffi_own_ticket" ~desc:(FuncImport_symbol "ffi_own_ticket_type");
            import ~item:"ffi_mint_ticket" ~desc:(FuncImport_symbol "ffi_mint_ticket_type");
            import ~item:"ffi_self_" ~desc:(FuncImport_symbol "ffi_self__type");
            import ~item:"ffi_source" ~desc:(FuncImport_symbol "ffi_source_type");
            import ~item:"ffi_sender" ~desc:(FuncImport_symbol "ffi_sender_type");


            (* helper functions *)
            import ~item:"right_rotate" ~desc:(FuncImport_symbol "right_rotate_type");
            import ~item:"c_set_add_insert_value" ~desc:(FuncImport_symbol "c_set_add_insert_value_type");
            import ~item:"c_set_left_child" ~desc:(FuncImport_symbol "c_set_left_child_type");
            import ~item:"to_int" ~desc:(FuncImport_symbol "to_int_type");
            import ~item:"__ligo_internal__set_size" ~desc:(FuncImport_symbol "__ligo_internal__set_size_type");
            import ~item:"__ligo_internal__set_remove" ~desc:(FuncImport_symbol "__ligo_internal__set_remove_type");
            import ~item:"__ligo_internal__string_concat" ~desc:(FuncImport_symbol "__ligo_internal__string_concat_type");
            import ~item:"__ligo_internal__string_slice" ~desc:(FuncImport_symbol "__ligo_internal__string_slice_type");
            import ~item:"__ligo_internal__set_iter" ~desc:(FuncImport_symbol "__ligo_internal__set_iter_type");
            import ~item:"__ligo_internal__set_mem" ~desc:(FuncImport_symbol "__ligo_internal__set_mem_type");
            import ~item:"__ligo_internal__set_fold" ~desc:(FuncImport_symbol "__ligo_internal__set_fold_type");
            import ~item:"__ligo_internal__set_add" ~desc:(FuncImport_symbol "__ligo_internal__set_add_type");
            import ~item:"__ligo_internal__map_add" ~desc:(FuncImport_symbol "__ligo_internal__map_add_type");
            import ~item:"__ligo_internal__set_fold_right" ~desc:(FuncImport_symbol "__ligo_internal__set_fold_right_type");
            import ~item:"__ligo_internal__map_iter" ~desc:(FuncImport_symbol "__ligo_internal__map_iter_type");

            import ~item:"__ligo_internal__set_update" ~desc:(FuncImport_symbol "__ligo_internal__set_update_type");
            
          ];
        symbols =
          [
            
            symbol_data ~name:"C_SET_EMPTY" ~index:0l ~size:4l ~offset:0l;
            symbol_data ~name:"C_LIST_EMPTY" ~index:1l ~size:4l ~offset:4l;
            symbol_data ~name:"C_MAP_EMPTY" ~index:2l ~size:4l ~offset:8l;
            symbol_data ~name:"C_BIG_MAP_EMPTY" ~index:3l ~size:4l ~offset:12l;
            symbol_data ~name:"__heap_base" ~index:4l ~size:4l ~offset:16l;
            symbol ~name:"malloc" ~details:(Import ([NumType I32Type], [NumType I32Type]));
            symbol ~name:"c_add_i32"
              ~details:(Import ([NumType I32Type; NumType I32Type], [NumType I32Type]));

            (* for testing *)
            symbol ~name:"print" ~details:(Import ([NumType I32Type], []));

            (* wasi *)
            symbol ~name:"fd_write" ~details:(Import ([NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type; ], [NumType I32Type]));

            (* deku specific *)
            symbol ~name:"ffi_read_ticket" ~details:(Import ([NumType I32Type], [NumType I32Type]));
            symbol ~name:"ffi_split_ticket" ~details:(Import([NumType I32Type; NumType I64Type; NumType I64Type], [NumType I32Type]));
            symbol ~name:"ffi_join_tickets" ~details:(Import([NumType I32Type; NumType I32Type], [NumType I32Type]));
            symbol ~name:"ffi_drop_ticket" ~details:(Import ([NumType I32Type], []));
            symbol ~name:"ffi_own_ticket" ~details:(Import  ([NumType I32Type], [NumType I32Type]));
            symbol ~name:"ffi_mint_ticket" ~details:(Import ([NumType I32Type; NumType I64Type], [NumType I32Type]));
            symbol ~name:"ffi_self_" ~details:(Import       ([], [NumType I32Type]));
            symbol ~name:"ffi_source" ~details:(Import      ([], [NumType I32Type]));
            symbol ~name:"ffi_sender" ~details:(Import      ([], [NumType I32Type]));

            (* helper functions *)
            symbol ~name:"right_rotate"           ~details:(Import ([NumType I32Type], [NumType I32Type]));
            symbol ~name:"c_set_add_insert_value" ~details:(Import ([NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type], [NumType I32Type]));
            symbol ~name:"c_set_left_child" ~details:(Import ([NumType I32Type], [NumType I32Type]));
            symbol ~name:"to_int" ~details:(Import ([NumType I32Type], [NumType I32Type]));
            symbol ~name:"__ligo_internal__set_size"
              ~details:(Import ([NumType I32Type], [NumType I32Type]));
            symbol ~name:"__ligo_internal__set_remove"
              ~details:(Import ([NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type;], [NumType I32Type]));
            symbol ~name:"__ligo_internal__string_concat"
              ~details:(Import ([NumType I32Type; NumType I32Type], [NumType I32Type]));
            symbol ~name:"__ligo_internal__string_slice"
              ~details:(Import ([NumType I32Type; NumType I32Type; NumType I32Type], [NumType I32Type]));
            symbol ~name:"__ligo_internal__set_iter"
              ~details:(Import ([NumType I32Type; NumType I32Type; NumType I32Type], [NumType I32Type]));
            symbol ~name:"__ligo_internal__set_mem"
              ~details:(Import ([NumType I32Type; NumType I32Type; NumType I32Type], [NumType I32Type]));
            symbol ~name:"__ligo_internal__set_fold"
              ~details:(Import ([NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type], [NumType I32Type]));
            symbol ~name:"__ligo_internal__set_fold_right"
              ~details:(Import ([NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type], [NumType I32Type]));
            symbol ~name:"__ligo_internal__set_add"
              ~details:(Import ([NumType I32Type; NumType I32Type; NumType I32Type], [NumType I32Type]));
            symbol ~name:"__ligo_internal__map_add"
              ~details:(Import ([NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type], [NumType I32Type]));
            symbol ~name:"__ligo_internal__map_iter"
              ~details:(Import ([NumType I32Type; NumType I32Type; NumType I32Type], [NumType I32Type]));
            symbol ~name:"__ligo_internal__set_update"
              ~details:(Import ([NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type], [NumType I32Type]));
              
                
          ];
      };
    at;
  }

let offset = 20l (* TODO: this needs to be made more robust *)
