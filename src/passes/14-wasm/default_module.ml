(**
  TODO: move to Rust.
**)

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
          ];
        imports =
          [
            import ~item:"__linear_memory"
              ~desc:(MemoryImport (MemoryType {min = 0l; max = None}));
            import ~item:"__stack_pointer"
              ~desc:(GlobalImport (GlobalType (NumType I32Type, Mutable)));
            import ~item:"malloc" ~desc:(FuncImport_symbol "malloc_type");
            import ~item:"c_add_i32" ~desc:(FuncImport_symbol "c_add_i32_type");

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
          ];
          tables = [{
            it = {ttype = TableType ({min = 0l; max = Some 0l}, FuncRefType)}; at 
          }];
      };
    at;
  }

let offset = 20l (* TODO: this needs to be made more robust *)
