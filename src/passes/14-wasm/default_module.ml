module W = WasmObjectFile
open W.Source
open W.Ast
open Helpers

let at = no_region

let mod_ : module_ =
  { it =
      { empty_module with
        imports =
          [ import
              ~item:"__linear_memory"
              ~desc:(MemoryImport (MemoryType { min = 0l; max = None }))
          ; import
              ~item:"__stack_pointer"
              ~desc:(GlobalImport (GlobalType (NumType I32Type, Mutable)))
          ]
      ; symbols = [ symbol_data ~name:"__heap_base" ~index:0l ~size:4l ~offset:0l ]
      }
  ; at
  }


let mod_ =
  add_imports
    mod_
    [ (* general *)
      "env", "malloc", ([ NumType I32Type ], [ NumType I32Type ])
    ; "env", "compare", ([ NumType I32Type; NumType I32Type ], [ NumType I32Type ])
    ; (* toplevel *)
      "env", "to_int", ([ NumType I32Type ], [ NumType I32Type ])
    ; "env", "__ligo_internal__log", ([ NumType I32Type ], [ NumType I32Type ])
    ; (* for testing *)
      "host", "print", ([ NumType I32Type ], [])
    ; (* wasi *)
      ( "wasi_unstable"
      , "fd_write"
      , ( [ NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type ]
        , [ NumType I32Type ] ) )
    ; (* set *)
      "env", "__ligo_internal__set_size", ([ NumType I32Type ], [ NumType I32Type ])
    ; ( "env"
      , "__ligo_internal__set_remove"
      , ([ NumType I32Type; NumType I32Type; NumType I32Type ], [ NumType I32Type ]) )
    ; ( "env"
      , "__ligo_internal__set_iter"
      , ([ NumType I32Type; NumType I32Type ], [ NumType I32Type ]) )
    ; ( "env"
      , "__ligo_internal__set_mem"
      , ([ NumType I32Type; NumType I32Type ], [ NumType I32Type ]) )
    ; ( "env"
      , "__ligo_internal__set_fold"
      , ([ NumType I32Type; NumType I32Type; NumType I32Type ], [ NumType I32Type ]) )
    ; ( "env"
      , "__ligo_internal__set_fold_right"
      , ([ NumType I32Type; NumType I32Type; NumType I32Type ], [ NumType I32Type ]) )
    ; ( "env"
      , "__ligo_internal__set_add"
      , ([ NumType I32Type; NumType I32Type ], [ NumType I32Type ]) )
    ; ( "env"
      , "__ligo_internal__set_update"
      , ( [ NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type ]
        , [ NumType I32Type ] ) )
    ; (* map *)
      ( "env"
      , "__ligo_internal__map_iter"
      , ([ NumType I32Type; NumType I32Type ], [ NumType I32Type ]) )
    ; ( "env"
      , "__ligo_internal__map_map"
      , ([ NumType I32Type; NumType I32Type ], [ NumType I32Type ]) )
    ; ( "env"
      , "__ligo_internal__map_fold"
      , ([ NumType I32Type; NumType I32Type; NumType I32Type ], [ NumType I32Type ]) )
    ; ( "env"
      , "__ligo_internal__map_add"
      , ([ NumType I32Type; NumType I32Type; NumType I32Type ], [ NumType I32Type ]) )
    ; ( "env"
      , "__ligo_internal__map_find_opt"
      , ([ NumType I32Type; NumType I32Type ], [ NumType I32Type ]) )
    ; ( "env"
      , "__ligo_internal__map_update"
      , ( [ NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type ]
        , [ NumType I32Type ] ) )
    ; ( "env"
      , "__ligo_internal__map_get_update"
      , ( [ NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type ]
        , [ NumType I32Type ] ) )
    ; (* list *)
      ( "env"
      , "__ligo_internal__list_map"
      , ([ NumType I32Type; NumType I32Type ], [ NumType I32Type ]) )
    ; "env", "__ligo_internal__list_size", ([ NumType I32Type ], [ NumType I32Type ])
    ; ( "env"
      , "__ligo_internal__list_fold"
      , ([ NumType I32Type; NumType I32Type; NumType I32Type ], [ NumType I32Type ]) )
    ; ( "env"
      , "__ligo_internal__list_fold_right"
      , ([ NumType I32Type; NumType I32Type; NumType I32Type ], [ NumType I32Type ]) )
    ; ( "env"
      , "__ligo_internal__list_iter"
      , ([ NumType I32Type; NumType I32Type ], [ NumType I32Type ]) )
    ; (* string *)
      ( "env"
      , "__ligo_internal__string_concat"
      , ([ NumType I32Type; NumType I32Type ], [ NumType I32Type ]) )
    ; ( "env"
      , "__ligo_internal__string_slice"
      , ([ NumType I32Type; NumType I32Type; NumType I32Type ], [ NumType I32Type ]) )
    ; (* bytes*)
      "env", "__ligo_internal__bytes_unpack", ([ NumType I32Type ], [ NumType I32Type ])
    ; "env", "__ligo_internal__bytes_pack", ([ NumType I32Type ], [ NumType I32Type ])
    ]


let offset = 4l (* TODO: this needs to be made more robust *)
