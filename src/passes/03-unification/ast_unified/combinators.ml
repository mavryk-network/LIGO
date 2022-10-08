open Types

type expression_content = [%import: Types.expression_content]
[@@deriving ez {
      prefixes = [
        ("make_e" , fun ?(loc = Location.generated) expression_content ->
                  ({ expression_content ; location = loc } : expression)) ;
        ("get" , fun x -> x.expression_content) ;
      ] ;
      wrap_constructor = ("expression_content" , (fun expression_content ?loc () -> make_e ?loc expression_content)) ;
      wrap_get = ("expression_content" , get) ;
    } ]

type type_expression_content = [%import: Types.type_expression_content]
[@@deriving ez {
      prefixes = [
        ("make_t" , fun ?(loc = Location.generated) type_expression_content ->
                  ({ type_expression_content ; location = loc } : type_expression)) ;
        ("get" , fun x -> x.type_expression_content) ;
      ] ;
      wrap_constructor = ("type_expression_content" , (fun type_expression_content ?loc () -> make_t ?loc type_expression_content)) ;
      wrap_get = ("declaration_content" , get) ;
    } ]

type declaration_content = [%import: Types.declaration_content]
[@@deriving ez {
      prefixes = [
        ("make_d" , fun ?(loc = Location.generated) declaration_content ->
                  ({ declaration_content ; location = loc } : declaration)) ;
        ("get" , fun x -> x.declaration_content) ;
      ] ;
      wrap_constructor = ("declaration_content" , (fun declaration_content ?loc () -> make_d ?loc declaration_content)) ;
      wrap_get = ("declaration_content" , get) ;
    } ]

type statement_content = [%import: Types.statement_content]
[@@deriving ez {
      prefixes = [
        ("make_s" , fun ?(loc = Location.generated) statement_content ->
                  ({ statement_content ; location = loc } : statement)) ;
        ("get" , fun x -> x.statement_content) ;
      ] ;
      wrap_constructor = ("statement_content" , (fun statement_content ?loc () -> make_s ?loc statement_content)) ;
      wrap_get = ("statement_content" , get) ;
    } ]

type module_content = [%import: Types.module_content]
[@@deriving ez {
      prefixes = [
        ("make_m" , fun ?(loc = Location.generated) module_content ->
                  ({ module_content ; location = loc } : module_)) ;
        ("get" , fun x -> x.module_content) ;
      ] ;
      wrap_constructor = ("module_content" , (fun module_content ?loc () -> make_m ?loc module_content)) ;
      wrap_get = ("module_content" , get) ;
    } ]

type instruction_content = [%import: Types.instruction_content]
[@@deriving ez {
      prefixes = [
        ("make_i" , fun ?(loc = Location.generated) instruction_content ->
                  ({ instruction_content ; location = loc } : instruction)) ;
        ("get" , fun x -> x.instruction_content) ;
      ] ;
      wrap_constructor = ("instruction_content" , (fun instruction_content ?loc () -> make_i ?loc instruction_content)) ;
      wrap_get = ("statement_content" , get) ;
    } ]

type pattern_content = [%import: Types.pattern_content]
[@@deriving ez {
      prefixes = [
        ("make_p" , fun ?(loc = Location.generated) pattern_content ->
                  ({ pattern_content ; location = loc } : pattern)) ;
        ("get" , fun x -> x.pattern_content) ;
      ] ;
      wrap_constructor = ("pattern_content" , (fun pattern_content ?loc () -> make_p ?loc pattern_content)) ;
      wrap_get = ("statement_content" , get) ;
    } ]

let e_literal ?loc l : expression = make_e ?loc @@ E_Literal l
let e__type_ ?loc p : expression = make_e ?loc @@ E_Literal (Literal__type_ p)
[@@map (_type_, ("address", "signature", "key", "key_hash", "chain_id"))]

let e__type__z ?loc n : expression = make_e ?loc @@ E_Literal (Literal__type_ n)
[@@map (_type_, ("int", "nat", "timestamp", "mutez"))]

let e__type_ ?loc n : expression = e__type__z ?loc @@ Z.of_int n
[@@map (_type_, ("int", "nat", "timestamp", "mutez"))]

let e_unit ?loc () : expression = make_e ?loc @@ E_Literal (Literal_unit)

let e_bytes_raw ?loc (b: bytes) : expression = make_e ?loc @@ E_Literal (Literal_bytes b)
let e_bytes_hex ?loc b : expression = e_bytes_raw ?loc @@ Hex.to_bytes b

let e_constant ?loc name lst = make_e ?loc @@ E_Constant {cons_name=name ; arguments = lst}
