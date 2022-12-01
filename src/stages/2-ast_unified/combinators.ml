open Types

type ('a, 'b, 'c, 'd, 'e) expression_content_ =
  [%import: ('a, 'b, 'c, 'd, 'e) Types.expression_content_]
[@@deriving
  ez
    { prefixes =
        [ ("make_e", fun ~loc content : expr -> { fp = Location.wrap ~loc content })
        ; ("get_e", fun (x : Types.expr) -> Location.unwrap x.fp)
        ; ("get_e_loc", fun (x : Types.expr) -> Location.get_location x.fp)
        ]
    ; wrap_constructor =
        ("expression_content_", fun ~loc content -> make_e ~loc content)
    ; wrap_get = "expression_content_", get_e
    }]

type ('a, 'b) pattern_content_ =
  [%import: ('a, 'b) Types.pattern_content_]
[@@deriving
  ez
    { prefixes =
        [ ("make_p", fun ~loc content : pattern -> { fp = Location.wrap ~loc content })
        ; ("get_p", fun (x : Types.pattern) -> Location.unwrap x.fp)
        ]
    ; wrap_constructor =
        ("pattern_content_", fun ~loc content -> make_p ~loc content)
    ; wrap_get = "pattern_content_", get_p
    }]

type 'a type_expression_content_ = [%import: 'a Types.type_expression_content_]
[@@deriving
  ez
    { prefixes =
        [ ("make_t", fun ~loc content : ty_expr -> { fp = Location.wrap ~loc content })
        ; ("get_t", fun (x : Types.ty_expr) -> Location.unwrap x.fp)
        ]
    ; wrap_constructor =
        ("type_expression_content_", fun ~loc content -> make_t ~loc content)
    ; wrap_get = "type_expression_content_", get_t
    }]

type ('a, 'b, 'c, 'd, 'e) declaration_content_ =
  [%import: ('a, 'b, 'c, 'd, 'e) Types.declaration_content_]
[@@deriving
  ez
    { prefixes =
        [ ("make_d", fun ~loc content : declaration -> { fp = Location.wrap ~loc content })
        ; ("get_d", fun (x : Types.declaration) -> Location.unwrap x.fp)
        ; ("get_d_loc", fun (x : Types.declaration) -> Location.get_location x.fp)
        ]
    ; wrap_constructor =
        ( "declaration_content_"
        , fun ~loc declaration_content -> make_d ~loc declaration_content )
    ; wrap_get = "declaration_content_", get_d
    }]

type ('a, 'b, 'c) statement_content_ = [%import: ('a, 'b, 'c) Types.statement_content_]
[@@deriving
  ez
    { prefixes =
        [ ( "make_s"
          , fun ~loc content : statement ->
              { fp = (Location.wrap ~loc content : ('a, 'b, 'c) Types.statement_) } )
        ; ("get_s", fun (x : Types.statement) -> Location.unwrap x.fp)
        ; ("get_s_loc", fun (x : Types.statement) -> Location.get_location x.fp)
        ]
    ; wrap_constructor =
        ("statement_content_", fun ~loc statement_content -> make_s ~loc statement_content)
    ; wrap_get = "statement_content_", get_s
    }]

type ('a, 'b, 'c) mod_expr_content_ = [%import: ('a, 'b, 'c) Types.mod_expr_content_]
[@@deriving
  ez
    { prefixes =
        [ ("make_m", fun ~loc content : mod_expr -> { fp = Location.wrap ~loc content })
        ; ("get_m", fun (x : Types.mod_expr) -> Location.unwrap x.fp)
        ]
    ; wrap_constructor =
        ("mod_expr_content_", fun module_content ~loc -> make_m ~loc module_content)
    ; wrap_get = "mod_expr_content_", get_m
    }]

type ('a, 'b, 'c, 'd) instruction_content_ =
  [%import: ('a, 'b, 'c, 'd) Types.instruction_content_]
[@@deriving
  ez
    { prefixes =
        [ ( "make_i"
          , fun ~loc content : instruction ->
              { fp = Location.wrap ~loc content } )
        ; ("get_i", fun (x : Types.instruction) -> Location.unwrap x.fp)
        ]
    ; wrap_constructor =
        ( "instruction_content_"
        , fun instruction_content ~loc -> make_i ~loc instruction_content )
    ; wrap_get = "instruction_content_", get_i
    }]

let e_literal ~loc l : expr = make_e ~loc @@ E_Literal l

let e__type_ ~loc p : expr = make_e ~loc @@ E_Literal (Literal__type_ p)
  [@@map _type_, ("address", "signature", "key", "key_hash", "chain_id")]


let e__type__z ~loc n : expr = make_e ~loc @@ E_Literal (Literal__type_ n)
  [@@map _type_, ("int", "nat", "timestamp", "mutez")]


let e__type_ ~loc n : expr = e__type__z ~loc @@ Z.of_int n
  [@@map _type_, ("int", "nat", "timestamp", "mutez")]


let e_unit ~loc : expr = make_e ~loc @@ E_Literal Literal_unit
let e_bytes_raw ~loc (b : bytes) : expr = make_e ~loc @@ E_Literal (Literal_bytes b)
let e_bytes_hex ~loc b : expr = e_bytes_raw ~loc @@ Hex.to_bytes b

let program_entry p : program_entry = { fp = p }