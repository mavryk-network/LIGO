open Types

type ('a, 'b, 'c, 'd, 'e) expression_content_ = [%import: ('a, 'b, 'c, 'd, 'e) Types.expression_content_ ]
[@@deriving
  ez
    { prefixes =
        [ ( "make_e"
          , fun ?(loc = Location.generated) content : Types.expr ->
              { fp = Location.wrap ~loc content } )
        ; ("get", fun (x:Types.expr) -> Location.unwrap x.fp)
        ]
    ; wrap_constructor =
        ( "wrap_content"
        , fun content ?loc -> make_e ?loc content )
    ; wrap_get = "wrap_content", get
    }]

(* type ty_expr = [%import: Types.ty_expr]
[@@deriving
  ez
    { prefixes =
        [ ( "make_t"
          , fun ?(loc = Location.generated) content : ty_expr ->
            { fp = Location.wrap ~loc content } )
        ; ("get", fun x -> Location.unwrap x.fp)
        ]
    ; wrap_constructor =
        ( "wrap_content"
        , fun content ?loc () -> make_t ?loc content)
    ; wrap_get = "wrap_content", get
    }]
*)

type ('a, 'b, 'c, 'd, 'e) declaration_content_ = [%import: ('a, 'b, 'c, 'd, 'e) Types.declaration_content_ ]
type ('a, 'b, 'c, 'd, 'e) declaration_content = ('a, 'b, 'c, 'd, 'e) declaration_content_
[@@deriving
  ez
    { prefixes =
        [ ( "make_d"
          , fun ?(loc = Location.generated) content : declaration ->
            { fp = Location.wrap ~loc content } )
        (* ; ("get", fun (x:Types.declaration) -> Location.unwrap x.fp) *)
        ]
    ; wrap_constructor =
        ( "wrap_content"
        , fun declaration_content ?loc () -> make_d ?loc declaration_content )
    ; wrap_get = "wrap_content", get
    }]

type ('a,'b,'c) statement_content_ = [%import: ('a, 'b, 'c) Types.statement_content_ ]
type ('a,'b,'c) statement_content = ('a,'b,'c) statement_content_
[@@deriving
  ez
    { prefixes =
        [ ( "make_s"
          , fun ?(loc = Location.generated) content : statement ->
            { fp = Location.wrap ~loc content } )
        ; ("get", fun (x:Types.statement) -> Location.unwrap x.fp)
        ]
    ; wrap_constructor =
        ( "wrap_content"
        , fun statement_content ?loc () -> make_s ?loc statement_content )
    ; wrap_get = "wrap_content", get
    }]

(*

type module_content = [%import: Types.mod_expr]
[@@deriving
  ez
    { prefixes =
        [ ( "make_m"
          , fun ?(loc = Location.generated) content : mod_expr ->
            { fp = Location.wrap ~loc content } )
        ; ("get", fun x -> Location.unwrap x.fp)
        ]
    ; wrap_constructor =
        ("module_content", fun module_content ?loc () -> make_m ?loc module_content)
    ; wrap_get = "module_content", get
    }]

type instruction_content = [%import: Types.instruction]
[@@deriving
  ez
    { prefixes =
        [ ( "make_i"
          , fun ?(loc = Location.generated) content : instruction ->
            { fp = Location.wrap ~loc content } )
        ; ("get", fun x -> Location.unwrap x.fp)
        ]
    ; wrap_constructor =
        ( "instruction_content"
        , fun instruction_content ?loc () -> make_i ?loc instruction_content )
    ; wrap_get = "instruction_content", get
    }]

let e_literal ?loc l : expr = make_e ?loc @@ E_Literal l

let e__type_ ?loc p : expr = make_e ?loc @@ E_Literal (Literal__type_ p)
  [@@map _type_, ("address", "signature", "key", "key_hash", "chain_id")]


let e__type__z ?loc n : expr = make_e ?loc @@ E_Literal (Literal__type_ n)
  [@@map _type_, ("int", "nat", "timestamp", "mutez")]


let e__type_ ?loc n : expr = e__type__z ?loc @@ Z.of_int n
  [@@map _type_, ("int", "nat", "timestamp", "mutez")]


let e_unit ?loc () : expr = make_e ?loc @@ E_Literal Literal_unit
let e_bytes_raw ?loc (b : bytes) : expr = make_e ?loc @@ E_Literal (Literal_bytes b)
let e_bytes_hex ?loc b : expr = e_bytes_raw ?loc @@ Hex.to_bytes b *)
