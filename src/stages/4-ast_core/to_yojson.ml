open Types
open Stage_common.To_yojson

type json = Yojson.Safe.t
type 'a json_printer = 'a -> json

let constant' = Stage_common.To_yojson.constant'
let label = label_to_yojson

let option f o =
  match o with
  | None -> `List [ `String "None"; `Null ]
  | Some v -> `List [ `String "Some"; f v ]


let pair f g (x, y) = `Tuple [ f x; g y ]
let list f lst = `List (List.map ~f lst)

let label_map f lmap =
  let lst =
    List.sort
      ~compare:(fun (Label a, _) (Label b, _) -> String.compare a b)
      (LMap.bindings lmap)
  in
  let lst' =
    List.fold_left ~f:(fun acc (Label k, v) -> (k, f v) :: acc) ~init:[] lst
  in
  `Assoc lst'


let layout = function
  | L_comb -> `List [ `String "L_comb"; `Null ]
  | L_tree -> `List [ `String "L_tree"; `Null ]


let sugar = function
  | Type_expr type_expr ->
    `List [ `String "Type_expr"; Ast_sugar.Yojson.type_expression type_expr ]
  | Expr expr -> `List [ `String "Expr"; Ast_sugar.Yojson.expression expr ]


let rec term { term_content = tc; location; sugar = s } =
  `Assoc
    [ "term_content", term_content tc
    ; "location", Location.to_yojson location
    ; "sugar", option sugar s
    ]


and term_content = function
  (* Base *)
  | T_literal lit ->
    `List [ `String "T_literal"; Stage_common.To_yojson.literal lit ]
  | T_variable x -> `List [ `String "T_variable"; TermVar.to_yojson x ]
  | T_constant const -> `List [ `String "T_constant"; constant const ]
  | T_application app -> `List [ `String "T_application"; application app ]
  | T_lambda lamb -> `List [ `String "T_lambda"; lambda lamb ]
  | T_recursive rec_ -> `List [ `String "T_recursive"; recursive rec_ ]
  | T_let_in l -> `List [ `String "T_let_in"; let_in l ]
  | T_mod_in m -> `List [ `String "T_mod_in"; mod_in m ]
  | T_raw_code code -> `List [ `String "T_raw_code"; raw_code code ]
  (* Variant *)
  | T_constructor constr ->
    `List [ `String "T_constructor"; constructor term constr ]
  | T_matching m -> `List [ `String "T_matching"; match_exp term term m ]
  (* Record *)
  | T_record rec_ -> `List [ `String "T_record"; record rec_ ]
  | T_record_accessor rec_acc ->
    `List [ `String "T_record_accessor"; record_accessor rec_acc ]
  | T_record_update rec_upd ->
    `List [ `String "T_record_update"; record_update rec_upd ]
  | T_module_accessor mod_acc ->
    `List [ `String "T_module_accessor"; module_access term mod_acc ]
  | T_ascription asc ->
    `List [ `String "T_module_accessor"; ascription term term asc ]
  | T_assign ass -> `List [ `String "T_assign"; assign term term ass ]
  | T_sum r -> `List [ `String "T_sum"; rows r ]
  | T_prod r -> `List [ `String "T_record"; rows r ]
  | T_arrow arr -> `List [ `String "T_arrow"; arrow arr ]
  | T_type -> `List [ `String "T_type" ]
  | T_pi p -> `List [ `String "T_pi"; pi term p ]


and constant { cons_name; arguments } =
  `Assoc [ "cons_name", constant' cons_name; "arguments", list term arguments ]


and application { lamb; args } = `Assoc [ "lamb", term lamb; "args", term args ]

and lambda { binder = b; output_type; result } =
  `Assoc
    [ "binder", binder term b
    ; "output_type", option term output_type
    ; "result", term result
    ]


and recursive { fun_name; fun_type; lambda = l } =
  `Assoc
    [ "fun_name", ValueVar.to_yojson fun_name
    ; "fun_type", term fun_type
    ; "lambda", lambda l
    ]


and let_in { let_binder; rhs; let_result; attr } =
  `Assoc
    [ "let_binder", binder term let_binder
    ; "rhs", term rhs
    ; "let_result", term let_result
    ; "attr", known_attribute attr
    ]


and type_in { type_binder; rhs; let_result } =
  `Assoc
    [ "type_binder", TypeVar.to_yojson type_binder
    ; "rhs", term rhs
    ; "let_result", term let_result
    ]


and known_attribute { inline; no_mutation; public; view; hidden } =
  `Assoc
    [ "inline", `Bool inline
    ; "no_mutation", `Bool no_mutation
    ; "public", `Bool public
    ; "view", `Bool view
    ; "hidden", `Bool hidden
    ]


and rows { fields; layout = l } =
  `Assoc [ "fields", label_map row_element fields; "layout", option layout l ]


and row_element { associated_type; michelson_annotation; decl_pos } =
  `Assoc
    [ "associated_type", term associated_type
    ; "michelson_annotation", option (fun s -> `String s) michelson_annotation
    ; "decl_pos", `Int decl_pos
    ]


and arrow { type1; type2 } = `Assoc [ "type1", term type1; "type2", term type2 ]

and type_attribute { public; hidden } =
  `Assoc [ "public", `Bool public; "hidden", `Bool hidden ]


and module_attribute { public; hidden } =
  `Assoc [ "public", `Bool public; "hidden", `Bool hidden ]


and mod_in m =
  Stage_common.To_yojson.mod_in
    term
    term
    known_attribute
    type_attribute
    module_attribute
    m


and raw_code { language; code } =
  `Assoc [ "language", `String language; "code", term code ]


and constructor
  term
  ({ constructor; element } : _ Stage_common.Types.constructor)
  : json
  =
  `Assoc [ "constructor", label constructor; "element", term element ]


and matching x = match_exp term term x
and record r = label_map term r

and record_accessor { record; path } =
  `Assoc [ "record", term record; "path", label path ]


and record_update { record; path; update } =
  `Assoc [ "record", term record; "path", label path; "update", term update ]


and declaration_type x =
  Stage_common.To_yojson.declaration_type term type_attribute x


and declaration_constant x =
  Stage_common.To_yojson.declaration_constant term term known_attribute x


and declaration_module x =
  Stage_common.To_yojson.declaration_module
    term
    term
    known_attribute
    type_attribute
    module_attribute
    x


and declaration x =
  Stage_common.To_yojson.declaration
    term
    term
    known_attribute
    type_attribute
    module_attribute
    x


and declarations x =
  Stage_common.To_yojson.declarations
    term
    term
    known_attribute
    type_attribute
    module_attribute
    x
