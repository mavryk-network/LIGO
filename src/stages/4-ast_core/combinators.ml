open Ligo_prim
open Ligo_prim.Literal_types
open Types

(* Helpers for accessing and constructing elements are derived using
   `ppx_woo` (`@@deriving ez`) *)

type expression_content = [%import: Types.expression_content]
[@@deriving
  ez
    { prefixes =
        [ ( "make_e"
          , fun ?(loc = Location.generated) expression_content : expression ->
              { expression_content; location = loc } )
        ; ("get", fun x -> x.expression_content)
        ]
    ; wrap_constructor =
        ( "expression_content"
        , fun expression_content ?loc () -> make_e ?loc expression_content )
    ; wrap_get = "expression_content", get
    }]

type type_content = [%import: Types.type_content]
[@@deriving
  ez
    { prefixes =
        [ ( "make_t"
          , fun ?(loc = Location.generated) type_content : type_expression ->
              { type_content; location = loc } )
        ; ("get", fun x -> x.type_content)
        ]
    ; wrap_constructor =
        ("type_content", fun type_content ?loc () -> make_t ?loc type_content)
    ; wrap_get = "type_content", get
    ; default_get = `Option
    }]

let t_constant ?loc type_operator arguments : type_expression =
  make_t
    ?loc
    (T_app
       { type_operator =
           Type_var.of_input_var (Literal_types.to_string type_operator)
       ; arguments
       })


let t_abstraction ?loc ty_binder kind type_ =
  make_t ?loc (T_abstraction { ty_binder; kind; type_ })


let t_for_all ?loc ty_binder kind type_ =
  make_t ?loc (T_for_all { ty_binder; kind; type_ })


(* TODO?: X_name here should be replaced by X_injection *)
let t__type_ ?loc () : type_expression = t_constant ?loc _type_ []
  [@@map
    _type_
    , ( "signature"
      , "chain_id"
      , "string"
      , "bytes"
      , "key"
      , "key_hash"
      , "int"
      , "address"
      , "operation"
      , "nat"
      , "tez"
      , "timestamp"
      , "unit"
      , "bls12_381_g1"
      , "bls12_381_g2"
      , "bls12_381_fr"
      , "never"
      , "mutation"
      , "pvss_key"
      , "baker_hash"
      , "chest_key"
      , "chest" )]


let t__type_ ?loc t : type_expression = t_constant ?loc _type_ [ t ]
  [@@map _type_, ("list", "set", "contract", "ticket")]


let t__type_ ?loc t t' : type_expression = t_constant ?loc _type_ [ t; t' ]
  [@@map _type_, ("map", "big_map", "typed_address")]


let t_mutez = t_tez

let t_abstraction1 ?loc name kind : type_expression =
  let ty_binder = Type_var.fresh ~name:"_a" () in
  let type_ = t_constant name [ t_variable ty_binder () ] in
  t_abstraction ?loc ty_binder kind type_


let t_abstraction2 ?loc name kind_l kind_r : type_expression =
  let ty_binder_l = Type_var.fresh ~name:"_l" () in
  let ty_binder_r = Type_var.fresh ~name:"_r" () in
  let type_ =
    t_constant name [ t_variable ty_binder_l (); t_variable ty_binder_r () ]
  in
  t_abstraction
    ?loc
    ty_binder_l
    kind_l
    (t_abstraction ?loc ty_binder_r kind_r type_)


let t_record ?loc ?layout fields : type_expression =
  make_t ?loc @@ T_record { fields; attributes = { layout } }


let default_layout = Layout.L_tree

let make_t_ez_record ?loc ?layout (row : (string * type_expression) list)
    : type_expression
  =
  row
  |> List.mapi ~f:(fun i (x, y) ->
         ( Label.of_string x
         , { Rows.Elem.associated_type = y
           ; attributes = { michelson_annotation = None }
           ; decl_pos = i
           } ))
  |> Label.Map.of_alist_exn
  |> t_record ?loc ?layout


let ez_t_record ?loc ?(layout = default_layout) row : type_expression =
  t_record ?loc ~layout (Label.Map.of_alist_exn row)


let t_pair ?loc a b : type_expression = make_t ?loc (T_tuple [ a; b ])

let t_sum ?loc ?layout fields : type_expression =
  make_t ?loc @@ T_sum { fields; attributes = { layout } }


let t_sum_ez ?loc ?layout (row : (string * type_expression) list)
    : type_expression
  =
  row
  |> List.mapi ~f:(fun i (x, y) ->
         ( Label.of_string x
         , { Rows.Elem.associated_type = y
           ; attributes = { michelson_annotation = None }
           ; decl_pos = i
           } ))
  |> Label.Map.of_alist_exn
  |> t_record ?loc ?layout


let t_bool ?loc () : type_expression =
  t_sum_ez ?loc [ "True", t_unit (); "False", t_unit () ]


let t_arrow ?loc param result : type_expression =
  t_arrow ?loc { type1 = param; type2 = result } ()


let t_shallow_closure ?loc param result : type_expression =
  make_t ?loc (T_arrow { type1 = param; type2 = result })


let get_t_bool (t : type_expression) : unit option =
  match t.type_content with
  | t when Types.compare_type_content t (t_bool ()).type_content = 0 -> Some ()
  | _ -> None


let get_t_option (t : type_expression) : type_expression option =
  match t.type_content with
  | T_sum { fields; _ } ->
    let labels = Label.Map.keys fields in
    (match labels with
    | [ Label "Some"; Label "None" ] | [ Label "None"; Label "Some" ] ->
      let some_type = Map.find_exn fields (Label "Some") in
      Some some_type.associated_type
    | _ -> None)
  | _ -> None


let get_t_tuple (t : type_expression) : type_expression list option =
  match t.type_content with
  | T_tuple types -> Some types
  | _ -> None


let get_t_pair (t : type_expression)
    : (type_expression * type_expression) option
  =
  match t.type_content with
  | T_tuple [ fst_type; snd_type ] -> Some (fst_type, snd_type)
  | _ -> None


let ez_e_record (fields : (Label.t * expression) list) : expression =
  e_record (Label.Map.of_alist_exn fields) ()


let e_var ?loc n : expression =
  e_variable (Value_var.of_input_var ?loc n) ?loc ()


let e_unit ?loc () : expression = e_literal Literal_unit ?loc ()
let e_literal ?loc l : expression = e_literal l ?loc ()

let e__type_ ?loc p : expression = make_e ?loc @@ E_literal (Literal__type_ p)
  [@@map
    _type_
    , ( "int"
      , "nat"
      , "mutez"
      , "string"
      , "bytes"
      , "timestamp"
      , "address"
      , "signature"
      , "key"
      , "key_hash"
      , "chain_id"
      , "operation"
      , "bls12_381_g1"
      , "bls12_381_g2"
      , "bls12_381_fr" )]


let e'_bytes b : expression_content =
  let bytes = Hex.to_bytes (`Hex b) in
  E_literal (Literal_bytes bytes)


let e_bytes_hex ?loc b : expression =
  let e' = e'_bytes b in
  make_e ?loc e'


let e_bytes_raw ?loc (b : bytes) : expression =
  make_e ?loc @@ E_literal (Literal_bytes b)


let e_bytes_string ?loc (s : string) : expression =
  make_e ?loc @@ E_literal (Literal_bytes (Hex.to_bytes (Hex.of_string s)))


let e_constant ?loc cons_name arguments =
  e_constant ?loc { cons_name; arguments } ()


let e_variable v : expression = e_variable v ()
let e_application lamb args : expression = e_application { lamb; args } ()

let e_lambda ?loc binder output_type result =
  e_lambda ?loc { binder; output_type; result } ()


let e_type_abs ?loc type_binder result =
  e_type_abstraction ?loc { type_binder; result } ()


let e_recursive ?loc fun_name fun_type lambda =
  e_recursive ?loc { fun_name; fun_type; lambda } ()


let e_let_in ?loc let_binder rhs let_result attributes =
  e_let_in ?loc { let_binder; rhs; let_result; attributes } ()


let e_let_mut_in ?loc let_binder rhs let_result attributes =
  e_let_mut_in ?loc { let_binder; rhs; let_result; attributes } ()


let e_type_in type_binder rhs let_result =
  e_type_in { type_binder; rhs; let_result } ()


let e_mod_in ?loc module_binder rhs let_result =
  e_mod_in ?loc { module_binder; rhs; let_result } ()


let e_raw_code ?loc language code = e_raw_code ?loc { language; code } ()

let e_constructor constructor element : expression =
  e_constructor { constructor; element } ()


let e_matching ?loc matchee cases : expression =
  e_matching ?loc { matchee; cases } ()


let e_record_accessor ?loc struct_ path =
  e_accessor ?loc ({ struct_; path } : _ Types.Accessor.t) ()


let e_record_update ?loc struct_ path update =
  e_update ?loc ({ struct_; path; update } : _ Types.Update.t) ()


let e_module_accessor ?loc module_path element =
  e_module_accessor ?loc { module_path; element } ()


let e_ascription ?loc anno_expr type_annotation : expression =
  e_ascription ?loc { anno_expr; type_annotation } ()


let e_lambda_ez ?loc var ?ascr ?mut_flag output_type result : expression =
  e_lambda ?loc (Ligo_prim.Param.make ?mut_flag var ascr) output_type result


let e_let_in_ez ?loc var ?ascr ?(mut = false) attributes rhs let_result =
  let binder = Ligo_prim.Binder.make var ascr in
  if mut
  then e_let_mut_in ?loc binder rhs let_result attributes
  else e_let_in ?loc binder rhs let_result attributes


(* Constants *)
let e_some ?loc s : expression = e_constant ?loc C_SOME [ s ]
let e_none ?loc () : expression = e_constant ?loc C_NONE []
let e_string_cat ?loc sl sr : expression = e_constant ?loc C_CONCAT [ sl; sr ]

let e_map_add ?loc k v old : expression =
  e_constant ?loc C_MAP_ADD [ k; v; old ]


let e_bool b : expression =
  if b
  then e_constructor (Label "True") (e_ascription (e_unit ()) (t_unit ()))
  else e_constructor (Label "False") (e_ascription (e_unit ()) (t_unit ()))


let get_e_int (t : expression) =
  match t.expression_content with
  | E_literal (Literal_int n) -> Some n
  | _ -> None


let get_e_string (t : expression) =
  match t.expression_content with
  | E_literal (Literal_string s) -> Some (Ligo_string.extract s)
  | _ -> None


let get_e_verbatim (t : expression) =
  match t.expression_content with
  | E_literal (Literal_string (Verbatim v)) -> Some v
  | _ -> None


let get_e_unit (t : expression) =
  match t.expression_content with
  | E_literal Literal_unit -> Some ()
  | _ -> None


let get_e_pair t =
  match t with
  | E_tuple [ fst_expr; snd_expr ] -> Some (fst_expr, snd_expr)
  | _ -> None


let get_e_list t =
  let rec aux t =
    match t with
    | E_constant { cons_name = C_CONS; arguments = [ key; lst ] } ->
      let lst = aux lst.expression_content in
      Some key :: lst
    | E_constant { cons_name = C_LIST_EMPTY; arguments = [] } -> []
    | _ -> [ None ]
  in
  Option.all @@ aux t


let get_e_tuple t =
  match t with
  | E_tuple exprs -> Some exprs
  | _ -> None


let get_record_field_type (t : type_expression) (label : Label.t)
    : type_expression option
  =
  match get_t_record t with
  | None -> None
  | Some struct_ ->
    (match Map.find struct_.fields label with
    | None -> None
    | Some row_element -> Some row_element.associated_type)


let get_e_ascription a =
  match a with
  | E_ascription { anno_expr; type_annotation } ->
    Some (anno_expr, type_annotation)
  | _ -> None


let get_type_abstractions (e : expression) =
  let rec aux tv e =
    match get_e_type_abstraction e with
    | None -> tv, e
    | Some { type_binder; result } -> aux (type_binder :: tv) result
  in
  aux [] e


let get_e_map : expression -> (expression * expression) list option =
 fun expr ->
  let exception Not_value in
  let rec loop expr =
    match expr.expression_content with
    | E_big_map map | E_map map -> map
    | E_constant { cons_name = C_UPDATE | C_MAP_ADD; arguments = [ k; v; map ] }
      ->
      let map = loop map in
      (k, v) :: map
    | E_constant { cons_name = C_MAP_EMPTY | C_BIG_MAP_EMPTY; arguments = [] }
      -> []
    | _ -> raise Not_value
  in
  match loop expr with
  | result -> Some result
  | exception Not_value -> None
