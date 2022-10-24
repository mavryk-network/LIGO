open Types
open Simple_utils


type expression_content = [%import: Types.expression_content]
[@@deriving ez {
      prefixes = [
        ("make_e" , fun ?(location = Location.generated) expression_content type_expression ->
                  ({ expression_content ; location ; type_expression } : expression)) ;
        ("get" , fun x -> x.expression_content) ;
        ("get_type" , fun x -> x.type_expression) ;
      ] ;
      wrap_constructor = ("expression_content" , make_e) ;
      wrap_get = ("expression_content" , get) ;
    } ]

type type_content = [%import: Types.type_content]
[@@deriving ez {
      prefixes = [
        ("make_t" , fun ?(loc = Location.generated) ?source_type type_content ->
                  ({ type_content ; location = loc ; orig_var = None ; source_type } : type_expression)) ;
        ("get" , fun x -> x.type_content) ;
      ] ;
      wrap_constructor = ("type_content" , (fun type_content ?loc ?source_type () -> make_t ?loc ?source_type type_content)) ;
      wrap_get = ("type_content" , get) ;
      default_get = `Option ;
    } ]

open Ligo_prim
open Literal_types

let t_constant ?loc injection parameters : type_expression =
  t_constant ?loc {language=Backend.Michelson.name; injection  ; parameters} ()
let get_t_inj (t:type_expression) (v:Ligo_prim.Literal_types.t) : (type_expression list) option =
  match t.type_content with
  | T_constant {language=_;injection; parameters} when Ligo_prim.Literal_types.equal injection v -> Some parameters
  | _ -> None
let get_t_unary_inj (t:type_expression) (v:Ligo_prim.Literal_types.t) : type_expression option =
  match get_t_inj t v with
  | Some [a] -> Some a
  | _ -> None
let t__type_ ?loc () : type_expression = t_constant ?loc _type_ []
[@@map (_type_, ("signature","chain_id", "string", "bytes", "key", "key_hash", "int", "address", "operation", "nat", "tez", "timestamp", "unit", "bls12_381_g1", "bls12_381_g2", "bls12_381_fr", "never", "mutation", "pvss_key", "baker_hash", "chest_key", "chest"))]
let get_t__type_ (t : type_expression) : type_expression option = get_t_unary_inj t _type_
[@@map (_type_, ("contract", "list", "set", "ticket", "sapling_state", "sapling_transaction", "gen"))]
let default_layout = Layout.L_tree

let t_record ?loc ~layout fields  : type_expression = make_t ?loc (T_record {fields;layout})
let make_t_ez_record ?loc ?(layout=default_layout) (lst:(string * type_expression) list) : type_expression =
  let lst = List.mapi ~f:(fun i (x,y) -> (Label.of_string x, ({associated_type=y;michelson_annotation=None;decl_pos=i} : row_element)) ) lst in
  let map = Record.of_list lst in
  t_record ?loc ~layout map

let get_a_string (t:expression) =
  match t.expression_content with
  | E_literal (Literal_string s) -> Some (Ligo_string.extract s)
  | _ -> None

let e_a_variable v ty = e_variable v ty

let get_t_option (t:type_expression) : type_expression option =
  let l_none = Label.of_string "None" in
  let l_some = Label.of_string "Some" in
  match t.type_content with
  | T_sum {fields;_} ->
    let keys = Record.LMap.keys fields in
    (match keys with
      [a ; b] when (Label.equal a l_none && Label.equal b l_some)
    || (Label.equal a l_some && Label.equal b l_none) ->
      let some = Record.LMap.find l_some fields in
      Some some.Rows.associated_type
    | _ -> None)
  | _ -> None

let e_a_let_mut_in x =
  e_let_mut_in x (get_type x.let_result)

let get_sum_type (t : type_expression) (label : Label.t) : type_expression =
  match get_t_sum t with
  | None -> failwith "pattern expanded: could not get sum type"
  | Some struct_ ->
    match Record.LMap.find_opt label struct_.fields with
    | None -> failwith "pattern expanded: could not get row from its label"
    | Some row_element -> row_element.associated_type