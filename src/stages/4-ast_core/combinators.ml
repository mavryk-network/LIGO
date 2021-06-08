open Types
module S = Ast_sugar
open Stage_common.Constant

let make_t ?(loc = Location.generated) ?sugar type_content = {type_content; location=loc; sugar }
let make_e ?(loc = Location.generated) ?sugar expression_content = {  
  expression_content ;
  location = loc ;
  sugar ;
  }
let t_variable   ?loc ?sugar t  : type_expression = make_t ?loc ?sugar (T_variable t)

let t_constant ?loc ?sugar type_operator arguments : type_expression =
  make_t ?loc ?sugar (T_app {type_operator=make_t @@  T_variable (Var.of_name type_operator);arguments})

  (*X_name here should be replaced by X_injection*)
let t_signature  ?loc ?sugar () : type_expression = t_constant ?loc ?sugar signature_name []
let t_chain_id   ?loc ?sugar () : type_expression = t_constant ?loc ?sugar chain_id_name []
let t_string     ?loc ?sugar () : type_expression = t_constant ?loc ?sugar string_name []
let t_bytes      ?loc ?sugar () : type_expression = t_constant ?loc ?sugar bytes_name []
let t_key        ?loc ?sugar () : type_expression = t_constant ?loc ?sugar key_name []
let t_key_hash   ?loc ?sugar () : type_expression = t_constant ?loc ?sugar key_hash_name []
let t_int        ?loc ?sugar () : type_expression = t_constant ?loc ?sugar int_name []
let t_address    ?loc ?sugar () : type_expression = t_constant ?loc ?sugar address_name []
let t_operation  ?loc ?sugar () : type_expression = t_constant ?loc ?sugar operation_name []
let t_nat        ?loc ?sugar () : type_expression = t_constant ?loc ?sugar nat_name []
let t_mutez      ?loc ?sugar () : type_expression = t_constant ?loc ?sugar tez_name []
let t_timestamp  ?loc ?sugar () : type_expression = t_constant ?loc ?sugar timestamp_name []
let t_unit       ?loc ?sugar () : type_expression = t_constant ?loc ?sugar unit_name []
let t_bls12_381_g1 ?loc ?sugar () : type_expression = t_constant ?loc ?sugar bls12_381_g1_name []
let t_bls12_381_g2 ?loc ?sugar () : type_expression = t_constant ?loc ?sugar bls12_381_g2_name []
let t_bls12_381_fr ?loc ?sugar () : type_expression = t_constant ?loc ?sugar bls12_381_fr_name []


let t_option         ?loc ?sugar o   : type_expression = t_constant ?loc ?sugar option_name [o]
let t_list           ?loc ?sugar t   : type_expression = t_constant ?loc ?sugar list_name [t]
let t_set            ?loc ?sugar t   : type_expression = t_constant ?loc ?sugar set_name [t]
let t_contract       ?loc ?sugar t   : type_expression = t_constant ?loc ?sugar contract_name [t]
let t_ticket         ?loc ?sugar t   : type_expression = t_constant ?loc ?sugar ticket_name [t]
let t_map            ?loc ?sugar k v : type_expression = t_constant ?loc ?sugar map_name [ k ; v ]
let t_big_map        ?loc ?sugar k v : type_expression = t_constant ?loc ?sugar big_map_name [ k ; v ]
let t_map_or_big_map ?loc ?sugar k v : type_expression = t_constant ?loc ?sugar map_or_big_map_name [ k ; v ]

let k_axiom ?loc ?sugar ?(language=Stage_common.Constant.michelson_injection_name) injection kind =
  ignore(loc, sugar); (* TODO: use these once kinds are merged with types *)
  K_axiom {
    language;
    injection = Simple_utils.Ligo_string.verbatim injection;
    kind;
  }

let t_axiom ?loc ?sugar ?(language=Stage_common.Constant.michelson_injection_name) injection kind = make_t ?loc ?sugar @@ T_axiom {
  language;
  injection = Simple_utils.Ligo_string.verbatim injection;
  kind;
}

let is_t_axiom name = function 
  { type_content = T_axiom { language=_; injection = inj; kind=_ }; sugar=_; location=_ }
  when String.equal (Ligo_string.extract inj) name -> true
  | _ -> false

let (>?) x f = match x with Some a -> f a | None -> None

let (@*) kind_operator kind_arguments =           K_app { kind_operator; kind_arguments; }
let (@.) type_operator      arguments = make_t @@ T_app { type_operator;      arguments; }
let is_type_expression f { type_content; sugar=_; location=_ } = f type_content

module Star_ = struct
  (* kind Star = axiom "k_star_name" : K_higher *)
  let k_name = Stage_common.Constant.k_star_name
  let k_constructor ?loc ?sugar () = k_axiom ?loc ?sugar k_name K_higher
  (* The kind of simple types like nat, string, … *)
  let k ?loc ?sugar () = k_constructor ?loc ?sugar ()
  let k_ = k ()
end

module Arrow_ = struct
  (* kind (-*>) = axiom "arrow_name" : K_higher (* takes two arguments *) *)
  let k_name = Stage_common.Constant.arrow_name
  let k_constructor ?loc ?sugar () = k_axiom ?loc ?sugar k_name K_higher
  let k ?loc ?sugar a b = k_constructor ?loc ?sugar () @* [ a; b ]
  let (-*>) = k

  (* type (-.>) = axiom "arrow_name" : Star -*> Star -*> Star
     Usage: type t : Star = (int : Star) -.> (string : Star) *)
  let t_name = Stage_common.Constant.arrow_name
  let t_constructor ?loc ?sugar () = t_axiom ?loc ?sugar t_name (Star_.k_ -*> Star_.k_ -*> Star_.k_)
  let t ?loc ?sugar a b = t_constructor ?loc ?sugar () @. [ a; b ]
  let (-.>) = t
  let get_t = is_type_expression @@ function
  | T_app { type_operator; arguments=[arg; ret] }
  when is_t_axiom Stage_common.Constant.arrow_name type_operator -> Some (arg, ret)
  | _ -> None
end

let (-.>) = Arrow_.(-.>)
let (-*>) = Arrow_.(-*>)

module String_ = struct
  (* kind k_type_level_string = axiom "string" : K_higher *)
  let k_name = Stage_common.Constant.string_name
  (* the kind of type-level strings *)
  let k_constructor ?loc ?sugar () = k_axiom ?loc ?sugar k_name K_higher
  let k ?loc ?sugar () = k_axiom ?loc ?sugar k_name K_higher
  let k_ = k ()
  (* type type_level_string s = BUILTIN : k_type_level_string *)
  let t s = make_t @@ T_string s
  let get_t = is_type_expression @@ function T_string s -> Some s | _ -> None
end

module Int_ = struct
  (* the kind of type-level integers *)
  (* kind k_type_level_int = axiom "int" : K_higher *)
  let k_name = Stage_common.Constant.int_name
  let k_constructor ?loc ?sugar () = k_axiom ?loc ?sugar k_name K_higher
  let k ?loc ?sugar () = k_constructor ?loc ?sugar ()
  let k_ = k ()
  (* type type_level_int s = BUILTIN : k_type_level_int *)
  let t ?loc ?sugar i = make_t ?loc ?sugar @@ T_int i
  let get_t = is_type_expression @@ function T_int i -> Some i | _ -> None
end

module List_ = struct
  let k_name = Stage_common.Constant.list_name
  let k_constructor ?loc ?sugar () = k_axiom ?loc ?sugar k_name K_higher
  (* The kind of type-level lists *)
  let k a = k_constructor () @* [a]
  let t ?loc ?sugar a = t_axiom ?loc ?sugar Stage_common.Constant.list_name (k a)

  let get_t = is_type_expression @@ function
    | T_app { type_operator; arguments } when is_t_axiom Stage_common.Constant.list_name type_operator -> Some arguments
    | _ -> None
end

module Pair_ = struct
  let k_name = Stage_common.Constant.michelson_pair_name
  let k_constructor ?loc ?sugar () = k_axiom ?loc ?sugar k_name K_higher
  let k ?loc ?sugar a b = k_constructor ?loc ?sugar () @* [a; b]
  let t_name = Stage_common.Constant.michelson_pair_name
  let t_constant ?loc ?sugar () = t_axiom ?loc ?sugar t_name K_higher
  let t = t_constant ()
end

module Label_ = struct
  let k_name = Stage_common.Constant.label_name
  let k_constructor ?loc ?sugar () = k_axiom ?loc ?sugar k_name K_higher
  let k ?loc ?sugar () = k_constructor ?loc ?sugar ()
  let k_ = k ()
  let t ?loc ?sugar l = make_t ?loc ?sugar @@ T_label l
  let get_t = is_type_expression @@ function T_label l -> Some l | _ -> None
end

module Option_ = struct
  let k_constructor = k_axiom Stage_common.Constant.option_name K_higher

  (* The kind of type-level options *)
  let k a = k_constructor @* [a]
  (* type t : type_level_option k_int = type_level_some k_int 42
     type u : type_level_option k_int = type_level_none k_int *)
  let t_constructor_some a = t_axiom Stage_common.Constant.some_name (k a)
  let t_some a x = t_constructor_some a @. [x]
  let t_constructor_none a = t_axiom Stage_common.Constant.none_name (k a)
  let t_none a = t_constructor_none a
  let t a x = match x with Some x -> t_some a x | None -> t_none a
  let get_t = is_type_expression @@ function
    | T_app { type_operator; arguments=[x] } when is_t_axiom Stage_common.Constant.some_name type_operator -> Some (Some x)
    | T_app { type_operator; arguments=[]  } when is_t_axiom Stage_common.Constant.none_name type_operator -> Some (None)
    | _ -> None
end

(* some type used for combinators only *)
type layout =
| L_comb
| L_tree

type row_ = { fields : ty_expr row_element label_map ; layout : layout option }


module Layout_ = struct
  let k_name = Stage_common.Constant.layout_name
  let k_constructor = k_axiom k_name K_higher
  let k () = k_constructor
  let k_ = k ()
  let t_comb = t_axiom Stage_common.Constant.layout_comb_name k_
  let t_tree = t_axiom Stage_common.Constant.layout_tree_name k_
  let t (layout : layout option) (* : Option_.k Layout_.k_ *) =
    (* TODO: probably use a layout variable instead of None *)
    match layout with
    | Some L_comb -> Option_.t_some k_ t_comb
    | Some L_tree -> Option_.t_some k_ t_tree
    | None -> Option_.t_none k_
  let get_t x = 
    Option_.get_t x >?
    (function
      | Some x ->
        if      is_t_axiom Stage_common.Constant.layout_comb_name x then Some (Some L_comb)
        else if is_t_axiom Stage_common.Constant.layout_tree_name x then Some (Some L_tree)
        else                                                             Some (None)
      | None -> None)
end

module Row_element_ = struct
  let k_constructor = k_axiom Stage_common.Constant.row_element_name K_higher
  let k () = k_constructor
  let k_ = Label_.k_ -*> Star_.k_ -*> List_.k String_.k_ -*> Int_.k_ -*> k ()
  let t = t_axiom Stage_common.Constant.row_element_name k_
  let t_ label associated_type attributes decl_pos =
    t @. [label; associated_type; attributes; decl_pos]

  let get_t = is_type_expression @@ function
    | T_app { type_operator; arguments = [label; associated_type; attributes; decl_pos] }
      when is_t_axiom Stage_common.Constant.row_element_name type_operator ->
      let label = Label_.get_t label in
      let aux = fun x -> List.map String_.get_t x |> Base.Option.all in
      let attributes = List_.get_t attributes >? aux in
      let decl_pos = Int_.get_t decl_pos in
      (match label, associated_type, attributes, decl_pos with
         Some label, associated_type, Some attributes, Some decl_pos -> Some (label, associated_type, attributes, decl_pos)
       | _ -> None)
    | _ -> None
end

module Field_ = struct
  let t label associated_type attributes decl_pos =
    Row_element_.t_
      (Label_.t label)
      associated_type
      (List_.t String_.k_ @. (List.map (fun a -> String_.t a) attributes))
      (Int_.t decl_pos)
  let get_t : type_expression -> _ option =
    (* TODO: (label,{associated_type; michelson_annotation; decl_pos}) *)
    Row_element_.get_t
end

module Fields_ = struct
  let t fields =
    let fields = LMap.to_kv_list fields in
    let fields = List.map (fun (label,{associated_type; attributes; decl_pos}) ->
                            Field_.t label associated_type attributes decl_pos) fields in
    List_.t Row_element_.k_ @. fields

  let get_t : type_expression -> _ option =
    fun x -> x |> List_.get_t >? (fun l -> List.map Field_.get_t l |> Base.Option.all)
end

module Row_ = struct

  let k_name = Stage_common.Constant.row_name
  let k_constructor = k_axiom k_name K_higher
  let k = List_.k Row_element_.k_ -*> Layout_.k_ -*> k_constructor
  let t_name = Stage_common.Constant.row_name
  let t_constructor = t_axiom t_name k
  let t elements layout = t_constructor @. [elements; layout]
  let t_ ?(layout : layout option) (fields : ty_expr row_element label_map) : type_expression =
    t (Fields_.t fields) (Layout_.t layout)
  let get_t : _ -> row_ option = is_type_expression @@ function
  | T_app { type_operator = row_op; arguments = [elements; layout]}
    when is_t_axiom Stage_common.Constant.row_name row_op ->
    let elements = List_.get_t elements >? (fun l -> List.map Row_element_.get_t l |> Base.Option.all) in
    let layout = Layout_.get_t layout in
    let aux = fun (a,associated_type,attributes,decl_pos) -> (a,{associated_type;attributes;decl_pos}) in
    (match elements, layout with
     | Some fields, Some layout -> Some { fields = LMap.of_list (List.map aux fields); layout }
     | _ -> None)
  | _ -> None
end

module Record_ = struct
  let k_name = Stage_common.Constant.record_name
  let k_constructor = k_axiom k_name K_higher
  let k = Row_.k -*> k_constructor
  let t_name = Stage_common.Constant.record_name
  let t_constructor ?loc ?sugar () = t_axiom ?loc ?sugar t_name k
  let t ?loc ?sugar row = t_constructor ?loc ?sugar () @. [ row ]
  let t_ ?loc ?sugar ?(layout : layout option) (fields : ty_expr row_element label_map) : type_expression =
    t ?loc ?sugar @@ Row_.t_ ?layout fields
  let get_t : _ -> _ option = is_type_expression @@ function
  | T_app { type_operator = record_op; arguments = [row]}
    when is_t_axiom Stage_common.Constant.record_name record_op ->
    Row_.get_t row
  | _ -> None
end

module Sum_ = struct
  let k_name = Stage_common.Constant.sum_name
  let k_constructor = k_axiom k_name K_higher
  let k = Row_.k -*> k_constructor
  let t_name = Stage_common.Constant.sum_name
  let t_constructor ?loc ?sugar () = t_axiom ?loc ?sugar t_name k
  let t ?loc ?sugar row = t_constructor ?loc ?sugar () @. [ row ]
  let t_ ?loc ?sugar ?(layout : layout option) (fields : ty_expr row_element label_map) : type_expression =
    t ?loc ?sugar @@ Row_.t_ ?layout fields
  let get_t : _ -> _ option = is_type_expression @@ function
  | T_app { type_operator = sum_op; arguments = [row]}
    when is_t_axiom Stage_common.Constant.sum_name sum_op ->
    Row_.get_t row
  | _ -> None
end

let t_record ?loc ?sugar ?(layout : layout option) (fields : ty_expr row_element label_map) = Record_.t_ ?loc ?sugar ?layout fields
let t_sum ?loc ?sugar ?(layout : layout option) (fields : ty_expr row_element label_map) = Sum_.t_ ?loc ?sugar ?layout fields

let default_layout = L_tree
let make_t_ez_record ?loc ?sugar ?layout (lst:(string * type_expression) list) : type_expression =
  let lst = List.mapi (fun i (x,y) -> (Label x, ({associated_type=y;attributes=[];decl_pos=i} : ty_expr row_element)) ) lst in
  let map = LMap.of_list lst in
  t_record ?loc ?sugar ?layout map

let ez_t_record ?loc ?sugar ?(layout=default_layout) lst : type_expression =
  let m = LMap.of_list lst in
  t_record ?loc ?sugar ~layout m
let t_pair ?loc ?sugar a b : type_expression =
  ez_t_record ?loc ?sugar [
    (Label "0",{associated_type=a;attributes=[] ; decl_pos = 0}) ;
    (Label "1",{associated_type=b;attributes=[] ; decl_pos = 1}) ]

let t_sum ?loc ?sugar ?layout fields : type_expression = t_sum ?loc ?sugar fields ?layout
let t_sum_ez ?loc ?sugar ?layout (lst:(string * type_expression) list) : type_expression =
  let lst = List.mapi (fun i (x,y) -> (Label x, ({associated_type=y;attributes=[];decl_pos=i}:ty_expr row_element)) ) lst in
  let map = LMap.of_list lst in
  t_sum ?loc ?sugar ?layout map

let t_bool ?loc ?sugar ()       : type_expression = t_sum_ez ?loc ?sugar
  [("true", t_unit ());("false", t_unit ())]

let t_function ?loc ?sugar param result : type_expression = Arrow_.t ?loc ?sugar param result
let t_shallow_closure ?loc ?sugar param result: type_expression = Arrow_.t ?loc ?sugar param result

let get_type' (x:type_expression) = x.type_content
let get_expression (x:expression) = x.expression_content

let get_lambda e : (_,_) lambda option = match e.expression_content with
  | E_lambda l -> Some l
  | _ -> None

let get_t_bool (t:type_expression) : unit option = match t.type_content with
  | t when (compare t (t_bool ()).type_content) = 0-> Some ()
  | _ -> None


let tuple_of_record ({fields; layout = _} : row_) : type_expression list option =
  (* Returns Some [type_expression;…] if the row uses only numeric strings as indices, None otherwise *)
  let l = List.map (fun i -> LMap.find_opt (Label (string_of_int i)) fields) (Base.List.range 0 (LMap.cardinal fields)) in
  Base.Option.all l >? (fun l -> Some (List.map (fun { associated_type; attributes=_; decl_pos=_ } -> associated_type) l))

let get_t_tuple (t:type_expression) : type_expression list option = match Record_.get_t t with
  | Some row ->
    tuple_of_record row
  | None -> None

let get_t_pair (t:type_expression) : (type_expression * type_expression) option = match Record_.get_t t >? tuple_of_record with
  | Some [a; b] -> Some (a, b)
  | _ -> None

let get_t_function (t:type_expression) : (type_expression * type_expression) option =
  Arrow_.get_t t >? fun (arg, ret) -> Some (arg, ret)

let get_t_function_exn t = match get_t_function t with
  | Some x -> x
  | None -> raise (Failure ("Internal error: broken invariant at " ^ __LOC__))

let get_t_sum : type_expression -> row_ option = Sum_.get_t

let get_t_sum_exn : type_expression -> row_ = fun t ->
  Base.Option.value_exn ~message:("Internal error: broken invariant at " ^ __LOC__) (Sum_.get_t t)
let get_t_record : type_expression -> row_ option = Record_.get_t

let e_record map : expression = make_e @@ E_record map
let ez_e_record (lst : (label * expression) list) : expression =
  let aux prev (k, v) = LMap.add k v prev in
  let map = List.fold_left aux LMap.empty lst in
  e_record map

let e_var       ?loc ?sugar n  : expression = make_e ?loc ?sugar @@ E_variable (Location.wrap ?loc (Var.of_name n))
let e_literal   ?loc ?sugar l  : expression = make_e ?loc ?sugar @@ E_literal l
let e_unit      ?loc ?sugar () : expression = make_e ?loc ?sugar @@ E_literal (Literal_unit)
let e_int       ?loc ?sugar n  : expression = make_e ?loc ?sugar @@ E_literal (Literal_int n)
let e_nat       ?loc ?sugar n  : expression = make_e ?loc ?sugar @@ E_literal (Literal_nat n)
let e_timestamp ?loc ?sugar n  : expression = make_e ?loc ?sugar @@ E_literal (Literal_timestamp n)
let e_string    ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_string s)
let e_address   ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_address s)
let e_mutez     ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_mutez s)
let e_signature ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_signature s)
let e_key       ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_key s)
let e_key_hash  ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_key_hash s)
let e_chain_id  ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_chain_id s)
let e_operation ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_operation s)
let e'_bytes b : expression_content =
  let bytes = Hex.to_bytes (`Hex b) in
  E_literal (Literal_bytes bytes)
let e_bytes_hex ?loc ?sugar b : expression =
  let e' = e'_bytes b in
  make_e ?loc ?sugar e'
let e_bytes_raw ?loc ?sugar (b: bytes) : expression =
  make_e ?loc ?sugar @@ E_literal (Literal_bytes b)
let e_bytes_string ?loc ?sugar (s: string) : expression =
  make_e ?loc ?sugar @@ E_literal (Literal_bytes (Hex.to_bytes (Hex.of_string s)))
let e_some       ?loc ?sugar s        : expression = make_e ?loc ?sugar @@ E_constant {cons_name = C_SOME; arguments = [s]}
let e_none       ?loc ?sugar ()       : expression = make_e ?loc ?sugar @@ E_constant {cons_name = C_NONE; arguments = []}
let e_string_cat ?loc ?sugar sl sr    : expression = make_e ?loc ?sugar @@ E_constant {cons_name = C_CONCAT; arguments = [sl ; sr ]}
let e_map_add    ?loc ?sugar k v old  : expression = make_e ?loc ?sugar @@ E_constant {cons_name = C_MAP_ADD; arguments = [k ; v ; old]}

let e_constant    ?loc ?sugar name lst             = make_e ?loc ?sugar @@ E_constant {cons_name=name ; arguments = lst}
let e_variable v            : expression = make_e @@ E_variable v
let e_application lamb args : expression = make_e @@ E_application {lamb;args}
let e_lambda      ?loc ?sugar binder output_type result            = make_e ?loc ?sugar @@ E_lambda {binder; output_type; result ;  }
let e_lambda_ez   ?loc ?sugar var ?ascr ?const_or_var output_type result         = e_lambda ?loc ?sugar {var;ascr;attributes={const_or_var}} output_type result
let e_recursive   ?loc ?sugar fun_name fun_type lambda             = make_e ?loc ?sugar @@ E_recursive {fun_name; fun_type; lambda}
let e_let_in      ?loc ?sugar let_binder rhs let_result inline = make_e ?loc ?sugar @@ E_let_in { let_binder ; rhs ; let_result; inline }
let e_let_in_ez   ?loc ?sugar var ?ascr ?const_or_var inline rhs let_result = e_let_in ?loc ?sugar {var;ascr;attributes={const_or_var}} rhs let_result inline
let e_type_in type_binder rhs let_result = make_e @@ E_type_in { type_binder ; rhs ; let_result }
let e_mod_in      ?loc ?sugar module_binder rhs let_result         = make_e ?loc ?sugar @@ E_mod_in { module_binder ; rhs ; let_result }
let e_mod_alias  ?loc ?sugar  alias binders result = make_e ?loc ?sugar @@ E_mod_alias { alias ; binders ; result }
let e_raw_code    ?loc ?sugar language code                       = make_e ?loc ?sugar @@ E_raw_code {language; code}

let e_constructor constructor element: expression = make_e @@ E_constructor {constructor;element}
let e_matching    ?loc ?sugar a b : expression = make_e ?loc ?sugar @@ E_matching {matchee=a;cases=b}


let e_record          ?loc ?sugar map                = make_e ?loc ?sugar @@ E_record map
let e_record_accessor ?loc ?sugar record path        = make_e ?loc ?sugar @@ E_record_accessor ({record; path} : _ record_accessor)
let e_record_update   ?loc ?sugar record path update = make_e ?loc ?sugar @@ E_record_update ({record; path; update} : _ record_update)

let e_module_accessor ?loc ?sugar module_name element = make_e ?loc ?sugar @@ E_module_accessor {module_name;element}

let e_ascription ?loc ?sugar anno_expr type_annotation  : expression = make_e ?loc ?sugar @@ E_ascription {anno_expr;type_annotation}

let e_bool b : expression = e_constructor (Label (string_of_bool b)) (e_ascription (e_unit ())(t_unit()))

let get_e_int (t:expression) =
  match t.expression_content with
  | E_literal (Literal_int n) -> Some n
  | _ -> None

let get_e_string (t:expression) =
  match t.expression_content with
  | E_literal (Literal_string s) -> Some (Ligo_string.extract s)
  | _ -> None

let get_e_verbatim (t:expression) =
  match t.expression_content with
    E_literal (Literal_string (Verbatim v)) -> Some v
  | _ -> None

let get_e_unit (t:expression) =
  match t.expression_content with
  | E_literal (Literal_unit) -> Some ()
  | _ -> None

let get_e_bool (t:expression) =
  match t.expression_content with
  | E_constructor {constructor=Label name;element}
    when (String.equal name "true" || String.equal name "false")
    && element.expression_content = (e_unit ()).expression_content ->
      Some (bool_of_string name)
  | _ -> None


let get_e_pair = fun t ->
  match t with
  | E_record r -> (
  let lst = LMap.to_kv_list_rev r in
    match lst with
    | [(Label "O",a);(Label "1",b)]
    | [(Label "1",b);(Label "0",a)] ->
        Some (a , b)
    | _ -> None
    )
  | _ -> None

let get_e_list = fun t ->
  let rec aux t =
    match t with
      E_constant {cons_name=C_CONS;arguments=[key;lst]} ->
        let lst = aux lst.expression_content in
        (Some key)::(lst)
    | E_constant {cons_name=C_LIST_EMPTY;arguments=[]} ->
        []
    | _ -> [None]
  in
  let opts = aux t in
  if List.exists (Option.is_none) opts then None
  else Some (List.map Option.unopt_exn opts)

let get_e_tuple = fun t ->
  match t with
  | E_record r -> Some (List.map snd @@ Helpers.tuple_of_record r)
  | _ -> None

let get_e_record = fun t ->
  match t.expression_content with
  | E_record record -> Some record
  | _ -> None

let get_e_record_accessor = fun t ->
  match t.expression_content with
  | E_record_accessor {record; path} -> Some (record, path)
  | _ -> None

let get_declaration_by_name : module_ -> string -> declaration option = fun (p) name ->
  let aux : declaration -> bool = fun declaration ->
    match declaration with
    | Declaration_constant { name = name'; binder = _ ; expr=_ ; attr=_ } ->
      (match name' with
       | None -> false
       | Some name' -> String.equal name' name)
    | Declaration_type   _
    | Declaration_module _
    | Module_alias       _ -> false
  in
  List.find_opt aux @@ List.map Location.unwrap p

let get_record_field_type (t : type_expression) (label : label) : type_expression option =
  match get_t_record t with
  | None -> None
  | Some record ->
    match LMap.find_opt label record.fields with
    | None -> None
    | Some row_element -> Some row_element.associated_type

let get_e_ascription = fun a ->
  match a with
  | E_ascription {anno_expr; type_annotation} -> Some (anno_expr,type_annotation)
  | _ -> None

(* Same as get_e_pair *)
let extract_pair : expression -> (expression * expression) option = fun e ->
  match e.expression_content with
  | E_record r -> (
  let lst = LMap.to_kv_list_rev r in
    match lst with
    | [(Label "O",a);(Label "1",b)]
    | [(Label "1",b);(Label "0",a)] ->
      Some (a , b)
    | _ -> None
    )
  | _ -> None

let extract_record : expression -> (label * expression) list option = fun e ->
  match e.expression_content with
  | E_record lst -> Some (LMap.to_kv_list lst)
  | _ -> None

let extract_map : expression -> (expression * expression) list option = fun e ->
  let rec aux e =
    match e.expression_content with
      E_constant {cons_name=C_UPDATE|C_MAP_ADD; arguments=[k;v;map]} ->
        let map = aux map in
        (Some (k,v))::map
    | E_constant {cons_name=C_MAP_EMPTY|C_BIG_MAP_EMPTY; arguments=[]} -> []
    | _ -> [None]
  in
  let opts = aux e in
  if List.exists (Option.is_none) opts then None
  else Some (List.map Option.unopt_exn opts)

let make_c_constructor_simpl ?(reason_constr_simpl="") id_constructor_simpl original_id tv c_tag tv_list = {
  reason_constr_simpl ;
  id_constructor_simpl = ConstraintIdentifier.T (Int64.of_int id_constructor_simpl) ;
  original_id = Option.map (fun i -> ConstraintIdentifier.T (Int64.of_int i)) original_id;
  tv ;
  c_tag;
  tv_list
}

let make_c_row_simpl ?(reason_row_simpl="") id_row_simpl original_id tv r_tag tv_map_as_lst : c_row_simpl = { 
  reason_row_simpl ;
  id_row_simpl = ConstraintIdentifier.T (Int64.of_int id_row_simpl) ;
  original_id = Option.map (fun i -> ConstraintIdentifier.T (Int64.of_int i)) original_id;
  tv;
  r_tag;
  tv_map = LMap.of_list @@ List.mapi (fun i (k,v) -> (k,{associated_variable=v;michelson_annotation=None;decl_pos=i})) tv_map_as_lst ;
}


(* TODO: remove this () argument, it is just here to make sure that
   the ~bound and ~constraints arguments are given (while adding the
   fields to the record, we need to make sure they're supplied
   everywhere) *)
let make_c_typeclass_simpl ?(reason_typeclass_simpl="") ~bound ~constraints () id_typeclass_simpl original_id args tc : c_typeclass_simpl =
  {
    tc_bound = bound;
    tc_constraints = constraints;
    reason_typeclass_simpl ;
    id_typeclass_simpl = ConstraintIdentifier.T (Int64.of_int id_typeclass_simpl) ;
    original_id = Option.map (fun i -> ConstraintIdentifier.T (Int64.of_int i)) original_id;
    tc ;
    args ;
  }

let make_constructor_or ?(reason_constr_simpl = "") id_constructor_simpl original_id tv c_tag tv_list =
  `Constructor (make_c_constructor_simpl ~reason_constr_simpl id_constructor_simpl original_id tv c_tag tv_list)

let make_row_or ?(reason_row_simpl = "") id_row_simpl original_id tv r_tag tv_map_as_lst : constructor_or_row =
  `Row (make_c_row_simpl ~reason_row_simpl id_row_simpl original_id tv r_tag tv_map_as_lst)

let make_alias ?(reason_alias_simpl="") a b :  type_constraint_simpl = SC_Alias {
  reason_alias_simpl ;
  a ;
  b ;
}

let make_sc_alias ?(reason_alias_simpl="") a b : type_constraint_simpl =
  SC_Alias {
    reason_alias_simpl ;
    a ;
    b ;
  }
let make_sc_constructor ?(reason_constr_simpl="") id_constructor_simpl original_id tv c_tag tv_list : type_constraint_simpl =
  SC_Constructor (make_c_constructor_simpl ~reason_constr_simpl id_constructor_simpl original_id tv c_tag tv_list)
let make_sc_row ?(reason_row_simpl="") id_row_simpl original_id tv r_tag tv_map_as_lst : type_constraint_simpl =
  SC_Row (make_c_row_simpl ~reason_row_simpl id_row_simpl original_id tv r_tag tv_map_as_lst)
(* TODO: remove this () argument, it is just here to make sure that
   the ~bound and ~constraints arguments are given (while adding the
   fields to the record, we need to make sure they're supplied
   everywhere) *)
let make_sc_typeclass ?(reason_typeclass_simpl="") ~bound ~constraints () (tc : typeclass) (args : type_variable list) =
  SC_Typeclass {
    tc_bound = bound;
    tc_constraints = constraints;
    reason_typeclass_simpl ;
    id_typeclass_simpl = ConstraintIdentifier.fresh () ;
    original_id = None ;
    tc ;
    args ;
  }
let make_sc_poly ?(reason_poly_simpl="") (tv:type_variable) (forall:p_forall) =
  SC_Poly {
    reason_poly_simpl ;
    id_poly_simpl = ConstraintIdentifier.fresh () ;
    original_id = None ;
    tv ;
    forall ;
  }
let make_sc_access_label ?(reason_access_label_simpl="") (tv:type_variable) ~(record_type:type_variable) (label:label) =
  SC_Access_label {
    reason_access_label_simpl ;
    id_access_label_simpl = ConstraintIdentifier.fresh () ;
    tv ;
    record_type ;
    label ;
  }
