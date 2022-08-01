(* open Types
open Compare_enum

type 'a comparator = 'a -> 'a -> int
let (<?) ca cb = if ca = 0 then cb () else ca

let cmp2 f a1 b1 g a2 b2 = match f a1 b1 with 0 -> g a2 b2 | c -> c
let cmp3 f a1 b1 g a2 b2 h a3 b3 = match f a1 b1 with 0 -> (match g a2 b2 with 0 -> h a3 b3 | c -> c) | c -> c

let label (Label a) (Label b) = String.compare a b
let label_map ~compare lma lmb =
  let ra = LMap.to_kv_list_rev lma in
  let rb = LMap.to_kv_list_rev lmb in
  let aux (la,a) (lb,b) =
    cmp2 label la lb compare a b in
  List.compare aux ra rb

let expression_variable = ValueVar.compare
let type_variable       = TypeVar.compare
let module_variable     = ModuleVar.compare

let module_access f {module_path=mna; element=ea}
                    {module_path=mnb; element=eb} =
  cmp2
    (List.compare module_variable) mna mnb
    f ea eb

let layout_tag = function
  | L_comb -> 1
  | L_tree -> 2

let layout a b = Int.compare (layout_tag a) (layout_tag b)

let rec term_expression_tag ty_cont =
  match ty_cont with
  | T_variable _ -> 1
  | T_literal _ -> 2
  | T_constant _ -> 3
  | T_application _ -> 4
  | T_lambda _ -> 5
  | T_recursive _ -> 6
  | T_let_in _ -> 7
  | T_mod_in _ -> 8
  | T_module_accessor _ -> 9
  | T_raw_code _ -> 10
  | T_constructor _ -> 11
  | T_matching _ -> 12
  | T_record _ -> 13
  | T_record_accessor _ -> 14
  | T_record_update _ -> 15
  | T_ascription _ -> 16
  | T_assign _ -> 17
  | T_sum _ -> 18
  | T_prod _ -> 19
  | T_arrow _ -> 20
  | T_type -> 21
  | T_pi _ -> 22

and term a b =
  term_content a.term_content b.term_content

and term_content a b =
  match a, b with
  | T_variable a, T_variable b -> term_variable a b
  | T_literal a, T_literal b -> literal a b
  | T_constant a, T_constant b -> constant a b
  | T_application a, T_application b -> application a b
  | T_lambda a, T_lambda b -> lambda a b
  | T_recursive a, T_recursive b -> recursive a b
  | T_let_in a, T_let_in b -> let_in a b
  | T_mod_in a, T_mod_in b -> mod_in a b
  | T_module_accessor a, T_module_accessor b -> module_access a b
  | T_raw_code a, T_raw_code b -> raw_code a b
  | T_constructor a, T_constructor b -> constructor a b
  | T_matching a, T_matching b -> matching a b
  | T_record a, T_record b -> term_label_map a b
  | T_record_accessor a, T_record_accessor b -> record_accessor a b 
  | T_record_update a, T_record_update b -> record_update a b
  | T_ascription a, T_ascription b -> ascription a b
  | T_assign a, T_assign b -> assign a b
  | T_sum      a, T_sum      b -> rows a b
  | T_prod a, T_prod   b -> rows a b
  | T_arrow a, T_arrow    b -> arrow a b
  | T_module_accessor a, T_module_accessor b -> module_access type_variable a b
  | T_type, T_type -> 0
  | T_pi a, T_pi b -> pi a b
  | a, b ->
    Int.compare (term_expression_tag a) (term_expression_tag b)


and rows {fields=ca; layout=la} {fields=cb; layout=lb} =
  cmp2
    (label_map ~compare:row_element) ca cb
    (Option.compare layout) la lb

and row_element {associated_type=aa;michelson_annotation=ma;decl_pos=da} {associated_type=ab;michelson_annotation=mb;decl_pos=db} =
  cmp3
    type_expression aa ab
    (Option.compare String.compare) ma mb
    Int.compare     da db

and arrow {type1=ta1;type2=tb1} {type1=ta2;type2=tb2} =
  cmp2
    type_expression ta1 ta2
    type_expression tb1 tb2

and app {type_operator=ta;arguments=aa} {type_operator=tb;arguments=ab} =
  cmp2
    type_variable ta tb
    (List.compare type_expression) aa ab

and for_all {ty_binder = ba ; kind = _ ; type_ = ta } {ty_binder = bb ; kind = _ ; type_ = tb } =
  cmp2
    type_expression ta tb
    type_variable ba bb *)
