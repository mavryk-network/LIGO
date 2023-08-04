open Core
open Cst_shared.Fold
open CST
open Region

type 'a fold_control = 'a Cst_shared.Fold.fold_control

type _ sing =
    S_all_cases : all_cases sing
  | S_arguments : arguments sing
  | S_arrow : arrow sing
  | S_attr : Attr.t sing
  | S_attribute : attribute sing
  | S_bin_op : 'a sing -> 'a bin_op sing
  | S_bit_and : bit_and sing
  | S_bit_and_eq : bit_and_eq sing
  | S_bit_neg : bit_neg sing
  | S_bit_or  : bit_or sing
  | S_bit_xor : bit_xor sing
  | S_bit_sl  : bit_sl sing
  | S_bit_sr  : bit_sr sing
  | S_bit_sl_eq : bit_sl_eq sing
  | S_bit_sr_eq : bit_sr_eq sing
  | S_bit_or_eq : bit_or_eq sing
  | S_bit_xor_eq : bit_xor_eq sing
  | S_bool_and : bool_and sing
  | S_bool_neg : bool_neg sing
  | S_bool_or : bool_or sing
  | S_bool_xor : bool_xor sing
  | S_braces : 'a sing -> 'a braces sing
  | S_braces' : 'a sing -> 'a braces' sing
  | S_brackets : 'a sing -> 'a brackets sing
  | S_brackets' : 'a sing -> 'a brackets' sing
  | S_bytes_literal : bytes_literal sing
  | S_cartesian : cartesian sing
  | S_cases : cases sing
  | S_chevrons : 'a sing -> 'a chevrons sing
  | S_chevrons' : 'a sing -> 'a chevrons' sing
  | S_code_inj : code_inj sing
  | S_colon : colon sing
  | S_comma : comma sing
  | S_component : 'a sing -> 'a component sing
  | S_cond_stmt : cond_stmt sing
  | S_contract_of_expr : contract_of_expr sing
  | S_cst : CST.t sing
  | S_ctor : ctor sing
  | S_declaration : declaration sing
  | S_decrement : decrement sing
  | S_directive : Directive.t sing
  | S_div_eq : div_eq sing
  | S_dot : dot sing
  | S_ellipsis : ellipsis sing
  | S_eof : eof sing
  | S_equal : equal sing
  | S_equal_cmp : equal_cmp sing
  | S_expr : expr sing
  | S_field : 'a sing -> 'a field sing
  | S_field_id : field_id sing
  | S_field_name : field_name sing
  | S_field_sep : field_sep sing
  | S_file_path : file_path sing
  | S_for_of_stmt : for_of_stmt sing
  | S_for_stmt : for_stmt sing
  | S_fun_body : fun_body sing
  | S_fun_expr : fun_expr sing
  | S_fun_type : fun_type sing
  | S_fun_type_param : fun_type_param sing
  | S_fun_type_params : fun_type_params sing
  | S_geq : geq sing
  | S_gt : gt sing
  | S_hex : Hex.t sing
  | S_import_alias : import_alias sing
  | S_import_all_as : import_all_as sing
  | S_import_decl : import_decl sing
  | S_import_from : import_from sing
  | S_increment : increment sing
  | S_int_literal : int_literal sing
  | S_int64 : int64 sing
  | S_interface : interface sing
  | S_interface_decl : interface_decl sing
  | S_intf_body : intf_body sing
  | S_intf_const : intf_const sing
  | S_intf_entries : intf_entries sing
  | S_intf_entry : intf_entry sing
  | S_intf_expr : intf_expr sing
  | S_intf_name : intf_name sing
  | S_intf_type : intf_type sing
  | S_kwd_as : kwd_as sing
  | S_kwd_break : kwd_break sing
  | S_kwd_case : kwd_case sing
  | S_kwd_const : kwd_const sing
  | S_kwd_contract_of : kwd_contract_of sing
  | S_kwd_default : kwd_default sing
  | S_kwd_else : kwd_else sing
  | S_kwd_export : kwd_export sing
  | S_kwd_for : kwd_for sing
  | S_kwd_from : kwd_from sing
  | S_kwd_if : kwd_if sing
  | S_kwd_implements : kwd_implements sing
  | S_kwd_import : kwd_import sing
  | S_kwd_interface : kwd_interface sing
  | S_kwd_let : kwd_let sing
  | S_kwd_namespace : kwd_namespace sing
  | S_kwd_of : kwd_of sing
  | S_kwd_parameter_of : kwd_parameter_of sing
  | S_kwd_return : kwd_return sing
  | S_kwd_switch : kwd_switch sing
  | S_kwd_type : kwd_type sing
  | S_kwd_while : kwd_while sing
  | S_language : language sing
  | S_lbrace : lbrace sing
  | S_lbracket : lbracket sing
  | S_leq : leq sing
  | S_lexeme : lexeme sing
  | S_list : 'a sing -> 'a list sing
  | S_lpar : lpar sing
  | S_lt : lt sing
  | S_minus : minus sing
  | S_minus_eq : minus_eq sing
  | S_rem_eq : rem_eq sing
  | S_module_decl : module_decl sing
  | S_module_name : module_name sing
  | S_module_path : 'a sing -> 'a module_path sing
  | S_module_selection : module_selection sing
  | S_remainder : remainder sing
  | S_mutez_literal : mutez_literal sing
  | S_nat_literal : nat_literal sing
  | S_neq : neq sing
  | S_no_param : (lpar * rpar) reg sing
  | S_nsep_or_pref : 'a sing * 'b sing -> ('a, 'b) Utils.nsep_or_pref sing
  | S_nsep_or_term : 'a sing * 'b sing -> ('a, 'b) Utils.nsep_or_term sing
  | S_nsepseq : 'a sing * 'b sing -> ('a, 'b) Utils.nsepseq sing
  | S_nseq : 'a sing -> 'a Utils.nseq sing
  | S_option : 'a sing -> 'a option sing
  | S_par : 'a sing -> 'a par sing
  | S_par' : 'a sing -> 'a par' sing
  | S_parameter_of_type : parameter_of_type sing
  | S_parameters : 'a sing -> 'a parameters sing
  | S_pattern : pattern sing
  | S_plus : plus sing
  | S_plus_eq : plus_eq sing
  | S_projection : projection sing
  | S_qmark : qmark sing
  | S_range_for : range_for sing
  | S_range_of : range_of sing
  | S_rbrace : rbrace sing
  | S_rbracket : rbracket sing
  | S_record : 'a sing -> 'a record sing
  | S_reg : 'a sing -> 'a reg sing
  | S_region : region sing
  | S_return_stmt : return_stmt sing
  | S_rpar : rpar sing
  | S_selection : selection sing
  | S_semi : semi sing
  | S_sep_or_term : 'a sing * 'b sing -> ('a, 'b) Utils.sep_or_term sing
  | S_sepseq : 'a sing * 'b sing -> ('a, 'b) Utils.sepseq sing
  | S_slash : slash sing
  | S_statement : statement sing
  | S_statements : statements sing
  | S_string_literal : string_literal sing
  | S_switch_case : switch_case sing
  | S_switch_default : switch_default sing
  | S_switch_stmt : switch_stmt sing
  | S_ternary : ternary sing
  | S_times : times sing
  | S_times_eq : times_eq sing
  | S_top_decl : top_decl sing
  | S_tuple : 'a sing -> 'a tuple sing
  | S_tuple_2 : 'a sing * 'b sing -> ('a * 'b) sing
  | S_tuple_3 : 'a sing * 'b sing * 'c sing -> ('a * 'b * 'c) sing
  | S_tuple_4 : 'a sing * 'b sing * 'c sing * 'd sing -> ('a * 'b * 'c * 'd) sing
  | S_type_annotation : type_annotation sing
  | S_type_ctor : type_ctor sing
  | S_type_ctor_args : type_ctor_args sing
  | S_type_decl : type_decl sing
  | S_type_expr : type_expr sing
  | S_type_name : type_name sing
  | S_type_var : type_var sing
  | S_type_vars : type_vars sing
  | S_typed_expr : typed_expr sing
  | S_typed_pattern : typed_pattern sing
  | S_un_op : 'a sing -> 'a un_op sing
  | S_union_type : union_type sing
  | S_update_expr : update_expr sing
  | S_val_binding : val_binding sing
  | S_value_decl : value_decl sing
  | S_var_kind : var_kind sing
  | S_variable : variable sing
  | S_variant : variant sing
  | S_variant_comp : variant_comp sing
  | S_variant_type : variant_type sing
  | S_vbar : vbar sing
  | S_verbatim_literal : verbatim_literal sing
  | S_while_stmt : while_stmt sing
  | S_wild : wild sing
  | S_wrap : 'a sing -> 'a wrap sing
  | S_z : Z.t sing

type some_node = Some_node : 'b * 'b sing -> some_node
let (-|) a b = Some_node (a, b)

let fold
    (type a b)
    (init : b)
    (f : b -> a -> b)
    (instruction : some_node -> a fold_control)
    (cst : CST.t) : b =
  let acc = ref init in
  let rec process : some_node -> unit =
    fun some_node ->
      match instruction some_node with
        Stop -> ()
      | Skip -> fold some_node
      | Continue x -> acc := f !acc x; fold some_node
      | Last x -> acc := f !acc x

  and process_list : some_node list -> unit =
    fun l -> List.iter l ~f:process

  and fold : some_node -> unit =
  function (Some_node (node, sing)) -> match sing with
    S_all_cases -> process @@ node -| S_tuple_2
    (S_nseq (S_reg S_switch_case), S_option (S_reg S_switch_default))
  | S_arguments -> process
    ( node -| (S_par (S_sepseq (S_expr, S_comma)))
    )
  | S_arrow -> process @@ node -| S_wrap S_lexeme
  | S_attr -> () (* Leaf *)
  | S_attribute -> process @@ node -| S_wrap S_attr
  | S_bin_op sing -> let { arg1; op; arg2 } = node in
    process_list
    [ arg1 -| S_expr
    ; op -| sing
    ; arg2 -| S_expr ]
  | S_bit_and -> process @@ node -| S_wrap S_lexeme
  | S_bit_and_eq -> process @@ node -| S_wrap S_lexeme
  | S_bit_neg -> process @@ node -| S_wrap S_lexeme
  | S_bit_or -> process @@ node -| S_wrap S_lexeme
  | S_bit_xor -> process @@ node -| S_wrap S_lexeme
  | S_bit_sl -> process @@ node -| S_wrap S_lexeme
  | S_bit_sr -> process @@ node -| S_wrap S_lexeme
  | S_bit_sl_eq -> process @@ node -| S_wrap S_lexeme
  | S_bit_sr_eq -> process @@ node -| S_wrap S_lexeme
  | S_bit_or_eq -> process @@ node -| S_wrap S_lexeme
  | S_bit_xor_eq -> process @@ node -| S_wrap S_lexeme
  | S_bool_and -> process @@ node -| S_wrap S_lexeme
  | S_bool_or -> process @@ node -| S_wrap S_lexeme
  | S_bool_xor -> process @@ node -| S_wrap S_lexeme
  | S_braces sing -> process @@ node -| S_reg (S_braces' sing)
  | S_braces' sing -> let { lbrace; inside; rbrace } = node in
    process_list
    [ lbrace -| S_lbrace
    ; inside -| sing
    ; rbrace -| S_rbrace ]
  | S_brackets sing -> process @@ node -| S_reg (S_brackets' sing)
  | S_brackets' sing -> let { lbracket; inside; rbracket } = node in
    process_list
    [ lbracket -| S_lbracket
    ; inside -| sing
    ; rbracket -| S_rbracket ]
  | S_bytes_literal -> process @@ node -| S_wrap (S_tuple_2 (S_lexeme, S_hex))
  | S_cartesian -> process @@ node -| S_brackets (S_nsep_or_term (S_type_expr, S_comma))
  | S_cases -> process
    ( match node with
      AllCases node -> node -| S_all_cases
    | Default node -> node -| S_reg S_switch_default
    )
  | S_chevrons sing -> process @@ node -| S_reg (S_chevrons' sing)
  | S_chevrons' sing -> let { lchevron; inside; rchevron } = node in
    process_list
    [ lchevron -| S_lt
    ; inside -| sing
    ; rchevron -| S_gt ]
  | S_code_inj -> let { language; code } = node in
    process_list
    [ language -| S_language
    ; code -| S_expr ]
  | S_colon -> process @@ node -| S_wrap S_lexeme
  | S_comma -> process @@ node -| S_wrap S_lexeme
  | S_component sing -> process @@ node -| S_tuple_2 (S_option S_ellipsis, sing)
  | S_cond_stmt -> let {kwd_if; test; if_so; if_not} = node in
    process_list
    [kwd_if -| S_kwd_if
    ; test -| S_par S_expr
    ; if_so -| S_statement
    ; if_not -| S_option (S_tuple_2 (S_kwd_else, S_statement))]
  | S_contract_of_expr -> let { kwd_contract_of; module_path } = node in
    process_list
    [ kwd_contract_of -| S_kwd_contract_of
    ; module_path -| S_par S_module_selection ]
  | S_cst -> let { decl; eof } = node in
    process_list
    [ decl -| S_nseq S_top_decl
    ; eof -| S_eof ]
  | S_ctor -> process @@ node -| S_wrap S_lexeme
  | S_declaration -> process
    ( match node with
      D_Value node -> node -| S_reg S_value_decl
    | D_Import node -> node -| S_import_decl
    | D_Interface node -> node -| S_reg S_interface_decl
    | D_Module node -> node -| S_reg S_module_decl
    | D_Type node -> node -| S_reg S_type_decl
    )
  | S_decrement -> process @@ node -| S_wrap S_lexeme
  | S_directive -> () (* Leaf *)
  | S_div_eq -> process @@ node -| S_wrap S_lexeme
  | S_dot -> process @@ node -| S_wrap S_lexeme
  | S_ellipsis -> process @@ node -| S_wrap S_lexeme
  | S_eof -> process @@ node -| S_wrap S_lexeme
  | S_equal -> process @@ node -| S_wrap S_lexeme
  | S_equal_cmp -> process @@ node -| S_wrap S_lexeme
  | S_expr -> process
    ( match node with
      E_Add node -> node -| S_reg (S_bin_op S_plus)
    | E_AddEq node -> node -| S_reg (S_bin_op S_plus_eq)
    | E_And node -> node -| S_reg (S_bin_op S_bool_and)
    | E_App node -> node -| S_reg (S_tuple_2 (S_expr, S_arguments))
    | E_Assign node -> node -| S_reg (S_bin_op S_equal)
    | E_Attr node -> node -| S_tuple_2 (S_attribute, S_expr)
    | E_BitAnd node -> node -| S_reg (S_bin_op S_bit_and)
    | E_BitAndEq node -> node -| S_reg (S_bin_op S_bit_and_eq)
    | E_BitNeg node -> node -| S_reg (S_un_op S_bit_neg)
    | E_BitOr node -> node -| S_reg (S_bin_op S_bit_or)
    | E_BitOrEq node -> node -| S_reg (S_bin_op S_bit_or_eq)
    | E_BitSl node -> node -| S_reg (S_bin_op S_bit_sl)
    | E_BitSlEq node -> node -| S_reg (S_bin_op S_bit_sl_eq)
    | E_BitSr node -> node -| S_reg (S_bin_op S_bit_sr)
    | E_BitSrEq node -> node -| S_reg (S_bin_op S_bit_sr_eq)
    | E_BitXor node -> node -| S_reg (S_bin_op S_bit_xor)
    | E_BitXorEq node -> node -| S_reg (S_bin_op S_bit_xor_eq)
    | E_Bytes node -> node -| S_bytes_literal
    | E_CodeInj node -> node -| S_reg S_code_inj
    | E_Contract node -> node -| S_reg S_contract_of_expr
    | E_Ctor node -> node -| S_ctor
    | E_Div node -> node -| S_reg (S_bin_op S_slash)
    | E_DivEq node -> node -| S_reg (S_bin_op S_div_eq)
    | E_Equal node -> node -| S_reg (S_bin_op S_equal_cmp)
    | E_Fun node -> node -| S_reg (S_fun_expr)
    | E_Geq node -> node -| S_reg (S_bin_op S_geq)
    | E_Gt node -> node -| S_reg (S_bin_op S_gt)
    | E_Int node -> node -| S_int_literal
    | E_Leq node -> node -| S_reg (S_bin_op S_leq)
    | E_Lt node -> node -| S_reg (S_bin_op S_lt)
    | E_MinusEq node -> node -| S_reg (S_bin_op S_minus_eq)
    | E_Rem node -> node -| S_reg (S_bin_op S_remainder)
    | E_RemEq node -> node -| S_reg (S_bin_op S_rem_eq)
    | E_ModPath node -> node -| S_reg (S_module_path S_expr)
    | E_Mult node -> node -| S_reg (S_bin_op S_times)
    | E_Mutez node -> node -| S_mutez_literal
    | E_Nat node -> node -| S_nat_literal
    | E_Neg node -> node -| S_reg (S_un_op S_minus)
    | E_Neq node -> node -| S_reg (S_bin_op S_neq)
    | E_Not node -> node -| S_reg (S_un_op S_bool_neg)
    | E_Or node -> node -| S_reg (S_bin_op S_bool_or)
    | E_Par node -> node -| S_par S_expr
    | E_PostDecr node -> node -| S_reg (S_un_op S_decrement)
    | E_PostIncr node -> node -| S_reg (S_un_op S_increment)
    | E_PreDecr node -> node -| S_reg (S_un_op S_decrement)
    | E_PreIncr node -> node -| S_reg (S_un_op S_increment)
    | E_Proj node -> node -| S_reg S_projection
    | E_Record node -> node -| S_record S_expr
    | E_String node -> node -| S_string_literal
    | E_Sub node -> node -| S_reg (S_bin_op S_minus)
    | E_Ternary node -> node -| S_reg S_ternary
    | E_TimesEq node -> node -| S_reg (S_bin_op S_times_eq)
    | E_Tuple node -> node -| S_tuple S_expr
    | E_Typed node -> node -| S_reg S_typed_expr
    | E_Update node -> node -| S_braces S_update_expr
    | E_Var node -> node -| S_variable
    | E_Verbatim node -> node -| S_verbatim_literal
    | E_Xor node -> node -| S_reg (S_bin_op S_bool_xor)
    )
  | S_field sing -> let { attributes; field_id; field_rhs } = node in
    process_list
    [ attributes -| S_list S_attribute
    ; field_id -| S_field_id
    ; field_rhs -| S_option (S_tuple_2 (S_colon, sing)) ]
  | S_field_id -> process
    ( match node with
      F_Name node -> node -| S_field_name
    | F_Int node -> node -| S_int_literal
    | F_Str node -> node -| S_string_literal
    )
  | S_field_name -> process @@ node -| S_wrap S_lexeme
  | S_field_sep -> process @@ node -| S_wrap S_lexeme
  | S_file_path -> process @@ node -| S_wrap S_lexeme
  | S_for_of_stmt -> let { kwd_for; range; for_of_body } = node in
    process_list
    [ kwd_for -| S_kwd_for
    ; range -| S_par S_range_of
    ; for_of_body -| S_statement]
  | S_for_stmt ->
    let { kwd_for; range; for_body } = node in
    process_list
    [ kwd_for -| S_kwd_for
    ; range -| S_par S_range_for
    ; for_body -| S_option (S_statement)]
  | S_fun_body -> process
    ( match node with
      FunBody node -> node -| S_braces S_statements
    | ExprBody node -> node -| S_expr )
  | S_fun_expr ->
    let { type_vars; parameters; rhs_type; arrow; fun_body } = node in
    process_list
    [ type_vars -| S_option S_type_vars
    ; parameters -| S_parameters S_pattern
    ; rhs_type -| S_option (S_tuple_2 (S_colon, S_type_expr))
    ; arrow -| S_arrow
    ; fun_body -| S_fun_body ]
  | S_fun_type -> process @@ node -| S_reg (S_tuple_3 (S_fun_type_params, S_arrow, S_type_expr))
  | S_fun_type_param -> process @@ node -| S_tuple_2 (S_variable, S_type_annotation)
  | S_fun_type_params ->
      process @@ node -| S_parameters (S_reg S_fun_type_param)
  | S_geq -> process @@ node -| S_wrap S_lexeme
  | S_gt -> process @@ node -| S_wrap S_lexeme
  | S_hex -> () (* Leaf *)
  | S_import_alias -> let { kwd_import; alias; equal; module_path } = node in
    process_list
    [ kwd_import -| S_kwd_import
    ; alias -| S_module_name
    ; equal -| S_equal
    ; module_path -| S_module_selection ]
  | S_import_all_as -> let { kwd_import; times; kwd_as; alias; kwd_from; file_path } = node in
    process_list
    [ kwd_import -| S_kwd_import
    ; times -| S_times
    ; kwd_as -| S_kwd_as
    ; alias -| S_module_name
    ; kwd_from -| S_kwd_from
    ; file_path -| S_file_path ]
  | S_import_decl -> process
    ( match node with
      AliasModule node -> node -| S_reg S_import_alias
    | ImportAll node -> node -| S_reg S_import_all_as
    | ImportSome node -> node -| S_reg S_import_from )
  | S_import_from -> let { kwd_import; imported; kwd_from; file_path } = node in
    process_list
    [ kwd_import -| S_kwd_import
    ; imported -| S_braces (S_sep_or_term (S_field_name, S_comma))
    ; kwd_from -| S_kwd_from
    ; file_path -| S_file_path ]
  | S_increment -> process @@ node -| S_wrap S_lexeme
  | S_int_literal -> process @@ node -| S_wrap (S_tuple_2 (S_lexeme, S_z))
  | S_int64 -> () (* Leaf *)
  | S_interface -> process @@ node -| S_reg (S_tuple_2 (S_kwd_implements, S_intf_expr))
  | S_interface_decl -> let { kwd_interface; intf_name; intf_body } = node in
    process_list
    [ kwd_interface -| S_kwd_interface
    ; intf_name -| S_intf_name
    ; intf_body -| S_intf_body ]
  | S_intf_body -> process @@ node -| S_braces S_intf_entries
  | S_intf_const -> let { kwd_const; const_name; const_type } = node in
    process_list
    [ kwd_const -| S_kwd_const
    ; const_name -| S_variable
    ; const_type -| S_type_annotation ]
  | S_intf_entries -> process @@ node -| S_sep_or_term (S_intf_entry, S_semi)
  | S_intf_entry -> process
    ( match node with
      I_Attr node -> node -| S_tuple_2 (S_attribute, S_intf_entry)
    | I_Type node -> node -| S_reg S_intf_type
    | I_Const node -> node -| S_reg S_intf_const
    )
  | S_intf_expr -> process
    ( match node with
      I_Body node -> node -| S_intf_body
    | I_Path node -> node -| S_module_selection
    )
  | S_intf_name -> process @@ node -| S_wrap S_lexeme
  | S_intf_type -> let { kwd_type; type_name; type_rhs } = node in
    process_list
    [ kwd_type -| S_kwd_type
    ; type_name -| S_type_name
    ; type_rhs -| S_option (S_tuple_2 (S_equal, S_type_expr)) ]
  | S_kwd_as -> process @@ node -| S_wrap S_lexeme
  | S_kwd_break -> process @@ node -| S_wrap S_lexeme
  | S_kwd_case -> process @@ node -| S_wrap S_lexeme
  | S_kwd_const -> process @@ node -| S_wrap S_lexeme
  | S_kwd_contract_of -> process @@ node -| S_wrap S_lexeme
  | S_kwd_default -> process @@ node -| S_wrap S_lexeme
  | S_kwd_else -> process @@ node -| S_wrap S_lexeme
  | S_kwd_export -> process @@ node -| S_wrap S_lexeme
  | S_kwd_for -> process @@ node -| S_wrap S_lexeme
  | S_kwd_from -> process @@ node -| S_wrap S_lexeme
  | S_kwd_if -> process @@ node -| S_wrap S_lexeme
  | S_kwd_implements -> process @@ node -| S_wrap S_lexeme
  | S_kwd_import -> process @@ node -| S_wrap S_lexeme
  | S_kwd_interface -> process @@ node -| S_wrap S_lexeme
  | S_kwd_let -> process @@ node -| S_wrap S_lexeme
  | S_kwd_namespace -> process @@ node -| S_wrap S_lexeme
  | S_kwd_of -> process @@ node -| S_wrap S_lexeme
  | S_kwd_parameter_of -> process @@ node -| S_wrap S_lexeme
  | S_kwd_return -> process @@ node -| S_wrap S_lexeme
  | S_kwd_switch -> process @@ node -| S_wrap S_lexeme
  | S_kwd_type -> process @@ node -| S_wrap S_lexeme
  | S_kwd_while -> process @@ node -| S_wrap S_lexeme
  | S_language -> process @@ node -| S_wrap S_lexeme
  | S_lbrace -> process @@ node -| S_wrap S_lexeme
  | S_lbracket -> process @@ node -| S_wrap S_lexeme
  | S_leq -> process @@ node -| S_wrap S_lexeme
  | S_lexeme -> () (* Leaf *)
  | S_list sing -> process_list @@ List.map ~f:(fun x -> x -| sing) node
  | S_lpar -> process @@ node -| S_wrap S_lexeme
  | S_lt -> process @@ node -| S_wrap S_lexeme
  | S_minus -> process @@ node -| S_wrap S_lexeme
  | S_minus_eq -> process @@ node -| S_wrap S_lexeme
  | S_rem_eq -> process @@ node -| S_wrap S_lexeme
  | S_module_decl -> let { kwd_namespace; module_name; module_type; module_body } = node in
    process_list
    [ kwd_namespace -| S_kwd_namespace
    ; module_name -| S_module_name
    ; module_type -| S_option S_interface
    ; module_body -| S_braces S_statements ]
  | S_module_name -> process @@ node -| S_wrap S_lexeme
  | S_module_path sing ->
    let {module_path; selector; field} = node in
    process_list
    [ module_path -| S_nsepseq (S_module_name, S_dot)
    ; selector -| S_dot
    ; field -| sing ]
  | S_module_selection -> process
      (match node with
         M_Path  node -> node -| S_reg (S_module_path S_module_name)
       | M_Alias node -> node -| S_module_name)
  | S_remainder -> process @@ node -| S_wrap S_lexeme
  | S_mutez_literal -> process @@ node -| S_wrap (S_tuple_2 (S_lexeme, S_int64))
  | S_nat_literal -> process @@ node -| S_wrap (S_tuple_2 (S_lexeme, S_z))
  | S_bool_neg -> process @@ node -| S_wrap S_lexeme
  | S_neq -> process @@ node -| S_wrap S_lexeme
  | S_no_param -> process @@ node -| S_reg (S_tuple_2 (S_wrap S_lexeme, S_wrap S_lexeme))
  | S_nsep_or_pref (a_sing, b_sing) -> process
    ( match node with
      `Sep node -> node -| S_nsepseq (a_sing, b_sing)
    | `Pref node -> node -| S_nseq (S_tuple_2 (b_sing, a_sing)))
  | S_nsep_or_term (a_sing, b_sing) -> process
    ( match node with
      `Sep node -> node -| S_nsepseq (a_sing, b_sing)
    | `Term node -> node -| S_nseq (S_tuple_2 (a_sing, b_sing)))
  | S_nsepseq (sing_1, sing_2) ->
    process @@ node -| S_tuple_2 (sing_1, S_list (S_tuple_2 (sing_2, sing_1)))
  | S_nseq sing -> process @@ node -| S_tuple_2 (sing, S_list sing)
  | S_option sing ->
    ( match node with
      None -> () (* Leaf *)
    | Some node -> process @@ node -| sing
    )
  | S_par sing -> process @@ node -| S_reg (S_par' sing)
  | S_par' sing -> let { lpar; inside; rpar } = node in
    process_list
    [ lpar -| S_lpar
    ; inside -| sing
    ; rpar -| S_rpar ]
  | S_parameter_of_type -> let { kwd_parameter_of; module_path } = node in
    process_list
    [ kwd_parameter_of -| S_kwd_parameter_of
    ; module_path -| S_module_selection ]
  | S_parameters sing -> process
    ( match node with
        ParParams node -> node -| S_par (S_sep_or_term (sing, S_comma))
      | VarParam node -> node -| S_variable
    )
  | S_pattern -> process
    ( match node with
      P_Attr node -> node -| S_tuple_2 (S_attribute, S_pattern)
    | P_Bytes node -> node -| S_bytes_literal
    | P_Ctor node -> node -| S_ctor
    | P_Int node -> node -| S_int_literal
    | P_Mutez node -> node -| S_mutez_literal
    | P_Nat node -> node -| S_nat_literal
    | P_Record node -> node -| S_record S_pattern
    | P_String node -> node -| S_string_literal
    | P_Tuple node -> node -| S_tuple S_pattern
    | P_Typed node -> node -| S_reg S_typed_pattern
    | P_Var node -> node -| S_variable
    | P_Verbatim node -> node -| S_verbatim_literal
    )
  | S_plus -> process @@ node -| S_wrap S_lexeme
  | S_plus_eq -> process @@ node -| S_wrap S_lexeme
  | S_projection -> let { record_or_tuple; field_path } = node in
    process_list
    [ record_or_tuple -| S_expr
    ; field_path -| S_nseq S_selection ]
  | S_qmark -> process @@ node -| S_wrap S_lexeme
  | S_range_for -> let { initialiser; semi1; condition; semi2; afterthought } = node in
    process_list
    [ initialiser -| S_option S_statement
    ; semi1 -| S_semi
    ; condition -| S_option S_expr
    ; semi2 -| S_semi
    ; afterthought -| S_option (S_nsepseq (S_expr, S_comma)) ]
  | S_range_of -> let { index_kind; index; kwd_of; expr } = node in
    process_list
    [ index_kind -| S_var_kind
    ; index -| S_variable
    ; kwd_of -| S_kwd_of
    ; expr -| S_expr ]
  | S_rbrace -> process @@ node -| S_wrap S_lexeme
  | S_rbracket -> process @@ node -| S_wrap S_lexeme
  | S_record sing -> process @@ node -| S_braces (S_sep_or_term (S_reg (S_field sing), S_semi))
  | S_reg sing -> let { region; value } = node in
    process_list
    [ region -| S_region
    ; value -| sing ]
  | S_region -> () (* Leaf *)
  | S_return_stmt -> process @@ node -| S_tuple_2 (S_kwd_return, S_option S_expr)
  | S_rpar -> process @@ node -| S_wrap S_lexeme
  | S_selection -> process
    ( match node with
      FieldName node -> node -| S_tuple_2 (S_dot, S_field_name)
    | FieldStr node -> node -| S_brackets S_string_literal
    | Component node -> node -| S_brackets S_nat_literal
    )
  | S_semi -> process @@ node -| S_wrap S_lexeme
  | S_sep_or_term (a_sing, b_sing) -> process @@ node -| S_option (S_nsep_or_term (a_sing, b_sing))
  | S_sepseq (sing_1, sing_2) ->
    process @@ node -| S_option (S_nsepseq (sing_1, sing_2))
  | S_slash -> process @@ node -| S_wrap S_lexeme
  | S_statement -> process
    ( match node with
      S_Attr node -> node -| S_tuple_2 (S_attribute, S_statement)
    | S_Block node -> node -| S_braces S_statements
    | S_Break node -> node -| S_kwd_break
    | S_Cond node -> node -| S_reg S_cond_stmt
    | S_Decl node -> node -| S_declaration
    | S_Export node -> node -| S_reg (S_tuple_2 (S_kwd_export, S_statement))
    | S_Expr node -> node -| S_expr
    | S_For node -> node -| S_reg S_for_stmt
    | S_ForOf node -> node -| S_reg S_for_of_stmt
    | S_Return node -> node -| S_reg S_return_stmt
    | S_Switch node -> node -| S_reg S_switch_stmt
    | S_While node -> node -| S_reg S_while_stmt
    )
  | S_statements -> process @@ node -| S_nseq (S_tuple_2 (S_statement, S_option S_semi))
  | S_string_literal -> process @@ node -| S_wrap S_lexeme
  | S_switch_case -> let { kwd_case; expr; colon; case_body } = node in
    process_list
    [ kwd_case -| S_kwd_case
    ; expr -| S_expr
    ; colon -| S_colon
    ; case_body -| S_option S_statements ]
  | S_switch_default -> let { kwd_default; colon; default_body } = node in
    process_list
    [ kwd_default -| S_kwd_default
    ; colon -| S_colon
    ; default_body -| S_option S_statements ]
  | S_switch_stmt -> let { kwd_switch; subject; cases } = node in
    process_list
    [ kwd_switch -| S_kwd_switch
    ; subject -| S_par S_expr
    ; cases -| S_braces S_cases ]
  | S_ternary -> let {condition; qmark; truthy; colon; falsy } = node in
    process_list
    [ condition -| S_expr
    ; qmark -| S_qmark
    ; truthy -| S_expr
    ; colon -| S_colon
    ; falsy -| S_expr ]
  | S_times -> process @@ node -| S_wrap S_lexeme
  | S_times_eq -> process @@ node -| S_wrap S_lexeme
  | S_top_decl -> process
    ( match node with
      TL_Decl node -> node -| S_tuple_2 (S_declaration, S_option S_semi)
    | TL_Attr node -> node -| S_tuple_2 (S_attribute , S_top_decl)
    | TL_Export node -> node -| S_reg (S_tuple_2 (S_kwd_export, S_top_decl))
    | TL_Directive node -> node -| S_directive
    )
  | S_tuple sing -> process @@ node -| S_brackets (S_sep_or_term (S_component sing, S_comma))
  | S_tuple_2 (sing_1, sing_2) ->
    ( match node with
      (node_1, node_2) -> process_list
      [ node_1 -| sing_1
      ; node_2 -| sing_2 ] )
  | S_tuple_3 (sing_1, sing_2, sing_3) ->
    ( match node with
      (node_1, node_2, node_3) -> process_list
      [ node_1 -| sing_1
      ; node_2 -| sing_2
      ; node_3 -| sing_3 ] )
  | S_tuple_4 (sing_1, sing_2, sing_3, sing_4) ->
    ( match node with
      (node_1, node_2, node_3, node_4) -> process_list
      [ node_1 -| sing_1
      ; node_2 -| sing_2
      ; node_3 -| sing_3
      ; node_4 -| sing_4 ] )
  | S_type_annotation -> process @@ node -| S_tuple_2 (S_colon, S_type_expr)
  | S_type_ctor -> process @@ node -| S_wrap S_lexeme
  | S_type_ctor_args -> process @@ node -| S_chevrons (S_nsep_or_term (S_type_expr, S_comma))
  | S_type_decl -> let { kwd_type; name; type_vars; eq; type_expr } = node in
    process_list
    [ kwd_type -| S_kwd_type
    ; name -| S_type_name
    ; type_vars -| S_option S_type_vars
    ; eq -| S_equal
    ; type_expr -| S_type_expr ]
  | S_type_expr -> process
    ( match node with
      T_App node -> node -| S_reg (S_tuple_2 (S_type_expr, S_type_ctor_args))
    | T_Attr node -> node -| S_tuple_2 (S_attribute, S_type_expr)
    | T_Cart node -> node -| S_cartesian
    | T_Fun node -> node -| S_fun_type
    | T_Int node -> node -| S_int_literal
    | T_ModPath node -> node -| S_reg (S_module_path S_type_expr)
    | T_Par node -> node -| S_par S_type_expr
    | T_Parameter node -> node -| S_reg S_parameter_of_type
    | T_Record node -> node -| S_record S_type_expr
    | T_String node -> node -| S_string_literal
    | T_Union node -> node -| S_union_type
    | T_Var node -> node -| S_variable
    | T_Variant node -> node -| S_variant_type )
  | S_type_name -> process @@ node -| S_wrap S_lexeme
  | S_type_var -> process @@ node -| S_wrap S_lexeme
  | S_type_vars -> process @@ node -| S_chevrons (S_sep_or_term (S_type_var, S_comma))
  | S_typed_expr -> process @@ node -| S_tuple_3 (S_expr, S_kwd_as, S_type_expr)
  | S_typed_pattern -> process @@ node -| S_tuple_2 (S_pattern, S_type_annotation)
  | S_un_op sing -> let { op; arg } = node in
    process_list
    [ op -| sing
    ; arg -| S_expr ]
  | S_union_type -> process @@ node -| S_reg (S_nsep_or_pref (S_record S_type_expr, S_vbar))
  | S_update_expr -> let { ellipsis; record; sep; updates } = node in
    process_list
    [ ellipsis -| S_ellipsis
    ; record -| S_expr
    ; sep -| S_field_sep
    ; updates -| S_sep_or_term (S_reg (S_field S_expr), S_semi) ]
  | S_val_binding -> let { pattern; type_vars; rhs_type; eq; rhs_expr } = node in
    process_list
    [ pattern -| S_pattern
    ; type_vars -| S_option S_type_vars
    ; rhs_type -| S_option S_type_annotation
    ; eq -| S_equal
    ; rhs_expr -| S_expr ]
  | S_value_decl -> let { kind; bindings } = node in
    process_list
    [ kind -| S_var_kind
    ; bindings -| S_nsepseq (S_reg S_val_binding, S_comma) ]
  | S_var_kind -> process @@
    ( match node with
      `Let node -> node -| S_kwd_let
    | `Const node -> node -| S_kwd_const
    )
  | S_variable -> process @@ node -| S_wrap S_lexeme
  | S_variant -> let { attributes; tuple } = node in
    process_list
    [ attributes -| S_list S_attribute
    ; tuple -| S_brackets S_variant_comp ]
  | S_variant_comp -> let { ctor; ctor_params } = node in
    process_list
    [ ctor -| S_ctor
    ; ctor_params -| S_option (S_tuple_2 (S_comma, S_nsep_or_term (S_type_expr, S_comma))) ]
  | S_variant_type -> process @@ node -| S_reg (S_nsep_or_pref (S_variant, S_vbar))
  | S_vbar -> process @@ node -| S_wrap S_lexeme
  | S_verbatim_literal -> process @@ node -| S_wrap S_lexeme
  | S_while_stmt -> let { kwd_while; invariant; while_body } = node in
    process_list
    [ kwd_while -| S_kwd_while
    ; invariant -| S_par S_expr
    ; while_body -| S_statement ]
  | S_wild -> process @@ node -| S_wrap S_lexeme
  | S_wrap sing -> process_list
    [ node#payload -| sing
    ; node#attributes -| S_list (S_reg S_attr)
    ; node#region -| S_region
    ; node#directives -| S_list S_directive ]
  | S_z -> () (* Leaf *)
  in
  process @@ cst -| S_cst;
  !acc

let fold_map
  (type a)
  (m : a monoid)
  (f : some_node -> a fold_control)
  (cst : CST.t) : a = fold m.empty m.append f cst
