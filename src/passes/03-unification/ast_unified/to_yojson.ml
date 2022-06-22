open Types
include Stage_common.To_yojson

let rec type_expression {type_expression_content=ec;location=_} =
  `Assoc [
    ("type_expression_content", type_expression_content ec);
    (* ("location", Location.to_yojson location); *)
  ]

and type_expression_content = function
  | _ -> `String "type expressions not supported yet"  (* TODO NP : Add formatting *)

and pattern = fun _ -> `String "patterns not supported yet" (* TODO NP : Add patterns *)

and statement = fun _ -> `String "statements not supported yet" (* TODO NP : Add statements *)

and module_decl = fun _ -> `String "module decl not supported yet" (* TODO NP : Add module decl *)

let rec expression {expression_content = ec; location = loc} : json =
  `Assoc (
    ("location", Location.to_yojson loc)
    :: (expression_content ec)
  )

and expression_content : expression_content -> (string * json) list = function
  (* Base *)
  | E_Literal      e         -> ("name", `String "E_Literal")          :: ["literal", literal e]
  | E_Constant     e         -> ("name", `String "E_Constant")         :: constant e
  | E_Par          e         -> ("name", `String "E_Par")              :: ["expression", expression e]

  (* Variables *)
  | E_UserVar      e         -> ("name", `String "E_UserVar")          :: ["value", `String e]

  | E_variable     e         -> ("name", `String "E_Variable")         :: ["value", ValueVar.to_yojson e]

  (* Strings *)
  | E_Cat          (e1, e2)  -> ("name", `String "E_Cat")              :: ["expr1", expression e1; "expr2", expression e2]
  | E_String       s         -> ("name", `String "E_String")           :: ["value", `String s]
  | E_Verbatim     s         -> ("name", `String "E_Verbatim")         :: ["value", `String s]

  (* Function calls *)
  | E_Call         (e, args) -> ("name", `String "E_Call")             :: call (e, args)
  | E_CallJsligo   (e, args) -> ("name", `String "E_CallJsligo")       :: call (e, args)

  (* Custom operators on functions *)
  | E_RevApp       e         -> ("name", `String "E_RevApp")           :: rev_app e 

  (* Data structures *)
  | E_Tuple        e         -> ("name", `String "E_Tuple")            :: ["elements", ne_list expression e]
  | E_Rec          e         -> ("name", `String "E_Rec")              :: ["elements",    list (field expression expression) e]
  | E_Record       e         -> ("name", `String "E_Record")           :: ["elements", ne_list field_assign e]
  | E_Array        e         -> ("name", `String "E_Array")            :: ["elements",    list expression e]
  | E_ArrayJsligo  e         -> ("name", `String "E_ArrayJsligo")      :: ["elements",    list array_item_jsligo e]
  | E_ObjectJsligo e         -> ("name", `String "E_ObjectJsligo")     :: ["elements", ne_list property_jsligo e]
    
  | E_Proj         e         -> ("name", `String "E_Proj")             :: projection e
  | E_ProjJsligo   e         -> ("name", `String "E_ProjJsligo")       :: projection_jsligo e
    
  (* Module access *)    
  | E_ModA         e         -> ("name", `String "E_ModA")             :: module_access e
  | E_ModPath      e         -> ("name", `String "E_ModPath")          :: module_path e

  (* Record update *)
  | E_UpdateCameligo  e      -> ("name", `String "E_UpdateCameligo")   :: update_cameligo e
  | E_UpdatePascaligo e      -> ("name", `String "E_UpdatePascaligo")  :: update_pascaligo e
  | E_Updates         e      -> ("name", `String "E_Updates")          :: updates e
  | E_Update          e      -> ("name", `String "E_Update")           :: update e
    
  | E_FunCameligo  e         -> ("name", `String "E_FunCameligo")      :: fun_expr_cameligo e
  | E_FunPascaligo e         -> ("name", `String "E_FunPascaligo")     :: fun_expr_pascaligo e
  | E_FunJsligo    e         -> ("name", `String "E_FunJsligo")        :: fun_expr_jsligo e
    
  | E_Constr (n, e)          -> ("name", `String "E_Constr")           :: ["name", `String n; "expr", (option expression) e]
  | E_App (f, a)             -> ("name", `String "E_App")              :: ["func", expression f; "args", option (ne_list expression) a]
    
  | E_Case e                 -> ("name", `String "E_Case")             :: case e
    
  | E_Annot (e, te)          -> ("name", `String "E_Annot")            :: ["expr", expression e; "type_expr", type_expression te]
  | E_AnnotJsligo (e, te)    -> ("name", `String "E_AnnotJsligo")      :: ["expr", expression e; "type_expr", type_expression te]
    
  | E_Cond e                 -> ("name", `String "E_Cond")             :: cond_expr e
    
  | E_List e                 -> ("name", `String "E_List")             :: ["value", list expression e]
  | E_Cons (e1, e2)          -> ("name", `String "E_Cons")             :: ["expr1", expression e1; "expr2", expression e2]
    
  | E_Set e                  -> ("name", `String "E_Set")              :: ["elements", list expression e]
    
  | E_MapLookup e            -> ("name", `String "E_MapLookup")        :: map_lookup e
    
  | E_Map e                  -> ("name", `String "E_Map")              :: ["elements", map e]
  | E_BigMap e               -> ("name", `String "E_BigMap")           :: ["elements", map e]
    
  | E_LetInCameligo e        -> ("name", `String "E_LetInCameligo")    :: let_in_cameligo e
  | E_TypeIn e               -> ("name", `String "E_TypeIn")           :: type_in e
  | E_ModIn e                -> ("name", `String "E_ModIn")            :: mod_in e
  | E_ModAlias e             -> ("name", `String "E_ModAlias")         :: mod_alias e
    
  | E_RawCode e              -> ("name", `String "E_RawCode")          :: raw_code e
    
  | E_Seq e                  -> ("name", `String "E_Seq")              :: ["elements", list expression e]
  | E_Sequence (e1, e2)      -> ("name", `String "E_Sequence")         :: ["head", expression e1; "tail", expression e2]
    
  | E_Block e                -> ("name", `String "E_Block")            :: block_with e
    
  | E_Attr e                 -> ("name", `String "E_Attr")             :: attr e
    
  | E_AssignJsligo e         -> ("name", `String "E_AssignJsligo")     :: assign_jsligo e

and print_str s = `String s

and map : (expr * expr) list -> json = fun m ->
  let kv_pair : (expr * expr) -> json = fun (k, v) ->
  `Assoc [
    ("key",  expression k);
    ("value", expression v);
  ]
  in list kv_pair m

and field print_lhs print_rhs = function
  | Punned lhs -> `List [ `String "PunnedField"; print_lhs lhs ]
  | Complete e -> `List [ `String "CompleteField"; pair print_lhs print_rhs e ]

and field_assign ({name; expr} : field_assign) =
`Assoc [
  ("name", `String name);
  ("expression", expression expr)
]

and call (expr, args) : (string * json) list = [
  ("expression", expression expr);
  ("arguments", ne_list expression args);
]

and rev_app ({x; f} : rev_app) : (string * json) list = [
  ("x", expression x);
  ("f", expression f);
]

and selection : selection -> json = function
| FieldName s -> `List [ `String "FieldName"; `String s ]
| Component z -> `List [ `String "Component"; `String (Z.to_string z) ]

and selection_jsligo : selection_jsligo -> json = function
| FieldName s -> `List [ `String "FieldName"; `String s ]
| Component e -> `List [ `String "Component"; expression e ]

and projection ({expr; field_path} : projection) : (string * json) list = [
  ("expr", expression expr);
  ("field_path", ne_list selection field_path);
]

and projection_jsligo ({expr; selection=s} : projection_jsligo) : (string * json) list = [
  ("expr",      expression expr);
  ("selection_jsligo", selection_jsligo s);
]

and path : path -> json = function
| Name s -> `List [ `String "Name"; `String s ]
| Path proj -> `List [`String "Path"; `Assoc (projection proj)]

and constant { cons_name ; arguments } : (string * json) list = [
  ("cons_name", rich_constant cons_name);
  ("arguments", list expression arguments);
]

and module_access {module_name; field} : (string * json) list = [
  ("module_name", `String module_name);
  ("field", expression field);
]

and module_path {module_path; field} : (string * json) list = [
  ("module_path", ne_list string module_path);
  ("field", expression field);
]

and field_path_assignment ({field_path; field_expr} : field_path_assignment) : (string * json) list = [
  ("field_path", path field_path);
  ("field_expr", expression field_expr)
]
and field_path_assignment_json f = `Assoc (field_path_assignment f)

and update_cameligo ({record; updates} : update_cameligo) : (string * json) list = [
  ("record", path record);
  ("updates", ne_list field_path_assignment_json updates);
]

and update_pascaligo ({structure; update} : update_pascaligo) : (string * json) list = [
  ("structure", expression structure);
  ("update", expression update)
]

and updates ({record; updates} : updates) : (string * json) list = [
  ("record", expression record);
  ("updates", ne_list field_path_assignment_json updates)
]

and update ({record; path; update} : update) : (string * json) list = [
  ("record", expression record);
  ("path", ne_list (access expression) path);
  ("update", expression update)
]

and fun_expr_cameligo ({ type_params; binders; rhs_type; body } : fun_expr_cameligo) : (string * json) list = [
  ("type_params", option (ne_list string) type_params);
  ("binders", ne_list pattern binders);
  ("rhs_type", option type_expression rhs_type);
  ("body", expression body);
]

and param_decl ({ param_kind; pattern=p; param_type } : param_decl) : (string * json) list = [
  ("param_kind", match param_kind with `Var -> `String "Var" | `Const -> `String "Const");
  ("pattern",    pattern p);
  ("param_type", option type_expression param_type);
]
and param_decl_json x = `Assoc (param_decl x)

and fun_expr_pascaligo ({ type_params; parameters; ret_type; return } : fun_expr_pascaligo) : (string * json) list = [
  ("type_params", option (ne_list string) type_params);
  ("parameters",  list param_decl_json parameters);
  ("ret_type",    option type_expression ret_type);
  ("return",      expression return);
]

and body_jsligo : body_jsligo -> json = function
| FunctionBody   b -> `List [ `String "FunctionBody"; ne_list statement b ]
| ExpressionBody e -> `List [ `String "ExpressionBody"; expression e ]

and fun_expr_jsligo ({ parameters; lhs_type; body } : fun_expr_jsligo) : (string * json) list = [
  ("parameters", expression parameters);
  ("lhs_type",   option type_expression lhs_type);
  ("body",       body_jsligo body);
]

and case ({ expr; cases } : case) : (string * json) list = [
  ("expr", expression expr);
  ("cases", ne_list case_clause_json cases)
]

and case_clause ({ pattern=p; rhs } : case_clause) : (string * json) list = [
  ("pattern", pattern p);
  ("rhs", expression rhs);
]
and case_clause_json x = `Assoc (case_clause x)

and cond_expr ({ test; ifso; ifnot } : cond_expr) : (string * json) list = [
  ("test", expression test);
  ("ifso", expression ifso);
  ("ifnot", option expression ifnot);
]

and map_lookup ({ map; keys } : map_lookup) : (string * json) list = [
  ("map",  expression map);
  ("keys", ne_list expression keys);
]

and let_in_cameligo ({ is_rec; type_params; binders; rhs_type; let_rhs; body } : let_in_cameligo) : (string * json) list = [
  ("is_rec",      `String (string_of_bool is_rec));
  ("type_params",  option (ne_list string) type_params);
  ("binders",      ne_list pattern binders);
  ("rhs_type",     option type_expression rhs_type);
  ("let_rhs",      expression let_rhs);
  ("body",         expression body);
]

and type_in ({ type_binder; rhs; body } : type_in) : (string * json) list = [
  ("type_binder",  `String type_binder);
  ("rhs",          type_expression rhs);
  ("body",         expression body);
]

and mod_in ({ module_name; rhs; body } : mod_in) : (string * json) list = [
  ("module_name",  `String module_name);
  ("rhs",          module_decl rhs);
  ("body",         expression body);
]

and mod_alias ({ module_name; binders; body } : mod_alias) : (string * json) list = [
  ("module_name", string module_name);
  ("binders",     ne_list string binders);
  ("body",        expression body);
]

and raw_code ({ language; code } : raw_code) : (string * json) list = [
  ("language", string language);
  ("code",     expression code);
]

and block_with ({ block; expr } : block_with) : (string * json) list = [
  ("block", ne_list statement block);
  ("expr",  expression expr);
]

and attr ({ key; value }, expr : attr_pascaligo * expr) : (string * json) list = [
  ("key",   string key);
  ("value", option string value);
  ("expr", expression expr);
]

and array_item_jsligo : array_item_jsligo -> json = function
| Expr_entry e -> `List [ `String "Expr_entry"; expression e ]
| Rest_entry e -> `List [ `String "Rest_entry"; expression e ]

and array_jsligo e = list array_item_jsligo e

and property_jsligo : property_jsligo -> json = function
| Punned_property e -> `List [ `String "Punned_property"; expression e ]
| Property (e1, e2) -> `List [ `String "Property"; pair expression expression (e1, e2) ]
| Property_rest e   -> `List [ `String "Property_rest"; expression e ]

and object_jsligo e = ne_list property_jsligo e

and assignment_operator_jsligo : assignment_operator_jsligo -> json = function
| Times_eq  -> `String "Times_eq"
| Div_eq    -> `String "Div_eq"
| Min_eq    -> `String "Min_eq"
| Plus_eq   -> `String "Plus_eq"
| Mod_eq    -> `String "Mod_eq"

and operator_jsligo : operator_jsligo -> json = function
| Eq                    -> `String "Eq"
| Assignment_operator e -> `List [ `String "Assignment_operator"; assignment_operator_jsligo e ]

and assign_jsligo ({ expr1; op; expr2 } : assign_jsligo) : (string * json) list = [
  ("expr1", expression expr1);
  ("op",    operator_jsligo op);
  ("expr2", expression expr2);
]

and declaration {declaration_content = dc; location = loc} : json =
  `Assoc (
    ("location", Location.to_yojson loc)
    :: declaration_content dc
  )

and declaration_content : declaration_content -> (string * json) list = function
| D_Directive      d -> ("name", `String "D_Directive" )      :: ["value", d_directive d]
| D_ToplevelJsligo d -> ("name", `String "D_ToplevelJsligo" ) :: ["statement", statement d]
| D_Let            d -> ("name", `String "D_Let" )            :: d_let_binding d

and d_directive : Directive.t -> json = fun d ->
  let flag : Directive.flag -> json = function
    | Push -> `String "Push"
    | Pop -> `String "Pop"
  in
  let linemarker : Directive.linemarker -> json = fun (linenum, filepath, flag_opt) ->
    `List [
      `String "Linemarker"; 
      `Assoc [
        ("linenum", `String (Int.to_string linenum));
        ("filepath", `String filepath);
        ("flag_opt", option flag flag_opt)
      ]
    ]
  in
  match d with
  Linemarker l -> linemarker l


and d_let_binding ({type_params; binders; rhs_type; let_rhs} : let_binding) : (string * json) list = [
  ("type_params",  option (ne_list string) type_params);
  ("binders",      ne_list pattern binders);
  ("rhs_type",     option type_expression rhs_type);
  ("let_rhs",      expression let_rhs);
]


and program : program -> json = fun p -> list declaration p
