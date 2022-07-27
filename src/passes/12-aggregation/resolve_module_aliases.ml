module AST = Ast_typed

module Aliases = struct
  module MMap = Simple_utils.Map.Make(AST.ModuleVar)
  type t = {inside : (t * AST.ModuleVar.t list option) MMap.t}
  let rec pp ppf {inside} =
    Format.fprintf ppf "%a" (PP_helpers.list_sep_d (fun ppf (k,(t,v)) -> Format.fprintf ppf "%a => (%a,%a)" AST.ModuleVar.pp k pp t PP_helpers.(option (list_sep_d AST.ModuleVar.pp)) v )) @@ MMap.to_kv_list inside
  let empty = {inside = MMap.empty}
  let push aliases mvar path mod_aliases =
    {inside = MMap.add mvar (path,mod_aliases) aliases.inside}
  let get_opt aliases mvar = MMap.find mvar aliases.inside
  let get aliases mvar =
    let aliases,path = get_opt aliases mvar in
    aliases, Option.value ~default:[mvar] path
end

let rec type_expression : Aliases.t -> AST.type_expression -> AST.type_expression = fun aliases te ->
  let self ?(aliases = aliases) = type_expression aliases in
  let return type_content = {te with type_content} in
  match te.type_content with
    T_variable type_variable ->
    return @@ T_variable type_variable
  | T_sum {content;layout} ->
    let content = AST.LMap.map AST.(fun {associated_type;michelson_annotation;decl_pos} ->
      let associated_type = self associated_type in
      {associated_type;michelson_annotation;decl_pos}
    ) content in
    return @@ T_sum {content;layout}
  | T_record {content;layout} ->
    let content = AST.LMap.map AST.(fun {associated_type;michelson_annotation;decl_pos} ->
      let associated_type = self associated_type in
      {associated_type;michelson_annotation;decl_pos}
    ) content in
    return @@ T_record {content;layout}
  | T_arrow {type1;type2} ->
    let type1 = self type1 in
    let type2 = self type2 in
    return @@ T_arrow {type1;type2}
  | T_constant {language;injection;parameters} ->
    let parameters = List.map ~f:self parameters in
    return @@ T_constant {language;injection;parameters}
  | T_singleton literal ->
    return @@ T_singleton literal
  | T_abstraction {ty_binder;kind;type_} ->
    let type_ = self type_ in
    return @@ T_abstraction {ty_binder;kind;type_}
  | T_for_all {ty_binder;kind;type_} ->
    let type_ = self type_ in
    return @@ T_for_all {ty_binder;kind;type_}

let rec expression : Aliases.t -> AST.expression -> AST.expression = fun aliases e ->
  let self ?(aliases = aliases) = expression aliases in
  let self_type ?(aliases = aliases) = type_expression aliases in
  let return expression_content = {e with expression_content} in
  match e.expression_content with
    E_literal literal ->
    return @@ E_literal literal
  | E_constant {cons_name;arguments} ->
    let arguments = List.map ~f:self arguments in
    return @@ E_constant {cons_name;arguments}
  | E_variable variable ->
    return @@ E_variable variable
  | E_application {lamb;args} ->
    let lamb = self lamb in
    let args = self args in
    return @@ E_application {lamb;args}
  | E_lambda {binder={var;ascr;attributes};result} ->
    let ascr = Option.map ~f:self_type ascr in
    let result = self result in
    return @@ E_lambda {binder={var;ascr;attributes};result}
  | E_type_abstraction {type_binder;result} ->
    let result = self result in
    return @@ E_type_abstraction {type_binder;result}
  | E_recursive {fun_name;fun_type;lambda={binder={var;ascr;attributes};result}} ->
    let fun_type = self_type fun_type in
    let ascr = Option.map ~f:self_type ascr in
    let result = self result in
    return @@ E_recursive {fun_name;fun_type;lambda={binder={var;ascr;attributes};result}}
  | E_let_in {let_binder={var;ascr;attributes};rhs;let_result;attr} ->
    let ascr = Option.map ~f:self_type ascr in
    let rhs = self rhs in
    let let_result = self let_result in
    return @@ E_let_in {let_binder={var;ascr;attributes};rhs;let_result;attr}
  | E_type_inst {forall; type_} ->
    let forall = self forall in
    let type_  = self_type type_ in
    return @@ E_type_inst {forall; type_}
  | E_raw_code {language;code} ->
    let code = self code in
    return @@ E_raw_code {language;code}
  | E_constructor {constructor; element} ->
    let element = self element in
    return @@ E_constructor {constructor; element}
  | E_matching {matchee;cases} ->
    let matchee = self matchee in
    let cases = matching_cases aliases cases in
    return @@ E_matching {matchee;cases}
  | E_record record ->
    let record = AST.LMap.map self record in
    return @@ E_record record
  | E_record_accessor {record;path} ->
    let record = self record in
    return @@ E_record_accessor {record;path}
  | E_record_update {record;path;update} ->
    let record = self record in
    let update = self update in
    return @@ E_record_update {record;path;update}
  | E_mod_in  {module_binder; rhs; let_result} ->
    let mod_aliases,path,rhs = compile_module_expr aliases rhs in
    let aliases = Aliases.push aliases module_binder mod_aliases path in
    let let_result = self ~aliases let_result in
    (match rhs with None -> let_result
    | Some rhs -> return @@ E_mod_in {module_binder;rhs;let_result})
  | E_module_accessor {module_path;element} ->
    let _,module_path = List.fold ~init:(aliases,[]) module_path ~f:(
      fun (a,module_path) mvar ->
        let aliases,path = Aliases.get_opt a mvar in
        aliases, Option.value ~default:(mvar::module_path) path) in
    let module_path = List.rev module_path in
    return @@ E_module_accessor {module_path;element}
  | E_assign {binder={var;ascr;attributes};expression} ->
    let ascr = Option.map ~f:self_type ascr in
    let expression = self expression in
    return @@ E_assign {binder={var;ascr;attributes};expression}

and matching_cases : Aliases.t -> AST.matching_expr -> AST.matching_expr = fun scope me ->
  let self ?(scope = scope) = expression scope in
  let self_type ?(scope = scope) = type_expression scope in
  let return x = x in
  match me with
    Match_variant {cases;tv} ->
    let cases = List.map ~f:AST.(fun {constructor;pattern;body} ->
        let body = self body in
        {constructor;pattern;body}
      ) cases in
    let tv   = self_type tv in
    return @@ AST.Match_variant {cases;tv}
  | Match_record {fields;body;tv} ->
    let fields = AST.LMap.map AST.(fun {var;ascr;attributes} ->
      let ascr = Option.map ~f:self_type ascr in
      {var;ascr;attributes}
    ) fields in
    let body = self body in
    let tv   = self_type tv in
    return @@ AST.Match_record {fields;body;tv}

and compile_declaration aliases (d : AST.declaration) : Aliases.t * AST.declaration option =
  let return_s aliases wrap_content = aliases, Some {d with wrap_content} in
  let return_n aliases = aliases, None in
  match Location.unwrap d with
    Declaration_constant {binder;expr;attr} ->
      let expr   = expression aliases expr in
      let binder = Stage_common.Maps.binder (type_expression aliases) binder in
      return_s aliases @@ AST.Declaration_constant {binder;expr;attr}
  | Declaration_type {type_binder;type_expr;type_attr} ->
      let type_expr = type_expression aliases type_expr in
      return_s aliases @@ AST.Declaration_type {type_binder;type_expr;type_attr}
  | Declaration_module {module_binder;module_;module_attr} ->
      let mod_aliases,path,module_  = compile_module_expr aliases module_ in
      let aliases = Aliases.push aliases module_binder mod_aliases path in
      match module_ with None -> return_n aliases
      | Some module_ -> return_s aliases @@ AST.Declaration_module {module_binder;module_;module_attr}

and compile_declaration_list aliases (program : AST.program) : Aliases.t * AST.program =
  let aliases,dcl = List.fold_map ~init:aliases ~f:(compile_declaration) program in
  let dcl = List.filter_opt dcl in
  aliases,dcl

and compile_module_expr : Aliases.t -> AST.module_expr -> Aliases.t * AST.module_variable list option * AST.module_expr option =
  fun aliases mexpr ->
    match mexpr.wrap_content with
    | M_struct prg -> (
      let aliases,prg = compile_declaration_list aliases prg in
      aliases, None, Some {mexpr with wrap_content=AST.M_struct prg}
    )
    | M_variable v -> (
      let aliases,path = Aliases.get aliases v in
      aliases, Some path, None
    )
    | M_module_path (hd,tl) -> (
    let aliases,module_path = List.fold ~init:(aliases,[]) (hd::tl) ~f:(
      fun (a,module_path) mvar ->
        let aliases,path = Aliases.get_opt a mvar in
        aliases, Option.value ~default:(mvar::module_path) path) in
      aliases, Some (module_path), None
    )



let program : AST.program -> Aliases.t * AST.program = fun prg ->
  let aliases = Aliases.empty in
  compile_declaration_list aliases prg

let expression ?(aliases = Aliases.empty) : AST.expression -> AST.expression = fun e ->
  let e = expression aliases e in
  e


