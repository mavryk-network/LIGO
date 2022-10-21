(*
 This algorithm remove duplicate variables name in the same scope to remove shadowing.
*)
open Ligo_prim
open Ast_aggregated

module Scope : sig
  type t
  val empty : t
  val new_value_var  : t -> Value_var.t -> t * Value_var.t
  val get_value_var  : t -> Value_var.t -> Value_var.t
  val new_type_var   : t -> Type_var.t -> t * Type_var.t
  val get_type_var   : t -> Type_var.t -> Type_var.t
  val new_mut_var : t -> Value_var.t -> t * Value_var.t
  val get_mut_var : t -> Value_var.t -> Value_var.t
  val diff           : t -> t -> t
  type swapper = {
    value   : Value_var.t -> Value_var.t;
    type_   : Type_var.t       -> Type_var.t;
    mut : Value_var.t -> Value_var.t
  }
  val make_swapper : t -> swapper
end =
struct
  module VMap = Simple_utils.Map.Make(Value_var)
  module TMap = Simple_utils.Map.Make(Type_var)
  type t = {value:Value_var.t VMap.t;type_:Type_var.t TMap.t ; mut : Value_var.t VMap.t }
  let empty = {value = VMap.empty;type_ = TMap.empty; mut = VMap.empty }
  let new_value_var map var =
    let var' = match VMap.find_opt var map.value with
      Some (v) -> Value_var.fresh_like ~loc:(Value_var.get_location var) v
    | None -> Value_var.fresh_like var in
    let value = VMap.add var var' map.value in
    {map with value}, var'

  let get_value_var map var =
    (* The default value is for variable coming from other files *)
    Option.value ~default:var @@ VMap.find_opt var map.value
    |> Value_var.set_location  @@ Value_var.get_location var

  let new_type_var (map : t) var =
    let var' = match TMap.find_opt var map.type_ with
      Some (v) -> Type_var.fresh_like ~loc:(Type_var.get_location var) v
    | None -> Type_var.fresh_like var in
    let type_ = TMap.add var var' map.type_ in
    {map with type_}, var'

  let get_type_var (map : t) var =
    (* The default value is for variable coming from other files *)
    Option.value ~default:var @@ TMap.find_opt var map.type_
    |> Type_var.set_location @@ Type_var.get_location var
  let diff (a:t) (b:t) = {
    value   = VMap.diff Value_var.equal  a.value   b.value  ;
    type_   = TMap.diff Type_var.equal   a.type_   b.type_  ;
    mut     = VMap.diff Value_var.equal a.mut b.mut
  }

  type swapper = {
    value   : Value_var.t -> Value_var.t;
    type_   : Type_var.t       -> Type_var.t;
    mut : Value_var.t -> Value_var.t
  }
  let make_swapper (scope:t) : swapper =
    let swap_value   = List.map ~f:(fun (k,v) -> v,k) in
    let value   = VMap.of_list @@ swap_value @@ VMap.to_kv_list scope.value in
    let type_   = TMap.of_list @@ swap_value @@ TMap.to_kv_list scope.type_ in
    let mut = VMap.of_list @@ swap_value @@ VMap.to_kv_list scope.mut in
    {
      value   = (fun map v -> Option.value ~default:v @@ VMap.find_opt v map) value ;
      type_   = (fun map v -> Option.value ~default:v @@ TMap.find_opt v map) type_;
      mut   = (fun map v -> Option.value ~default:v @@ VMap.find_opt v map) mut
    }


  let get_mut_var (map : t) var = 
    Option.value ~default:var @@ VMap.find_opt var map.mut
    |> Value_var.set_location  @@ Value_var.get_location var

  let new_mut_var (map : t) var =
    let var' = match VMap.find_opt var map.mut with
      Some (v) -> Value_var.fresh_like ~loc:(Value_var.get_location var) v
    | None -> Value_var.fresh_like var in
    let mut = VMap.add var var' map.mut in
    {map with mut}, var'

end

let rec swap_type_expression : Scope.swapper -> type_expression -> type_expression = fun swaper te ->
  let self = swap_type_expression swaper in
  let return type_content = {te with type_content} in
  match te.type_content with
    T_variable ty_var ->
    let ty_var = swaper.type_ ty_var in
    return @@ T_variable ty_var
  | T_sum {fields;layout} ->
    let fields = Record.map (fun ({associated_type;michelson_annotation;decl_pos} : row_element) : row_element ->
      let associated_type = self associated_type in
      {associated_type;michelson_annotation;decl_pos}
    ) fields in
    return @@ T_sum {fields;layout}
  | T_record {fields;layout} ->
    let fields = Record.map (fun ({associated_type;michelson_annotation;decl_pos} : row_element) : row_element ->
      let associated_type = self associated_type in
      {associated_type;michelson_annotation;decl_pos}
    ) fields in
    return @@ T_record {fields;layout}
  | T_arrow {type1;type2} ->
    let type1 = self type1 in
    let type2 = self type2 in
    return @@ T_arrow {type1;type2}
  | T_constant {language;injection;parameters} ->
    let parameters = List.map ~f:self parameters in
    return @@ T_constant {language;injection;parameters}
  | T_singleton literal ->
    return @@ T_singleton literal
  | T_for_all {ty_binder;kind;type_} ->
    let ty_binder = swaper.type_ ty_binder in
    let type_ = self type_ in
    return @@ T_for_all {ty_binder;kind;type_}

let swap_binder : Scope.swapper -> _ Binder.t -> _ Binder.t = fun swaper binder ->
  let self_type = swap_type_expression swaper in
  let var = Binder.apply swaper.value binder in
  let binder = Binder.map self_type binder in
  let binder = Binder.set_var binder var in
  binder

let swap_param : Scope.swapper -> _ Param.t -> _ Param.t = fun swaper param ->
  let self_type = swap_type_expression swaper in
  let var_swapper = 
    match Param.get_mut_flag param with
    | Immutable -> swaper.value
    | Mutable -> swaper.mut
  in
  let param = Param.map self_type param in
  let param = Param.set_var param (var_swapper @@ Param.get_var param) in
  param

let swap_mut_binder : Scope.swapper -> _ Binder.t -> _ Binder.t = fun swaper binder ->
  let self_type = swap_type_expression swaper in
  let var = Binder.apply swaper.mut binder in
  let binder = Binder.map self_type binder in
  let binder = Binder.set_var binder var in
  binder

let rec swap_expression : Scope.swapper -> expression -> expression = fun swaper e ->
  let self = swap_expression swaper in
  let self_type = swap_type_expression swaper in
  let return expression_content = {e with expression_content} in
  match e.expression_content with
    E_literal literal ->
    return @@ E_literal literal
  | E_constant {cons_name;arguments} ->
    let arguments = List.map ~f:self arguments in
    return @@ E_constant {cons_name;arguments}
  | E_variable variable ->
    let variable = swaper.value variable in
    return @@ E_variable variable
  | E_application {lamb;args} ->
    let lamb = self lamb in
    let args = self args in
    return @@ E_application {lamb;args}
  | E_lambda {binder;output_type;result} ->
    let binder = swap_param swaper binder in
    let output_type = self_type output_type in
    let result = self result in
    return @@ E_lambda {binder;output_type;result}
  | E_type_abstraction {type_binder;result} ->
    let type_binder = swaper.type_ type_binder in
    let result = self result in
    return @@ E_type_abstraction {type_binder;result}
  | E_recursive {fun_name;fun_type;lambda={binder;output_type;result}} ->
    let fun_name = swaper.value fun_name in
    let fun_type = self_type fun_type in
    let binder = swap_param swaper binder in
    let output_type = self_type output_type in
    let result = self result in
    return @@ E_recursive {fun_name;fun_type;lambda={binder;output_type;result}}
  | E_let_in {let_binder;rhs;let_result;attributes} ->
    let _,let_binder = Pattern.map_pattern
      (fun p -> match p.wrap_content with P_var b -> {p with wrap_content = P_var (swap_binder swaper b)} | _ -> p)
      let_binder in
    let rhs = self rhs in
    let let_result = self let_result in
    return @@ E_let_in {let_binder;rhs;let_result;attributes}
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
    let cases = matching_cases swaper cases in
    return @@ E_matching {matchee;cases}
  | E_record record ->
    let record = Record.map self record in
    return @@ E_record record
  | E_accessor {struct_;path} ->
    let struct_ = self struct_ in
    return @@ E_accessor {struct_;path}
  | E_update {struct_;path;update} ->
    let struct_ = self struct_ in
    let update = self update in
    return @@ E_update {struct_;path;update}
  | E_assign {binder;expression} ->
    let binder = swap_mut_binder swaper binder in
    let expression = self expression in
    return @@ E_assign {binder;expression}
  | E_let_mut_in {let_binder;rhs;let_result;attributes} ->
    let _,let_binder = Pattern.map_pattern
      (fun p -> match p.wrap_content with
        | P_var b -> {p with wrap_content = P_var (swap_mut_binder swaper b)}
        | _ -> p)
      let_binder in
    let rhs = self rhs in
    let let_result = self let_result in
    return @@ E_let_mut_in {let_binder;rhs;let_result;attributes}
  | E_while while_loop ->
    let while_loop = While_loop.map self while_loop in
    return @@ E_while while_loop
  | E_for { binder; start; final; incr; f_body } ->
    let binder = swaper.value binder in
    let start = self start
    and final = self final
    and incr = self incr
    and f_body = self f_body in
    return @@ E_for { binder; start; final; incr; f_body }
  | E_for_each { fe_binder = binder1, binder2; collection; collection_type; fe_body } ->
    let fe_binder = 
      swaper.value binder1, Option.map binder2 ~f:swaper.value
    in
    let collection = self collection 
    and fe_body = self fe_body in
    return @@ E_for_each { fe_binder; collection; collection_type; fe_body }
  | E_deref mut_var -> return @@ E_deref (swaper.mut mut_var)

and matching_cases : Scope.swapper -> _ -> _ = fun swaper me ->
  let self = swap_expression swaper in
  let self_type = swap_type_expression swaper in
  let return x = x in
  match me with
    Match_variant {cases;tv} ->
    let cases = List.map ~f:(fun {constructor;pattern;body} ->
        let body = self body in
        {constructor;pattern;body}
      ) cases in
    let tv   = self_type tv in
    return @@ Match_variant {cases;tv}
  | Match_record {fields;body;tv} ->
    let fields = Record.map (Binder.map self_type) fields in
    let body = self body in
    let tv   = self_type tv in
    return @@ Match_record {fields;body;tv}

let swap_declaration : Scope.swapper -> declaration -> declaration = fun swaper decl ->
  match Location.unwrap decl with
  | D_value { binder ; expr ; attr } ->
    let binder = swap_binder swaper binder in
    let expr = swap_expression swaper expr in
    Location.wrap ~loc:(Location.get_location decl) @@ D_value { binder ; expr ; attr }
  | D_pattern { matchee ; cases } ->
    let matchee = swap_expression swaper matchee in
    let swap_binders case = 
      match case with
      | Match_variant {cases;tv} -> 
        let cases = List.map cases ~f:(fun {constructor;pattern;body} ->
          let pattern = swap_binder swaper pattern in
          {constructor;pattern;body}
        ) in
        Match_variant {cases;tv}
      | Match_record {fields;body;tv} -> 
        let fields = Record.map (swap_binder swaper) fields in
        Match_record {fields;body;tv}
    in
    let cases = swap_binders cases in
    Location.wrap ~loc:(Location.get_location decl) @@ D_pattern { matchee ; cases }
    
let rec type_expression : Scope.t -> type_expression -> type_expression = fun scope te ->
  let self ?(scope = scope) = type_expression scope in
  let return type_content = {te with type_content} in
  match te.type_content with
    T_variable ty_var ->
    let ty_var = Scope.get_type_var scope ty_var in
    return @@ T_variable ty_var
  | T_sum {fields;layout} ->
    let fields = Record.map (fun ({associated_type;michelson_annotation;decl_pos} : row_element) : row_element ->
      let associated_type = self associated_type in
      {associated_type;michelson_annotation;decl_pos}
    ) fields in
    return @@ T_sum {fields;layout}
  | T_record {fields;layout} ->
    let fields = Record.map (fun ({associated_type;michelson_annotation;decl_pos} : row_element) : row_element ->
      let associated_type = self associated_type in
      {associated_type;michelson_annotation;decl_pos}
    ) fields in
    return @@ T_record {fields;layout}
  | T_arrow {type1;type2} ->
    let type1 = self type1 in
    let type2 = self type2 in
    return @@ T_arrow {type1;type2}
  | T_constant {language;injection;parameters} ->
    let parameters = List.map ~f:self parameters in
    return @@ T_constant {language;injection;parameters}
  | T_singleton literal ->
    return @@ T_singleton literal
  | T_for_all {ty_binder;kind;type_} ->
    (* With current implementation of polymorphism, deshadowing type var breaks stuff *)
    (* let scope,ty_binder = Scope.new_type_var scope ty_binder in *)
    let type_ = self ~scope type_ in
    return @@ T_for_all {ty_binder;kind;type_}

let binder_new : Scope.t -> _ Binder.t -> Scope.t * _ Binder.t = fun scope binder ->
  let self_type ?(scope = scope) = type_expression scope in
  let scope,var = Binder.apply (Scope.new_value_var scope) binder in
  let binder = Binder.map self_type binder in
  let binder = Binder.set_var binder var in
  scope, binder

let mut_binder_new : Scope.t -> _ Binder.t -> Scope.t * _ Binder.t = fun scope binder ->
  let self_type ?(scope = scope) = type_expression scope in
  let scope,var = Binder.apply (Scope.new_mut_var scope) binder in
  let binder = Binder.map self_type binder in
  let binder = Binder.set_var binder var in
  scope, binder

let param_new : Scope.t -> _ Param.t -> Scope.t * _ Param.t = fun scope param ->
  let self_type ?(scope = scope) = type_expression scope in
  let new_var = 
    match Param.get_mut_flag param with
    | Immutable -> Scope.new_value_var
    | Mutable -> Scope.new_mut_var
  in
  let scope, var = new_var scope @@ Param.get_var param in 
  let param = Param.map self_type param in
  let param = Param.set_var param var in
  scope, param

let binder_get : Scope.t -> _ Binder.t -> _ Binder.t = fun scope binder ->
  let self_type ?(scope = scope) = type_expression scope in
  let var = Binder.apply (Scope.get_value_var scope) binder in
  let binder = Binder.map self_type binder in
  Binder.set_var binder var

let mut_binder_get : Scope.t -> _ Binder.t -> _ Binder.t = fun scope binder ->
  let self_type ?(scope = scope) = type_expression scope in
  let var = Binder.apply (Scope.get_mut_var scope) binder in
  let binder = Binder.map self_type binder in
  Binder.set_var binder var

let rec expression : Scope.t -> expression -> Scope.t * expression = fun scope e ->
  let self ?(scope = scope) = expression scope in
  let self_type ?(scope = scope) = type_expression scope in
  let return ?(scope = scope) expression_content = scope, {e with expression_content} in
  match e.expression_content with
    E_literal literal ->
    return @@ E_literal literal
  | E_constant {cons_name;arguments} ->
    let _,arguments = List.unzip @@ List.map ~f:self arguments in
    return @@ E_constant {cons_name;arguments}
  | E_variable variable ->
    let variable = Scope.get_value_var scope variable in
    return @@ E_variable variable
  | E_application {lamb;args} ->
    let _,lamb = self lamb in
    let _,args = self args in
    return @@ E_application {lamb;args}
  | E_lambda {binder;output_type;result} ->
    let scope,binder = param_new scope binder in
    let output_type = self_type output_type in
    let _,result = self ~scope result in
    return @@ E_lambda {binder;output_type;result}
  | E_type_abstraction {type_binder;result} ->
    (* With current implementation of polymorphism, deshadowing type var breaks stuff *)
    (* let scope,type_binder = Scope.new_type_var scope type_binder in *)
    let _,result = self ~scope result in
    return @@ E_type_abstraction {type_binder;result}
  | E_recursive {fun_name;fun_type;lambda={binder;output_type;result}} ->
    let fun_name = Scope.get_value_var scope fun_name in
    let fun_type = self_type fun_type in
    let scope,binder = param_new scope binder in
    let output_type = self_type output_type in
    let _,result = self ~scope result in
    return @@ E_recursive {fun_name;fun_type;lambda={binder;output_type;result}}
  | E_let_in {let_binder;rhs={expression_content=E_recursive _;} as rhs;let_result;attr} ->
    let scope,let_binder = binder_new scope let_binder in
    let _,rhs = self ~scope rhs in
    let scope,let_result = self ~scope let_result in
    return ~scope @@ E_let_in {let_binder;rhs;let_result;attr}
  | E_let_in {let_binder;rhs;let_result;attr} ->
    let scope,let_binder = binder_new scope let_binder in
    let _,rhs = self rhs in
    let scope,let_result = self ~scope let_result in
    return ~scope @@ E_let_in {let_binder;rhs;let_result;attr}
  | E_type_inst {forall; type_} ->
    let _,forall = self forall in
    let type_  = self_type type_ in
    return @@ E_type_inst {forall; type_}
  | E_raw_code {language;code} ->
    let _,code = self code in
    return @@ E_raw_code {language;code}
  | E_constructor {constructor; element} ->
    let _,element = self element in
    return @@ E_constructor {constructor; element}
  | E_matching {matchee;cases} ->
    let _,matchee = self matchee in
    let cases = matching_cases scope cases in
    return @@ E_matching {matchee;cases}
  | E_record record ->
    let _,record = Record.LMap.unzip @@ Record.LMap.map self record in
    return @@ E_record record
  | E_accessor {struct_;path} ->
    let _,struct_ = self struct_ in
    return @@ E_accessor {struct_;path}
  | E_update {struct_;path;update} ->
    let _,struct_ = self struct_ in
    let _,update = self update in
    return @@ E_update {struct_;path;update}
  | E_assign {binder;expression} ->
    let binder = mut_binder_get scope binder in
    let _,expression = self expression in
    return @@ E_assign {binder;expression}
  | E_let_mut_in {let_binder;rhs;let_result;attr} ->
    let scope, let_binder = mut_binder_new scope let_binder in
    let _, rhs = self rhs in
    let scope, let_result = self ~scope let_result in
    return ~scope @@ E_let_mut_in {let_binder;rhs;let_result;attr}
  | E_while { cond; body } ->
    let _, cond = self cond in
    let _, body = self body in
    return @@ E_while { cond; body }
  | E_for { binder; start; final; incr; f_body } ->
    let scope, binder = Scope.new_value_var scope binder in
    let _, start = self start
    and _, final = self final
    and _, incr = self incr
    and scope, f_body = self ~scope f_body in
    return ~scope @@ E_for { binder; start; final; incr; f_body }
  | E_for_each { fe_binder = binder1, binder2; collection; collection_type; fe_body } ->
    let scope, binder1 = Scope.new_value_var scope binder1 in
    let scope, binder2 = Option.fold_map Scope.new_value_var scope binder2 in
    let _, collection = self collection 
    and scope, fe_body = self ~scope fe_body in
    return ~scope @@ E_for_each { fe_binder = binder1, binder2; collection; collection_type; fe_body }
  | E_deref mut_var -> return @@ E_deref (Scope.get_mut_var scope mut_var)
  

and matching_cases : Scope.t -> matching_expr -> matching_expr = fun scope me ->
  let self ?(scope = scope) = expression scope in
  let self_type ?(scope = scope) = type_expression scope in
  let return x = x in
  match me with
    Match_variant {cases;tv} ->
    let cases = List.map ~f:(fun {constructor;pattern;body} ->
        let scope,pvar = Scope.new_value_var scope (Binder.get_var pattern) in
        let pattern = Binder.set_var pattern pvar in
        let _,body = self ~scope body in
        {constructor;pattern;body}
      ) cases in
    let tv   = self_type tv in
    return @@ Match_variant {cases;tv}
  | Match_record {fields;body;tv} ->
    let scope,fields = Record.fold_map (binder_new) scope fields in
    let _,body = self ~scope body in
    let tv   = self_type tv in
    return @@ Match_record {fields;body;tv}


let program : expression -> expression = fun e ->
  let scope = Scope.empty in
  let scope,e = expression scope e in
  let swapper = Scope.make_swapper scope in
  let e = swap_expression swapper e in
  e

let declaration : Scope.t -> declaration -> Scope.t * declaration = fun scope decl ->
  match Location.unwrap decl with
  | D_value { binder ; expr ; attr } ->
    let scope,binder = binder_new scope binder in
    let _,expr = expression scope expr in
    scope, Location.wrap ~loc:(Location.get_location decl) @@ D_value { binder ; expr ; attr }
  | D_pattern { matchee ; cases } ->
    let binders_new scope case = 
      match case with
      | Match_variant {cases;tv} -> 
        let scope, cases = List.fold_map cases ~init:scope 
          ~f:(fun scope {constructor;pattern;body} ->
            let scope, pattern = binder_new scope pattern in
            scope, {constructor;pattern;body}
          ) 
        in
        scope, Match_variant {cases;tv}
      | Match_record {fields;body;tv} -> 
        let scope, fields = Record.fold_map (binder_new) scope fields in
        scope, Match_record {fields;body;tv}
    in
    let scope, cases = binders_new scope cases in
    let _, matchee = expression scope matchee in
    scope, Location.wrap ~loc:(Location.get_location decl) @@ D_pattern { matchee ; cases }
  

let program_ : program -> program = fun (ctxt, prg) ->
  let scope = Scope.empty in
  let scope, prg = expression scope prg in
  let f decl (scope, ctxt) = let scope, decl = declaration scope decl in scope, decl :: ctxt in
  let scope, ctxt = List.fold_right ctxt ~init:(scope, []) ~f in
  let swapper = Scope.make_swapper scope in
  let prg = swap_expression swapper prg in
  let f decl ctxt = let decl = swap_declaration swapper decl in decl :: ctxt in
  let ctxt = List.fold_right ctxt ~init:[] ~f in
  ctxt, prg
