(* open Errors *)

(* open Simple_utils.Trace *)

module Utils = Simple_utils.Utils
module List = Simple_utils.List
module Trace = Simple_utils.Trace
open Trace

module I = Cst.Pascaligo
module O = Ast_unified_poc.Ast
open Ast_unified_poc.Combinators

module Attr = Lexing_shared.Attr
module Location = Simple_utils.Location
(* module Var = Stage_common.Var *)

module Errors = struct
  type t = [
  | `Concrete_pascaligo_unknown_constant of string * Location.t
  | `Concrete_pascaligo_unsupported_pattern_type of I.pattern
  | `Concrete_pascaligo_unsupported_string_singleton of I.type_expr
  | `Concrete_pascaligo_michelson_type_wrong of I.type_expr * string
  | `Concrete_pascaligo_michelson_type_wrong_arity of Location.t * string
  | `Concrete_pascaligo_untyped_recursive_fun of Location.t
  | `Concrete_pascaligo_block_start_with_attribute of I.block I.Region.reg
  | `Concrete_pascaligo_unsupported_top_level_destructuring of I.Region.t
  | `Concrete_pascaligo_unsupported_type_ann_on_patterns of I.Region.t
  | `Concrete_pascaligo_ignored_attribute of Location.t
  | `Concrete_pascaligo_expected_variable of Location.t
  | `Concrete_pascaligo_expected_field_name of I.Region.t
  | `Concrete_pascaligo_expected_field_or_access of I.Region.t
  | `Concrete_pascaligo_wrong_functional_lens of I.Region.t
  (* | `Concrete_pascaligo_unexpected_wildcard of I.Region.t *)
  | `Concrete_pascaligo_wrong_functional_updator of I.Region.t
  | `Concrete_pascaligo_unsuported_pattern_in_function of I.Region.t
  | `Concrete_pascaligo_wrong_lvalue of I.Region.t
  ] [@@deriving poly_constructor { prefix = "concrete_pascaligo_" }]
end
open Errors

let nseq_to_list (hd, tl) = hd :: tl
let npseq_to_list (hd, tl) = hd :: (List.map ~f:snd tl)
let npseq_to_ne_list (hd, tl) = (hd, List.map ~f:snd tl)
let build_ins = ["Operator";"Test";"Tezos";"Crypto";"Bytes";"List";"Set";"Map";"Big_map";"Bitwise";"String";"Layout";"Option"]


(* open Predefined.Tree_abstraction.Pascaligo *)


let r_split = Location.r_split

let w_split (x: 'a I.Wrap.t) : 'a * Location.t =
  (x#payload, Location.lift x#region)

(* let mk_var ~loc var =
  if String.equal var "_" then Var.fresh ~loc ~name:"_" ()
  else Var.of_input_var ~loc var

let compile_variable (var : I.variable) =
  let (var,loc) = w_split var in mk_var ~loc var *)
let rec e_unpar : I.expr -> I.expr =
  function
    E_Par e -> e_unpar e.value.inside
  | e -> e
let rec get_var : I.expr -> (string * Location.t) option =
  function
  | E_Par x -> get_var x.value.inside
  | E_Var v -> Some (w_split v)
  | _ -> None
(* let compile_var_opt : I.expr -> AST.expression_variable option = fun expr ->
  Option.map (get_var expr) ~f:(fun (v,loc) -> mk_var ~loc v) *)

let compile_attributes : I.attribute list -> O.source_attribute list = fun _as ->
  let f : I.attribute -> O.source_attribute = fun a ->
    let ((k,v),location) = r_split a in
    let v = Option.map ~f:(function I.Attr.String x -> x) v in
    { wrap_content = (k,v) ; location }
  in
  List.map ~f _as

let compile_selection : I.selection -> 'a O.access * Location.t = function
  | FieldName name ->
    let name, loc = w_split name in
    Access_record name, loc
  | Component comp ->
    let (_, index), loc = w_split comp in
    Access_tuple  index, loc

(* let rec compile_type_expression ~(raise :Errors.abs_error Simple_utils.Trace.raise) : ?attr:I.attribute list -> I.type_expr -> AST.type_expression =
  fun ?(attr = []) te ->
    let self = compile_type_expression ~raise in
    match te with
    | T_Attr (x,te) -> compile_type_expression ~attr:(x::attr) ~raise te
    | T_Var v ->
      let (v,loc) = w_split v in
      t_variable_ez ~loc v

    | T_Cart {region; value = (fst, _, rest)} ->
      let loc = Location.lift region in
      let lst = List.map ~f:self (fst::npseq_to_list rest) in
      t_tuple ~loc lst

    | T_Sum { region ; value } ->
      let loc = Location.lift region in
      let attr = compile_attributes attr in
      let cases = Utils.nsepseq_to_list value.variants in
      let f : I.variant I.reg -> string * AST.type_expression * string list =
        fun { value = {ctor ; ctor_args ; attributes} ; region } ->
          let _loc = Location.lift region in
          let t = Option.value ~default:(t_unit ()) (Option.map ~f:(self <@ snd) ctor_args) in
          let case_attr = compile_attributes attributes in
          (ctor#payload, t, case_attr)
      in
      t_sum_ez_attr ~loc ~attr (List.map ~f cases)

    | T_App { region ; value = (type_constant, args) } -> (
      let loc = Location.lift region in
      let get_t_string_singleton_opt =
        function
        | I.T_String s -> Some s#payload
        | _ -> None
      in
      let get_t_int_singleton_opt = function
        | I.T_Int x ->
          let (_,z) = x#payload in
          Some z
        | _ -> None
      in
      match type_constant with
      | T_Var v when String.equal v#payload "michelson_or" -> (
        let lst = npseq_to_list args.value.inside in
        match lst with
        | [a ; b ; c ; d ] -> (
          let b' = trace_option ~raise (michelson_type_wrong te v#payload) @@ get_t_string_singleton_opt b in
          let d' = trace_option ~raise (michelson_type_wrong te v#payload) @@ get_t_string_singleton_opt d in
          let a' = self a in
          let c' = self c in
          t_michelson_or ~loc a' b' c' d'
          )
        | _ -> raise.raise @@ michelson_type_wrong_arity loc v#payload
      )
      | T_Var v when String.equal v#payload "michelson_pair" -> (
        let lst = npseq_to_list args.value.inside in
        match lst with
        | [a ; b ; c ; d ] -> (
          let b' = trace_option ~raise (michelson_type_wrong te v#payload) @@ get_t_string_singleton_opt b in
          let d' = trace_option ~raise (michelson_type_wrong te v#payload) @@ get_t_string_singleton_opt d in
          let a' = self a  in
          let c' = self c  in
          t_michelson_pair ~loc a' b' c' d'
        )
        | _ -> raise.raise @@ michelson_type_wrong_arity loc v#payload
      )
      | T_Var v when String.equal v#payload "sapling_state" -> (
        let lst = npseq_to_list args.value.inside in
        match lst with
        | [(a : I.type_expr)] -> (
          let sloc = Location.lift @@ Raw.type_expr_to_region a in
          let a' = trace_option ~raise (michelson_type_wrong te v#payload) @@ get_t_int_singleton_opt a in
          let singleton = t_singleton ~loc:sloc (Literal_int a') in
          t_sapling_state ~loc singleton
        )
        | _ -> raise.raise @@ michelson_type_wrong_arity loc v#payload
      )
      | T_Var v when String.equal v#payload "sapling_transaction" -> (
        let lst = npseq_to_list args.value.inside in
        match lst with
        | [(a : I.type_expr)] -> (
          let sloc = Location.lift @@ I.type_expr_to_region a in
          let a' = trace_option ~raise (michelson_type_wrong te v#payload) @@ get_t_int_singleton_opt a in
          let singleton = t_singleton ~loc:sloc (Literal_int a') in
          t_sapling_transaction ~loc singleton
          )
        | _ -> raise.raise @@ michelson_type_wrong_arity loc v#payload
      )
      | T_Var type_constant -> (
        let operator = mk_var ~loc:(Location.lift type_constant#region) type_constant#payload in
        let lst = npseq_to_list args.value.inside in
        let lst = List.map ~f:self lst in
        t_app ~loc operator lst
      )
      | _ -> failwith "TODO: t_app in the AST should be able to take a type expression in"
    )
    | T_Fun { region ; value = (lhs , _ , rhs) } -> (
      let loc = Location.lift region in
      t_arrow ~loc (self lhs) (self rhs)
    )
    | T_ModPath { region ; value = { module_path ; selector = _ ; field } } -> (
      let _loc = Location.lift region in
      let field = self field in
      let f : I.module_name -> AST.type_expression -> AST.type_expression =
        fun x prev ->
          let (v,loc) = w_split x in
          let module_name =  mk_var ~loc v in
          let loc = Location.cover loc prev.location in
          t_module_accessor ~loc module_name prev
      in
      List.fold_right ~init:field ~f (npseq_to_list module_path)
    )
    | T_Par { region ; value = { lpar = _ ; inside ; rpar = _} } -> (
      let loc = Location.lift region in
      let inside = self inside in
      { inside with location = loc }
    )
    | T_Record {region; value = {kind = _loc; opening =_;
                                 elements; closing=_; terminator=_}} -> (
      let elements = Utils.sepseq_to_list elements in
      let f : I.field_decl I.reg -> string * AST.type_expression * AST.attributes = fun decl ->
        let (({field_name;field_type;attributes} : I.field_decl), _loc) = r_split decl in
        let t =
          match field_type with
          | None -> (* type punning: { x } -> { x : x } *)
            let (v , loc) = w_split field_name in
            t_variable_ez ~loc v
          | Some (_colon , x) -> self x
        in
        let attributes = compile_attributes attributes in
        (field_name#payload , t ,attributes)
      in
      let lst = List.map ~f elements in
      t_record_ez_attr ~loc:(Location.lift region) ~attr:(compile_attributes attr) lst
    )
    | T_Int _ | T_String _ -> raise.raise @@ unsupported_string_singleton te *)

let rec compile_expression ~(raise :Errors.t Simple_utils.Trace.raise) : ?attr:I.attribute list -> I.expr -> O.expr = fun ?(attr = []) e ->
  ignore attr ;
  let self = compile_expression ~raise in
  (* let compile_tuple_expression : I.expr I.tuple -> O.expression = fun tuple_expr ->
    let (lst, loc) = r_split tuple_expr in
    let (hd,tl) = lst.inside in
    let tl = List.map ~f:snd tl in
    match tl with
    | [] -> self hd
    | _ ->
      let lst = List.map ~f:self (hd::tl) in
      e_tuple ~loc lst
  in
  *)
  let compile_bin_op : O.external_notation -> _ I.bin_op I.reg -> O.expression = fun notation op ->
    let (op, loc) = r_split op in
    let (_,loc_notation) = w_split op.op in
    let a = self op.arg1 in
    let b = self op.arg2 in
    make_e ~loc (e_external_operator O.{ op = Location.wrap ~loc:loc_notation notation ; args = [ a ; b] })
  in
  let compile_un_op : O.external_notation -> _ I.un_op I.reg -> O.expression = fun notation op ->
    let (op, loc) = r_split op in
    let (_,loc_notation) = w_split op.op in
    let a = self op.arg in
    make_e ~loc (e_external_operator O.{ op = Location.wrap ~loc:loc_notation notation ; args = [ a ] })
  in
  (*
  let compile_pseudomodule_access ~loc : I.expr -> string -> string = fun field module_name ->
    match field with
    | E_Var v -> v#payload
    | E_ModPath _ -> raise.raise @@ unknown_constant module_name loc
    | _ -> failwith "Corner case : This couldn't be produce by the parser"
  in *)
  match e with
  | E_Var var -> (
    let (var, loc) = w_split var in
    Location.wrap ~loc (O.E_external_variable var) 
  )
  | E_Par par -> self par.value.inside
  | E_Bytes bytes_ ->
    let (bytes_, loc) = w_split bytes_ in
    let (_s,b) = bytes_ in
    e_bytes_hex ~loc b
  | E_String str ->
    let (str, loc) = w_split str in
    e_string ~loc (Simple_utils.Ligo_string.standard str)
  | E_Verbatim str ->
    let (str, loc) = w_split str in
    e_string ~loc (Simple_utils.Ligo_string.verbatim str)
  | E_Cat x   -> compile_bin_op O.Concat x
  | E_Add x   -> compile_bin_op O.Plus_numbers x
  | E_Sub x   -> compile_bin_op O.Minus x
  | E_Mult x  -> compile_bin_op O.Mul x
  | E_Div x   -> compile_bin_op O.Div x
  | E_Mod x   -> compile_bin_op O.Mod x
  | E_Or x    -> compile_bin_op O.Or x
  | E_And x   -> compile_bin_op O.And x
  | E_Lt x    -> compile_bin_op O.Lt x
  | E_Leq x   -> compile_bin_op O.Leq x
  | E_Gt x    -> compile_bin_op O.Gt x
  | E_Geq x   -> compile_bin_op O.Geq x
  | E_Equal x -> compile_bin_op O.Equal x
  | E_Neq x   -> compile_bin_op O.Neq x
  | E_Neg x   -> compile_un_op O.Minus x
  | E_Not x   -> compile_un_op O.Not x
  | E_Int i ->
    let ((_,i), loc) = w_split i in
    e_int ~loc i
  | E_Nat n ->
    let ((_,n), loc) = w_split n in
    e_nat ~loc n
  | E_Mutez mtez ->
    let ((_,mtez), loc) = w_split mtez in
    e_mutez ~loc (Z.of_int64 mtez)
  | E_Call call ->
    let ((f, args), loc) = r_split call in
    let (tup,tup_loc) = r_split args in
    let args = List.map ~f:self (Utils.nsepseq_to_list args.value.inside) in
    make_e ~loc (e_complete_pseudo_call { f = self f ; arg_tuple = Location.wrap ~loc:tup_loc args })
  | E_Tuple lst ->
    let (tup,loc) = r_split lst in
    let args = List.map ~f:self (Utils.nsepseq_to_list tup.inside) in
    make_e ~loc (e_tuple args)
  | E_Record record ->
    let (record, loc) = r_split record in
    let aux : (I.expr, I.expr) I.field I.reg -> O.source_attribute O.field_ =
      fun field ->
      let (fa, loc_field) = r_split field in
      match fa with
      | I.Punned { pun ; attributes } ->
        let (label,label_loc) = trace_option ~raise (expected_field_name @@ I.expr_to_region pun) @@ get_var pun in
        { label = Location.wrap ~loc:label_loc (O.Label label) ;
          rhs = self pun ;
          attributes = compile_attributes attributes }
      | I.Complete {field_lhs ; field_lens ; field_rhs ; attributes } -> (
        match field_lens with
        | Lens_Id _ ->
          let (label,label_loc) = trace_option ~raise (expected_field_name @@ I.expr_to_region field_lhs) @@ get_var field_lhs in
          { label = Location.wrap ~loc:label_loc (O.Label label) ;
          rhs = self field_rhs ;
          attributes = compile_attributes attributes }
        | _ -> raise.raise (wrong_functional_lens @@ I.field_lens_to_region field_lens)
      )
    in
    let fields = List.map ~f:aux (Utils.sepseq_to_list record.elements) in
    make_e ~loc (e_record fields)
  
  | _ -> failwith "wait"

  (*
  | E_Proj proj ->
    let (proj, loc) = r_split proj in
    let expr = self proj.record_or_tuple in
    let (sels, _) = List.unzip @@ List.map ~f:compile_selection @@ Utils.nsepseq_to_list proj.field_path in
    e_accessor ~loc expr sels
  | E_ModPath ma -> (
    let (ma, loc) = r_split ma in
    match ma.module_path with
    | (module_name,[]) when List.mem ~equal:Caml.(=) build_ins module_name#payload -> (
      (*TODO: move to proper module*)
      let fun_name = compile_pseudomodule_access ~loc ma.field module_name#payload in
      let var = module_name#payload ^ "." ^ fun_name in
      match constants var with
      | Some const -> e_constant ~loc const []
      | None -> e_variable_ez ~loc var
    )
    | _ -> (
      let field = self ma.field in
      let f : I.module_name -> O.expression -> O.expression =
        fun module_name acc ->
          let (name,loc) = w_split module_name in
          e_module_accessor ~loc (mk_var ~loc name) acc
      in
      let lst = Utils.nsepseq_to_list ma.module_path in
      List.fold_right lst ~f ~init:field
    )
  )
  | E_Update { value = { structure ; kwd_with=_ ; update } ; region } -> (
    let loc = Location.lift region in
    let structure = self structure in
    match update with
    | E_Record record_lhs -> (
      let f : O.expression -> (I.expr, I.expr) I.field I.reg -> O.expression = fun acc x ->
        let field_loc = Location.lift x.region in
        match x.value with
        | I.Complete {field_lhs ; field_lens ; field_rhs ; attributes} -> (
          check_no_attributes ~raise field_loc attributes;
          let field_rhs = self field_rhs in
          let func_update self_accessor =
            match field_lens with
            | Lens_Id _ -> field_rhs
            | Lens_Add _ -> e_add ~loc:field_loc self_accessor field_rhs
            | Lens_Sub _ -> e_sub ~loc:field_loc self_accessor field_rhs
            | Lens_Mult _ -> e_mult ~loc:field_loc self_accessor field_rhs
            | Lens_Div _ -> e_div ~loc:field_loc self_accessor field_rhs
            | Lens_Fun _ -> e_application ~loc:field_loc self_accessor field_rhs
          in
          match field_lhs with
          | I.E_Var x -> (
            let label = fst @@ w_split x in
            let self_accessor = e_accessor ~loc structure [O.Access_record label] in
            e_update ~loc acc [O.Access_record label] (func_update self_accessor)
          )
          | I.E_Proj {region ; value = {record_or_tuple ; selector = _ ; field_path }} -> (
            let (label,_) = trace_option ~raise (expected_variable (Location.lift @@ I.expr_to_region record_or_tuple)) @@ get_var record_or_tuple in
            let path =
              let path = List.map (Utils.nsepseq_to_list field_path)
                ~f:(function FieldName x -> Access_record x#payload | Component x -> Access_tuple (snd @@ fst @@ w_split x))
              in
              (O.Access_record label::path)
            in
            let self_accessor = e_accessor ~loc:(Location.lift region) structure path in
            e_update ~loc acc path (func_update self_accessor)
          )
          | x -> raise.raise (expected_field_or_access @@ I.expr_to_region x)
        )
        | I.Punned {pun ; attributes} -> (
          check_no_attributes ~raise field_loc attributes;
          let label,v =
            let (label,loc) = trace_option ~raise (expected_variable (Location.lift @@ I.expr_to_region pun)) @@ get_var pun in
            if String.equal label "_" then raise.raise (unexpected_wildcard @@ I.expr_to_region pun) ;
            (label, mk_var ~loc label)
          in
          e_update ~loc acc [O.Access_record label] (e_variable v)
        )
      in
      List.fold_left (Utils.sepseq_to_list record_lhs.value.elements) ~f ~init:structure
    )
    | x -> raise.raise (wrong_functional_updator @@ I.expr_to_region x)
  )
  | E_Fun { value = { parameters; ret_type ; return ; _ } ; region} -> (
    check_no_attributes ~raise (Location.lift region) attr ;
    let compile_param : I.param_decl I.reg -> _  = fun { value = { param_kind ; pattern ; param_type } ; region } ->
      (* TODO: feels wrong, binders do not have loc in AST *)
      let _loc = Location.lift region in
      let (var, loc) =
        match pattern with
        | P_Var x -> w_split x
        | x -> raise.raise (unsuported_pattern_in_function @@ I.pattern_to_region x)
      in
      let ascr = Option.map ~f:(compile_type_expression ~raise <@ snd) param_type in
      let var = mk_var ~loc var in
      let attributes =
        match param_kind with
        | `Const _kwd -> Stage_common.Helpers.const_attribute
        | `Var _kwd -> Stage_common.Helpers.var_attribute
      in
      { var ; ascr ; attributes }
    in
    let loc = Location.lift region in
    let (lambda, fun_type) =
      let (params, loc_par)  = r_split parameters in
      let params = Utils.nsepseq_map compile_param params.inside in
      let body = self return in
      let ret_ty = Option.map ~f:(compile_type_expression ~raise <@ snd ) ret_type in
      match params with
      | (binder, []) ->
        let expr = e_lambda ~loc binder ret_ty body in
        let ty_opt = Option.map ~f:(fun (a,b) -> t_arrow ~loc a b) (Option.bind_pair (binder.ascr,ret_ty)) in
        (expr, ty_opt)
      | (hd,tl) ->
        let params = hd::(List.map ~f:snd tl) in
        let input_tuple_ty =
          (* TODOpoly: polymorphism should give some leeway (using Option.all feels wrong) *)
          let in_tys_opt = Option.all @@ List.map ~f:(fun b -> b.ascr) params in
          Option.map ~f:t_tuple in_tys_opt
        in
        let binder = Var.fresh ~loc ~name:"parameter" () in
        let expr =
          let body = e_matching_tuple ~loc:loc_par (e_variable binder) params body in
          e_lambda_ez ~loc binder ?ascr:input_tuple_ty ret_ty body
        in
        let ty_opt = Option.map ~f:(fun (a,b) -> t_arrow a b) (Option.bind_pair (input_tuple_ty,ret_ty)) in
        (expr, ty_opt)
    in
    Option.value_map ~default:lambda ~f:(e_annotation ~loc lambda) fun_type
  )
  | E_Ctor constr -> (
    let (v,loc) = w_split constr in
    match v with
    | "Unit" -> e_unit ~loc ()
    | _ -> e_constructor ~loc v (e_unit ())
  )
  | E_App x -> (
    let ((expr, args_opt), loc) = r_split x in
    match expr , args_opt with
    | I.E_Ctor x , None when String.equal (fst (w_split x)) "Unit" ->
      e_unit ~loc ()
    | I.E_Ctor x , _ ->
      let (ctor_name,_loc) = w_split x in
      let args_o = Option.map ~f:compile_tuple_expression args_opt in
      let args = Option.value ~default:(e_unit ()) args_o in
      e_constructor ~loc ctor_name args
    | _ -> failwith "impossible"
  )
  | E_Case case -> (
    let (I.{cases ; expr ; _}, loc) = r_split case in
    let matchee = self expr in
    let cases = compile_matching_expr ~raise self @@ npseq_to_ne_list cases in
    e_matching ~loc matchee cases
  )
  | E_Typed annot -> (
    let (annot, loc) = r_split annot in
    let (expr, (_,ty)) = annot.inside in
    let expr = self expr in
    let ty   = compile_type_expression ~raise ty  in
    e_annotation ~loc expr ty
  )
  | E_Cond cond -> (
    let (cond, loc) = r_split cond in
    let test        = self cond.test in
    let then_clause = self cond.if_so in
    let else_clause =
      match cond.if_not with
      | Some (_else, ifnot) -> self ifnot
      | None -> e_unit ~loc ()
    in
    e_cond ~loc test then_clause else_clause
  )
  | E_List lc -> (
    let (lc,loc) = r_split lc in
    let lst =
      Option.value ~default:[] @@
      Option.map ~f:npseq_to_list lc.elements
    in
    let lst = List.map ~f:self lst in
    e_list ~loc lst
  )
  | E_Cons cons -> (
    let (cons, loc) = r_split cons in
    let a  = self cons.arg1 in
    let b  = self cons.arg2 in
    e_constant ~loc (Const C_CONS) [a; b]
  )
  | E_Set set -> (
    let (si, loc) = r_split set in
    let set =
      Option.value ~default:[] @@
      Option.map ~f:npseq_to_list si.elements
    in
    let set = List.map ~f:self set in
    e_set ~loc set
  )
  | E_SetMem sm -> (
    let (sm, loc) = r_split sm in
    let set  = self sm.set in
    let elem = self sm.element in
    e_constant ~loc (Const C_SET_MEM) [elem;set]
  )
  | E_MapLookup mlu -> (
    let (mlu, loc) = r_split mlu in
    let expr  = self mlu.map in
    let keys = List.map ~f:(fun x -> Access_map (self x.value.inside)) (nseq_to_list mlu.keys) in
    e_accessor ~loc expr keys
  )
  | E_Map map -> (
      let (mij, loc) = r_split map in
      let lst = Option.value ~default:[] @@
        Option.map ~f:npseq_to_list mij.elements in
      let aux (binding : I.binding I.reg) =
        let (binding, _) = r_split binding in
        let key   = self binding.key in
        let value = self binding.value in
        (key,value)
      in
      let map = List.map ~f:aux lst in
      e_map ~loc map
  )
  | E_BigMap mij -> (
    let (mij, loc) = r_split mij in
    let lst = Option.value ~default:[] @@
      Option.map ~f:npseq_to_list mij.elements in
    let aux (binding : I.binding I.reg) =
      let (binding, _) = r_split binding in
      let key   = self binding.key in
      let value = self binding.value in
      (key,value)
    in
    let map = List.map ~f:aux lst in
    e_big_map ~loc map
  )
  | E_CodeInj ci ->
    let (ci, loc) = r_split ci in
    let (language, _) = r_split ci.language in
    let (language, _) = r_split language in
    let code = self ci.code in
    e_raw_code ~loc language code
  | E_Block be ->
    let be, _ = r_split be in
    let next = self be.expr in
    compile_block ~raise ~next be.block
  | E_Nil nil -> (
    let (_,loc) = w_split nil in
    e_list ~loc []
  )
  | E_Attr (a,x) -> compile_expression ~raise ~attr:(a::attr) x

and conv ~raise : ?const:bool -> I.pattern -> O.ty_expr O.pattern =
  fun ?(const = false) p ->
    let self = conv ~raise ~const in
    match p with
    | P_Verbatim _ | P_Attr _ -> raise.raise (unsupported_pattern_type p)
    | P_Var var -> (
      let (var,loc) = w_split var in
      let attributes = if const then Stage_common.Helpers.const_attribute else Stage_common.Helpers.var_attribute in
      let b =
        let var = mk_var ~loc var in
        { var ; ascr = None ; attributes }
      in
      Location.wrap ~loc (P_var b)
    )
    | P_Tuple tuple -> (
      let (tuple, loc) = r_split tuple in
      let lst = npseq_to_ne_list tuple.inside in
      let patterns = List.Ne.to_list lst in
      let nested = List.map ~f:self patterns in
      match nested with (* (x) == x *)
      | [x] -> x
      | _ -> Location.wrap ~loc @@ P_tuple nested
    )
    | P_App constr_pattern -> (
      let ((constr,p_opt), loc) = r_split constr_pattern in
      let rec get_ctor : I.pattern -> string option = function
        | P_Par x -> get_ctor x.value.inside
        | P_Ctor x -> Some x#payload
        | _ -> None
      in
      match get_ctor constr with
      | Some "Unit" -> Location.wrap ~loc @@ P_unit
      | Some label ->
        let carg = match p_opt with
          | Some p -> self (I.P_Tuple p)
          | None -> Location.wrap ~loc P_unit
        in
        Location.wrap ~loc @@ P_variant (Label label, carg)
      | None -> raise.raise (unsupported_pattern_type p)
    )
    | P_List { region ; value = { elements ; _ } } -> (
      (* let () = check_no_attributes attr in *)
      let loc = Location.lift region in
      let elements = Utils.sepseq_to_list elements in
      let f : I.pattern -> O.type_expression O.pattern -> O.type_expression O.pattern =
        fun x prev ->
          let p = self x in
          Location.wrap (P_list (Cons (p, prev)))
      in
      List.fold_right ~f ~init:(Location.wrap ~loc (P_list (List []))) elements
    )
    | P_Cons { region ; value = (hd,_,tl) } -> (
      let loc = Location.lift region in
      let hd = self hd in
      let tl = self tl in
      Location.wrap ~loc (P_list (Cons (hd,tl)))
    )
    | P_Nil (x: _ I.wrap) -> (
      let loc = Location.lift x#region in
      Location.wrap ~loc (P_list (List []))
    )
    | P_Par { region = _ ; value } -> (
      self value.inside
    )
    | P_Record { region ; value = { elements ; _ }} -> (
      let loc = Location.lift region in
      let lst = Utils.sepseq_to_list elements in
      let aux : I.field_pattern I.reg -> O.label * O.ty_expr O.pattern =
        fun x ->
          let (field, field_loc) = r_split x in
          match field with
          | Punned { pun ; attributes } ->
            check_no_attributes ~raise field_loc attributes ;
            let (label,loc) = match pun with
              | P_Var pun -> w_split pun
              | x -> raise.raise (expected_field_name @@ I.pattern_to_region x)
            in
            if String.equal label "_" then raise.raise (unexpected_wildcard @@ I.pattern_to_region pun) ;
            let attributes = if const then Stage_common.Helpers.const_attribute else Stage_common.Helpers.var_attribute in
            let binder = { var = mk_var ~loc label ; ascr = None ; attributes } in
            (O.Label label , Location.wrap ~loc (P_var binder))
          | Complete { field_lhs; field_rhs ; attributes ; _} ->
            check_no_attributes ~raise field_loc attributes ;
            let (lhs,_loc) = match field_lhs with
              | P_Var x -> w_split x
              | x -> raise.raise (expected_field_or_access @@ I.pattern_to_region x)
            in
            (O.Label lhs, self field_rhs)
      in
      let lst' = List.map ~f:aux lst in
      let (labels,patterns) = List.unzip lst' in
      Location.wrap ~loc (P_record (labels,patterns))
    )
    | P_Typed {region ; value = { pattern ; type_annot = (_,ty_expr) }} -> (
      let loc = Location.lift region in
      let p = self pattern in
      let ty_expr = compile_type_expression ~raise ty_expr in
      match p.wrap_content with
      | P_var x -> Location.wrap ~loc (P_var {x with ascr = Some ty_expr})
      | _ -> raise.raise (unsupported_type_ann_on_patterns @@ I.pattern_to_region pattern)
    )
    | P_Ctor x -> (
      let (c,loc) = w_split x in
      match x#payload with
      | "Unit" -> Location.wrap ~loc P_unit
      | _ -> Location.wrap ~loc (P_variant (Label c, Location.wrap P_unit))
    )
    | P_ModPath _ | P_Mutez _ | P_Bytes _ | P_Int _ | P_Nat _ | P_String _ -> raise.raise @@ unsupported_pattern_type p

and compile_matching_expr : type a . raise:'b raise -> (a-> O.expression) -> a I.case_clause I.reg List.Ne.t -> (O.expression, O.ty_expr) O.match_case list =
  fun ~raise compiler cases ->
    let aux (case : a I.case_clause I.reg) =
      let (case, _loc) = r_split case in
      let expr    = compiler case.rhs in
      (case.pattern, expr)
    in
    let cases = List.Ne.map aux cases in
    let cases : (I.pattern * O.expression) list = List.Ne.to_list cases in
    let aux : (I.pattern * O.expression) -> (O.expression , O.ty_expr) match_case =
      fun (raw_pattern, body) ->
        let pattern = conv ~raise ~const:true raw_pattern in
        { pattern ; body }
    in
    List.map ~f:aux cases

and compile_parameters ~raise : I.parameters -> (O.type_expression O.binder) list = fun params ->
  let aux : I.param_decl I.reg -> O.type_expression O.binder = fun param ->
    let (param, _loc) = r_split param in
    let (var, loc) = match param.pattern with
      | P_Var param -> w_split param
      | x -> raise.raise (unsuported_pattern_in_function @@ I.pattern_to_region x)
    in
    match param.param_kind with
    | `Var _ ->
      let var = mk_var ~loc var in
      let ascr = Option.map ~f:(compile_type_expression ~raise <@ snd) param.param_type in
      { var ; ascr ; attributes = Stage_common.Helpers.var_attribute }
    | `Const _ ->
      let var = mk_var ~loc var in
      let ascr = Option.map ~f:(compile_type_expression ~raise  <@ snd) param.param_type in
      { var ; ascr ; attributes = Stage_common.Helpers.const_attribute }
  in
  let (params, _loc) = r_split params in
  let params = npseq_to_list params.inside in
  List.map ~f:aux params

and compile_path : (I.selection, I.dot) Utils.nsepseq -> O.expression O.access list =
  fun x ->
    let f : I.selection -> O.expression O.access = function
      | FieldName name -> Access_record name#payload
      | Component v -> Access_tuple (snd v#payload)
    in
    List.map (Utils.nsepseq_to_list x) ~f


and compile_instruction ~raise : ?next: O.expression -> I.instruction -> O.expression  = fun ?next instruction ->
  let return expr = Option.value_map next ~default:expr ~f:(e_sequence expr) in
  let compile_if_clause : ?next:O.expression -> I.test_clause -> O.expression =
    fun ?next if_clause ->
      match if_clause with
      | ClauseInstr i -> compile_instruction ~raise ?next i
      | ClauseBlock block -> compile_block ~raise ?next block
  in
  match instruction with
  | I_Cond { region ; value = { test ; if_so ; if_not ; _ } } -> (
    let loc = Location.lift region in
    let test = compile_expression ~raise test in
    let ifso = compile_if_clause if_so in
    let ifnot = Option.value_map if_not ~default:(e_skip ()) ~f:(fun x -> compile_if_clause (snd x)) in
    return @@ e_cond ~loc test ifso ifnot
  )
  | I_Case { region ; value = { expr ; cases ; _ } } -> (
    let loc = Location.lift region in
    let matchee = compile_expression ~raise expr in
    let cases = compile_matching_expr ~raise compile_if_clause (npseq_to_ne_list cases) in
    return @@ e_matching ~loc matchee cases
  )
  | I_Assign {region ; value = { lhs ; rhs ; _ }} -> (
    let loc = Location.lift region in
    let (v,path) = path_of_lvalue ~raise lhs in
    match List.rev path with
    | [] ->
      let rhs = compile_expression ~raise rhs in
      return @@ e_assign ~loc v [] rhs
    | last_access::path -> (
      let path = List.rev path in
      match last_access with
      | Access_map k ->
        let default_rhs = e_map_add ~loc k (compile_expression ~raise rhs) (e_variable v) in
        let last_proj_update = fun last_proj -> e_map_add ~loc k (compile_expression ~raise rhs) last_proj in
        return @@ compile_assignment ~loc ~last_proj_update ~lhs:v ~path ~default_rhs
      | Access_record _ | Access_tuple _ ->
        let rhs = compile_expression ~raise rhs in
        let default_rhs = e_update ~loc (e_variable v) [last_access] rhs in
        let last_proj_update = fun last_proj -> e_update ~loc last_proj [last_access] rhs in
        return @@ compile_assignment ~loc ~last_proj_update ~lhs:v ~path ~default_rhs
    )
  )
  | I_While { region ; value = { cond ; block ; _ } } -> (
    let loc = Location.lift region in
    let cond = compile_expression ~raise cond in
    let body = compile_block ~raise block in
    return @@ e_while ~loc cond body
  )
  | I_For { value = { index ; init ; bound ; step ; block ; _ } ; region } -> (
    let loc = Location.lift region in
    let index =
      let (index,loc) = w_split index in
      mk_var ~loc index
    in
    let start = compile_expression ~raise init in
    let bound = compile_expression ~raise bound in
    let increment = Option.value_map step ~default:(e_int_z Z.one) ~f:(compile_expression ~raise <@ snd) in
    let body  = compile_block ~raise block in
    return @@ e_for ~loc index start bound increment body
  )
  | I_ForIn (ForMap { region ; value = { binding = (key,_,value) ; collection ; block ; _ } }) -> (
    let loc = Location.lift region in
    let binder =
      let (key, loc) = w_split key in
      let key' = mk_var ~loc key in
      let (value, loc) = w_split value in
      let value' = mk_var ~loc value in
      (key', Some value')
    in
    let collection = compile_expression ~raise collection in
    let body = compile_block ~raise block in
    return @@ e_for_each ~loc binder collection Map body
  )
  | I_ForIn (ForSetOrList { region ; value = { var ; for_kind ; collection ; block ; _ } }) -> (
    let loc = Location.lift region in
    let binder =
      let (var, loc) = w_split var in
      let var = mk_var ~loc var in
      (var, None)
    in
    let collection = compile_expression ~raise collection in
    let body = compile_block ~raise block in
    return @@ e_for_each ~loc binder collection (match for_kind with `Set _ -> Set | `List _ -> List) body
  )
  | I_Skip s -> (
    let loc = Location.lift s#region in
    return @@ e_skip ~loc ()
  )
  | I_Patch { region ; value = { collection ; patch ; patch_kind ; _ } } -> (
    let loc = Location.lift region in
    let (v,path) = path_of_lvalue ~raise collection in
    let patch = compile_expression ~raise patch in
    let last_proj_update, default_rhs =
      match patch.expression_content, patch_kind with
      | E_map kvl , `Map _ ->
        let f acc (k,v) = e_map_add ~loc k v acc in
        (fun last_proj -> List.fold kvl ~f ~init:last_proj), List.fold kvl ~f ~init:(e_variable v)
      | E_record kl , `Record _ ->
        let f acc (Label label,expr) = e_update ~loc acc [Access_record label] expr in
        (fun last_proj -> List.fold kl ~f ~init:last_proj), List.fold kl ~f ~init:(e_variable v)
      | E_set lst , `Set _ ->
        let f acc v = e_set_add ~loc v acc in
        (fun last_proj -> List.fold lst ~f ~init:last_proj), List.fold lst ~f ~init:(e_variable v)
      | _ -> failwith "impossible patch rhs"
    in
    return @@ compile_assignment ~loc ~last_proj_update ~lhs:v ~path ~default_rhs
  )
  | I_Remove { region ; value = { item; collection ; remove_kind ; _ }} -> (
    let loc = Location.lift region in
    let (v,path) = path_of_lvalue ~raise collection in
    let item = compile_expression ~raise item in
    let remove_func = match remove_kind with
      | `Set _ -> e_set_remove ~loc
      | `Map _ -> e_map_remove ~loc
    in
    let default_rhs = remove_func item (e_variable v) in
    let last_proj_update = fun prev_proj -> remove_func item prev_proj in
    return @@ compile_assignment ~loc ~last_proj_update ~lhs:v ~path ~default_rhs
  )
  | I_Call { region ; value = (f,args) } -> (
    return @@ compile_expression ~raise (E_Call { region ; value = (f,args)})
  )

and compile_let_destructuring ~raise :
  ?const:bool -> Location.t -> I.expr -> I.pattern -> O.expression -> O.type_expression option -> O.expression =
    fun ?(const = false) loc value pattern body ty_opt ->
      let init = compile_expression ~raise value in
      let pattern = conv ~raise ~const pattern in
      let match_case = { pattern ; body } in
      let match_ = e_matching ~loc init [match_case] in
      Option.value_map ty_opt ~default:match_ ~f:(e_annotation ~loc match_)

and compile_data_declaration ~raise : ?attr:I.attribute list -> next:O.expression -> I.declaration -> O.expression =
  fun ?(attr = []) ~next data_decl ->
  let return loc var ascr var_attr attr init =
    e_let_in ~loc {var;ascr;attributes=var_attr} attr init next
  in
  match data_decl with
  | D_Attr (a,x) -> compile_data_declaration ~raise ~attr:(a::attr) ~next x
  | D_Const const_decl -> (
    let cd, loc = r_split const_decl in
    let type_ = Option.map ~f:(compile_type_expression ~raise <@ snd) cd.const_type in
    match cd.pattern with
    | P_Var name -> (
      let name, ploc = w_split name in
      let init = compile_expression ~raise cd.init in
      let p = mk_var ~loc:ploc name in
      let attr = compile_attributes attr in
      return loc p type_ Stage_common.Helpers.const_attribute attr init
    )
    | pattern ->
      (* not sure what to do with  attributes in that case *)
      compile_let_destructuring ~raise ~const:true loc cd.init pattern next type_
  )
  | D_Directive _ -> next
  | D_Fun fun_decl -> (
    let fun_decl, loc = r_split fun_decl in
    let attr = compile_attributes [] in
    let fun_var, fun_type, lambda = compile_fun_decl ~raise fun_decl in
    return loc fun_var fun_type Stage_common.Helpers.empty_attribute attr lambda
  )
  | D_Type type_decl -> (
    let td,loc = r_split type_decl in
    let name,loc_name = w_split td.name in
    let rhs = compile_type_expression ~raise td.type_expr in
    let name = mk_var ~loc:loc_name name in
    e_type_in ~loc name rhs next
  )
  | D_Module module_decl -> (
    let md,loc = r_split module_decl in
    let name,loc_name = w_split md.name in
    let rhs = compile_module ~raise md.declarations in
    e_mod_in ~loc (mk_var ~loc:loc_name name) rhs next
  )
  | D_ModAlias module_alias -> (
    let ma,loc = r_split module_alias in
    let alias,loc_alias = w_split ma.alias in
    let lst = Utils.nsepseq_to_list ma.mod_path in
    let binders = List.map lst ~f:(fun x -> let (x,loc) = (w_split x) in mk_var ~loc x) in
    e_mod_alias ~loc (mk_var ~loc:loc_alias alias) (List.Ne.of_list binders) next
  )

and compile_statement ~raise : ?next:O.expression -> I.statement -> O.expression option =
  fun ?next statement ->
  match statement with
  | S_Attr (_,statement) -> compile_statement ~raise ?next statement
  | S_Instr i ->
    let i = compile_instruction ~raise ?next i in
    Some i
  | S_Decl dd ->
    let next = Option.value ~default:(e_skip ()) next in
    let dd = compile_data_declaration ~raise ~next dd in
    (Some dd)
  | S_VarDecl var_decl -> (
    let vd, loc = r_split var_decl in
    let type_ = Option.map ~f:(compile_type_expression ~raise <@ snd) vd.var_type in
    match vd.pattern with
    | P_Var name -> (
      let name, ploc = w_split name in
      let init = compile_expression ~raise vd.init in
      let var = mk_var ~loc:ploc name in
      match next with
      | Some next ->
        Some (e_let_in ~loc {var;ascr=type_;attributes=Stage_common.Helpers.var_attribute} [] init next)
      | None -> None
    )
    | pattern ->
      (* not sure what to do with  attributes in that case *)
      match next with
      | Some next ->
        let x = compile_let_destructuring ~raise ~const:false loc vd.init pattern next type_ in
        Some x
      | None -> None
  )

and compile_block ~raise : ?next:O.expression -> I.block I.reg -> O.expression =
  fun ?next block ->
    let (block', _loc) = r_split block in
    let statements = npseq_to_list block'.statements in
    let aux statement next = compile_statement ~raise ?next statement in
    let block' = List.fold_right ~f:aux ~init:next statements in
    match block' with
    | Some block -> block
    | None -> raise.raise @@ block_start_with_attribute block

and compile_fun_decl ~raise : I.fun_decl -> O.expression_variable * O.type_expression option * O.expression =
  fun ({kwd_recursive; kwd_function=_; fun_name; type_params; parameters; ret_type; kwd_is=_; return=r; terminator=_ }: I.fun_decl) ->
  let _ = ignore type_params in (*REMITODO: this should be handlded after merge with dev ??? *)
  let (fun_name, loc) = w_split fun_name in
  let fun_binder = mk_var ~loc fun_name in
  let ret_type = Option.map ~f:(compile_type_expression ~raise <@ snd) ret_type in
  let param = compile_parameters ~raise parameters in
  let result = compile_expression ~raise r in
  let (lambda, fun_type) =
    match param with
    | binder::[] ->
      let lambda : _ O.lambda = { binder; output_type = ret_type; result } in
      (lambda , Option.map ~f:(fun (a,b) -> t_arrow a b) @@ Option.bind_pair (binder.ascr,ret_type))
    | lst ->
      let lst = Option.all @@ List.map ~f:(fun e -> e.ascr) lst in
      let input_type = Option.map ~f:t_tuple lst in
      let var = Var.fresh ~name:"parameters" () in
      let binder = { var;ascr=input_type;attributes=Stage_common.Helpers.const_attribute} in
      let result = e_matching_tuple (e_variable var) param result in
      let lambda : _ O.lambda = { binder ; output_type = ret_type ; result } in
      (lambda, Option.map ~f:(fun (a,b) -> t_arrow a b) @@ Option.bind_pair (input_type,ret_type))
  in
  let func =
    match kwd_recursive with
    | Some reg ->
      let fun_type = trace_option ~raise (untyped_recursive_fun loc) @@ fun_type in
      e_recursive ~loc:(Location.lift reg#region) fun_binder fun_type lambda
    | None -> make_e ~loc @@ E_lambda lambda
  in
  (* This handle polymorphic annotation *)
  let func = Option.value_map ~default:func ~f:(fun tp ->
    let (tp,loc) = r_split tp in
    let tp : I.type_params = tp.inside in
    let type_vars = List.Ne.map compile_variable @@ npseq_to_ne_list tp in
    List.Ne.fold_right ~f:(fun t e -> e_type_abs ~loc t e) ~init:func type_vars
  ) type_params in
  (fun_binder, fun_type, func)

and compile_declaration ~raise : ?attr:I.attribute list -> I.declaration -> O.declaration =
  fun ?(attr=[]) decl ->
  let return reg decl = [Location.wrap ~loc:(Location.lift reg) decl] in
  match decl with
  | D_Attr (a,x) -> compile_declaration ~attr:(a::attr) ~raise x
  | D_Type { value = { name ; params ; type_expr; _ } ; region } ->
      let name, loc = w_split name in
      let type_expr =
        let rhs = compile_type_expression ~raise type_expr in
        match params with
        | None -> rhs
        | Some x ->
          let lst = Utils.nsepseq_to_list x.value.inside in
          let aux : I.variable -> O.type_expression -> O.type_expression =
            fun param type_ ->
              let (param,ploc) = w_split param in
              let ty_binder = mk_var ~loc:ploc param in
              t_abstraction ~loc:(Location.lift region) ty_binder () type_
          in
          List.fold_right ~f:aux ~init:rhs lst in
      let ast =
        O.Declaration_type {type_binder= mk_var ~loc name;
                              type_expr; type_attr=[]}
      in return region ast

  | D_Const {value={pattern; const_type; init; _}; region} -> (
    let attr = compile_attributes attr in
    match pattern with
    | P_Var name ->
      let name, loc = w_split name in
      let var = mk_var ~loc name in
      let ascr =
        (* drop the polymorphic variable *)
        Option.map ~f:(compile_type_expression ~raise <@ snd) const_type in
      let expr = compile_expression ~raise init in
      let attributes = Stage_common.Helpers.const_attribute in
      let binder = {var; ascr; attributes} in
      let ast = O.Declaration_constant {binder; attr; expr} in
      return region ast
    | _ ->
        raise.raise (unsupported_top_level_destructuring region)
  )

  | D_Fun {value; region} ->
    let var, ascr, expr = compile_fun_decl ~raise value in
    let attributes = Stage_common.Helpers.empty_attribute in
    let binder = {var; ascr; attributes} in
    let ast = O.Declaration_constant {binder; attr = compile_attributes attr; expr} in
    return region ast

  | D_Module {value; region} ->
    let { name; declarations ; _ } : I.module_decl = value in
    let name, loc = w_split name in
    let module_ = compile_module ~raise declarations in
    let ast = O.Declaration_module {module_binder= mk_var ~loc name; module_; module_attr=[]} in
    return region ast

  | D_ModAlias {value; region} ->
    let {alias; mod_path; _} : I.module_alias = value in
    let alias, loc = w_split alias in
    let alias = mk_var ~loc alias in
    let lst = Utils.nsepseq_to_list mod_path in
    let binders = List.Ne.of_list @@ List.map lst ~f:(fun x -> let x,loc = (w_split x) in mk_var ~loc x) in
    let ast = O.Module_alias {alias; binders} in
    return region ast

  | D_Directive _ -> []

and compile_module ~raise : I.declaration Utils.nseq -> O.module_ =
  fun decl ->
    let lst = List.map ~f:(compile_declaration ~raise) @@ nseq_to_list decl
    in List.concat lst *)
