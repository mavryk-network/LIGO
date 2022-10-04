[@@@coverage exclude_file]
module Location    = Simple_utils.Location
module Var         = Simple_utils.Var
module List        = Simple_utils.List
module Ligo_string = Simple_utils.Ligo_string
module Int64       = Caml.Int64
open Ligo_prim
open Types
open Format
open Simple_utils.PP_helpers

let lmap_sep value sep ppf m =
  let lst = List.sort ~compare:(fun (a,_) (b,_) -> Label.compare a b) m in
  let new_pp ppf (k, v) = fprintf ppf "@[<h>%a -> %a@]" Label.pp k value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst


let lmap_sep_d x = lmap_sep x (tag " ,@ ")

let record_sep value sep ppf (m : 'a Record.t) =
  let lst = Record.LMap.to_kv_list m in
  fprintf ppf "%a" (lmap_sep value sep) lst

let tuple_sep value sep ppf m =
  assert (Record.is_tuple m);
  let lst = Record.tuple_of_record m in
  let new_pp ppf (_, v) = fprintf ppf "%a" value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

(* Prints records which only contain the consecutive fields
   0..(cardinal-1) as tuples *)
let tuple_or_record_sep value format_record sep_record format_tuple sep_tuple ppf m =
  if Record.is_tuple m then
    fprintf ppf format_tuple (tuple_sep value (tag sep_tuple)) m
  else
    fprintf ppf format_record (record_sep value (tag sep_record)) m
let tuple_or_record_sep_t value format_record sep_record format_tuple sep_tuple ppf m =
  if Record.is_tuple m then
    fprintf ppf format_tuple (tuple_sep value (tag sep_tuple)) m
  else
    fprintf ppf format_record (record_sep value (tag sep_record)) m
let tuple_or_record_sep_expr value = tuple_or_record_sep value "@[<h>record[%a]@]" " ,@ " "@[<h>( %a )@]" " ,@ "
let tuple_or_record_sep_type value = tuple_or_record_sep_t value "@[<h>record[%a]@]" " ,@ " "@[<h>( %a )@]" " *@ "

open Format

let rec type_content : formatter -> type_content -> unit =
  fun ppf tc ->
  match tc with
  | T_variable        tv -> Type_var.pp ppf tv
  | T_sum              m -> fprintf ppf "@[<h>sum[%a]@]" (lmap_sep_d row) (Record.LMap.to_kv_list_rev m.fields)
  | T_record           m -> fprintf ppf "%a" (tuple_or_record_sep_type row) m.fields
  | T_arrow            a -> Arrow.pp      type_expression ppf a
  | T_constant        tc -> type_injection ppf tc
  | T_singleton       x  -> Literal_value.pp            ppf x
  | T_abstraction     x  -> Abstraction.pp   type_expression ppf x
  | T_for_all         x  -> Abstraction.pp   type_expression ppf x

and row : formatter -> row_element -> unit =
  fun ppf { associated_type ; michelson_annotation=_ ; decl_pos=_ } ->
    fprintf ppf "%a"
      type_expression associated_type

and type_injection ppf {language;injection;parameters} =
  (* fprintf ppf "[%s {| %s %a |}]" language (Ligo_string.extract injection) (list_sep_d_par type_expression) parameters *)
  ignore language;
  fprintf ppf "%s%a" (Literal_types.to_string injection) (list_sep_d_par type_expression) parameters
and bool ppf : unit = fprintf ppf "%a" Type_var.pp Literal_types.v_bool
and option ppf (te : type_expression) : unit =
  let t = Combinators.get_t_option te in
  (match t with
    Some t -> fprintf ppf "option (%a)" type_expression t
  | None   -> fprintf ppf "option ('a)")
and type_expression ppf (te : type_expression) : unit =
  (* TODO: we should have a way to hook custom pretty-printers for some types and/or track the "origin" of types as they flow through the constraint solver. This is a temporary quick fix *)
  if Option.is_some (Combinators.get_t_bool   te) then bool   ppf    else
  if Option.is_some (Combinators.get_t_option te) then option ppf te
  else
    fprintf ppf "%a" type_content te.type_content

let type_expression_annot ppf (te : type_expression) : unit =
  fprintf ppf " : %a" type_expression te
let type_expression_option ppf (te : type_expression option) : unit =
  match te with
  | Some te -> type_expression_annot ppf te
  | None    -> fprintf ppf ""

let rec type_content_orig : formatter -> type_content -> unit =
  fun ppf tc ->
  match tc with
  | T_variable        tv -> Type_var.pp ppf tv
  | T_sum              m -> fprintf ppf "@[<h>sum[%a]@]" (lmap_sep_d row) (Record.LMap.to_kv_list_rev m.fields)
  | T_record           m -> fprintf ppf "%a" (tuple_or_record_sep_type row) m.fields
  | T_arrow            a -> Arrow.pp      type_expression ppf a
  | T_constant        tc -> type_injection ppf tc
  | T_singleton       x  -> Literal_value.pp            ppf x
  | T_abstraction     x  -> Abstraction.pp   type_expression ppf x
  | T_for_all         x  -> Abstraction.pp   type_expression ppf x


and type_expression_orig ppf (te : type_expression) : unit =
  (* TODO: we should have a way to hook custom pretty-printers for some types and/or track the "origin" of types as they flow through the constraint solver. This is a temporary quick fix *)
  match te.orig_var with
  | None ->
    if Option.is_some (Combinators.get_t_bool   te) then bool   ppf    else
    if Option.is_some (Combinators.get_t_option te) then option ppf te
    else
      fprintf ppf "%a" type_content_orig te.type_content
  | Some v ->
     Ast_core.(PP.type_expression ppf (t_variable v ()))

let rec expression ppf (e : expression) =
  fprintf ppf "%a"
    expression_content e.expression_content

and expression_content ppf (ec: expression_content) =
  match ec with
  | E_literal     l -> Literal_value.pp   ppf l
  | E_variable    n -> Value_var.pp        ppf n
  | E_application a -> Application.pp expression ppf a
  | E_constructor c -> Constructor.pp expression ppf c
  | E_constant    c -> Constant.pp expression ppf c
  | E_record      m -> Record.pp expression ppf m
  | E_accessor    a -> Types.Accessor.pp    expression ppf a
  | E_update      u -> Types.Update.pp      expression ppf u
  | E_lambda      l -> Lambda.pp      expression type_expression ppf l
  | E_type_abstraction e -> Type_abs.pp expression ppf e
  | E_matching {matchee; cases;} ->
      fprintf ppf "@[<v 2> match @[%a@] with@ %a@]" expression matchee (matching expression) cases
  | E_recursive  r -> Recursive.pp expression type_expression ppf r
  | E_let_in {let_binder; rhs; let_result; attr = { hidden = false ; _ } as attr } ->
    fprintf ppf "@[let %a =@;<1 2>%a%a in@ %a@]"
      (Binder.pp type_expression_annot) let_binder
      expression rhs
      Types.ValueAttr.pp attr
      expression let_result
  | E_let_in {let_binder = _; rhs = _; let_result; attr = { inline = _; no_mutation = _; public=__LOC__ ; view = _ ; hidden = true ; thunk = _ } } ->
      fprintf ppf "%a" expression let_result
  | E_let_pattern_in x -> Let_pattern_in.pp expression type_expression ppf x
  | E_mod_in    mi -> Mod_in.pp  expression module_expr ppf mi
  | E_raw_code   r -> Raw_code.pp   expression ppf r
  | E_module_accessor ma -> Module_access.pp Value_var.pp ppf ma
  | E_type_inst ti -> type_inst ppf ti
  | E_let_mut_in { let_binder; rhs; let_result; attr } ->
    Format.fprintf ppf "@[let mut %a =@;<1 2>%a%a in@ %a@]"
      (Binder.pp type_expression_annot) let_binder
      expression rhs
      Types.ValueAttr.pp attr
      expression let_result
  | E_assign a -> Assign.pp expression type_expression ppf a
  | E_deref n -> Format.fprintf ppf "!%a" Value_var.pp n
  | E_for for_loop ->
    For_loop.pp expression ppf for_loop
  | E_for_each for_each ->
    For_each_loop.pp expression ppf for_each
  | E_while while_loop ->
    While_loop.pp expression ppf while_loop

and type_inst ppf {forall; type_} =
  fprintf ppf "%a@@{%a}" expression forall type_expression type_

and option_inline ppf inline =
  if inline then
    fprintf ppf "[@inline]"
  else
    fprintf ppf ""

and matching_variant_case : (formatter -> expression -> unit) -> formatter -> expression matching_content_case -> unit =
  fun f ppf {constructor=c; pattern; body} ->
  fprintf ppf "@[<v 2>| %a %a ->@ %a@]" Label.pp c Value_var.pp pattern f body

and matching : (formatter -> expression -> unit) -> _ -> matching_expr -> unit = fun f ppf m -> match m with
  | Match_variant {cases ; tv=_} ->
      fprintf ppf "@[%a@]" (list_sep (matching_variant_case f) (tag "@ ")) cases
  | Match_record {fields ; body ; tv = _} ->
      (* let with_annots f g ppf (a , b) = fprintf ppf "%a:%a" f a g b in *)
      fprintf ppf "| @[%a@] ->@ @[%a@]"
        (tuple_or_record_sep_expr (Binder.pp type_expression_annot)) fields
        f body

and declaration ?(use_hidden=true) ppf (d : declaration) = match Location.unwrap d with
    D_value vd  -> if (vd.attr.hidden && use_hidden) then () else Types.Value_decl.pp expression type_expression_option ppf vd
  | D_pattern pd -> if (pd.attr.hidden && use_hidden) then () else Types.Pattern_decl.pp expression type_expression ppf pd
  | D_type  td  -> if (td.type_attr.hidden && use_hidden) then () else Types.Type_decl.pp type_expression ppf td
  | D_module md -> if (md.module_attr.hidden && use_hidden) then () else Types.Module_decl.pp module_expr ppf md

and decl ppf d = declaration ppf d
and module_expr ppf (me : module_expr) : unit =
    Location.pp_wrap (Module_expr.pp decl) ppf me

let module_ ppf (m : module_) = list_sep decl (tag "@,") ppf m

let program ?(use_hidden=false) ppf (p : program) = list_sep (declaration ~use_hidden) (tag "@,") ppf p
