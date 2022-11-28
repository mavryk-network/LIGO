[@@@coverage exclude_file]

module Location = Simple_utils.Location
module Var = Simple_utils.Var
module List = Simple_utils.List
module Ligo_string = Simple_utils.Ligo_string
module Int64 = Caml.Int64
open Ligo_prim
open Types
open Format
open Simple_utils.PP_helpers

let lmap_sep value sep ppf m =
  let lst = List.sort ~compare:(fun (a, _) (b, _) -> Label.compare a b) m in
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
  if Record.is_tuple m
  then fprintf ppf format_tuple (tuple_sep value (tag sep_tuple)) m
  else fprintf ppf format_record (record_sep value (tag sep_record)) m


let tuple_or_record_sep_t value format_record sep_record format_tuple sep_tuple ppf m =
  if Record.is_tuple m
  then fprintf ppf format_tuple (tuple_sep value (tag sep_tuple)) m
  else fprintf ppf format_record (record_sep value (tag sep_record)) m


let tuple_or_record_sep_expr value =
  tuple_or_record_sep value "@[<h>record[%a]@]" " ,@ " "@[<h>( %a )@]" " ,@ "


let tuple_or_record_sep_type value =
  tuple_or_record_sep_t value "@[<h>record[%a]@]" " ,@ " "@[<h>( %a )@]" " *@ "


open Format

let rec type_content : formatter -> type_content -> unit =
 fun ppf tc ->
  match tc with
  | T_variable tv -> Type_var.pp ppf tv
  | T_sum m ->
    fprintf ppf "@[<h>sum[%a]@]" (lmap_sep_d row) (Record.LMap.to_kv_list_rev m.fields)
  | T_record m -> fprintf ppf "%a" (tuple_or_record_sep_type row) m.fields
  | T_arrow a -> Arrow.pp type_expression ppf a
  | T_constant tc -> type_injection ppf tc
  | T_singleton x -> Literal_value.pp ppf x
  | T_for_all x -> Abstraction.pp_forall type_expression ppf x


and row : formatter -> row_element -> unit =
 fun ppf { associated_type; michelson_annotation = _; decl_pos = _ } ->
  fprintf ppf "%a" type_expression associated_type


and type_injection ppf { language; injection; parameters } =
  (* fprintf ppf "[%s {| %s %a |}]" language (Ligo_string.extract injection) (list_sep_d_par type_expression) parameters *)
  ignore language;
  fprintf
    ppf
    "%s%a"
    (Literal_types.to_string injection)
    (list_sep_d_par type_expression)
    parameters


and bool ppf : unit = fprintf ppf "%a" Type_var.pp Literal_types.v_bool

and option ppf (te : type_expression) : unit =
  let t = Combinators.get_t_option te in
  match t with
  | Some t -> fprintf ppf "option (%a)" type_expression t
  | None -> fprintf ppf "option ('a)"


and type_expression ppf (te : type_expression) : unit =
  (* TODO: we should have a way to hook custom pretty-printers for some types and/or track the "origin" of types as they flow through the constraint solver. This is a temporary quick fix *)
  if Option.is_some (Combinators.get_t_bool te)
  then bool ppf
  else if Option.is_some (Combinators.get_t_option te)
  then option ppf te
  else fprintf ppf "%a" type_content te.type_content


let rec type_content_orig : formatter -> type_content -> unit =
 fun ppf tc ->
  match tc with
  | T_variable tv -> Type_var.pp ppf tv
  | T_sum m ->
    fprintf ppf "@[<h>sum[%a]@]" (lmap_sep_d row) (Record.LMap.to_kv_list_rev m.fields)
  | T_record m -> fprintf ppf "%a" (tuple_or_record_sep_type row) m.fields
  | T_arrow a -> Arrow.pp type_expression ppf a
  | T_constant tc -> type_injection ppf tc
  | T_singleton x -> Literal_value.pp ppf x
  | T_for_all x -> Abstraction.pp_forall type_expression ppf x


and type_expression_orig ppf (te : type_expression) : unit =
  (* TODO: we should have a way to hook custom pretty-printers for some types and/or track the "origin" of types as they flow through the constraint solver. This is a temporary quick fix *)
  match te.orig_var with
  | None ->
    if Option.is_some (Combinators.get_t_bool te)
    then bool ppf
    else if Option.is_some (Combinators.get_t_option te)
    then option ppf te
    else fprintf ppf "%a" type_content_orig te.type_content
  | Some v -> Ast_core.(PP.type_expression ppf (t_variable v ()))


let type_expression_annot ppf (te : type_expression) : unit =
  fprintf ppf " : %a" type_expression te


let rec expression ppf (e : expression) =
  fprintf ppf "%a" expression_content e.expression_content
(* type_expression e.type_expression *)


and expression_content ppf (ec : expression_content) =
  match ec with
  | E_literal l -> Literal_value.pp ppf l
  | E_variable n -> Value_var.pp ppf n
  | E_application a -> Application.pp expression ppf a
  | E_constructor c -> Constructor.pp expression ppf c
  | E_constant c -> Constant.pp expression ppf c
  | E_record m -> Record.pp expression ppf m
  | E_accessor a -> Types.Accessor.pp expression ppf a
  | E_update u -> Types.Update.pp expression ppf u
  | E_lambda l -> Lambda.pp expression type_expression_annot ppf l
  | E_type_abstraction e -> Type_abs.pp expression ppf e
  | E_matching m -> Types.Match_expr.pp expression type_expression ppf m
  | E_recursive r -> Recursive.pp expression type_expression_annot ppf r
  | E_let_in x when not x.attributes.hidden -> Let_in.pp expression type_expression ppf x
  | E_let_in x -> expression ppf x.let_result
  | E_raw_code r -> Raw_code.pp expression ppf r
  | E_type_inst ti -> type_inst ppf ti
  | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
    Format.fprintf
      ppf
      "@[let mut %a =@;<1 2>%a%a in@ %a@]"
      (Pattern.pp type_expression_annot)
      let_binder
      expression
      rhs
      Types.ValueAttr.pp
      attributes
      expression
      let_result
  | E_assign a -> Assign.pp expression type_expression ppf a
  | E_deref var -> Format.fprintf ppf "!%a" Value_var.pp var
  | E_for for_loop -> For_loop.pp expression ppf for_loop
  | E_for_each for_each -> For_each_loop.pp expression ppf for_each
  | E_while while_loop -> While_loop.pp expression ppf while_loop


and type_inst ppf { forall; type_ } =
  fprintf ppf "%a@@{%a}" expression forall type_expression type_


and option_inline ppf inline = if inline then fprintf ppf "[@inline]" else fprintf ppf ""

let program ppf : program -> unit =
 fun (prg, exp) -> fprintf ppf "%a" expression Combinators.(context_apply prg exp)
