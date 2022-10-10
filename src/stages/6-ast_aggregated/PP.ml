[@@@coverage exclude_file]

module Location = Simple_utils.Location
module Var = Simple_utils.Var
module List = Simple_utils.List
module Ligo_string = Simple_utils.Ligo_string
module Int64 = Caml.Int64
open Simple_utils.PP_helpers
open Ligo_prim
open Types

let rec type_content : Format.formatter -> type_content -> unit =
 fun ppf tc ->
  match tc with
  | T_variable tv -> Type_var.pp ppf tv
  | T_sum m -> Format.fprintf ppf "@[<h>sum[%a]@]" (Rows.pp type_expression) m
  | T_record m ->
    Format.fprintf ppf "@[<hv>record[%a]@]" (Rows.pp type_expression) m
  | T_tuple t -> Tuple.pp_type type_expression ppf t
  | T_arrow a -> Arrow.pp type_expression ppf a
  | T_constant tc -> type_injection ppf tc
  | T_singleton x -> Literal_value.pp ppf x
  | T_for_all x -> Abstraction.pp type_expression ppf x


and type_injection ppf { language; injection; parameters } =
  (* fprintf ppf "[%s {| %s %a |}]" language (Ligo_string.extract injection) (list_sep_d_par type_expression) parameters *)
  ignore language;
  Format.fprintf
    ppf
    "%s%a"
    (Literal_types.to_string injection)
    (list_sep_d_par type_expression)
    parameters


and bool ppf : unit = Format.fprintf ppf "%a" Type_var.pp Literal_types.v_bool

and option ppf (te : type_expression) : unit =
  let t = Combinators.get_t_option te in
  match t with
  | Some t -> Format.fprintf ppf "option (%a)" type_expression t
  | None -> Format.fprintf ppf "option ('a)"


and type_expression ppf (te : type_expression) : unit =
  (* TODO: we should have a way to hook custom pretty-printers for some types and/or track the "origin" of types as they flow through the constraint solver. This is a temporary quick fix *)
  if Option.is_some (Combinators.get_t_bool te)
  then bool ppf
  else if Option.is_some (Combinators.get_t_option te)
  then option ppf te
  else Format.fprintf ppf "%a" type_content te.type_content


let rec type_content_orig : Format.formatter -> type_content -> unit =
 fun ppf tc ->
  match tc with
  | T_variable tv -> Type_var.pp ppf tv
  | T_sum m -> Format.fprintf ppf "@[<h>sum[%a]@]" (Rows.pp type_expression) m
  | T_record m ->
    Format.fprintf ppf "@[<hv>record[%a]@]" (Rows.pp type_expression) m
  | T_tuple t -> Tuple.pp_type type_expression ppf t
  | T_arrow a -> Arrow.pp type_expression ppf a
  | T_constant tc -> type_injection ppf tc
  | T_singleton x -> Literal_value.pp ppf x
  | T_for_all x -> Abstraction.pp type_expression ppf x


and type_expression_orig ppf (te : type_expression) : unit =
  (* TODO: we should have a way to hook custom pretty-printers for some types and/or track the "origin" of types as they flow through the constraint solver. This is a temporary quick fix *)
  match te.orig_var with
  | None ->
    if Option.is_some (Combinators.get_t_bool te)
    then bool ppf
    else if Option.is_some (Combinators.get_t_option te)
    then option ppf te
    else Format.fprintf ppf "%a" type_content_orig te.type_content
  | Some v -> Ast_core.(PP.type_expression ppf (t_variable v ()))


let type_expression_annot ppf (te : type_expression) : unit =
  Format.fprintf ppf " : %a" type_expression te


let rec expression ppf (e : expression) =
  Format.fprintf ppf "%a" expression_content e.expression_content


and expression_content ppf (ec : expression_content) =
  match ec with
  | E_literal l -> Literal_value.pp ppf l
  | E_variable n -> Value_var.pp ppf n
  | E_application a -> Application.pp expression ppf a
  | E_constructor c -> Constructor.pp expression ppf c
  | E_constant c -> Constant.pp expression ppf c
  | E_record m -> Record.pp expression ppf m
  | E_tuple tuple -> Tuple.pp_expr expression ppf tuple
  | E_accessor a -> Types.Accessor.pp expression ppf a
  | E_update u -> Types.Update.pp expression ppf u
  | E_lambda l -> Lambda.pp expression type_expression_annot ppf l
  | E_type_abstraction e -> Type_abs.pp expression ppf e
  | E_matching { matchee; cases } ->
    Format.fprintf
      ppf
      "@[<v 2> match @[%a@] with@ %a@]"
      expression
      matchee
      (matching expression)
      cases
  | E_recursive r -> Recursive.pp expression type_expression_annot ppf r
  | E_let_in ({ attributes = { hidden = false; _ }; _ } as let_in) ->
    Let_in.pp expression type_expression_annot ppf let_in
  | E_let_in { attributes = { hidden = true; _ }; let_result; _ } ->
    Format.fprintf ppf "@[<h>%a@]" expression let_result
  | E_raw_code r -> Raw_code.pp expression ppf r
  | E_type_inst ti -> type_inst ppf ti
  | E_let_mut_in let_in ->
    Let_in.pp_mut expression type_expression_annot ppf let_in
  | E_assign a -> Assign.pp expression type_expression ppf a
  | E_deref var -> Format.fprintf ppf "!%a" Value_var.pp var
  | E_for for_loop -> For_loop.pp expression ppf for_loop
  | E_for_each for_each -> For_each_loop.pp expression ppf for_each
  | E_while while_loop -> While_loop.pp expression ppf while_loop


and type_inst ppf { forall; type_ } =
  Format.fprintf ppf "%a@@{%a}" expression forall type_expression type_


and option_inline ppf inline =
  if inline then fprintf ppf "[@inline]" else fprintf ppf ""


and matching_variant_case f ppf { constructor = c; pattern; body } =
  Format.fprintf
    ppf
    "@[<v 2>| %a %a ->@ %a@]"
    Label.pp
    c
    Value_var.pp
    pattern
    f
    body


and matching f ppf m =
  match m with
  | Match_variant { cases; tv = _ } ->
    Format.fprintf
      ppf
      "@[%a@]"
      (list_sep (matching_variant_case f) (tag "@ "))
      cases
  | Match_record { fields; body; tv = _ } ->
    Format.fprintf
      ppf
      "| @[%a@] ->@ @[%a@]"
      (Record.pp (Binder.pp type_expression_annot))
      fields
      f
      body
  | Match_tuple { binders; body; _ } ->
    Format.fprintf
      ppf
      "| @[%a@] ->@ @[%a@]"
      (Tuple.pp_expr (Binder.pp type_expression_annot))
      binders
      f
      body


let program ppf (prg, exp) =
  Format.fprintf ppf "%a" expression Combinators.(context_apply prg exp)
