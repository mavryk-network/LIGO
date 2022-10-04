[@@@coverage exclude_file]

module Int64 = Caml.Int64
open Simple_utils.PP_helpers
open Ligo_prim
open Types
open Format

let rec type_expression ppf (te : type_expression) : unit =
  (* TODO: we should have a way to hook custom pretty-printers for some types and/or track the "origin" of types as they flow through the constraint solver. This is a temporary quick fix *)
  if Option.is_some (Combinators.get_t_bool te)
  then bool ppf
  else if Option.is_some (Combinators.get_t_option te)
  then option ppf te
  else fprintf ppf "%a" type_content te.type_content


and bool ppf = fprintf ppf "%a" Type_var.pp Ligo_prim.Literal_types.v_bool

and option ppf (te : type_expression) =
  let t = Combinators.get_t_option te in
  match t with
  | Some t -> fprintf ppf "option (%a)" type_expression t
  | None -> assert false


and type_content : formatter -> type_content -> unit =
 fun ppf te ->
  match te with
  | T_variable tv -> Type_var.pp ppf tv
  | T_sum m -> Format.fprintf ppf "@[<h>sum[%a]@]" (Rows.pp type_expression) m
  | T_record m ->
    Format.fprintf ppf "@[<hv>record[%a]@]" (Rows.pp type_expression) m
  | T_tuple t -> Tuple.pp_type type_expression ppf t
  | T_arrow a -> Arrow.pp type_expression ppf a
  | T_app app -> Type_app.pp type_expression ppf app
  | T_module_accessor ma -> Module_access.pp Type_var.pp ppf ma
  | T_singleton x -> Literal_value.pp ppf x
  | T_abstraction x -> Abstraction.pp type_expression ppf x
  | T_for_all x -> Abstraction.pp type_expression ppf x


let type_expression_option ppf (te : type_expression option) : unit =
  match te with
  | Some te -> fprintf ppf " : %a" type_expression te
  | None -> fprintf ppf ""


let rec expression ppf (e : expression) =
  fprintf ppf "@[%a@]" expression_content e.expression_content


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
  | E_lambda l -> Lambda.pp expression type_expression_option ppf l
  | E_type_abstraction e -> Type_abs.pp expression ppf e
  | E_matching m -> Match_expr.pp expression type_expression_option ppf m
  | E_recursive r -> Recursive.pp expression type_expression ppf r
  | E_let_in let_in -> Let_in.pp expression type_expression_option ppf let_in
  | E_type_in ti -> Type_in.pp expression type_expression ppf ti
  | E_mod_in mi -> Mod_in.pp expression module_expr ppf mi
  | E_raw_code r -> Raw_code.pp expression ppf r
  | E_ascription a -> Ascription.pp expression type_expression ppf a
  | E_module_accessor ma -> Module_access.pp Value_var.pp ppf ma
  | E_let_mut_in let_in ->
    Let_in.pp_mut expression type_expression_option ppf let_in
  | E_assign a -> Assign.pp expression type_expression_option ppf a
  | E_for for_loop -> For_loop.pp expression ppf for_loop
  | E_for_each for_each -> For_each_loop.pp expression ppf for_each
  | E_while while_loop -> While_loop.pp expression ppf while_loop
  | E_cond c -> Conditional.pp expression ppf c
  | E_sequence s -> Sequence.pp expression ppf s
  | E_skip -> Skip.pp ppf ()
  | E_map m -> Map_expr.pp expression ppf m
  | E_big_map m -> Map_expr.pp expression ppf m
  | E_list l -> Set_expr.pp expression ppf l
  | E_set s -> Set_expr.pp expression ppf s
  | E_tuple t -> Tuple.pp_expr expression ppf t


and declaration ppf (d : declaration) =
  match Location.unwrap d with
  | D_value vd -> Types.Value_decl.pp expression type_expression_option ppf vd
  | D_type td -> Types.Type_decl.pp type_expression ppf td
  | D_module md -> Types.Module_decl.pp module_expr ppf md


and decl ppf d = declaration ppf d

and module_expr ppf (me : module_expr) : unit =
  Location.pp_wrap (Module_expr.pp decl) ppf me


let program ppf (p : program) = list_sep declaration (tag "@,") ppf p
