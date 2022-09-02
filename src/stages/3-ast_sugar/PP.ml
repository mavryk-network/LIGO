[@@@coverage exclude_file]
open Types
open Format
open Simple_utils.PP_helpers
open Ligo_prim

let rec type_content : formatter -> type_expression -> unit =
  fun ppf te ->
  match te.type_content with
  | T_variable        tv -> TypeVar.pp ppf tv
  | T_sum             sm -> Rows.PP.sum type_expression ppf sm.fields
  | T_record          rd -> Rows.PP.type_record type_expression ppf rd.fields
  | T_tuple            t -> Rows.PP.type_tuple  type_expression ppf t
  | T_arrow            a -> Arrow.pp      type_expression ppf a
  | T_app            app -> Type_app.pp      type_expression ppf app
  | T_module_accessor ma -> Module_access.pp TypeVar.pp ppf ma
  | T_singleton       x  -> Literal_value.pp            ppf x
  | T_abstraction     x  -> Abstraction.pp   type_expression ppf x
  | T_for_all         x  -> Abstraction.pp   type_expression ppf x

and type_expression ppf (te : type_expression) : unit =
  fprintf ppf "%a" type_content te

and type_expression_annot ppf (te : type_expression) : unit =
  fprintf ppf " : %a" type_expression te

let type_expression_option ppf (te : type_expression option) : unit =
  match te with
  | Some te -> fprintf ppf " : %a" type_expression te
  | None    -> fprintf ppf ""

let rec expression ppf (e : expression) =
  expression_content ppf e.expression_content
and expression_content ppf (ec : expression_content) =
  match ec with
  | E_literal     l -> Literal_value.pp   ppf l
  | E_variable    n -> ValueVar.pp        ppf n
  | E_application a -> Application.pp expression ppf a
  | E_constructor c -> Constructor.pp expression ppf c
  | E_constant    c -> Constant.pp expression ppf c
  | E_record      m -> Record.pp expression ppf m
  | E_tuple       t -> Record.pp_tuple       expression ppf t
  | E_accessor    a -> Types.Accessor.pp    expression ppf a
  | E_update      u -> Types.Update.pp      expression ppf u
  | E_lambda      l -> Lambda.pp      expression type_expression_option ppf l
  | E_type_abstraction e -> Type_abs.pp expression ppf e
  | E_matching    m -> Match_expr.pp expression type_expression_option ppf m
  | E_recursive  r -> Recursive.pp expression type_expression_annot ppf r
  | E_let_in     li -> Let_in.pp expression type_expression_option ppf li
  | E_type_in   ti -> Type_in.pp expression type_expression ppf ti
  | E_mod_in    mi -> Types.Declaration.PP.mod_in  expression decl ppf mi
  | E_raw_code   r -> Raw_code.pp   expression ppf r
  | E_ascription a -> Ascription.pp expression type_expression ppf a
  | E_module_accessor ma -> Module_access.pp ValueVar.pp ppf ma
  | E_cond       c -> Conditional.pp expression ppf c
  | E_sequence   s -> Sequence.pp   expression ppf s
  | E_skip       _ -> Skip.pp                  ppf ()
  | E_map        m -> Map_expr.pp   expression ppf m
  | E_big_map    m -> Map_expr.pp   expression ppf m
  | E_list       l -> Set_expr.pp   expression ppf l
  | E_set        s -> Set_expr.pp   expression ppf s
  | E_assign     a -> Assign.pp     expression type_expression_option ppf a


and declaration ppf (d : declaration) = Types.Declaration.PP.declaration expression type_expression decl ppf (Location.unwrap d)
and decl ppf (Types.Decl d) = declaration ppf d


let program ppf (p : program) = list_sep declaration (tag "@,") ppf p
