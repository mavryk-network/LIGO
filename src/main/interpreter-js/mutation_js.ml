open Interpreter
open Simple_utils
open Simple_utils.Trace
open Errors
module LT = Ligo_interpreter.Types
module LC = Ligo_interpreter.Combinators

let get_syntax ~raise syntax loc =
  match syntax with
  | Some syntax -> syntax
  | None ->
    (match Location.get_file loc with
    | None -> raise.error (Errors.generic_error loc "Could not detect syntax")
    | Some r ->
      let file = r#file in
      let syntax =
        Simple_utils.Trace.to_stdlib_result
          (Syntax.of_string_opt (Syntax_types.Syntax_name "auto") (Some file))
      in
      (match syntax with
      | Ok (r, _) -> r
      | Error _ -> raise.error (Errors.generic_error loc "Could not detect syntax")))


let mutate_some_contract
    :  raise:(interpreter_error, _) raise -> ?syntax:_ -> Z.t -> Ast_aggregated.expression
    -> (Ast_aggregated.expression * LT.mutation) option
  =
 fun ~raise:_ ?syntax:_ _z _main ->
  (* TODO(prometheansacrifice) mutation for browser *)
  None


let mutate_some_value
    :  raise:(interpreter_error, _) raise -> ?syntax:_ -> Location.t -> Z.t -> LT.value
    -> Ast_aggregated.type_expression -> (Ast_aggregated.expression * LT.mutation) option
  =
 fun ~raise:_ ?syntax:_ _loc _z _v _v_type ->
  (* TODO(prometheansacrifice) mutation for browser *)
  None


let value_gen (* TODO(prometheansacrifice) mutation for browser *)
    :  raise:(interpreter_error, _) raise -> ?small:bool
    -> ?known_addresses:LT.Contract.t list -> Ast_aggregated.type_expression
    -> LT.value QCheck.Gen.t
  =
 fun ~raise:_ ?small:_ ?known_addresses:_ _type_expr -> Obj.magic 0


let mutate_all_value
    :  raise:(interpreter_error, _) raise -> ?syntax:_ -> Location.t -> LT.value
    -> Ast_aggregated.type_expression -> (Ast_aggregated.expression * LT.mutation) list
  =
 fun ~raise:_ ?syntax:_ _loc _v _v_type -> []


let save ~dir:_ ~mutation:_ = Obj.magic 0
