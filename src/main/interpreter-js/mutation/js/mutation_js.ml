open Simple_utils
open Simple_utils.Trace
open Errors
module LT = Ligo_interpreter.Types
module LC = Ligo_interpreter.Combinators
open Ligo_prim

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
 fun ~raise ?syntax z main ->
  (* TODO(prometheansacrifice) mutation for browser *)
  None


let mutate_some_value
    :  raise:(interpreter_error, _) raise -> ?syntax:_ -> Location.t -> Z.t -> LT.value
    -> Ast_aggregated.type_expression -> (Ast_aggregated.expression * LT.mutation) option
  =
 fun ~raise ?syntax loc z v v_type ->
  (* TODO(prometheansacrifice) mutation for browser *)
  None

(* let rec value_gen *)
(*     :  raise:(interpreter_error, _) raise -> ?small:bool *)
(*     -> ?known_addresses:LT.Contract.t list -> Ast_aggregated.type_expression *)
(*     -> LT.value QCheck.Gen.t *)
(* TODO(prometheansacrifice) mutation for browser *)
