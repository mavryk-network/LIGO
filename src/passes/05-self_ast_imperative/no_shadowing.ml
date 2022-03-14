module Var = Simple_utils.Var
open Ast_imperative
open Errors
open Simple_utils.Trace

(* Prevents shadowing in the same scope. Needed for JsLIGO. *)

let rec check_block_scope ~raise vars types mods e =
   match e.expression_content with
   | E_let_in {let_binder; rhs; let_result; _} ->
      let var = let_binder.var in
      if (List.mem ~equal:ValueVar.equal vars var) then
         raise.raise @@ no_shadowing e.location
      else (
         check_block_scope ~raise [] [] [] rhs;
         check_block_scope ~raise (var :: vars) types mods let_result
      )
   | E_type_in {type_binder; let_result; _} ->
      if (List.mem ~equal:TypeVar.equal types type_binder) then
         raise.raise @@ no_shadowing e.location
      else
         check_block_scope ~raise vars (type_binder :: types) mods let_result
   | E_mod_in {module_binder; let_result; _} ->
      let mod_ = module_binder in
      if (List.mem ~equal:ModuleVar.equal mods mod_) then
         raise.raise @@ no_shadowing e.location
      else (
         check_block_scope ~raise vars types (mod_ :: mods) let_result
      )
   | _ -> ()

let peephole_expression ~raise : expression -> expression = fun e ->
   check_block_scope ~raise [] [] [] e;
   e

let peephole_module ~raise : module_ -> module_ = fun m ->
   let rec aux vars types mods = function
      Location.{wrap_content = Declaration_type t; location} :: remaining ->
         if (List.mem ~equal:TypeVar.equal types t.type_binder) then
            raise.raise @@ no_shadowing location
         else
            aux vars (t.type_binder :: types) mods remaining
   |  {wrap_content = Declaration_constant t; location} :: remaining ->
         let var = t.binder.var in
         if (List.mem ~equal:ValueVar.equal vars var) then
            raise.raise @@ no_shadowing location
         else
            aux (var :: vars) types mods remaining
   |  {wrap_content = Declaration_module t; location} :: remaining ->
            let mod_ = t.module_binder in
            if (List.mem ~equal:ModuleVar.equal mods mod_) then
               raise.raise @@ no_shadowing location
            else
               aux vars types (mod_ :: mods) remaining
   |  {wrap_content = Module_alias _; _} :: remaining ->
         aux vars types mods remaining
   | [] -> ()
   in
   aux [] [] [] m;
   m
