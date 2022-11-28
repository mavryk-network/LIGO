open Ligo_prim
open Ast_imperative
open Errors
open Simple_utils.Trace

(* Prevents shadowing in the same scope. Needed for JsLIGO. *)

let rec check_block_scope ~raise vars types mods e =
  match e.expression_content with
  | E_let_in { let_binder; rhs; let_result; _ } ->
    let var = Binder.get_var let_binder in
    if List.mem ~equal:Value_var.equal vars var
    then raise.error @@ no_shadowing e.location
    else (
      check_block_scope ~raise [] [] [] rhs;
      check_block_scope ~raise (var :: vars) types mods let_result)
  | E_type_in { type_binder; let_result; _ } ->
    if List.mem ~equal:Type_var.equal types type_binder
    then raise.error @@ no_shadowing e.location
    else check_block_scope ~raise vars (type_binder :: types) mods let_result
  | E_mod_in { module_binder; let_result; _ } ->
    let mod_ = module_binder in
    if List.mem ~equal:Module_var.equal mods mod_
    then raise.error @@ no_shadowing e.location
    else check_block_scope ~raise vars types (mod_ :: mods) let_result
  | _ -> ()


let peephole_expression ~raise : expression -> expression =
 fun e ->
  check_block_scope ~raise [] [] [] e;
  e


let peephole_program ~raise : program -> program =
 fun m ->
  let self (m : program) =
    let rec aux vars types mods = function
      | Location.{ wrap_content = D_value t; location } :: remaining ->
        let var = Binder.get_var t.binder in
        if List.mem ~equal:Value_var.equal vars var
        then raise.error @@ no_shadowing location
        else aux (var :: vars) types mods remaining
      | { wrap_content = D_type t; location } :: remaining ->
        if List.mem ~equal:Type_var.equal types t.type_binder
        then raise.error @@ no_shadowing location
        else aux vars (t.type_binder :: types) mods remaining
      | { wrap_content = D_module t; location } :: remaining ->
        let mod_ = t.module_binder in
        if List.mem ~equal:Module_var.equal mods mod_
        then raise.error @@ no_shadowing location
        else aux vars types (mod_ :: mods) remaining
      | { wrap_content = D_open { module_ = _ }; location = _ } :: remaining
      | { wrap_content = D_include { module_ = _ }; location = _ } :: remaining ->
        (*
        let vars', types', mods' = self module_ in
        let vars, types, mods = vars' @ vars, types' @ types, mods' @ mods in
        (match
           ( List.find_a_dup ~compare:Value_var.compare vars
           , List.find_a_dup ~compare:Type_var.compare types
           , List.find_a_dup ~compare:Module_var.compare mods )
         with
        | None, None, None -> aux vars types mods remaining
        | _, _, _ -> raise.error @@ no_shadowing location)
        *)
        aux vars types mods remaining
      | [] -> vars, types, mods
    in
    let vars, types, mods = aux [] [] [] m in
    vars, types, mods
  in
  let _, _, _ = self m in
  m
