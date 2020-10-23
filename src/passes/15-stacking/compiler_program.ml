open Stage_common.Types

open Tezos_micheline.Micheline

type compiled_expression = {
  expr_ty : (Location.t, string) node ;
  expr : (Location.t, string) node ;
}
include Ligo_coq_ocaml.Compiler
open Ligo_coq_ocaml.Ligo

let wipe_locations l e =
  inject_locations (fun _ -> l) (strip_locations e)

let generated = Location.generated

let literal_type_prim (l : literal) : string =
  match l with
  | Literal_unit -> "unit"
  | Literal_int _ -> "int"
  | Literal_nat _ -> "nat"
  | Literal_timestamp _ -> "timestamp"
  | Literal_mutez _ -> "mutez"
  | Literal_string _ -> "string"
  | Literal_bytes _ -> "bytes"
  | Literal_address _ -> "address"
  | Literal_signature _ -> "signature"
  | Literal_key _ -> "key"
  | Literal_key_hash _ -> "key_hash"
  | Literal_chain_id _ -> "chain_id"
  | Literal_operation _ -> "operation"

let literal_type (l : literal) : (Location.t, string) node =
  Prim (generated, literal_type_prim l, [], [])

let literal_value (l : literal) : (Location.t, string) node =
  match l with
  | Literal_unit -> Prim (generated, "Unit", [], [])
  | Literal_int x -> Int (generated, x)
  | Literal_nat x -> Int (generated, x)
  | Literal_timestamp x -> Int (generated, x)
  | Literal_mutez x -> Int (generated, x)
  | Literal_string x -> String (generated, Ligo_string.extract x)
  | Literal_bytes x -> Bytes (generated, x)
  | Literal_address x -> String (generated, x)
  | Literal_signature x -> String (generated, x)
  | Literal_key x -> String (generated, x)
  | Literal_key_hash x -> String (generated, x)
  | Literal_chain_id x -> String (generated, x)
  | Literal_operation x -> Bytes (generated, x)

let compile_binds' = compile_binds
let compile_expr' = compile_expr

let rec compile_binds env outer proj binds =
  compile_binds' generated compile_operator literal_type literal_value env outer proj binds

and compile_expr env outer expr =
  compile_expr' generated compile_operator literal_type literal_value env outer expr

and apply_static_args : string -> (_, constant', literal) static_args -> _ node =
  fun prim args ->
  match args with
  | Type_args (annot, types) ->
    Prim (generated, prim, types, Option.to_list annot)
  | Script_arg (Script (p, s, e)) ->
    (* prim will always be CREATE_CONTRACT, recursively compile the
       contract here *)
    let e = compile_binds [] [] [] e in
    let parameter = Prim (generated, "parameter", [p], []) in
    let storage = Prim (generated, "storage", [s], []) in
    let code = Prim (generated, "code", [Seq (generated, e)], []) in
    Prim (generated, prim, [Seq (generated, [parameter; storage; code])], [])

and compile_operator : location -> constant' -> (_, constant', literal) static_args -> (location, string) node list =
  fun loc c args ->
  match Predefined.Stacking.get_operators c with
  | Some x -> [Predefined.Stacking.unpredicate
                 loc
                 (* hmm *)
                 (fun prim -> wipe_locations () (apply_static_args prim args))
                 x]
  | None ->
    failwith (Format.asprintf "no operator %a %s"
                Stage_common.PP.constant' c
                __LOC__)

let compile_expr env outer e =
  Seq (generated, compile_expr env outer e)

let compile_function_body e =
  Seq (generated, compile_binds [] [] [] e)
