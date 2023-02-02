open Ligo_prim.Constant
open Ast_unified
include Fuzz_shared.Monad

(* Helpers for swapping operators *)

let binary_num_constants = [ C_MUL; C_DIV; C_MOD; C_SUB; C_ADD ]
let binary_bool_constants = [ C_AND; C_OR; C_XOR ]
let cmp_constants = [ C_EQ; C_NEQ; C_LT; C_GT; C_LE; C_GE ]
let op_class = [ binary_num_constants; binary_bool_constants; cmp_constants ]

let rec find_class op = function
  | [] -> raise Caml.Not_found
  | x :: _ when List.mem x op ~equal:Caml.( = ) -> x
  | _ :: xs -> find_class op xs


(* Helpers for transforming literals *)

let transform_int =
  let const0 _ = 0 in
  let id n = n in
  let negative n = -n in
  let incr n = n + 1 in
  let pred n = n - 1 in
  let prod n = 2 * n in
  [ id; const0; negative; incr; pred; prod ]


let transform_nat =
  let const0 _ = 0 in
  let id n = n in
  let incr n = n + 1 in
  let prod n = 2 * n in
  [ id; const0; incr; prod ]


let transform_string =
  let constn _ = "" in
  let double s = s ^ s in
  let id s = s in
  [ id
  ; String.capitalize
  ; String.uncapitalize
  ; String.lowercase
  ; String.uppercase
  ; constn
  ; double
  ]


module Mutator (M : Monad) = struct
  open Monad_context (M)
  open Ligo_prim.Literal_value

  let mutate_literal = function
    | Literal_int z ->
      let* z = mutate_int (Z.to_int z) in
      let* t = oneof (List.map ~f:return transform_int) in
      return (Literal_int (Z.of_int (t z)))
    | Literal_nat z ->
      let* n = mutate_nat (Z.to_int z) in
      let* t = oneof (List.map ~f:return transform_nat) in
      return (Literal_nat (Z.of_int (t n)))
    | Literal_mutez z ->
      let* n = mutate_nat (Z.to_int z) in
      let* t = oneof (List.map ~f:return transform_nat) in
      return (Literal_mutez (Z.of_int (t n)))
    | Literal_string (Standard s) ->
      let* s = mutate_string s in
      let* t = oneof (List.map ~f:return transform_string) in
      return (Literal_string (Standard (t s)))
    | Literal_string (Verbatim s) ->
      let* s = mutate_string s in
      let* t = oneof (List.map ~f:return transform_string) in
      return (Literal_string (Verbatim (t s)))
    | l -> return l


  let mutate_constant cons_name =
    match cons_name with
    | c when List.exists ~f:(fun l -> List.mem l c ~equal:Caml.( = )) op_class ->
      let ops = find_class c op_class in
      let mapper x = return x in
      oneof @@ List.map ~f:mapper ops
    | _ -> return cons_name

  let unwrap : 'a M.t -> 'a =
    fun x -> let (_,x) = get_one x in x 

  let mutate_expression (expr : (expr M.t, ty_expr M.t, pattern M.t,block M.t, mod_expr M.t) expr_) : expr M.t =
    let loc = Location.get_location expr in
    match Location.unwrap expr with
    | E_Literal l ->
      let* l = mutate_literal l in
      return (e_literal ~loc l)
    | E_constant { cons_name; arguments } ->
      let* c = mutate_constant cons_name in
      let* arguments = bind_list arguments in
      return (e_constant ~loc { cons_name = c; arguments })
    | _ ->
      let fp = map_expression_ unwrap unwrap unwrap unwrap unwrap expr in
      return ({fp } : expr)

  let mutate_program ?n (p : program) =
    ignore (n,p) ; failwith "wait a bit"
    (* let open Catamorphism in
    let rndmod_ =
      cata_program
        ~f:(mutpass mutate_expression)
        p
    in
    get_one ?n (bind_list rndmod_) *)
end
(* 

Error: This expression has type
         
(instruction t, expr t, pattern t, statement t, block t) Ast_unified.Types.instruction_ =
(instruction t, expr t, pattern t, statement t, block t) instruction_content_ Location.wrap
       

but an expression was expected of type

(instruction t, (expr t, ty_expr t, pattern t, block t, mod_expr t) Ast_unified.Types.expression_, 'a t, 'b t, 'c t) instruction_content_ Location.wrap
       
       
         Type expr t is not compatible with type
         (expr t, ty_expr t, pattern t, block t, mod_expr t)
         Ast_unified.Types.expression_ =
           (expr t, ty_expr t, pattern t, block t, mod_expr t)
           expression_content_ Location.wrap *)