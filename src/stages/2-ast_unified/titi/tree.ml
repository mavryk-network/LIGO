(* LIBRARY-SIDE *)

(* Tree to be iterated (and AST) *)
[@@@warning "-30"]
[@@@warning "-27"]

module AST = struct
  type ('self, 'expr, 'type_expr) decl_ =
    | Fun of string * 'expr
    | Type of string * 'type_expr
    | Expr of 'expr * 'type_expr option
  [@@deriving map, sexp]

  type ('self, 'declaration) expr_ =
    | Call of string * 'self list
    | Value
    | Block of 'declaration list
  [@@deriving map, sexp]

  type 'self type_expr_ =
    | Int
    | String
    | Arrow of 'self * 'self
  [@@deriving map, sexp]

  type ('self, 'decl, 'expr, 'type_expr) program_ = ('decl, 'expr, 'type_expr) decl_ list [@@deriving map, sexp]

  (* we generate both map and sexp for our different AST sub-lang *)

  (* fix points *)
  type decl = { fp : (decl, expr, type_expr) decl_ }
  and expr = { fp : (expr, decl) expr_ }
  and type_expr = { fp : type_expr type_expr_ }
  and program = { fp : (program, decl, expr, type_expr) program_ }
end

(* building catamorphism/fold on top of generated maps *)
module Fold = struct
  open AST

  (* f algebras *)
  type ('vd, 've, 'vt) cata_decl = ('vd, 've, 'vt) decl_ -> 'vd
  type ('vp, 'vd, 've, 'vt) cata_program = ('vp, 'vd, 've, 'vt) program_ -> 'vp
  type ('ve, 'vd) cata_expr = ('ve, 'vd) expr_ -> 've
  type 'vt cata_type_expr = 'vt type_expr_ -> 'vt

  let rec cata_expr : type vd ve vt. (vd, ve, vt) cata_decl -> (ve, vd) cata_expr -> vt cata_type_expr -> expr -> ve =
   fun fd fe ft x -> map_expr_ (cata_expr fd fe ft) (cata_decl fd fe ft) x.fp |> fe


  and cata_decl : type vd ve vt. (vd, ve, vt) cata_decl -> (ve, vd) cata_expr -> vt cata_type_expr -> decl -> vd =
   fun fd fe ft x -> map_decl_ (cata_decl fd fe ft) (cata_expr fd fe ft) (cata_type_expr ft) x.fp |> fd


  and cata_type_expr : type vt. vt cata_type_expr -> type_expr -> vt = fun ft x -> map_type_expr_ (cata_type_expr ft) x.fp |> ft

  and cata_program
      : type vp vd ve vt.
        (vp, vd, ve, vt) cata_program -> (vd, ve, vt) cata_decl -> (ve, vd) cata_expr -> vt cata_type_expr -> program -> vp
    =
   fun fp fd fe ft x ->
    map_program_ (cata_program fp fd fe ft) (cata_decl fd fe ft) (cata_expr fd fe ft) (cata_type_expr ft) x.fp |> fp
end

(* building tree traversal (post/pre) on top of catamorphism *)
module Order = struct
  open AST
  open Fold

  (* given + , revert application ('left -> right' to 'right -> left') *)
  let rev_plus plus x y = plus y x
  (* sounds very stupid *)
  let pre
      (fp' : ('a, 'a, 'a, 'a) program_ -> 'a)
      (fd' : ('a, 'a, 'a) decl_ -> 'a)
      (fe' : ('a, 'a) expr_ -> 'a)
      (ft' : 'a type_expr_ -> 'a)
      ~(plus : 'a -> 'a -> 'a)
      ~(neutral : 'a)
      tree
      : 'a
    =
    let ( + ) = plus in
    let list_plus = List.fold ~init:neutral ~f:( + ) in
    let rec fp : _ -> 'a =
     fun decl_lst ->
      let lst = List.map ~f:fd decl_lst in
      fp' decl_lst + list_plus lst
    and fd : _ -> 'a =
     fun x ->
      fd' x
      +
      match x with
      | Fun (_, e) | Type (_, e) -> e
      | Expr (e, ty_opt) -> Option.value_map ty_opt ~default:e ~f:(fun ty -> e + ty)
    and fe : _ -> 'a =
     fun x ->
      fe' x
      +
      match x with
      | Call (_, lst) | Block lst -> list_plus lst
      | Value -> neutral
    and ft : 'a type_expr_ -> 'a =
     fun x ->
      ft' x
      +
      match x with
      | Int | String -> neutral
      | Arrow (l, r) -> l + r
    in
    cata_program fp fd fe ft tree


  let post ~plus = pre ~plus:(rev_plus plus)

end

(* CLIENT-SIDE *)
module Example = struct
  (* As an example, we gather the names of the function being
    called, and the number of integer types. *)
  open AST

  type t =
    { fun_names : string list
    ; int_types : int
    }

  let neutral = { fun_names = []; int_types = 0 }
  let ( + ) (x : t) (y : t) = { fun_names = x.fun_names @ y.fun_names; int_types = x.int_types + y.int_types }

  let name_fun : (t, t) expr_ -> t = function
    | Call (name, _) -> { neutral with fun_names = [ name ] }
    | _ -> neutral


  let count_int : t type_expr_ -> t = function
    | Int -> { neutral with int_types = 1 }
    | _ -> neutral


  let nothing_todo _ = neutral
  let count_post (tree : program) : t = Order.post nothing_todo nothing_todo name_fun count_int ~plus:( + ) ~neutral tree
  let count_pre (tree : program) : t = Order.pre nothing_todo nothing_todo name_fun count_int ~plus:( + ) ~neutral tree
end

(* Example *)

open AST

(* some sexp because why not ?*)
let rec expr_of_sexp x : expr = { fp = AST.expr__of_sexp expr_of_sexp decl_of_sexp x }
and decl_of_sexp x : decl = { fp = AST.decl__of_sexp decl_of_sexp expr_of_sexp type_expr_of_sexp x }
and type_expr_of_sexp x : type_expr = { fp = AST.type_expr__of_sexp type_expr_of_sexp x }
and program_of_sexp x : program = { fp = AST.program__of_sexp program_of_sexp decl_of_sexp expr_of_sexp type_expr_of_sexp x }

let sexp x = program_of_sexp (Sexp.of_string x)

let tree =
  sexp
    {| (
    (Fun x (Block ((Type t Int) (Fun y (Call a (Value))))))
    (Type u String)
    (Expr (Call b ()) ((Arrow Int (Arrow String int))))
    (Expr (Call c ( (Call d (Value)) (Call e (Value)))) ())
  )|}


let print_res (x : Example.t) =
  print_endline "int_types";
  print_endline (Format.asprintf "%d" x.int_types);
  print_endline "fun_names";
  List.iter x.fun_names ~f:(fun x -> print_string (Format.asprintf "%s " x));
  print_endline "\n--"


let () =
  print_res (Example.count_pre tree);
  print_res (Example.count_post tree);
  ()
