module AST = Ast_typed
open AST
open Errors
module Append_tree = Tree.Append
open! Mini_c
open Trace
open Function

let annotation_or_label annot label = Option.unopt ~default:label (Helpers.remove_empty_annotation annot)

let t_sum ~layout return compile_type m =
  let open AST.Helpers in
  let lst = kv_list_of_t_sum ~layout m in
  match layout with
  | L_tree -> (
    let node = Append_tree.of_list lst in
    let aux a b : (type_expression annotated, spilling_error) result =
      let%bind t = return @@ T_or (a, b) in
      ok (None, t)
    in
    let%bind m' = Append_tree.bind_fold_ne
      (fun (Label label, ({associated_type;michelson_annotation}: AST.row_element)) ->
          let label = String.uncapitalize_ascii label in
          let%bind a = compile_type associated_type in
          ok (Some (annotation_or_label michelson_annotation label), a)
      )
      aux node in
    ok @@ snd m'
  )
  | L_comb -> (
    (* Right combs *)
    let aux (Label l , (x : _ row_element_mini_c )) =
      let l = String.uncapitalize_ascii l in
      let%bind t = compile_type x.associated_type in
      let annot_opt = Some (annotation_or_label x.michelson_annotation l) in
      ok (annot_opt,t)
    in
    let rec lst_fold_right = function
      | [] -> fail (corner_case ~loc:__LOC__ "t_record empty")
      | [ x ] -> aux x
      | hd :: tl -> (
          let%bind hd = aux hd in
          let%bind tl = lst_fold_right tl in
          let%bind t = return @@ T_or (hd, tl) in
          ok (None,t)
        )
    in
      map snd @@ lst_fold_right lst
  )

let t_record_to_pairs ~layout return compile_type m =
  let open AST.Helpers in
  let is_tuple_lmap = is_tuple_lmap m in
  let lst = kv_list_of_t_record_or_tuple ~layout m in
  match layout with
  | L_tree -> (
      let node = Append_tree.of_list lst in
      let aux a b : (type_expression annotated, spilling_error) result =
        let%bind t = return @@ T_pair (a, b) in
        ok (None, t)
      in
      let%bind m' = Append_tree.bind_fold_ne
          (fun (Label label, ({associated_type;michelson_annotation}: AST.row_element)) ->
             let%bind a = compile_type associated_type in
             ok ((if is_tuple_lmap then
                    None
                  else
                    Some (annotation_or_label michelson_annotation label)),
                 a)
          )
          aux node in
      ok @@ snd m'
    )
  | L_comb -> (
      (* Right combs *)
      let aux (Label l , (x : _ row_element_mini_c)) =
        let%bind t = compile_type x.associated_type in
        let annot_opt = Some (annotation_or_label x.michelson_annotation l) in
        ok (annot_opt,t)
      in
      let rec lst_fold_right = function
        | [] -> fail (corner_case ~loc:__LOC__ "t_record empty")
        | [ x ] -> aux x
        | hd :: tl -> (
          let%bind hd = aux hd in
          let%bind tl = lst_fold_right tl in
          let%bind t = return @@ T_pair (hd, tl) in
            ok (None, t)
          )
      in
      map snd @@ lst_fold_right lst
    )

let record_access_to_lr ~layout ty m_ty index =
  let open AST.Helpers in
  let lst = kv_list_of_t_record_or_tuple ~layout m_ty in
  match layout with
  | L_tree -> (
      let node_tv = Append_tree.of_list lst in
      let%bind path =
        trace_option (corner_case ~loc:__LOC__ "record access leaf") @@
        Append_tree.exists_path_to index node_tv
      in
      let lr_path = List.map (fun b -> if b then `Right else `Left) path in
        let aux = fun (ty , acc) cur ->
          let%bind (a , b) =
            trace_option (corner_case ~loc:__LOC__ "record access pair") @@
            Mini_c.get_t_pair ty
          in
          match cur with
          | `Left  -> ok (a , (a , `Left)  :: acc)
          | `Right -> ok (b , (b , `Right) :: acc )
        in
        map (List.rev <@ snd) @@ bind_fold_list aux (ty , []) lr_path
    )
  | L_comb -> (
      let rec aux n ty last =
        match n , last with
        | 0 , true -> ok []
        | 0 , false -> (
            let%bind (a , _) =
              trace_option (corner_case ~loc:__LOC__ "record access pair") @@
              Mini_c.get_t_pair ty
            in
            ok [(a , `Left)]
          )
        | n , last -> (
            let%bind (_ , b) =
              trace_option (corner_case ~loc:__LOC__ "record access pair") @@
              Mini_c.get_t_pair ty
            in
            let%bind prec = aux (n - 1) b last in
            ok ((b , `Right)::prec)
          )
      in
      let%bind index =
        Trace.generic_try (corner_case ~loc:__LOC__ "record access index") @@
        fun () -> List.find_index (fun (label , _) -> label = index) lst
      in
      let last = (index + 1 = List.length lst) in
      aux index ty last
    )

let record_to_pairs compile_expression (return:?tv:_ -> _) record_t record : Mini_c.expression spilling_result =
  let open AST.Helpers in
  let lst = kv_list_of_record_or_tuple ~layout:record_t.layout record_t.content record in
  match record_t.layout with
  | L_tree -> (
    let node = Append_tree.of_list lst in
    let aux a b : (expression , spilling_error) result =
      let a_ty = Combinators.Expression.get_type a in
      let b_ty = Combinators.Expression.get_type b in
      let tv   = Combinators.Expression.make_t @@ T_pair ((None, a_ty) , (None, b_ty)) in
      return ~tv @@ ec_pair a b
    in
    trace_strong (corner_case ~loc:__LOC__ "record build") @@
    Append_tree.bind_fold_ne (compile_expression) aux node
  )
  | L_comb -> (
      let rec aux = function
        | [] -> fail (corner_case ~loc:__LOC__ "record build")
        | [x] -> compile_expression x
        | hd::tl -> (
            let%bind hd' = compile_expression hd in
            let%bind tl' = aux tl in
            let tv = t_pair (None,hd'.type_expression) (None,tl'.type_expression) in
            return ~tv (ec_pair hd' tl')
          ) in
      aux lst
    )

let constructor_to_lr ~(layout) ty m_ty index =
  let open AST.Helpers in
  let lst = kv_list_of_t_sum ~layout m_ty in
  match layout with
  | L_tree -> (
      let node_tv = Append_tree.of_list lst in
      let%bind path =
        trace_option (corner_case ~loc:__LOC__ "constructor leaf") @@
        Append_tree.exists_path_to index node_tv
      in
      let lr_path = List.map (fun b -> if b then `Right else `Left) path in
      let%bind (_ , lst) =
        let aux = fun (ty , acc) cur ->
          let%bind (a , b) =
            trace_option (corner_case ~loc:__LOC__ "constructor union") @@
            Mini_c.get_t_or ty
          in
          match cur with
          | `Left  -> ok (a , (ty , `Left)  :: acc)
          | `Right -> ok (b , (ty , `Right) :: acc )
        in
        bind_fold_list aux (ty , []) lr_path
      in
      ok @@ lst
    )
  | L_comb -> (
    let%bind index =
      Trace.generic_try (corner_case ~loc:__LOC__ "constructor access") @@
      fun () -> List.find_index (fun (label , _) -> label = index) lst
    in
    let last = (index + 1 = List.length lst) in
    let rec aux n ty =
      match n , last with
      | 0 , true -> ok []
      | 0 , false -> ok [(ty , `Left)]
      | n , _ -> (
          let%bind (_ , b) =
            trace_option (corner_case ~loc:__LOC__ "constructor union") @@
            Mini_c.get_t_or ty
          in
          let%bind prec = aux (n - 1) b in
          ok ((ty , `Right)::prec)
        )
    in
    map List.rev @@ aux index ty
    )

type variant_tree = [
  | `Leaf of label
  | `Node of variant_pair * variant_pair
]

and variant_pair = variant_tree * Mini_c.type_expression

let match_variant_to_tree ~layout ~compile_type content : variant_pair spilling_result =
  match layout with
  | L_tree -> (
      let kt_tree =
        let kt_list = List.map (fun (k,({associated_type;_}:AST.row_element)) -> (k,associated_type)) (LMap.to_kv_list content) in
        Append_tree.of_list kt_list
      in
      let%bind ne_tree = match kt_tree with
        | Empty -> fail (corner_case ~loc:__LOC__ "match empty variant")
        | Full x -> ok x in
      let%bind vp =
        let rec aux t : variant_pair spilling_result =
          match (t : _ Append_tree.t') with
          | Leaf (name , tv) ->
            let%bind tv' = compile_type tv in
            ok (`Leaf name , tv')
          | Node {a ; b} ->
            let%bind a' = aux a in
            let%bind b' = aux b in
            let tv' = t_union (None, snd a') (None, snd b') in
            ok (`Node (a' , b') , tv')
        in
        aux ne_tree
      in
      ok vp
    )
  | L_comb -> (
      let rec aux : _ -> variant_pair spilling_result = function
        | [] -> fail (corner_case ~loc:__LOC__ "variant build")
        | [(k , ty)] -> (
            let%bind t = compile_type ty in
            ok (`Leaf k , t)
          )
        | (khd , thd)::tl -> (
            let%bind thd' = compile_type thd in
            let%bind (tl',ttl) = aux tl in
            let tv' = t_union (None, thd') (None, ttl) in
            let left = `Leaf khd , thd' in
            ok (`Node (left , (tl' , ttl)) , tv')
          ) in
      let lst = List.map (fun (k,({associated_type;_} : _ row_element_mini_c)) -> (k,associated_type)) @@ Ast_typed.Helpers.kv_list_of_t_sum ~layout content in
      let%bind vp = aux lst in
      ok vp
    )

let extract_record ~(layout:layout) (v : value) (lst : (AST.label * AST.type_expression) list) : _ list spilling_result =
  match layout with
  | L_tree -> (
    let open Append_tree in
    let%bind tree = match Append_tree.of_list lst with
      | Empty -> fail @@ corner_case ~loc:__LOC__ "empty record"
      | Full t -> ok t in
    let rec aux tv : (AST.label * (value * AST.type_expression)) list spilling_result =
      match tv with
      | Leaf (s, t), v -> ok @@ [s, (v, t)]
      | Node {a;b}, D_pair (va, vb) ->
          let%bind a' = aux (a, va) in
          let%bind b' = aux (b, vb) in
          ok (a' @ b')
      | _ -> fail @@ corner_case ~loc:__LOC__ "bad record path"
    in
    aux (tree, v)
  )
  | L_comb -> (
    let rec aux lst_record v : (AST.label * (value * AST.type_expression)) list spilling_result =
      match lst_record,v with
      | [], _ -> fail @@ corner_case ~loc:__LOC__ "empty record"
      | [(s,t)], v -> ok [s,(v,t)]
      | [(sa,ta);(sb,tb)], D_pair (va,vb) ->
          let%bind a' = aux [sa, ta] va in
          let%bind b' = aux [sb, tb] vb in
          ok (a' @ b')
      | (shd,thd)::tl, D_pair (va,vb) ->
        let%bind tl' = aux tl vb in
        ok ((shd,(va,thd))::tl')
      | _ -> fail @@ corner_case ~loc:__LOC__ "bad record path"
    in
    aux lst v
  )

let extract_constructor ~(layout:layout) (v : value) (lst : (AST.label * AST.type_expression) list) : (label * value * AST.type_expression) spilling_result =
  match layout with
  | L_tree ->
    let open Append_tree in
    let%bind tree = match Append_tree.of_list lst with
      | Empty -> fail @@ corner_case ~loc:__LOC__ "empty variant"
      | Full t -> ok t in
    let rec aux tv : (label * value * AST.type_expression) spilling_result=
      match tv with
      | Leaf (k, t), v -> ok (k, v, t)
      | Node {a}, D_left v -> aux (a, v)
      | Node {b}, D_right v -> aux (b, v)
      | _ -> fail @@ corner_case ~loc:__LOC__ "bad constructor path"
    in
    let%bind (s, v, t) = aux (tree, v) in
    ok (s, v, t)
  | L_comb -> (
    let rec aux tv : (label * value * AST.type_expression) spilling_result =
      match tv with
      | [], _ -> failwith "lal"
      | ((l,t)::tl), v-> ( match v with
        | D_left v -> ok (l,v,t)
        | D_right v -> aux (tl,v)
        | v -> ok (l,v,t)
      )
    in
    aux (lst,v)
  )
