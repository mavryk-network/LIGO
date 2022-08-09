(* This file represente the context which give the association of values to types *)
module Location = Simple_utils.Location
open Ast_typed

module Exists_var = struct
  type t = TypeVar.t [@@deriving compare]

  let equal t1 t2 = compare t1 t2 = 0
  let of_type_var tvar = if TypeVar.is_exists tvar then Some tvar else None
  let pp ppf t = Format.fprintf ppf "%a" Ast_typed.PP.type_variable t
end

type exists_variable = Exists_var.t

type t = item list

and item =
  | C_value of expression_variable * type_expression
  | C_type of type_variable * type_expression
  | C_type_var of type_variable * kind
  | C_exists_var of exists_variable * kind
  | C_solved of exists_variable * kind * type_expression
  | C_marker of exists_variable
  | C_module of module_variable * t

module PP = struct
  open Ast_typed.PP
  open Types

  let list ~pp ppf xs =
    let rec loop ppf = function
      | [] -> Format.fprintf ppf ""
      | x :: xs -> Format.fprintf ppf "%a@,%a" loop xs pp x
    in
    Format.fprintf ppf "@[<hv>%a@]" loop xs


  let rec context ppf t =
    list ppf t ~pp:(fun ppf item ->
      match item with
      | C_value (evar, type_) ->
        Format.fprintf ppf "%a : %a" expression_variable evar type_expression type_
      | C_type (tvar, type_) ->
        Format.fprintf ppf "type %a = %a" type_variable tvar type_expression type_
      | C_type_var (tvar, kind) ->
        Format.fprintf ppf "%a :: %a" type_variable tvar kind_ kind
      | C_exists_var (evar, kind) ->
        Format.fprintf ppf "%a :: %a" Exists_var.pp evar kind_ kind
      | C_solved (evar, kind, type_) ->
        Format.fprintf
          ppf
          "%a :: %a = %a"
          Exists_var.pp
          evar
          kind_
          kind
          type_expression
          type_
      | C_marker evar -> Format.fprintf ppf "|>%a" Exists_var.pp evar
      | C_module (mvar, ctx) ->
        Format.fprintf ppf "module %a = %a" module_variable mvar context ctx)
end

let pp = PP.context
let empty = []
let add t item = item :: t
let join t1 t2 = t2 @ t1
let of_list t = List.rev t

(* Inifix notations for [add] and [join] *)
let ( |:: ) = add
let ( |@ ) = join
let add_value t evar type_ = t |:: C_value (evar, type_)
let add_type t tvar type_ = t |:: C_type (tvar, type_)
let add_type_var t tvar kind = t |:: C_type_var (tvar, kind)
let add_exists_var t evar kind = t |:: C_exists_var (evar, kind)
let add_marker t evar = t |:: C_marker evar
let add_module t mvar mctx = t |:: C_module (mvar, mctx)

let get_value t evar =
  List.find_map t ~f:(function
    | C_value (evar', type_) when ValueVar.equal evar evar' -> Some type_
    | _ -> None)


let get_value t evar =
  List.find_map t ~f:(function
    | C_value (evar', type_) when ValueVar.equal evar evar' -> Some type_
    | _ -> None)


let get_type t tvar =
  List.find_map t ~f:(function
    | C_type (tvar', type_) when TypeVar.equal tvar tvar' -> Some type_
    | _ -> None)


let get_module t mvar =
  List.find_map t ~f:(function
    | C_module (mvar', mctx) when ModuleVar.equal mvar mvar' -> Some mctx
    | _ -> None)


let get_type_vars t =
  List.filter_map t ~f:(function
    | C_type_var (tvar, _) -> Some tvar
    | _ -> None)


let get_exists_vars t =
  List.filter_map t ~f:(function
    | C_exists_var (evar, _) -> Some evar
    | _ -> None)


let get_markers t =
  List.filter_map t ~f:(function
    | C_marker evar -> Some evar
    | _ -> None)


let get_exists_var t evar =
  List.find_map t ~f:(function
    | (C_exists_var (evar', kind) | C_solved (evar', kind, _))
      when Exists_var.equal evar evar' -> Some kind
    | _ -> None)


let get_type_var t tvar =
  List.find_map t ~f:(function
    | C_type_var (tvar', kind) when TypeVar.equal tvar tvar' -> Some kind
    | _ -> None)


module ValueMap = Simple_utils.Map.Make (ValueVar)
module TypeMap = Ast_typed.Helpers.IdMap.Make (TypeVar)
module ModuleMap = Ast_typed.Helpers.IdMap.Make (ModuleVar)

let to_type_map t =
  List.fold_left t ~init:TypeMap.empty ~f:(fun map item ->
    match item with
    | C_type (tvar, type_) -> TypeMap.add map tvar type_
    | _ -> map)


let to_module_map t =
  List.fold_left t ~init:ModuleMap.empty ~f:(fun map item ->
    match item with
    | C_module (mvar, mctx) -> ModuleMap.add map mvar mctx
    | _ -> map)


(* Recursively fetches all types from the given module and its submodules

    For example, to get the list of all types declared in a module and its submodules,
    we perform a recusive search in the context maps and accumulate the types found.
    Then, in order to convert those maps into a id-sorted list, we can :
    1. Use [merge], and convert the merged map into a (sorted) kv_list. This will remove duplicate eponym types
    2. Use [to_kvi_list], append all the kvi_lists, and sort the resulting kvi_list by id, into a kv_list, this keeps duplicates     
*)
let get_module_types : t -> (type_variable * type_expression) list =
 fun ctxt ->
  let rec aux : t -> type_expression TypeMap.kvi_list =
   fun ctxt ->
    (* First, get types in the current scope *)
    let accu_types = TypeMap.to_kvi_list @@ to_type_map ctxt in
    (* Then recursively fetch those in the submodules*)
    let module_list = ModuleMap.to_kv_list @@ to_module_map ctxt in
    List.fold module_list ~init:accu_types ~f:(fun accu_types (_, ctxt) ->
      List.rev_append accu_types @@ aux ctxt)
  in
  TypeMap.sort_to_kv_list @@ aux ctxt


(*
  for any constructor [ctor] that belong to a sum-type `t` in the context [ctxt] return a 4-uple list:
  1. the declaration name for type `t`
  2. list of abstracted type variables in the constructor parameter (e.g. ['a ; 'b] for `Foo of ('a * int * 'b)`)
  3. type of the constructor parameter (e.g. `'a * int * 'b` for `Foo of ('a * int * 'b)`)
  4. type of the sum-type found in the context

  NOTE : Here, we return all the matching types found in the module and its submodules, even if we found matching types in current scope.
  Indeed, we want to check for other matching types in submodules anyway, to warn the user in case of conflict.
  For example :
    module Mod_a = struct
      type tx = A of int
    end
    type ty = A of int
    let a = A 42
  Here, for [a], we find a matching type [ty] in the current scope, but we still want to warn the user that type [Mod_a.tx] matches too.
*)
let get_sum
  :  label -> t
  -> (type_variable * type_variable list * type_expression * type_expression) list
  =
 fun ctor ctxt ->
  let filter_tsum (var, type_) =
    let t_params, type_ = Ast_typed.Helpers.destruct_type_abstraction type_ in
    match type_.type_content with
    | T_sum m ->
      (match LMap.find_opt ctor m.content with
       | Some { associated_type; _ } -> Some (var, t_params, associated_type, type_)
       | None -> None)
    | _ -> None
  in
  (* Fetch all types declared in current module and its submodules *)
  let module_types = get_module_types ctxt in
  (*  Also add the shadowed t_sum types nested in the fetched types.
        Since context is made of maps, all shadowed types are absent from the context.
        However we still want the shadowed nested t_sum, see [add_shadowed_nested_t_sum] *)
  let module_types =
    List.fold
      (List.rev module_types)
      ~init:[]
      ~f:Ast_typed.Helpers.add_shadowed_nested_t_sum
  in
  (* For all types found, pick only the T_sum, and make 4-uple out of them  *)
  let matching_t_sum = List.filter_map ~f:filter_tsum @@ module_types in
  (* Filter out duplicates (this prevents false warnings of "infered type is X but could also be X"
       when a same type is present several times in the context) *)
  let remove_doubles l
    : (type_variable * type_variable list * type_expression * type_expression) list
    =
    let add_no_dup l elt
      : (type_variable * type_variable list * type_expression * type_expression) list
      =
      let (_tv, _tvs, _te, te)
            : type_variable * type_variable list * type_expression * type_expression
        =
        elt
      in
      match
        List.find l ~f:(fun (_tv, _tvs, _te, te') ->
          Hash.hash_type_expression te = Hash.hash_type_expression te')
      with
      | Some _ -> l
      | None -> elt :: l
    in
    List.rev @@ List.fold l ~f:add_no_dup ~init:[]
  in
  let matching_t_sum = remove_doubles matching_t_sum in
  let general_type_opt =
    List.find ~f:(fun (_, tvs, _, _) -> not @@ List.is_empty tvs) matching_t_sum
  in
  match general_type_opt with
  | Some general_type -> [ general_type ]
  | None -> matching_t_sum


let get_record : _ label_map -> t -> (type_variable option * rows) option =
 fun lmap e ->
  let lst_kv = LMap.to_kv_list_rev lmap in
  let rec rec_aux e =
    let aux (_, type_) =
      match type_.type_content with
      | T_record m ->
        Simple_utils.Option.(
          let lst_kv' = LMap.to_kv_list_rev m.content in
          let m =
            map ~f:(fun () -> m)
            @@ Ast_typed.Misc.assert_list_eq
                 (fun (ka, va) (kb, vb) ->
                   let (Label ka) = ka in
                   let (Label kb) = kb in
                   let* () = Ast_typed.Misc.assert_eq ka kb in
                   Ast_typed.Misc.assert_type_expression_eq
                     (va.associated_type, vb.associated_type))
                 lst_kv
                 lst_kv'
          in
          map ~f:(fun m -> type_.orig_var, m) @@ m)
      | _ -> None
    in
    match List.find_map ~f:aux @@ TypeMap.to_kv_list @@ to_type_map e with
    | Some _ as s -> s
    | None ->
      let modules = to_module_map e in
      List.fold_left
        ~f:(fun res (__, module_) ->
          match res with
          | Some _ as s -> s
          | None -> rec_aux module_)
        ~init:None
        (ModuleMap.to_kv_list modules)
  in
  rec_aux e


let rec context_of_module_expr : outer_context:t -> Ast_typed.module_expr -> t =
 fun ~outer_context me ->
  match me.wrap_content with
  | M_struct declarations ->
    let f : t -> Ast_typed.declaration -> t =
     fun acc d ->
      match Location.unwrap d with
      | Declaration_constant { binder; expr; attr = { public; _ } } ->
        if public then add_value acc binder.var expr.type_expression else acc
      | Declaration_type { type_binder; type_expr; type_attr = { public; _ } } ->
        if public then add_type acc type_binder type_expr else acc
      | Declaration_module { module_binder; module_; module_attr = { public; _ } } ->
        if public
        then (
          let context =
            context_of_module_expr ~outer_context:(join outer_context acc) module_
          in
          add_module acc module_binder context)
        else acc
    in
    List.fold ~f ~init:empty declarations
  | M_variable module_binder ->
    let ctxt_opt = get_module outer_context module_binder in
    (match ctxt_opt with
     | Some x -> x
     | None -> empty)
  | M_module_path path ->
    Simple_utils.List.Ne.fold_left
      path
      ~f:(fun ctxt name ->
        match get_module ctxt name with
        | Some x -> x
        | None -> empty)
      ~init:outer_context


(* Load context from the outside declarations *)
let init ?env () =
  match env with
  | None -> empty
  | Some env ->
    let f : t -> Ast_typed.declaration -> t =
     fun c d ->
      match Location.unwrap d with
      | Declaration_constant { binder; expr; attr = _ } ->
        add_value c binder.var expr.type_expression
      | Declaration_type { type_binder; type_expr; type_attr = _ } ->
        add_type c type_binder type_expr
      | Declaration_module { module_binder; module_; module_attr = _ } ->
        let mod_context = context_of_module_expr ~outer_context:c module_ in
        add_module c module_binder mod_context
    in
    Environment.fold ~f ~init:empty env


module Well_formed : sig
  val context : t -> bool
  val type_expr : ctx:t -> type_expression -> kind option
end = struct
  let rec context ctx =
    match ctx with
    | [] -> true
    | item :: ctx ->
      context ctx
      &&
      (match item with
       | C_value (_evar, type_) ->
         (match type_expr type_ ~ctx with
          | Some Type -> true
          | _ -> false)
       | C_type (_tvar, type_) -> type_expr type_ ~ctx |> Option.is_some
       | C_type_var _ ->
         (* Shadowing permitted *)
         true
       | C_exists_var (evar, _) ->
         not (List.mem ~equal:Exists_var.equal (get_exists_vars ctx) evar)
       | C_solved (evar, kind, type_) ->
         (not (List.mem ~equal:Exists_var.equal (get_exists_vars ctx) evar))
         &&
         (match type_expr type_ ~ctx with
          | Some kind' -> compare_kind kind kind' = 0
          | _ -> false)
       | C_marker evar ->
         (not (List.mem ~equal:Exists_var.equal (get_markers ctx) evar))
         && not (List.mem ~equal:Exists_var.equal (get_exists_vars ctx) evar)
       | C_module (_mvar, mctx) ->
         (* Shadowing permitted *)
         context mctx)


  and type_expr ~ctx t : kind option =
    let open Option.Let_syntax in
    let rec loop (t : type_expression) ~ctx =
      let self ?(ctx = ctx) t = loop t ~ctx in
      match t.type_content with
      | T_variable tvar ->
        (match Exists_var.of_type_var tvar with
         | Some evar -> get_exists_var ctx evar
         | None -> get_type_var ctx tvar)
      | T_constant { parameters; _ } ->
        (* Hack. No HKT parameters, so simply check if all params are
           of kind: *. *)
        if List.for_all parameters ~f:(fun param ->
             match self param with
             | Some Type -> true
             | _ -> false)
        then return Type
        else None
      | T_singleton _ -> return Type
      | T_arrow { type1 = arg_type; type2 = ret_type } ->
        let%bind arg_kind = self arg_type in
        let%bind ret_kind = self ret_type in
        (match arg_kind, ret_kind with
         | Type, Type -> Some Type
         | _ -> None)
      | T_abstraction { ty_binder = tvar; kind; type_ } ->
        let%bind kind' = self ~ctx:(ctx |:: C_type_var (tvar, kind)) type_ in
        return @@ Arrow (kind, kind')
      | T_for_all { ty_binder = tvar; kind; type_ } ->
        (match%bind self ~ctx:(ctx |:: C_type_var (tvar, kind)) type_ with
         | Type -> return Type
         | _ -> None)
      | T_sum rows | T_record rows ->
        if LMap.for_all
             (fun _label { associated_type; _ } ->
               match self associated_type with
               | Some Type -> true
               | _ -> false)
             rows.content
        then return Type
        else None
    in
    loop t ~ctx
end

module Hashes = struct
  module HTBL = Caml.Hashtbl.Make (struct
    type t = type_expression

    let hash = Hash.hash_type_expression

    let equal t1 t2 =
      match assert_type_expression_eq (t1, t2) with
      | Some _ -> true
      | None -> false
  end)

  let hashtbl : (module_variable list * type_variable) HTBL.t = HTBL.create 256
  let context = ref (false, empty)
  let set_context (t : t) : unit = context := false, t

  let hash_types () : unit =
    let hashed, t = !context in
    if hashed
    then ()
    else (
      let rec aux path (t : t) =
        let types = TypeMap.to_kv_list @@ to_type_map t in
        let modules = ModuleMap.to_kv_list @@ to_module_map t in
        List.iter (List.rev types) ~f:(fun (v, t) -> HTBL.add hashtbl t (path, v));
        List.iter (List.rev modules) ~f:(fun (v, t) -> aux (path @ [ v ]) t)
      in
      HTBL.clear hashtbl;
      aux [] t;
      context := true, t)


  let find_type (t : type_expression) : (module_variable list * type_variable) option =
    HTBL.find_opt hashtbl t
end
