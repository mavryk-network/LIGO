(* This file represente the context which give the association of values to types *)
module Location = Simple_utils.Location
open Ast_typed

module Exists_var = struct
  type t = TypeVar.t [@@deriving compare]

  module Map = Simple_utils.Map.Make (struct
    type nonrec t = t [@@deriving compare]
  end)

  let equal t1 t2 = compare t1 t2 = 0
  let yojson_of_t t = TypeVar.to_yojson t
  let loc = TypeVar.get_location
  let of_type_var tvar = if TypeVar.is_exists tvar then Some tvar else None
  let pp ppf t = Format.fprintf ppf "%a" Ast_typed.PP.type_variable t
  let fresh = TypeVar.fresh_exists
end

type exists_variable = Exists_var.t

type t =
  { items : item list
  ; solved : (kind * type_expression) Exists_var.Map.t
  }

and item =
  | C_value of expression_variable * type_expression
  | C_type of type_variable * type_expression
  | C_type_var of type_variable * kind
  | C_exists_var of exists_variable * kind
  | C_exists_eq of exists_variable * kind * type_expression
  | C_marker of exists_variable
  | C_module of module_variable * t

module PP = struct
  open Ast_typed.PP

  let list ~pp ppf xs =
    let rec loop ppf = function
      | [] -> Format.fprintf ppf ""
      | x :: xs -> Format.fprintf ppf "%a@,%a" loop xs pp x
    in
    Format.fprintf ppf "@[<hv>%a@]" loop xs


  let rec context ppf t =
    list ppf t.items ~pp:(fun ppf item ->
      match item with
      | C_value (evar, type_) ->
        Format.fprintf ppf "%a : %a" expression_variable evar type_expression type_
      | C_type (tvar, type_) ->
        Format.fprintf ppf "type %a = %a" type_variable tvar type_expression type_
      | C_type_var (tvar, kind) ->
        Format.fprintf ppf "%a :: %a" type_variable tvar kind_ kind
      | C_exists_var (evar, kind) ->
        Format.fprintf ppf "%a :: %a" Exists_var.pp evar kind_ kind
      | C_exists_eq (evar, kind, type_) ->
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
let empty = { items = []; solved = Exists_var.Map.empty }
let add t item = { t with items = item :: t.items }

let join t1 t2 =
  { items = t2.items @ t1.items
  ; solved =
      Exists_var.Map.merge
        (fun _ eq1 eq2 ->
          match eq1, eq2 with
          | eq1, None -> eq1
          | _, eq2 -> eq2)
        t1.solved
        t2.solved
  }


let of_list items = { empty with items = List.rev items }

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
  List.find_map t.items ~f:(function
    | C_value (evar', type_) when ValueVar.equal evar evar' -> Some type_
    | _ -> None)


let get_type t tvar =
  List.find_map t.items ~f:(function
    | C_type (tvar', type_) when TypeVar.equal tvar tvar' -> Some type_
    | _ -> None)


let get_module t mvar =
  List.find_map t.items ~f:(function
    | C_module (mvar', mctx) when ModuleVar.equal mvar mvar' -> Some mctx
    | _ -> None)


let get_type_vars t =
  List.filter_map t.items ~f:(function
    | C_type_var (tvar, _) -> Some tvar
    | _ -> None)


let get_exists_vars t =
  List.filter_map t.items ~f:(function
    | C_exists_var (evar, _) -> Some evar
    | _ -> None)


let get_markers t =
  List.filter_map t.items ~f:(function
    | C_marker evar -> Some evar
    | _ -> None)


let get_exists_var t evar =
  List.find_map t.items ~f:(function
    | C_exists_var (evar', kind) when Exists_var.equal evar evar' -> Some kind
    | _ -> None)


let get_type_var t tvar =
  List.find_map t.items ~f:(function
    | C_type_var (tvar', kind) when TypeVar.equal tvar tvar' -> Some kind
    | _ -> None)


let get_exists_eq t evar =
  List.find_map t.items ~f:(function
    | C_exists_eq (evar', _kind, type_) when Exists_var.equal evar evar' -> Some type_
    | _ -> None)


let rec equal_item : item -> item -> bool =
 fun item1 item2 ->
  match item1, item2 with
  | C_value (x1, type1), C_value (x2, type2) ->
    ValueVar.equal x1 x2 && Compare.type_expression type1 type2 = 0
  | C_type (tvar1, type1), C_type (tvar2, type2) ->
    TypeVar.equal tvar1 tvar2 && Compare.type_expression type1 type2 = 0
  | C_type_var (tvar1, kind1), C_type_var (tvar2, kind2) ->
    TypeVar.equal tvar1 tvar2 && compare_kind kind1 kind2 = 0
  | C_exists_var (evar1, kind1), C_exists_var (evar2, kind2) ->
    Exists_var.equal evar1 evar2 && compare_kind kind1 kind2 = 0
  | C_exists_eq (evar1, kind1, type1), C_exists_eq (evar2, kind2, type2) ->
    Exists_var.equal evar1 evar2
    && compare_kind kind1 kind2 = 0
    && Compare.type_expression type1 type2 = 0
  | C_marker evar1, C_marker evar2 -> Exists_var.equal evar1 evar2
  | C_module (mvar1, mctx1), C_module (mvar2, mctx2) ->
    ModuleVar.equal mvar1 mvar2 && List.equal equal_item mctx1.items mctx2.items
  | _, _ -> false


let drop_until t ~at =
  let rec loop t =
    match t.items with
    | [] -> t
    | item :: items when equal_item item at -> { t with items }
    | item :: items ->
      loop
        { items
        ; solved =
            (match item with
             | C_exists_eq (evar, kind, type_) ->
               Exists_var.Map.add evar (kind, type_) t.solved
             | _ -> t.solved)
        }
  in
  loop t


let split_at t ~at =
  let rec loop t =
    match t with
    | [] -> [], []
    | item :: t ->
      if equal_item item at
      then [], t
      else (
        let t1, t2 = loop t in
        item :: t1, t2)
  in
  (* Left context gets solved *)
  let solved = t.solved in
  let l, r = loop t.items in
  { items = l; solved }, { empty with items = r }


let insert_at t ~at ~hole =
  let t1, t2 = split_at t ~at in
  t1 |@ hole |@ t2


let add_exists_eq t evar kind type_ =
  let t1, t2 = split_at t ~at:(C_exists_var (evar, kind)) in
  t1 |@ of_list [ C_exists_eq (evar, kind, type_) ] |@ t2


let rec apply t (type_ : type_expression) : type_expression =
  let self = apply t in
  let return content = { type_ with type_content = content } in
  match type_.type_content with
  | T_variable tvar ->
    (match Exists_var.of_type_var tvar with
     | Some evar ->
       (match get_exists_eq t evar with
        | Some type_' -> self type_'
        | None -> type_)
     | None -> type_)
  | T_constant inj ->
    let parameters = List.map ~f:self inj.parameters in
    return @@ T_constant { inj with parameters }
  | T_sum rows ->
    let content =
      LMap.map
        (fun row_elem ->
          let associated_type = self row_elem.associated_type in
          { row_elem with associated_type })
        rows.content
    in
    return @@ T_sum { rows with content }
  | T_record rows ->
    let content =
      LMap.map
        (fun row_elem ->
          let associated_type = self row_elem.associated_type in
          { row_elem with associated_type })
        rows.content
    in
    return @@ T_record { rows with content }
  | T_arrow { type1; type2 } ->
    let type1 = self type1 in
    let type2 = self type2 in
    return @@ T_arrow { type1; type2 }
  | T_singleton _ -> type_
  | T_abstraction abs ->
    let type_ = self abs.type_ in
    return @@ T_abstraction { abs with type_ }
  | T_for_all for_all ->
    let type_ = self for_all.type_ in
    return @@ T_for_all { for_all with type_ }


module ValueMap = Simple_utils.Map.Make (ValueVar)
module TypeMap = Ast_typed.Helpers.IdMap.Make (TypeVar)
module ModuleMap = Ast_typed.Helpers.IdMap.Make (ModuleVar)

let to_type_map t =
  List.fold_left t.items ~init:TypeMap.empty ~f:(fun map item ->
    match item with
    | C_type (tvar, type_) -> TypeMap.add map tvar type_
    | _ -> map)


let to_module_map t =
  List.fold_left t.items ~init:ModuleMap.empty ~f:(fun map item ->
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
    let rec loop items =
      match items with
      | [] -> true
      | item :: items ->
        loop items
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
         | C_exists_eq (evar, kind, type_) ->
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
    in
    loop ctx.items


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

module Elaboration = struct
  type 'a t = unit -> 'a

  include Monad.Make (struct
    type nonrec 'a t = 'a t

    let return x () = x

    let bind t ~f () =
      let x = t () in
      f x ()


    let map = `Define_using_bind
  end)

  (* "Zonking" is performed by these context application functions *)

  let rec t_apply ctx (type_ : type_expression) : type_expression =
    let self = t_apply ctx in
    let return content = { type_ with type_content = content } in
    match type_.type_content with
    | T_variable tvar ->
      (match Exists_var.of_type_var tvar with
       | Some evar ->
         (match Exists_var.Map.find_opt evar ctx.solved with
          | Some (_, type_') -> self type_'
          | None ->
            (match get_exists_eq ctx evar with
             | Some type_' -> self type_'
             | None -> type_))
       | None -> type_)
    | T_constant inj ->
      let parameters = List.map ~f:self inj.parameters in
      return @@ T_constant { inj with parameters }
    | T_sum rows ->
      let content =
        LMap.map
          (fun row_elem ->
            let associated_type = self row_elem.associated_type in
            { row_elem with associated_type })
          rows.content
      in
      return @@ T_sum { rows with content }
    | T_record rows ->
      let content =
        LMap.map
          (fun row_elem ->
            let associated_type = self row_elem.associated_type in
            { row_elem with associated_type })
          rows.content
      in
      return @@ T_record { rows with content }
    | T_arrow { type1; type2 } ->
      let type1 = self type1 in
      let type2 = self type2 in
      return @@ T_arrow { type1; type2 }
    | T_singleton _ -> type_
    | T_abstraction abs ->
      let type_ = self abs.type_ in
      return @@ T_abstraction { abs with type_ }
    | T_for_all for_all ->
      let type_ = self for_all.type_ in
      return @@ T_for_all { for_all with type_ }


  let rec e_apply ctx expr =
    let self = e_apply ctx in
    let return expression_content =
      let type_expression = t_apply ctx expr.type_expression in
      { expr with expression_content; type_expression }
    in
    return
    @@
    match expr.expression_content with
    | E_literal lit -> E_literal lit
    | E_constant { cons_name; arguments } ->
      E_constant { cons_name; arguments = List.map ~f:self arguments }
    | E_variable var -> E_variable var
    | E_application { lamb; args } -> E_application { lamb = self lamb; args = self args }
    | E_lambda lambda -> E_lambda (lambda_apply ctx lambda)
    | E_recursive { fun_name; fun_type; lambda } ->
      E_recursive
        { fun_name; fun_type = t_apply ctx fun_type; lambda = lambda_apply ctx lambda }
    | E_let_in { let_binder; rhs; let_result; attr } ->
      E_let_in
        { let_binder = binder_apply ctx let_binder
        ; rhs = self rhs
        ; let_result = self let_result
        ; attr
        }
    | E_mod_in mod_in ->
      (* TODO: Modules *)
      E_mod_in { mod_in with let_result = self mod_in.let_result }
    | E_raw_code { language; code } -> E_raw_code { language; code = self code }
    | E_type_inst { forall; type_ } ->
      E_type_inst { forall = self forall; type_ = t_apply ctx type_ }
    | E_type_abstraction type_abs ->
      E_type_abstraction { type_abs with result = self type_abs.result }
    | E_constructor { constructor; element } ->
      E_constructor { constructor; element = self element }
    | E_matching { matchee; cases } ->
      E_matching { matchee = self matchee; cases = matching_expr_apply ctx cases }
    | E_record expr_label_map -> E_record (LMap.map self expr_label_map)
    | E_record_accessor { record; path } ->
      E_record_accessor { record = self record; path }
    | E_record_update { record; path; update } ->
      E_record_update { record = self record; path; update = self update }
    | E_module_accessor mod_access -> E_module_accessor mod_access
    | E_assign { binder; expression } ->
      E_assign { binder = binder_apply ctx binder; expression = self expression }


  and lambda_apply ctx { binder; result } =
    { binder = binder_apply ctx binder; result = e_apply ctx result }


  and binder_apply ctx binder =
    { binder with ascr = Option.map ~f:(t_apply ctx) binder.ascr }


  and matching_expr_apply ctx match_expr =
    match match_expr with
    | Match_variant { cases; tv } ->
      Match_variant
        { cases =
            List.map cases ~f:(fun content ->
              { content with body = e_apply ctx content.body })
        ; tv = t_apply ctx tv
        }
    | Match_record { fields; body; tv } ->
      Match_record
        { fields = LMap.map (fun binder -> binder_apply ctx binder) fields
        ; body = e_apply ctx body
        ; tv = t_apply ctx tv
        }


  let run t ~ctx = e_apply ctx (t ())
end

let unsolved { items; solved } =
  let solved =
    List.fold items ~init:solved ~f:(fun solved item ->
      match item with
      | C_exists_eq (evar, kind, type_) -> Exists_var.Map.add evar (kind, type_) solved
      | _ -> solved)
  in
  { items =
      List.filter items ~f:(function
        | C_exists_var _ -> true
        | _ -> false)
  ; solved
  }


let enter ~ctx ~at ~in_ =
  let ctx, ret_type = in_ ctx in
  let ctxl, ctxr = split_at ctx ~at in
  let ret_type = apply ctxr ret_type in
  let ctxr = unsolved ctxr in
  ctxl |@ ctxr, ret_type


let t_subst t ~tvar ~type_ = Helpers.subst_no_capture_type tvar type_ t

let t_exists (evar : Exists_var.t) =
  t_variable ~loc:(Exists_var.loc evar) (evar :> type_variable) ()


let t_subst_var t ~tvar ~tvar' = t_subst t ~tvar ~type_:(t_variable tvar' ())
let t_subst_evar t ~evar ~type_ = t_subst t ~tvar:evar ~type_

module Generalization = struct
  let generalize_type ~tvars type_ =
    (* Substitute existentials for rigid variables *)
    let ret_type =
      Exists_var.Map.fold
        (fun evar (_kind, tvar) ret_type ->
          t_subst_evar ret_type ~evar ~type_:(t_variable tvar ()))
        tvars
        type_
    in
    (* Quantify rigid variables *)
    Exists_var.Map.fold
      (fun _evar (kind, tvar) ret_type -> t_for_all (tvar : type_variable) kind ret_type)
      tvars
      ret_type


  let unsolved { items; solved } =
    let solved =
      List.fold items ~init:solved ~f:(fun solved item ->
        match item with
        | C_exists_eq (evar, kind, type_) -> Exists_var.Map.add evar (kind, type_) solved
        | _ -> solved)
    in
    let tvars =
      List.fold items ~init:Exists_var.Map.empty ~f:(fun tvars item ->
        match item with
        | C_exists_var (evar, kind) -> Exists_var.Map.add evar kind tvars
        | _ -> tvars)
    in
    { empty with solved }, tvars


  let enter ~ctx ~in_ =
    let marker = C_marker (Exists_var.fresh ()) in
    let ctx, ret_type = in_ (ctx |:: marker) in
    let ctxl, ctxr = split_at ctx ~at:marker in
    let ret_type = apply ctxr ret_type in
    let ctxr, tvars = unsolved ctxr in
    let tvars = Exists_var.Map.map (fun kind -> kind, TypeVar.fresh ()) tvars in
    ctxl |@ ctxr, generalize_type ~tvars ret_type
end
