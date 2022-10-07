module Location = Simple_utils.Location
module List = Simple_utils.List
module Ligo_string = Simple_utils.Ligo_string
open Ligo_prim
open Types

let remove_empty_annotation (ann : string option) : string option =
  match ann with
  | Some "" -> None
  | Some ann -> Some ann
  | None -> None


(* This function transforms a type `fun v1 ... vn . t` into the pair `([ v1 ; .. ; vn ] , t)` *)
let destruct_type_abstraction (t : type_expression) =
  let rec destruct_type_abstraction type_vars (t : type_expression) =
    match t.type_content with
    | T_abstraction { ty_binder; type_; _ } ->
      destruct_type_abstraction (ty_binder :: type_vars) type_
    | _ -> List.rev type_vars, t
  in
  destruct_type_abstraction [] t


(* This function transforms a type `∀ v1 ... vn . t` into the pair `([ v1 ; .. ; vn ] , t)` *)
let destruct_for_alls (t : type_expression) =
  let rec destruct_for_alls type_vars (t : type_expression) =
    match t.type_content with
    | T_for_all { ty_binder; type_; _ } ->
      destruct_for_alls (ty_binder :: type_vars) type_
    | _ -> List.rev type_vars, t
  in
  destruct_for_alls [] t


(* This function transforms a type `t1 -> ... -> tn -> t` into the pair `([ t1 ; .. ; tn ] , t)` *)
let destruct_arrows_n (t : type_expression) (n : int) =
  let rec destruct_arrows type_vars (t : type_expression) =
    match t.type_content with
    | T_arrow { type1; type2 } when List.length type_vars < n ->
      destruct_arrows (type1 :: type_vars) type2
    | _ -> List.rev type_vars, t
  in
  destruct_arrows [] t


(* This function transforms a type `t1 -> ... -> tn -> t` into the pair `([ t1 ; .. ; tn ] , t)` *)
let destruct_arrows (t : type_expression) =
  let rec destruct_arrows type_vars (t : type_expression) =
    match t.type_content with
    | T_arrow { type1; type2 } -> destruct_arrows (type1 :: type_vars) type2
    | _ -> List.rev type_vars, t
  in
  destruct_arrows [] t


let destruct_tuple (t : type_expression) =
  match t.type_content with
  | T_tuple types -> types
  | _ -> [ t ]


let destruct_tuples (t : type_expression list) =
  List.concat_map ~f:destruct_tuple t


(* This function takes an expression l and a list of arguments [e1; ...; en] and constructs `l e1 ... en`,
   but it checks that types make sense (i.e. l has a function type with enough arguments) *)
let build_applications_opt (lamb : expression) (args : expression list) =
  let rec aux lamb' (args : expression list) (t : type_expression) =
    match args, t.type_content with
    | arg :: args', T_arrow { type1 = _; type2 } ->
      aux
        (Combinators.make_e (E_application { lamb = lamb'; args = arg }) type2)
        args'
        type2
    | [], _ -> Some { lamb' with type_expression = t }
    | _, _ -> None
  in
  aux lamb args lamb.type_expression


(* This function re-builds a term prefixed with E_type_abstraction:
   given an expression e and a list of type variables [t1; ...; tn],
   it constructs an expression /\ t1 . ... . /\ tn . e *)
let rec build_type_abstractions e = function
  | [] -> e
  | abs_var :: abs_vars ->
    let e = build_type_abstractions e abs_vars in
    { e with
      expression_content =
        E_type_abstraction { type_binder = abs_var; result = e }
    ; type_expression = Combinators.t_for_all abs_var Type e.type_expression
    }


(* Free type variables in a type *)
let rec get_fv_type_expression : type_expression -> Type_var.Set.t =
 fun u ->
  let module Set = Type_var.Set in
  let self = get_fv_type_expression in
  match u.type_content with
  | T_variable v -> Set.singleton v
  | T_arrow { type1; type2 } ->
    let type1 = self type1 in
    let type2 = self type2 in
    Set.union type1 type2
  | T_abstraction { ty_binder; kind = _; type_ } ->
    Set.remove (self type_) ty_binder
  | T_for_all { ty_binder; kind = _; type_ } ->
    Set.remove (self type_) ty_binder
  | T_constant { language = _; injection = _; parameters } ->
    parameters |> List.map ~f:self |> Set.union_list
  | T_sum { fields; _ } | T_record { fields; _ } ->
    fields
    |> Map.map ~f:(fun elem -> self elem.content.Rows.Elem.associated_type)
    |> Map.data
    |> Set.union_list
  | T_tuple types -> types |> List.map ~f:self |> Set.union_list
  | T_singleton _ -> Set.empty


(* Substitutes a type variable `v` for a type `t` in the type `u`. In
   principle, variables could be captured. But in case a binder
   (forall, abstraction) is found in `fv`, a new (fresh) binder is
   generated and subtituted to prevent capture. *)
let rec subst_type ?(fv = Type_var.Set.empty) v t (u : type_expression) =
  let module Set = Type_var.Set in
  let self = subst_type ~fv in
  match u.type_content with
  | T_variable v' when Type_var.equal v v' -> t
  | T_variable _ -> u
  | T_arrow { type1; type2 } ->
    let type1 = self v t type1 in
    let type2 = self v t type2 in
    { u with type_content = T_arrow { type1; type2 } }
  | T_abstraction { ty_binder; kind; type_ } when Set.mem fv ty_binder ->
    let ty_binder' = Type_var.fresh () in
    let type_ = self ty_binder (Combinators.t_variable ty_binder' ()) type_ in
    let ty_binder = ty_binder' in
    self v t { u with type_content = T_abstraction { ty_binder; kind; type_ } }
  | T_abstraction { ty_binder; kind; type_ }
    when not (Type_var.equal ty_binder v) ->
    let type_ = self v t type_ in
    { u with type_content = T_abstraction { ty_binder; kind; type_ } }
  | T_abstraction _ -> u
  | T_for_all { ty_binder; kind; type_ } when Set.mem fv ty_binder ->
    let ty_binder' = Type_var.fresh () in
    let type_ = self ty_binder (Combinators.t_variable ty_binder' ()) type_ in
    let ty_binder = ty_binder' in
    self v t { u with type_content = T_for_all { ty_binder; kind; type_ } }
  | T_for_all { ty_binder; kind; type_ } when not (Type_var.equal ty_binder v)
    ->
    let type_ = self v t type_ in
    { u with type_content = T_for_all { ty_binder; kind; type_ } }
  | T_for_all _ -> u
  | T_constant { language; injection; parameters } ->
    let parameters = List.map ~f:(self v t) parameters in
    { u with type_content = T_constant { language; injection; parameters } }
  | T_sum row ->
    let row = subst_row ~fv v t row in
    { u with type_content = T_sum row }
  | T_record row ->
    let row = subst_row ~fv v t row in
    { u with type_content = T_record row }
  | T_singleton _ -> u
  | T_tuple types ->
    let types = List.map ~f:(self v t) types in
    { u with type_content = T_tuple types }


and subst_row ~fv v t (row : ty_expr Rows.t) =
  let fields =
    row.fields
    |> Map.map ~f:(fun (row_elem : _ Rows.Elem.t) ->
           { row_elem with
             content =
               { row_elem.content with
                 associated_type =
                   subst_type ~fv v t row_elem.content.associated_type
               }
           })
  in
  { row with fields }


(* Substitution as `subst_type`, but does not capture variables in
   `t`, by using `fv` = free variables of `t`. *)
let subst_no_capture_type v t (u : type_expression) =
  let fv = get_fv_type_expression t in
  subst_type ~fv v t u


(* Parallel substitution, it takes a map of variables pointing to
   expressions. Variables can be captured. *)
let rec psubst_type t (u : type_expression) =
  let self = psubst_type t in
  match u.type_content with
  | T_variable v' ->
    (match Map.find t v' with
    | Some t -> t
    | None -> u)
  | T_arrow { type1; type2 } ->
    let type1 = self type1 in
    let type2 = self type2 in
    { u with type_content = T_arrow { type1; type2 } }
  | T_abstraction { ty_binder; kind; type_ } when not (Map.mem t ty_binder) ->
    let type_ = self type_ in
    { u with type_content = T_abstraction { ty_binder; kind; type_ } }
  | T_abstraction _ -> u
  | T_for_all { ty_binder; kind; type_ } when not (Map.mem t ty_binder) ->
    let type_ = self type_ in
    { u with type_content = T_for_all { ty_binder; kind; type_ } }
  | T_for_all _ -> u
  | T_constant { language; injection; parameters } ->
    let parameters = List.map ~f:self parameters in
    { u with type_content = T_constant { language; injection; parameters } }
  | T_sum row ->
    let row = psubst_row t row in
    { u with type_content = T_sum row }
  | T_record row ->
    let row = psubst_row t row in
    { u with type_content = T_record row }
  | T_singleton _ -> u
  | T_tuple types ->
    let types = List.map ~f:self types in
    { u with type_content = T_tuple types }


and psubst_row t (row : ty_expr Rows.t) =
  let fields =
    row.fields
    |> Map.map ~f:(fun (row_elem : _ Rows.Elem.t) ->
           { row_elem with
             content =
               { row_elem.content with
                 associated_type =
                   psubst_type t row_elem.content.associated_type
               }
           })
  in
  { row with fields }


type 'a fold_mapper = 'a -> expression -> bool * 'a * expression

let rec fold_map_expression
    : 'a fold_mapper -> 'a -> expression -> 'a * expression
  =
 fun f a e ->
  let self = fold_map_expression f in
  let self_type acc t = acc, t in
  let continue, init, e' = f a e in
  if not continue
  then init, e'
  else (
    let return expression_content = { e' with expression_content } in
    match e'.expression_content with
    | E_matching { matchee = e; cases } ->
      let res, e' = self init e in
      let res, cases' = fold_map_cases f res cases in
      res, return @@ E_matching { matchee = e'; cases = cases' }
    | E_accessor accessor ->
      let res, accessor = Accessor.fold_map self init accessor in
      res, return @@ E_accessor accessor
    | E_record record ->
      let res, record = Record.fold_map self init record in
      res, return @@ E_record record
    | E_update update ->
      let res, update = Update.fold_map self init update in
      res, return @@ E_update update
    | E_constructor constr ->
      let res, constr = Constructor.fold_map self init constr in
      res, return @@ E_constructor constr
    | E_application app ->
      let res, app = Application.fold_map self init app in
      res, return @@ E_application app
    | E_let_in let_in ->
      let res, let_in = Let_in.fold_map self self_type init let_in in
      res, return @@ E_let_in let_in
    | E_mod_in mod_in ->
      let res, mod_in =
        Mod_in.fold_map self (fold_map_expression_in_module_expr f) init mod_in
      in
      res, return @@ E_mod_in mod_in
    | E_type_inst { forall; type_ } ->
      let res, forall = self init forall in
      res, return @@ E_type_inst { forall; type_ }
    | E_lambda l ->
      let res, l = Lambda.fold_map self self_type init l in
      res, return @@ E_lambda l
    | E_type_abstraction ta ->
      let res, ta = Type_abs.fold_map self init ta in
      res, return @@ E_type_abstraction ta
    | E_recursive r ->
      let res, r = Recursive.fold_map self self_type init r in
      res, return @@ E_recursive r
    | E_constant const ->
      let res, const = Constant.fold_map self init const in
      res, return @@ E_constant const
    | E_raw_code { language; code } ->
      let res, code = self init code in
      res, return @@ E_raw_code { language; code }
    | E_assign a ->
      let res, a = Assign.fold_map self self_type init a in
      res, return @@ E_assign a
    | E_let_mut_in let_in ->
      let res, let_in = Let_in.fold_map self self_type init let_in in
      res, return @@ E_let_mut_in let_in
    | E_for f ->
      let res, f = For_loop.fold_map self init f in
      res, return @@ E_for f
    | E_for_each fe ->
      let res, fe = For_each_loop.fold_map self init fe in
      res, return @@ E_for_each fe
    | E_while w ->
      let res, w = While_loop.fold_map self init w in
      res, return @@ E_while w
    | E_cond cond ->
      let res, cond = Conditional.fold_map self init cond in
      res, return @@ E_cond cond
    | E_sequence seq ->
      let res, seq = Sequence.fold_map self init seq in
      res, return @@ E_sequence seq
    | E_tuple tuple ->
      let res, tuple = Tuple.fold_map self init tuple in
      res, return @@ E_tuple tuple
    | E_set set_expr ->
      let res, set_expr = Set_expr.fold_map self init set_expr in
      res, return @@ E_set set_expr
    | E_map map_expr ->
      let res, map_expr = Map_expr.fold_map self init map_expr in
      res, return @@ E_map map_expr
    | E_big_map big_map_expr ->
      let res, big_map_expr = Map_expr.fold_map self init big_map_expr in
      res, return @@ E_big_map big_map_expr
    | E_list list_expr ->
      let res, list_expr = List_expr.fold_map self init list_expr in
      res, return @@ E_list list_expr
    | (E_deref _ | E_skip | E_literal _ | E_variable _ | E_module_accessor _) as
      e' -> init, return e')


and fold_map_cases : 'a fold_mapper -> 'a -> matching_expr -> 'a * matching_expr
  =
 fun f init m ->
  match m with
  | Match_variant { cases; tv } ->
    let aux init { constructor; pattern; body } =
      let init, body = fold_map_expression f init body in
      init, { constructor; pattern; body }
    in
    let init, cases = List.fold_map ~f:aux ~init cases in
    init, Match_variant { cases; tv }
  | Match_record { fields; body; tv } ->
    let init, body = fold_map_expression f init body in
    init, Match_record { fields; body; tv }
  | Match_tuple { binders; body; tv } ->
    let init, body = fold_map_expression f init body in
    init, Match_tuple { binders; body; tv }


and fold_map_declaration m acc (x : declaration) =
  match Location.unwrap x with
  | D_value { binder; expr; attr } ->
    let acc', expr = fold_map_expression m acc expr in
    let wrap_content = D_value { binder; expr; attr } in
    acc', { x with wrap_content }
  | D_type t ->
    let wrap_content = D_type t in
    acc, { x with wrap_content }
  | D_module { module_binder; module_; module_attr } ->
    let acc', module_ = (fold_map_expression_in_module_expr m) acc module_ in
    let wrap_content = D_module { module_binder; module_; module_attr } in
    acc', { x with wrap_content }


and fold_map_decl m = fold_map_declaration m

and fold_map_module : 'a fold_mapper -> 'a -> module_ -> 'a * module_ =
 fun m init -> List.fold_map ~f:(fold_map_decl m) ~init


and fold_map_expression_in_module_expr
    : 'a fold_mapper -> 'a -> module_expr -> 'a * module_expr
  =
 fun fold_mapper acc x ->
  let return r wrap_content = r, { x with wrap_content } in
  match x.wrap_content with
  | M_struct decls ->
    let res, decls = fold_map_module fold_mapper acc decls in
    return res (Module_expr.M_struct decls)
  | M_module_path _ as x -> return acc x
  | M_variable _ as x -> return acc x


let fold_map_program : 'a fold_mapper -> 'a -> program -> 'a * program =
 fun m init -> List.fold_map ~f:(fold_map_declaration m) ~init


let rec fold_type_expression
    : type a. type_expression -> init:a -> f:(a -> type_expression -> a) -> a
  =
 fun te ~init ~f ->
  let self te = fold_type_expression te ~f in
  let init = f init te in
  match te.type_content with
  | T_variable _ -> init
  | T_constant { parameters; _ } -> List.fold parameters ~init ~f
  | T_sum { fields; _ } | T_record { fields; _ } ->
    Label.Map.fold
      fields
      ~init
      ~f:(fun ~key:_ ~data:(row_elem : _ Rows.Elem.t) acc ->
        self ~init:acc row_elem.content.associated_type)
  | T_arrow { type1; type2 } -> self type2 ~init:(self type1 ~init)
  | T_singleton _ -> init
  | T_abstraction { type_; _ } | T_for_all { type_; _ } -> self type_ ~init
  | T_tuple types ->
    List.fold types ~init ~f:(fun init type_ -> self ~init type_)


(* An [IdMap] is a [Map] augmented with an [id] field (which is wrapped around the map [value] field).
  Using a map instead of a list makes shadowed modules inaccessible,
  since they are overwritten from the map when adding the shadower, whilst they were kept when using lists.
  The [id] field in the map values is used to infer the type to which a constructor belong when they are not annotated
  e.g. we need to keep the declaration order to infer that 'c' has type z in:

    type x = A of int | B
    module M = struct
      type y = A of int
    end
    type z = A of int | AA

    let c = A 2
*)

let global_id = ref 0

module IdMap = struct
  module type OrderedType = Caml.Map.OrderedType

  module type IdMapSig = sig
    type key
    type 'a t
    type 'a kvi_list = (key * 'a * int) list

    val empty : 'a t
    val add : 'a t -> key -> 'a -> 'a t

    (* In case of merge conflict between two values with same keys, this merge function keeps the value with the highest id.
        This follows the principle that this map always keeps the latest value in case of conflict *)
    val merge : 'a t -> 'a t -> 'a t

    (* Converts the map into an unsorted (key * value * id) 3-uple *)
    val to_kvi_list : 'a t -> (key * 'a * int) list

    (* Converts the kvi_list into a id-sorted kv_list *)
    val sort_to_kv_list : (key * 'a * int) list -> (key * 'a) list
    val to_kv_list : 'a t -> (key * 'a) list
  end
  (* of module type S *)

  module Make (Ord : OrderedType) : IdMapSig with type key = Ord.t = struct
    module Map = Simple_utils.Map.Make (Ord)

    type key = Ord.t

    type 'a id_wrapped =
      { id : int
            (* This is is used in [filter_values], to return a list of matching values in chronological order *)
      ; value : 'a
      }

    type 'a t = 'a id_wrapped Map.t
    type 'a kvi_list = (key * 'a * int) list

    let empty = Map.empty

    let add : 'a t -> key -> 'a -> 'a t =
     fun map key value ->
      global_id := !global_id + 1;
      let id = !global_id in
      let id_value = { id; value } in
      Map.add key id_value map


    let merge : 'a t -> 'a t -> 'a t =
     fun m1 m2 ->
      let merger
          :  key -> 'a id_wrapped option -> 'a id_wrapped option
          -> 'a id_wrapped option
        =
       fun _ v1 v2 ->
        match v1, v2 with
        | None, None -> None
        | Some v, None -> Some v
        | None, Some v -> Some v
        | Some v1, Some v2 -> if v1.id > v2.id then Some v1 else Some v2
      in
      Map.merge merger m1 m2


    let to_kvi_list : 'a t -> (key * 'a * int) list =
     fun map ->
      List.map ~f:(fun (key, value) -> key, value.value, value.id)
      @@ Map.to_kv_list map


    let sort_to_kv_list : (key * 'a * int) list -> (key * 'a) list =
     fun list ->
      let sorted_list =
        List.sort list ~compare:(fun (_, _, id1) (_, _, id2) ->
            Int.compare id2 id1)
      in
      List.map ~f:(fun (k, v, _) -> k, v) sorted_list


    let to_kv_list : 'a t -> (key * 'a) list =
     fun map -> sort_to_kv_list @@ to_kvi_list map
  end
  (* of module IdMap.Make*)
end
(* of module IdMap *)

(*
    Add the shadowed t_sum types nested in the fetched types.

    After using [get_modules_types], we have the ctxt types, i.e. all types declared current scope and submodules.
    There is no shadowed type in ctxt types (since ctxt is a map, shadowed types are removed when adding the shadower).
    However we want shadowed types when they are nested in another type :
      type a = Foo of int | Bar of string
      type a = a list
    Here, we want [Foo of int | Bar of string] to be found
    But we want to add nested t_sum types _only_ if they are shadowed, we don't want to add them in that case for example :
      type foo_variant = Foo of int | Bar of string
      type foo_record = { foo : foo_variant ; bar : foo_variant}
    Because [foo_variant] would appear three times in the list instead of one.

    NOTE : We could append nested types on top of the [module_types] list we have so far,
    but having a final list with all nested-types before toplevel types triggers some errors.

    NOTE : We can't just do a final id-sort of the list to have everything in declarartion order
    because the fetched nested types don't have id, only the ones retrieved from the ctxt do.

    So, if we have ctxt types :
      [t1; t2; t3]
    After adding the shadowed t_sums, we want the final list :
      [t1; tsum_shadowed_by_t1; t2; tsum_shadowed_by_t2; t3; tsum_shadowed_by_t3]

    NOTE : When [fold_type_expression] is used on t1, it will add tsum types nested in t1,
    but it might also add t1 (or not), we don't know.
    However, we want to make sure t1 is in the final list *exactly once*.
      - If it's not here, we'll lose a type and have incorrect "type [t1] not found" errors
      - If it's here more than once, we'll have a false "warning, [t1] inferred but could also be of type [t1]"
    To ensure [t1] appears once exactly, we tweak the fold function by passing a [is_top] boolean
    to ensure it will fold over all nested type in [t1] but not the toplevel one (i.e. [t1]),
    we then add [t1] manually to the list.
*)
let add_shadowed_nested_t_sum tsum_list (tv, te) =
  let add_if_shadowed_t_sum
      :  Type_var.t -> (Type_var.t * type_expression) list * bool
      -> type_expression -> (Type_var.t * type_expression) list * bool
    =
   fun shadower_tv (accu, is_top) te ->
    let ret x = x, false in
    match te.type_content, te.orig_var with
    | T_sum _, Some tv ->
      if Type_var.equal tv shadower_tv && not is_top
      then ret ((tv, te) :: accu)
      else ret accu
    | T_sum _, None ->
      ret accu
      (* TODO : What should we do with those sum types with no binder ? *)
    | _ -> ret accu
  in
  let (nested_t_sums, _) : (Type_var.t * type_expression) list * bool =
    fold_type_expression
      te
      ~init:(tsum_list, true)
      ~f:(add_if_shadowed_t_sum tv)
  in
  (tv, te) :: nested_t_sums


(* get_views [p] looks for top-level declaration annotated with [@view] in program [p] and return declaration data *)
let get_views : program -> (Value_var.t * Location.t) list =
 fun p ->
  let f
      :  declaration -> (Value_var.t * Location.t) list
      -> (Value_var.t * Location.t) list
    =
   fun { wrap_content = decl; location = _ } acc ->
    match decl with
    | D_value { binder; expr = _; attr } when attr.view ->
      let var = Binder.get_var binder in
      (var, Value_var.get_location var) :: acc
    | _ -> acc
  in
  List.fold_right ~init:[] ~f p


let fetch_view_type
    : declaration -> (type_expression * type_expression Binder.t) option
  =
 fun declt ->
  match Location.unwrap declt with
  | D_value { binder; expr; attr } when attr.view ->
    Some
      (expr.type_expression, Binder.map (fun _ -> expr.type_expression) binder)
  | _ -> None


let fetch_views_in_program
    : program -> (type_expression * type_expression Binder.t) list
  =
 fun prg -> List.filter_map ~f:fetch_view_type prg
