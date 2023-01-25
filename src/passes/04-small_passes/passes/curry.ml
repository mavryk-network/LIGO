open Ast_unified
open Pass_type
open Simple_utils.Trace
open Simple_utils.Function
open Simple_utils
open Syntax_types
open Errors
module Location = Simple_utils.Location

let is_curry syntax =
  match syntax with
  | JsLIGO | PascaLIGO -> false
  | CameLIGO -> true


let loc_of_params parameters =
  List.fold
    (List.map ~f:(fun x -> get_p_loc Param.(x.pattern)) parameters)
    ~init:Location.generated
    ~f:Location.cover


let parameter_type_as_tuple_opt ~loc =
  Option.map ~f:(fun lst -> t_prod ~loc (List.Ne.of_list lst))
  <@ Option.all
  <@ List.map ~f:(fun p -> Param.(p.param_type))


let mut_flag = function
  | `Var -> Ligo_prim.Param.Mutable
  | `Const -> Ligo_prim.Param.Immutable


let compile_poly_fun_curry ~loc parameters ret_type body =
  fst
  @@ List.fold_right
       parameters
       ~init:(body, ret_type)
       ~f:(fun Param.{ param_kind; pattern; param_type } (acc, output_type) ->
         match get_p_var pattern with
         | Some v ->
           let binder =
             Ligo_prim.Param.make ~mut_flag:(mut_flag param_kind) v param_type
           in
           e_lambda ~loc Lambda.{ binder; output_type; result = acc }, None
         | None ->
           let fresh_binder = Variable.fresh ~loc:(get_p_loc pattern) () in
           let result =
             e_simple_let_in
               ~loc:(get_e_loc body)
               { binder = pattern; rhs = e_variable ~loc fresh_binder; let_result = acc }
           in
           let binder =
             Ligo_prim.Param.make ~mut_flag:Immutable fresh_binder param_type
           in
           e_lambda ~loc Lambda.{ binder; output_type; result }, None)


let compile_poly_fun_uncurry ~loc parameters ret_type body =
  let param_loc = loc_of_params parameters in
  let fresh_binder = Variable.fresh ~loc:param_loc () in
  let binder =
    let binder_ty = parameter_type_as_tuple_opt ~loc parameters in
    Ligo_prim.Param.make ~mut_flag:Immutable fresh_binder binder_ty
  in
  let result =
    let param_pattern =
      p_tuple ~loc:param_loc (List.map ~f:(fun x -> x.pattern) parameters)
    in
    e_simple_let_in
      ~loc:(get_e_loc body)
      { binder = param_pattern; rhs = e_variable ~loc fresh_binder; let_result = body }
  in
  e_lambda ~loc Lambda.{ binder; output_type = ret_type; result }


let compile ~syntax =
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_Poly_fun { type_params = None; parameters; ret_type; body } ->
      if is_curry syntax
      then compile_poly_fun_curry ~loc parameters ret_type body
      else compile_poly_fun_uncurry ~loc parameters ret_type body
    | E_Poly_recursive
        { fun_name
        ; fun_type
        ; lambda = { type_params = None; parameters; ret_type; body }
        } ->
      let lambda_ =
        if is_curry syntax
        then compile_poly_fun_curry ~loc parameters ret_type body
        else compile_poly_fun_uncurry ~loc parameters ret_type body
      in
      let fun_type =
        let ret, tys =
          let hd, tl = List.Ne.of_list (List.rev fun_type) in
          hd, List.rev tl
        in
        if is_curry syntax
        then List.fold_right tys ~init:ret ~f:(fun ty acc -> t_fun ~loc (ty, acc))
        else t_fun ~loc (t_prod ~loc (List.Ne.of_list tys), ret)
      in
      (match get_e_lambda lambda_ with
      | None -> failwith "impossible"
      | Some lambda -> e_recursive ~loc { fun_name; fun_type; lambda })
    | E_Call (f, args) ->
      if is_curry syntax
      then
        List.fold args ~init:f ~f:(fun acc arg ->
            e_application ~loc Application.{ lamb = acc; args = arg })
      else (
        let args =
          match args with
          | [] -> e_unit ~loc
          | hd :: tl -> e_tuple ~loc (hd, tl)
        in
        e_application ~loc Application.{ lamb = f; args })
    | e -> make_e ~loc e
  in
  let declaration : _ declaration_ -> declaration =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | d -> make_d ~loc d
  in
  `Cata { idle_cata_pass with expr; declaration }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_Poly_fun _ | E_Poly_recursive _ | E_Call _; _ } ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  ; declaration =
      (function
      | { wrap_content = D_Let _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let pass ~raise ~syntax =
  cata_morph
    ~name:__MODULE__
    ~compile:(compile ~syntax)
    ~decompile:`None (* for now ? *)
    ~reduction_check:(reduction ~raise)