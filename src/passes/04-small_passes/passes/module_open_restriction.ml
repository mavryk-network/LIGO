open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

let compile ~raise =
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_Module_open_in { module_path; field } ->
      (match get_e_variable field with
      | Some v -> e_module_access ~loc { module_path; field = v }
      | None -> raise.error (unsupported_module_access (`Expr field)))
    | e -> make_e ~loc e
  in
  let ty_expr : _ ty_expr_ -> ty_expr =
   fun ty ->
    let loc = Location.get_location ty in
    match Location.unwrap ty with
    | T_Module_open_in { module_path; field } ->
      (match get_t_var field with
      | Some v -> t_module_access ~loc { module_path; field = v }
      | None -> raise.error (unsupported_module_access (`Type field)))
    | t -> make_t ~loc t
  in
  `Cata { idle_cata_pass with expr; ty_expr }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_Module_open_in _; _ } ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  ; ty_expr =
      (function
      | { wrap_content = T_Module_open_in _; _ } ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let pass ~raise =
  cata_morph
    ~name:__MODULE__
    ~compile:(compile ~raise)
    ~decompile:`None
    ~reduction_check:(reduction ~raise)
