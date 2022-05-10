let rec internalize_typed (ds : Ast_typed.program) =
  let open Ast_typed in
  let f (d : _) = match d with
    | Declaration_module { module_binder ; module_ = { wrap_content = M_struct m ; _} as module_ ; module_attr = _ } ->
       let module_attr = { public = false ; hidden = true } in
       let m = internalize_typed m in
       let module_ = { module_ with wrap_content = M_struct m } in
       Declaration_module { module_binder ; module_ ; module_attr }
    | Declaration_module { module_binder ; module_ ; module_attr = _ } ->
       let module_attr = { public = false ; hidden = true } in
       Declaration_module { module_binder ; module_ ; module_attr }
    | Declaration_constant { binder ; expr ; attr } ->
       let name = Format.asprintf "%a" ValueVar.pp binder.var in
       let binder = if String.is_prefix ~prefix:"_hash_" name then
                      { binder with var = ValueVar.of_input_var @@ "#" ^ String.chop_prefix_if_exists ~prefix:"_hash_" name }
                    else if String.is_prefix ~prefix:"_remove_" name then
                      { binder with var = ValueVar.of_input_var @@ String.chop_prefix_if_exists ~prefix:"_remove_" name }
                    else
                      binder in
       Declaration_constant { binder ; expr ; attr }
    | Declaration_type { type_binder ; type_expr ; type_attr = _ } ->
       let type_attr = { public = false ; hidden = true } in
       Declaration_type { type_binder ; type_expr ; type_attr } in
  let f (d : _ Ast_typed.location_wrap) = Simple_utils.Location.map f d in
  List.map ~f ds

let rec internalize_core (ds : Ast_core.module_) =
  let open Ast_core in
  let f (d : _) = match d with
    | Declaration_module { module_binder ; module_ = { wrap_content = M_struct m ; _} as module_ ; module_attr = _ } ->
       let module_attr = { public = false ; hidden = true } in
       let m = internalize_core m in
       let module_ = { module_ with wrap_content = M_struct m } in
       Declaration_module { module_binder ; module_ ; module_attr }
    | Declaration_module { module_binder ; module_ ; module_attr = _ } ->
       let module_attr = { public = false ; hidden = true } in
       Declaration_module { module_binder ; module_ ; module_attr }
    | Declaration_constant { binder ; expr ; attr } ->
       let name = Format.asprintf "%a" ValueVar.pp binder.var in
       let binder = if String.is_prefix ~prefix:"_hash_" name then
                      { binder with var = ValueVar.of_input_var @@ "#" ^ String.chop_prefix_if_exists ~prefix:"_hash_" name }
                    else if String.is_prefix ~prefix:"_remove_" name then
                      { binder with var = ValueVar.of_input_var @@ String.chop_prefix_if_exists ~prefix:"_remove_" name }
                    else
                      binder in
       Declaration_constant { binder ; expr ; attr }
    | Declaration_type { type_binder ; type_expr ; type_attr = _ } ->
       let type_attr = { public = false ; hidden = true } in
       Declaration_type { type_binder ; type_expr ; type_attr } in
  let f (d : _ Ast_core.location_wrap) = Simple_utils.Location.map f d in
  List.map ~f ds
