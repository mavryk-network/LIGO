open Ast_unified
open Pass_type
open Simple_utils.Trace
open Simple_utils
open Errors
module Location = Simple_utils.Location

let compile ~syntax =
  let expr : (expr, ty_expr, pattern, _, _) expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_ctor_app (c, args) ->
      (match get_e c with
      | E_constr c
        when Label.(equal c (of_string "Unit"))
             && Option.is_none args
             && Syntax_types.equal syntax PascaLIGO ->
        (* should not be necessary in principle *)
        e_literal ~loc Literal_unit
      | E_constr constructor ->
        let element =
          match args with
          | None -> e_unit ~loc
          | Some (one, []) -> one
          | Some lst ->
            let loc =
              List.fold
                ~init:Location.generated
                ~f:Location.cover
                (List.map ~f:get_e_loc (List.Ne.to_list lst))
            in
            e_tuple ~loc lst
        in
        e_constructor ~loc { constructor; element }
      | _ -> failwith "impossible: parsing invariant")
    | e -> make_e ~loc e
  in
  `Cata { idle_cata_pass with expr }


let decompile =
  let expr : (expr, ty_expr, pattern, _, _) expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_constructor { constructor; element } ->
      e_ctor_app ~loc (e_constr ~loc constructor, Some (List.Ne.singleton element))
    | e -> make_e ~loc e
  in
  `Cata { idle_cata_pass with expr }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_ctor_app _; _ } | { wrap_content = E_constr _; _ } ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let pass ~raise ~syntax =
  morph
    ~name:__MODULE__
    ~compile:(compile ~syntax)
    ~decompile
    ~reduction_check:(reduction ~raise)
