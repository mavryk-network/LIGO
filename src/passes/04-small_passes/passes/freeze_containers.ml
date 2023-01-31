open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

let compile ~raise =
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    let op cons_name arguments = e_constant ~loc { cons_name; arguments } in
    let mk_list lst =
      let init = op C_LIST_EMPTY [] in
      List.fold lst ~f:(fun acc x -> op C_CONS [ x; acc ]) ~init
    in
    let mk_pair_list lst =
      let init = op C_LIST_EMPTY [] in
      List.fold
        lst
        ~f:(fun acc (k, v) ->
          let el = e_tuple ~loc (k, [ v ]) in
          op C_CONS [ el; acc ])
        ~init
    in
    match Location.unwrap e with
    | E_MapLookup { map; keys } -> failwith "this node will go with pascaligo .."
    | E_Map kvlst ->
      let lst = mk_pair_list kvlst in
      op C_MAP_LITERAL [ lst ]
    | E_BigMap kvlst ->
      let lst = mk_pair_list kvlst in
      op C_BIG_MAP_LITERAL [ lst ]
    | E_List lst -> mk_list lst
    | E_Set set ->
      let lst = mk_list set in
      op C_SET_LITERAL [ lst ]
    | e -> make_e ~loc e
  in
  `Cata { idle_cata_pass with expr }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content =
            E_MapLookup _ | E_Map _ | E_BigMap _ | E_Sequence _ | E_List _ | E_Set _
        ; _
        } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let pass ~raise =
  cata_morph
    ~name:__MODULE__
    ~compile:(compile ~raise)
    ~decompile:`None
    ~reduction_check:(reduction ~raise)
