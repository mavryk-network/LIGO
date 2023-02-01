open Ast_unified
open Pass_type
open Simple_utils.Trace
open Simple_utils
open Errors
module Location = Simple_utils.Location

(*
  warns about unreachable code and restrict returns in unsupported instructions (loops)
*)

let is_return : instruction -> bool =
 fun i ->
  match get_i i with
  | I_Return _ -> true
  | _ -> false


let is_s_return : statement -> bool =
 fun s ->
  match get_s s with
  | S_Instr i -> is_return i
  | _ -> false


let unreachable_code ~raise : _ block_ -> unit =
 fun block ->
  let stmts = Location.unwrap block in
  let f (has_returned, unreachable) x =
    match get_s x with
    | S_Instr i when is_return i ->
      true, if has_returned then x :: unreachable else unreachable
    | _ -> has_returned, if has_returned then x :: unreachable else unreachable
  in
  let _, unreachable = List.fold ~init:(false, []) ~f (List.Ne.to_list stmts) in
  raise.warning
    (`Jsligo_unreachable_code
      (unreachable
      |> List.map ~f:get_s_loc
      |> List.fold ~init:Location.generated ~f:Location.cover))


let compile ~raise =
  let block : _ block_ -> block =
   fun b ->
    unreachable_code ~raise b;
    make_b ~loc:b.location b.wrap_content
  in
  let instruction : (_, _, _, statement, block) instruction_ -> instruction = function
    | { wrap_content =
          ( I_While { block; _ }
          | I_For { block; _ }
          | I_ForOf
              { for_stmt =
                  { fp =
                      { wrap_content =
                          S_Instr { fp = { wrap_content = I_Block block; _ } }
                      ; _
                      }
                  }
              ; _
              }
          | I_ForIn (ForMap { block; _ })
          | I_ForIn (ForSetOrList { block; _ }) ) as i
      ; location = loc
      } ->
      let block = get_b block in
      if List.exists ~f:is_s_return (List.Ne.to_list block)
      then raise.error (unsupported_return (List.Ne.to_list block))
      else make_i ~loc i
    | { wrap_content; location = loc } -> make_i ~loc wrap_content
  in
  let expr : _ expr_ -> expr = function
    | { wrap_content = E_Block_with { block; expr = _ } as e; location = loc } ->
      let block = get_b block in
      if List.exists ~f:is_s_return (List.Ne.to_list block)
      then raise.error (unsupported_return (List.Ne.to_list block))
      else make_e ~loc e
    | { wrap_content; location = loc } -> make_e ~loc wrap_content
  in
  `Cata { idle_cata_pass with instruction; block; expr }


let pass ~raise =
  cata_morph
    ~name:__MODULE__
    ~compile:(compile ~raise)
    ~decompile:`None
    ~reduction_check:Iter.defaults