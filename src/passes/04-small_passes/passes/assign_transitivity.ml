open Ast_unified
open Pass_type
open Simple_utils.Trace
open Simple_utils
open Errors
module Location = Simple_utils.Location

let computation ~raise ~loc l r op =
  let open Assign_jsligo in
  let v =
    match get_e_variable l with
    | Some x -> x
    | None -> raise.error (wrong_lvalue l)
  in
  let assign r =
    e_assign ~loc { binder = Ligo_prim.Binder.make v None; expression = r }
  in
  let res =
    match op with
    | Eq -> r
    | Assignment_operator x ->
      let computation cons_name = e_constant ~loc { cons_name; arguments = [ l; r ] } in
      let op : Ligo_prim.Constant.constant' =
        match x with
        | Times_eq -> C_MUL
        | Div_eq -> C_DIV
        | Min_eq -> C_POLYMORPHIC_SUB
        | Plus_eq -> C_POLYMORPHIC_ADD
        | Mod_eq -> C_MOD
      in
      computation op
  in
  assign res, res


let compile ~raise =
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_AssignJsligo { expr1; op; expr2 } ->
      let assignment, res = computation ~raise ~loc expr1 expr2 op in
      sequence assignment res
    | e -> make_e ~loc e
  in
  `Cata { idle_cata_pass with expr }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_AssignJsligo _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let pass ~raise =
  cata_morph
    ~name:__MODULE__
    ~compile:(compile ~raise)
    ~decompile:`None (* for now ? *)
    ~reduction_check:(reduction ~raise)
