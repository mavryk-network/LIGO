open Ast_unified
open Pass_type
open Simple_utils.Trace
open Simple_utils
open Errors
module Location = Simple_utils.Location

module Statement_result : sig
  type t =
    | Binding of (expr -> expr)
    | Return of expr
    | Control_flow of (t -> expr)

  val merge : t -> t -> t
  val to_expression : loc:Location.t -> t -> expr
end = struct
  type t =
    | Binding of (expr -> expr)
    | Return of expr
    (* at the time we are elaborating a control flow statement
      (if _ then X else Y ; case _ with _ -> X | _ -> Y)

      the next statement results (t) are merged within the continuation
      to ensure return in control flow branches are terminal
    *)
    | Control_flow of (t -> expr)

  let rec merge : t -> t -> t =
   fun bef aft ->
    let open Simple_utils.Function in
    match bef, aft with
    | Binding a, Binding b -> Binding (a <@ b)
    | Binding a, Return b -> Return (a b)
    | Binding a, Control_flow f -> Control_flow (fun t -> a @@ f t)
    | Control_flow f, x -> Control_flow (fun t -> f (merge x t))
    | Return a, _ -> Return a


  let to_expression ~loc : t -> expr =
   fun statement_result ->
    match statement_result with
    | Binding b -> b (e_unit ~loc)
    | Return r -> r
    | Control_flow f -> f (Return (e_unit ~loc))
end

let sequence a b =
  e_let_in
    ~loc:(Location.cover (get_e_loc a) (get_e_loc b))
    { is_rec = false
    ; type_params = None
    ; lhs = List.Ne.singleton @@ p_unit ~loc:Location.generated
    ; rhs_type = None
    ; rhs = a
    ; body = b
    }


(* temprorary until pass 'expand_polymorphism' is written and E_fun added *)
let e_fun ~loc x = e_poly_fun ~loc x

let rec decl : declaration -> Statement_result.t =
 fun d ->
  let loc = get_d_loc d in
  match get_d d with
  | D_Directive _ -> Binding Fun.id
  | D_Attr (attr, d) ->
    let d = decl d in
    Statement_result.merge (Binding (fun x -> e_attr ~loc:(get_e_loc x) (attr, x))) d
  | D_Import (Import_rename { alias; module_path }) ->
    Binding
      (fun x ->
        e_modin
          ~loc:(get_e_loc x)
          { module_name = alias; rhs = m_path ~loc module_path; body = x })
  | D_Import (Import_all_as _ | Import_selected _) -> failwith "not supported"
  | D_Export d ->
    (* weird .. *)
    decl (d_attr ~loc (Attribute.{ key = "public"; value = None }, d))
  | D_Var { type_params; pattern; rhs_type; let_rhs } ->
    Binding
      (fun x ->
        e_let_mut_in
          ~loc:(Location.cover loc (get_e_loc x))
          { is_rec = false
          ; type_params
          ; lhs = List.Ne.singleton pattern
          ; rhs_type
          ; rhs = let_rhs
          ; body = x
          })
  | D_Const { type_params; pattern; rhs_type; let_rhs } ->
    Binding
      (fun x ->
        e_let_in
          ~loc:(Location.cover loc (get_e_loc x))
          { is_rec = false
          ; type_params
          ; lhs = List.Ne.singleton pattern
          ; rhs_type
          ; rhs = let_rhs
          ; body = x
          })
  | D_Let { is_rec; type_params; pattern; rhs_type; let_rhs } ->
    Binding
      (fun x ->
        e_let_in
          ~loc:(Location.cover loc (get_e_loc x))
          { is_rec; type_params; lhs = pattern; rhs_type; rhs = let_rhs; body = x })
  | D_Fun { is_rec; fun_name; type_params; parameters; ret_type; return } ->
    Binding
      (fun x ->
        e_let_in
          ~loc:(Location.cover loc (get_e_loc x))
          { is_rec
          ; type_params
          ; lhs = List.Ne.singleton (p_var ~loc:(Variable.get_location fun_name) fun_name)
          ; rhs_type = None
          ; rhs = e_fun ~loc { type_params; parameters; ret_type; body = return }
          ; body = x
          })
  | D_Module { name; mod_expr } ->
    Binding
      (fun x ->
        e_modin
          ~loc:(Location.cover loc (get_e_loc x))
          { module_name = name; rhs = mod_expr; body = x })
  | D_Type { name; type_expr } ->
    Binding
      (fun x ->
        e_typein
          ~loc:(Location.cover loc (get_e_loc x))
          { type_binder = name; rhs = type_expr; body = x })
  | D_Multi_var _ | D_Multi_const _ -> failwith "removed"
  | D_Type_abstraction _ -> failwith "removed"


and instr : instruction -> Statement_result.t =
 fun i ->
  let loc = get_i_loc i in
  match get_i i with
  | I_Return expr_opt -> Return (Option.value expr_opt ~default:(e_unit ~loc))
  | I_Block block ->
    let init, stmts = List.Ne.map statement block in
    let res = List.fold ~init ~f:Statement_result.merge stmts in
    (match res with
    | Return _ -> res
    | Binding f -> Binding (fun x -> sequence (f (e_unit ~loc)) x)
    | Control_flow (_ : Statement_result.t -> expr) ->
      (* will go wrong because of shadowing :
        const g = (n:int) => {
        let output = n;
        
        {
          let x = 1 ;
          output += x ;
          if (n > 1) {
            return (output + 12)
          } else {
            output += 2;
          }
        }
        
        return output + x // x should not be visible here
      }
      *)
      failwith "not supported")
  | I_Skip -> Binding Fun.id
  | I_Call (f, args) -> Binding (fun x -> sequence (e_call ~loc f args) x)
  | I_Case { expr; cases } ->
    Control_flow
      (fun x ->
        let f
            :  (pattern, (instruction, statement) Test_clause.t) Case.clause
            -> (pattern, expr) Case.clause
          =
         fun { pattern; rhs } ->
          let branch_result =
            match rhs with
            | ClauseInstr instruction -> Statement_result.merge (instr instruction) x
            | ClauseBlock block ->
              let init, stmts = List.Ne.map statement block in
              List.fold ~init ~f:Statement_result.merge (stmts @ [ x ])
          in
          { pattern; rhs = Statement_result.to_expression ~loc branch_result }
        in
        let cases = List.Ne.map f cases in
        e_match ~loc { expr; cases })
  | I_Cond { test; ifso; ifnot } ->
    Control_flow
      (fun hole ->
        let f : (instruction, statement) Test_clause.t -> expr =
         fun clause ->
          let branch_result =
            match clause with
            | ClauseInstr instruction -> Statement_result.merge (instr instruction) hole
            | ClauseBlock block ->
              let init, stmts = List.Ne.map statement block in
              List.fold ~init ~f:Statement_result.merge (stmts @ [ hole ])
          in
          Statement_result.to_expression ~loc branch_result
        in
        e_cond
          ~loc
          { test
          ; ifso = f ifso
          ; ifnot =
              Some
                (Option.value_map
                   ~default:(Statement_result.to_expression ~loc hole)
                   ~f
                   ifnot)
          })
  | I_For _ | I_ForIn _ | I_ForOf _ | I_Patch _ | I_Remove _ | I_While _ | I_Expr _
  | I_Assign (_, _) -> failwith "<hole> -> let () = assign x y in <hole>"
  | I_Struct_assign _ | I_Switch _ | I_break -> failwith "removed"


and statement : statement -> Statement_result.t =
 fun s ->
  match get_s s with
  | S_Attr (attr, x) ->
    let s = statement x in
    (* unsure about the following (need testing) *)
    Statement_result.merge (Binding (fun x -> e_attr ~loc:(get_e_loc x) (attr, x))) s
  | S_Instr i -> instr i
  | S_Decl d -> decl d


let compile ~raise =
  ignore raise;
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_Block_fun { parameters; lhs_type; body = ExpressionBody body } ->
      let parameters =
        List.map
          ~f:(fun pattern -> Param.{ param_kind = `Const; pattern; param_type = None })
          parameters
      in
      e_poly_fun ~loc { type_params = None; parameters; ret_type = lhs_type; body }
    | E_Block_fun { parameters; lhs_type; body = FunctionBody (init, stmts) } ->
      let body =
        let loc =
          List.fold
            ~init:Location.generated
            ~f:Location.cover
            (List.map ~f:get_s_loc ([ init ] @ stmts))
        in
        let init = statement init in
        let stmts = List.map ~f:statement stmts in
        let statement_result = List.fold ~init ~f:Statement_result.merge stmts in
        Statement_result.to_expression ~loc statement_result
      in
      let parameters =
        List.map
          ~f:(fun pattern -> Param.{ param_kind = `Const; pattern; param_type = None })
          parameters
      in
      e_poly_fun ~loc { type_params = None; parameters; ret_type = lhs_type; body }
    | E_Block_with { block; expr } ->
      let statement_result =
        let init, stmts = List.Ne.map statement block in
        List.fold ~init ~f:Statement_result.merge (stmts @ [ Return expr ])
      in
      Statement_result.to_expression ~loc statement_result
    | e -> make_e ~loc e
  in
  `Cata { idle_cata_pass with expr }


let reduction ~raise =
  let fail _ = raise.error (wrong_reduction __MODULE__) in
  { Iter.defaults with
    instruction = fail
  ; statement = fail
  ; expr =
      (function
      | { wrap_content = E_Block_fun _; _ } -> raise.error (wrong_reduction __MODULE__)
      | { wrap_content = E_Block_with _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let pass ~raise =
  cata_morph
    ~name:__MODULE__
    ~compile:(compile ~raise)
    ~decompile:`None (* for now ? *)
    ~reduction_check:Iter.defaults