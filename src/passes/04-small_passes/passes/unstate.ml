open Ast_unified
open Pass_type
open Simple_utils.Trace
open Simple_utils
open Errors
module Location = Simple_utils.Location

module Statement_result = struct
  (* a statement result is the temporary representation of a statement, awaiting to be morphed into an expression *)
  type t =
    (* terminal statement *)
    | Return of expr
    (* continuation to elaborate bindings such as `let X = Y in <>` ; `let () = X in <>`
       from declarative statement *)
    | Binding of (expr -> expr)
    (* continuation to elaborate conditional expression such as `if T then X else Y` ; `case x with P1 -> X | P2 -> Y`
      from control flow statements.
      It operates on a statement result so that each statement in the original branches can be merged
      within the client logic : sometimes one branch is terminal 

      those 2 statements:
      ```
      if (T) { return } else { x += 1 } ;
      return
      ```

      will result in 2 statement results:
      ```
      [ Control_flow (<hole> -> if (T) then <merge Return <hole>> else <merge (Binding ..) <hole> ;
      ; Return
      ]
      ```
      since the merge happens within the control flow continuation, it allowed the positive branch to ignore any
      in incoming results (`<merge Return <hole>>` is always `Return`)
      
      if the control_flow was operating on an expression, it would be hard to decide if an expression is terminal
    *)
    | Control_flow of (t -> expr)

  (* merge two consecutive statement result *)
  let rec merge : t -> t -> t =
   fun bef aft ->
    let open Simple_utils.Function in
    match bef, aft with
    | Binding a, Binding b -> Binding (a <@ b)
    | Binding a, Return b -> Return (a b)
    | Binding a, Control_flow f -> Control_flow (fun t -> a @@ f t)
    | Control_flow f, x -> Control_flow (fun t -> f (merge x t))
    | Return a, _ -> Return a


  let merge_block : t List.Ne.t -> t = fun (hd, tl) -> List.fold ~f:merge ~init:hd tl

  (* morph a statement_result into an expression *)
  let to_expression ~loc : t -> expr =
   fun statement_result ->
    match statement_result with
    | Binding b -> b (e_unit ~loc)
    | Return r -> r
    | Control_flow f -> f (Return (e_unit ~loc))
end

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
  | D_Import (Import_all_as _ | Import_selected _) -> failwith "not supported"
  | D_Multi_var _ | D_Multi_const _ -> failwith "multi vars removed"
  | D_Type_abstraction _ -> failwith "type abs removed"


and instr ~raise : instruction -> Statement_result.t =
 fun i ->
  let loc = get_i_loc i in
  match get_i i with
  | I_Return expr_opt -> Return (Option.value expr_opt ~default:(e_unit ~loc))
  | I_Block block ->
    let block = get_b block in
    (match Statement_result.merge_block (List.Ne.map (statement ~raise) block) with
    | Binding f -> Binding (fun hole -> sequence (f (e_unit ~loc)) hole)
    | Return _ as res -> res
    | Control_flow _ ->
      raise.error (unsupported_control_flow (List.Ne.to_list block))
      (* AS in :
      ```jsligo  
      const g = (n:int) => {
        let output = n;
        
        {
          let x = 1 ;
          output += x ;
          if (n > 1) {
            return (output + 12)
          } else {
            output += x;
          }
        }
        
        return output + x // x should not be visible here
      }
      ```
      *))
  | I_Skip -> Binding Fun.id
  | I_Call (f, args) -> Binding (fun x -> sequence (e_call ~loc f args) x)
  | I_Case { expr; cases } ->
    Control_flow
      (fun hole ->
        let f
            :  (pattern, (instruction, block) Test_clause.t) Case.clause
            -> (pattern, expr) Case.clause
          =
         fun { pattern; rhs } ->
          let branch_result =
            match rhs with
            | ClauseInstr instruction ->
              Statement_result.merge (instr ~raise instruction) hole
            | ClauseBlock block ->
              let block = get_b block in
              Statement_result.merge_block
                List.Ne.(append (map (statement ~raise) block) (singleton hole))
          in
          { pattern; rhs = Statement_result.to_expression ~loc branch_result }
        in
        let cases = List.Ne.map f cases in
        e_match ~loc { expr; cases })
  | I_Cond { test; ifso; ifnot } ->
    Control_flow
      (fun hole ->
        let f : (instruction, block) Test_clause.t -> expr =
         fun clause ->
          let branch_result =
            match clause with
            | ClauseInstr instruction ->
              Statement_result.merge (instr ~raise instruction) hole
            | ClauseBlock block ->
              let block = get_b block in
              Statement_result.merge_block
                List.Ne.(append (map (statement ~raise) block) (singleton hole))
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
  | I_Assign (v, e) ->
    Binding
      (fun hole ->
        sequence
          (e_assign ~loc { binder = Ligo_prim.Binder.make v None; expression = e })
          hole)
  | I_Expr { fp = { wrap_content = E_AssignJsligo _; _ } } -> failwith "removed"
  | I_Expr e -> Binding (fun hole -> sequence e hole)
  | I_For _ | I_ForIn _ | I_ForOf _ | I_Patch _ | I_Remove _ | I_While _ ->
    failwith "not supported"
  | I_Struct_assign _ | I_Switch _ | I_break -> failwith "removed"


and statement ~raise : statement -> Statement_result.t =
 fun s ->
  match get_s s with
  | S_Attr (attr, x) ->
    let s = statement ~raise x in
    Statement_result.merge (Binding (fun x -> e_attr ~loc:(get_e_loc x) (attr, x))) s
  | S_Instr i -> instr ~raise i
  | S_Decl d -> decl d


let compile ~raise =
  let expr : (_,_,_,_,_) expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_Block_fun ({ body = FunctionBody block ; _ } as block_fun) ->
      let body =
        let loc = get_b_loc block in
        let statement_result =
          Statement_result.merge_block (List.Ne.map (statement ~raise) (get_b block))
        in
        Statement_result.to_expression ~loc statement_result
      in
      e_block_fun ~loc ({block_fun with body = ExpressionBody body})
    | E_Block_with { block; expr } ->
      let statement_result =
        Statement_result.merge_block
          List.Ne.(
            append
              (map (statement ~raise) (get_b block))
              (singleton (Statement_result.Return expr)))
      in
      Statement_result.to_expression ~loc statement_result
    | e -> make_e ~loc e
  in
  `Cata { idle_cata_pass with expr }


let reduction ~raise =
  let fail () = raise.error (wrong_reduction __MODULE__) in
  { Iter.defaults with
    instruction = (fun _ -> fail ())
  ; statement = (fun _ -> fail ())
  ; block = (fun _ -> fail ())
  ; expr =
      (function
      | { wrap_content = E_Block_fun { body = FunctionBody _ ; _ }; _ } -> fail ()
      | { wrap_content = E_Block_with _; _ } -> fail ()
      | _ -> ())
  }


let pass ~raise =
  cata_morph
    ~name:__MODULE__
    ~compile:(compile ~raise)
    ~decompile:`None (* for now ? *)
    ~reduction_check:(reduction ~raise)


open Unit_test_helpers

let%expect_test "unsupported case" =
  {|
  ((PE_Declaration
    (D_Const
    ((pattern (P_var g))
      (let_rhs
      (E_Block_fun
        ((parameters ((P_var n))) (lhs_type ())
        (body
          (FunctionBody
          ((S_Decl
            (D_Var ((pattern (P_var output)) (let_rhs (E_variable n)))))
            (S_Instr
            (I_Block
              ((S_Decl
                (D_Var
                ((pattern (P_var x)) (let_rhs (E_Literal (Literal_int 1))))))
              (S_Instr
                (I_Expr
                (E_Let_in
                  ((lhs (P_unit))
                  (rhs
                    (E_assign
                    ((binder ((var output) (ascr ())))
                      (expression
                      (E_constant
                        ((cons_name C_POLYMORPHIC_ADD)
                        (arguments ((E_variable output) (E_variable x)))))))))
                  (body (E_variable output))))))
              (S_Instr
                (I_Cond
                ((test
                  (E_Binary_op
                    ((operator GT) (left (E_variable n))
                    (right (E_Literal (Literal_int 1))))))
                  (ifso
                  (ClauseBlock
                    ((S_Instr
                      (I_Block
                      ((S_Instr
                        (I_Return
                          ((E_Binary_op
                            ((operator PLUS) (left (E_variable output))
                            (right (E_Literal (Literal_int 12))))))))))))))
                  (ifnot
                  (ClauseBlock
                    ((S_Instr
                      (I_Block
                      ((S_Instr
                        (I_Expr
                          (E_Let_in
                          ((lhs (P_unit))
                            (rhs
                            (E_assign
                              ((binder ((var output) (ascr ())))
                              (expression
                                (E_constant
                                ((cons_name C_POLYMORPHIC_ADD)
                                  (arguments
                                  ((E_variable output) (E_variable x)))))))))
                            (body (E_variable output))))))))))))))))))
            (S_Instr
            (I_Return
              ((E_Binary_op
                ((operator PLUS) (left (E_variable output))
                (right (E_variable x)))))))))))))))))
  |}
  |->! pass;
  [%expect
    {|
    Err : (Small_passes_unsupported_control_flow
              ((S_Decl
                   (D_Var
                       ((pattern (P_var x))
                           (let_rhs (E_Literal (Literal_int 1))))))
                  (S_Instr
                      (I_Expr
                          (E_Let_in
                              ((lhs (P_unit))
                                  (rhs
                                      (E_assign
                                          ((binder ((var output) (ascr ())))
                                              (expression
                                                  (E_constant
                                                      ((cons_name
                                                           C_POLYMORPHIC_ADD)
                                                          (arguments
                                                              ((E_variable
                                                                   output)
                                                                  (E_variable x)))))))))
                                  (body (E_variable output))))))
                  (S_Instr
                      (I_Cond
                          ((test
                               (E_Binary_op
                                   ((operator GT) (left (E_variable n))
                                       (right (E_Literal (Literal_int 1))))))
                              (ifso
                                  (ClauseBlock
                                      ((S_Instr
                                           (I_Block
                                               ((S_Instr
                                                    (I_Return
                                                        ((E_Binary_op
                                                             ((operator PLUS)
                                                                 (left
                                                                     (E_variable
                                                                        output))
                                                                 (right
                                                                     (E_Literal
                                                                        (Literal_int
                                                                        12))))))))))))))
                              (ifnot
                                  (ClauseBlock
                                      ((S_Instr
                                           (I_Block
                                               ((S_Instr
                                                    (I_Expr
                                                        (E_Let_in
                                                            ((lhs (P_unit))
                                                                (rhs
                                                                    (E_assign
                                                                        ((binder
                                                                        ((var
                                                                        output)
                                                                        (ascr ())))
                                                                        (expression
                                                                        (E_constant
                                                                        ((cons_name
                                                                        C_POLYMORPHIC_ADD)
                                                                        (arguments
                                                                        ((E_variable
                                                                        output)
                                                                        (E_variable
                                                                        x)))))))))
                                                                (body
                                                                    (E_variable
                                                                        output))))))))))))))))) |}]