open Ast_unified
open Pass_type
open Simple_utils.Trace
open Simple_utils
open Errors
module Location = Simple_utils.Location

type group =
  { case_values : expr list
  ; body : statement List.Ne.t
  }

type default_case = statement List.Ne.t option

(*
  [group_cases] groups each consecutive group cases that are not followed by statements.
  the `default:` case is also returned if any.

  this function will throw if a default case is
  not in last position, but parser would not accept that
*)
let group_cases cases : group list * default_case =
  let open Switch in
  let impossible () = failwith "imposible: parser won't parse" in
  let groups =
    let break x y =
      match x, y with
      | Switch_case (_, None), Switch_case (_, None)
      | Switch_case (_, None), Switch_case (_, Some _) -> false
      | Switch_case (_, Some _), Switch_case (_, None) -> true
      | _, Switch_default_case _ -> true
      | Switch_default_case _, _ -> impossible ()
      | _ -> false
    in
    List.group ~break (List.Ne.to_list cases)
  in
  let cases, default =
    List.split_while
      ~f:(function
        | [ Switch_default_case _ ] -> false
        | _ -> true)
      groups
  in
  let cases =
    (* aggregate groups value and statement *)
    List.map cases ~f:(fun lst ->
        let rec agg (values, stmts) = function
          | Switch_default_case _ :: _ -> impossible ()
          | Switch_case (value, None) :: tl -> agg (value :: values, stmts) tl
          | Switch_case (value, Some stmts) :: tl ->
            if List.is_empty tl
            then value :: values, List.Ne.to_list stmts
            else impossible ()
          | [] -> values, stmts
        in
        let case_values, body = agg ([], []) lst in
        { case_values; body = List.Ne.of_list body })
  in
  let default =
    match default with
    | [] -> None
    | [ [ Switch_default_case x ] ] -> x
    | _ -> impossible ()
  in
  cases, default


(* return all the statements before a break instruction, and append
  a true/false assignment on ft (depending on the presence of a break statement)
  
  if any statement follows the break instruction, emits a warning.
*)
let until_break ~loc ft stmts =
  let f x =
    match get_s x with
    | S_Instr x ->
      (match get_i x with
      | I_break -> false
      | _ -> true)
    | _ -> true
  in
  let bef, aft = List.split_while ~f (List.Ne.to_list stmts) in
  List.Ne.of_list
    (match aft with
    | [] -> bef
    | [ _ ] | _ ->
      (* if aft do not contain only one instruction (break), the following ones are unreachable.
         a dedicated pass warns the user
      *)
      bef
      @ [ s_instr ~loc (i_struct_assign ~loc { lhs_expr = ft; rhs_expr = e_false ~loc }) ])


let switch_to_decl loc switchee cases =
  let open Test_clause in
  let eq a b = e_constant ~loc { cons_name = C_EQ; arguments = [ a; b ] } in
  let or_ a b = e_constant ~loc { cons_name = C_OR; arguments = [ a; b ] } in
  let not_ a = e_constant ~loc { cons_name = C_NOT; arguments = [ a ] } in
  let false_ = e_constant ~loc { cons_name = C_FALSE; arguments = [] } in
  let cases, default_opt = group_cases cases in
  let ft = Variable.fresh ~loc ~name:"fallthrough" () in
  let ft_var = e_variable ~loc ft in
  let grouped_switch_cases : (expr * statement list) list =
    List.mapi
      ~f:(fun i group ->
        let test_ = Variable.fresh ~loc ~name:("g" ^ string_of_int i) () in
        let test_var = e_variable ~loc test_ in
        let test_decl =
          (* build one matching condition for each groups:
            `switchee == v1 || <...> || switchee == vN`
          *)
          let test =
            List.fold group.case_values ~init:ft_var ~f:(fun acc val_ ->
                or_ (eq switchee val_) acc)
          in
          simpl_const_decl ~loc test_ test
        in
        (* generate one condition for each group:
          `if (ft || gN) then <statement> ; ft = <true/false>`
        *)
        let cond =
          let ifso = ClauseBlock (until_break ~loc ft_var group.body) in
          s_instr ~loc (i_cond ~loc { test = or_ ft_var test_var; ifso; ifnot = None })
        in
        test_var, [ test_decl; cond ])
      cases
  in
  let default_statement =
    (* if present, the default case is executed under the following codition
      `ft || !(g1 || .. || gN)` *)
    let tests = List.map ~f:fst grouped_switch_cases in
    Option.value_map default_opt ~default:[] ~f:(fun default_stmts ->
        let test =
          let ortests = List.fold tests ~init:false_ ~f:(fun acc val_ -> or_ val_ acc) in
          or_ ft_var (not_ ortests)
        in
        [ s_instr
            ~loc
            (i_cond ~loc { test; ifso = ClauseBlock default_stmts; ifnot = None })
        ])
  in
  let case_statements = List.concat (List.map ~f:snd grouped_switch_cases) in
  List.Ne.of_list (case_statements @ default_statement)


let compile =
  let pass_declaration : _ instruction_ -> instruction = function
    | { wrap_content = I_Switch { switchee; cases } ; location = loc } ->
      i_block ~loc (switch_to_decl loc switchee cases)
    | { wrap_content ; location = loc } -> make_i ~loc wrap_content
  in
  `Cata { idle_cata_pass with instruction = pass_declaration }


let reduction ~raise =
  { Iter.defaults with
    instruction =
      (function
      | { wrap_content = I_Switch _ | I_break; _ } ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let pass ~raise =
  cata_morph
    ~name:__MODULE__
    ~compile
    ~decompile:`None
    ~reduction_check:(reduction ~raise)


open Unit_test_helpers

let%expect_test "compile" =
  {|
  ((PE_Declaration
  (D_Const
   ((pattern (P_var f))
     (let_rhs
      (E_Block_fun
       ((parameters ((P_var n)))
        (lhs_type ())
        (body
         (FunctionBody
          ((S_Attr
            (((key private) (value ()))
             (S_Decl
              (D_Var
               ((pattern (P_var output))
                 (let_rhs (E_Literal (Literal_int 0))))))))
           (S_Instr
            (I_Switch
             ((switchee (E_variable n))
              (cases
               ((Switch_case (E_Literal (Literal_int 1)) ())
                (Switch_case
                 (E_Literal (Literal_int 2))
                 (((S_Instr
                    (I_Expr
                     (E_AssignJsligo
                      ((expr1 (E_variable output))
                       (op (Assignment_operator Plus_eq))
                       (expr2 (E_Literal (Literal_int 1))))))))))
                (Switch_case (E_Literal (Literal_int 3)) ())
                (Switch_case
                 (E_Literal (Literal_int 4))
                 (((S_Instr
                    (I_Expr
                     (E_AssignJsligo
                      ((expr1 (E_variable output))
                       (op (Assignment_operator Plus_eq))
                       (expr2 (E_Literal (Literal_int 2)))))))
                   (S_Instr I_break))))
                (Switch_default_case
                 (((S_Instr
                    (I_Expr
                     (E_AssignJsligo
                      ((expr1 (E_variable output))
                       (op (Assignment_operator Plus_eq))
                       (expr2 (E_Literal (Literal_int 3)))))))))))))))
           (S_Instr (I_Return ((E_variable output))))))))))))))

  |}
  |-> pass ~raise;
  [%expect
    {|
    ((PE_Declaration
      (D_Const
       ((pattern (P_var f))
        (let_rhs
         (E_Block_fun
          ((parameters ((P_var n))) (lhs_type ())
           (body
            (FunctionBody
             ((S_Attr
               (((key private) (value ()))
                (S_Decl
                 (D_Var
                  ((pattern (P_var output))
                   (let_rhs (E_Literal (Literal_int 0))))))))
              (S_Instr
               (I_Block
                ((S_Decl
                  (D_Const
                   ((pattern (P_var g0))
                    (let_rhs
                     (E_constant
                      ((cons_name C_OR)
                       (arguments
                        ((E_constant
                          ((cons_name C_EQ)
                           (arguments
                            ((E_variable n) (E_Literal (Literal_int 1))))))
                         (E_constant
                          ((cons_name C_OR)
                           (arguments
                            ((E_constant
                              ((cons_name C_EQ)
                               (arguments
                                ((E_variable n) (E_Literal (Literal_int 2))))))
                             (E_variable fallthrough)))))))))))))
                 (S_Instr
                  (I_Cond
                   ((test
                     (E_constant
                      ((cons_name C_OR)
                       (arguments ((E_variable fallthrough) (E_variable g0))))))
                    (ifso
                     (ClauseBlock
                      ((S_Instr
                        (I_Expr
                         (E_AssignJsligo
                          ((expr1 (E_variable output))
                           (op (Assignment_operator Plus_eq))
                           (expr2 (E_Literal (Literal_int 1)))))))))))))
                 (S_Decl
                  (D_Const
                   ((pattern (P_var g1))
                    (let_rhs
                     (E_constant
                      ((cons_name C_OR)
                       (arguments
                        ((E_constant
                          ((cons_name C_EQ)
                           (arguments
                            ((E_variable n) (E_Literal (Literal_int 3))))))
                         (E_constant
                          ((cons_name C_OR)
                           (arguments
                            ((E_constant
                              ((cons_name C_EQ)
                               (arguments
                                ((E_variable n) (E_Literal (Literal_int 4))))))
                             (E_variable fallthrough)))))))))))))
                 (S_Instr
                  (I_Cond
                   ((test
                     (E_constant
                      ((cons_name C_OR)
                       (arguments ((E_variable fallthrough) (E_variable g1))))))
                    (ifso
                     (ClauseBlock
                      ((S_Instr
                        (I_Expr
                         (E_AssignJsligo
                          ((expr1 (E_variable output))
                           (op (Assignment_operator Plus_eq))
                           (expr2 (E_Literal (Literal_int 2)))))))
                       (S_Instr
                        (I_struct_assign
                         ((lhs_expr (E_variable fallthrough))
                          (rhs_expr (E_constant ((cons_name C_FALSE)))))))))))))
                 (S_Instr
                  (I_Cond
                   ((test
                     (E_constant
                      ((cons_name C_OR)
                       (arguments
                        ((E_variable fallthrough)
                         (E_constant
                          ((cons_name C_NOT)
                           (arguments
                            ((E_constant
                              ((cons_name C_OR)
                               (arguments
                                ((E_variable g1)
                                 (E_constant
                                  ((cons_name C_OR)
                                   (arguments
                                    ((E_variable g0)
                                     (E_constant ((cons_name C_FALSE))))))))))))))))))))
                    (ifso
                     (ClauseBlock
                      ((S_Instr
                        (I_Expr
                         (E_AssignJsligo
                          ((expr1 (E_variable output))
                           (op (Assignment_operator Plus_eq))
                           (expr2 (E_Literal (Literal_int 3))))))))))))))))
              (S_Instr (I_Return ((E_variable output))))))))))))))
  |}]