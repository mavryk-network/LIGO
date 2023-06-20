open Ast_unified
open Simple_utils.Trace
open Pass_type

(* This pass forces definition of a storage type in contracts.
   Contracts are recognize as a "top-level" that has at least one [@entry] *)
let name = __MODULE__

include Flag.No_arg ()

let get_d_attrs d =
  let rec aux acc d =
    Option.value_map (get_d_attr d) ~default:acc ~f:(fun (attr, d) -> aux (attr :: acc) d)
  in
  aux [] d

let get_d_after_attr d =
  let rec aux d =
    Option.value_map (get_d_attr d) ~default:d ~f:(fun (_attr, d) -> aux d)
  in
  aux d

let compile ~raise =
  let program : _ program_ -> unit =
   fun prg ->
    let is_contract =
      List.exists
        ~f:(fun pe ->
          let opt =
            let open Simple_utils.Option in
            let* d = get_pe_declaration pe in
            let attrs = get_d_attrs d in
            return (List.exists ~f:(fun {key;value} -> String.equal key "entry" && Option.is_none value) attrs)
          in
          Option.value ~default:false opt)
        prg
    in
    let has_storage =
      List.exists
        ~f:(fun pe ->
          let opt =
            let open Simple_utils.Option in
            let* d = get_pe_declaration pe in
            let* { name; _ } = get_d_type (get_d_after_attr d) in
            return (Ty_variable.is_name name "storage")
          in
          Option.value ~default:false opt)
        prg
    in
    if is_contract && not has_storage
    then raise.error (Errors.no_storage_in_contract (make_prg prg))
  in
  Check { Iter.defaults with program }


let reduction ~raise:_ = Iter.defaults
let decompile ~raise:_ = Nothing

open Unit_test_helpers.Program

let%expect_test "not_ok" =
  {|
    ((PE_declaration (D_attr (((key entry)) (D_const ((pattern (PATTERN1)) (let_rhs (EXPR1)))))))
     (PE_declaration (D_attr (((key entry)) (D_const ((pattern (PATTERN2)) (let_rhs (EXPR2))))))))
  |}
  |->! compile;
  [%expect
    {|
    Err : (Small_passes_no_storage_in_contract
              ((PE_declaration
                   (D_attr
                       (((key entry))
                           (D_const
                               ((pattern (P_var #PATTERN1))
                                   (let_rhs (E_variable #EXPR1)))))))
                  (PE_declaration
                      (D_attr
                          (((key entry))
                              (D_const
                                  ((pattern (P_var #PATTERN2))
                                      (let_rhs (E_variable #EXPR2))))))))) |}]

let%expect_test "ok" =
  {|
    ((PE_declaration (D_type ((name storage) (type_expr (TY_EXPR)))))
     (PE_declaration
      (D_attr (((key entry)) (D_const ((pattern (PATTERN2)) (let_rhs (EXPR2))))))))
  |}
  |-> compile;
  [%expect
  {|
    ((PE_declaration (D_type ((name storage) (type_expr (TY_EXPR)))))
     (PE_declaration
      (D_attr (((key entry)) (D_const ((pattern (PATTERN2)) (let_rhs (EXPR2))))))))
  |}]


let%expect_test "ok nested attributes" =
  {|
    ((PE_declaration (D_type ((name storage) (type_expr (TY_EXPR)))))
     (PE_declaration
      (D_attr
       (((key other_attr))
       (D_attr
        (((key entry)) (D_const ((pattern (PATTERN2)) (let_rhs (EXPR2))))))))))
  |}
  |-> compile;
  [%expect
  {|
    ((PE_declaration (D_type ((name storage) (type_expr (TY_EXPR)))))
     (PE_declaration
      (D_attr
       (((key other_attr))
        (D_attr
         (((key entry)) (D_const ((pattern (PATTERN2)) (let_rhs (EXPR2))))))))))
  |}]
    