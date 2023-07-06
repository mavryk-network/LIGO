open Ast_unified
open Pass_type
(* open Simple_utils.Trace *)
(* open Simple_utils.Function *)
(* open Errors *)
module Location = Simple_utils.Location

(*
   on top level type declarations:
    - upon puning on 'dynamic_entries' on storage type definition: pre-declare type 'dynamic_entries' (big_map nat to bytes)
    - if Dynamic_entries module is defined, check that no [@entry] [@view] inside?
    - check that Dynamic_entries module within a module is unique
    - if Dynamic_entries module is defined, but no type storage -> warning
*)

include Flag.No_arg ()

let rec get_within_attr : type a. (a -> (Attribute.t * a) option) -> a -> a =
 fun get_attr x ->
  match get_attr x with
  | Some (_attr, x) -> get_within_attr get_attr x
  | None -> x


(* let dynamic_entries_t =
  let loc = Location.generated in
  pe_declaration
    (d_type
       ~loc
       { name = Ty_variable.of_input_var ~loc "dynamic_entries"
       ; type_expr =
           t_app
             ~loc
             { constr = t_var ~loc (Ty_variable.of_input_var ~loc "big_map")
             ; type_args =
                 ( t_var ~loc (Ty_variable.of_input_var ~loc "nat")
                 , [ t_var ~loc (Ty_variable.of_input_var ~loc "bytes") ] )
             }
       }) *)


let name = __MODULE__
let reduction ~raise:_ = Iter.defaults
let decompile ~raise:_ = Nothing

let rec compile ~raise:_ =
  let program : _ program_ -> program =
   fun p ->
    match is_dynamic_storage p with
    | Some _rows ->
      let () = () (* check that rows has a storage type *) in
      make_prg p
    | None -> make_prg p
  in
  Fold { idle_fold with program }


and is_dynamic_storage : program_entry list -> 'a option =
 fun p ->
  p
  |> List.rev
  |> List.find_map ~f:(fun x ->
         let open Simple_utils.Option in
         let* d = get_pe_declaration (get_within_attr get_pe_attr x) in
         let* { name; type_expr } = get_d_type (get_within_attr get_d_attr d) in
         let* () = if Ty_variable.is_name name "storage" then Some () else None in
         let* (row : ty_expr option Non_linear_rows.t) =
           get_t_record_raw (get_within_attr get_t_attr type_expr)
         in
         List.find
           ~f:(fun (l, t) ->
             Label.equal (Label.of_string "dynamic_entries") l
             && Option.is_none t.associated_type)
           row)
