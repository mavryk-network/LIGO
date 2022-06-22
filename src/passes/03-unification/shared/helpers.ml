module Utils     = Simple_utils.Utils
module Region    = Simple_utils.Region

module AST = Ast_unified

let r_split = Simple_utils.Location.r_split  (* TODO NP : Factor with cameligo into helpers *)
let r_fst x = fst (r_split x)

(* TODO NP : Find a better way to remove [Directive _] in the declaration list *)
let filter_opt : 'a . 'a option list -> 'a list = fun decls ->
  List.fold
    ~init:[]
    ~f:(fun acc elt ->
      match elt with
      | None -> acc
      | Some d -> d :: acc )
    decls
  |> List.rev

let translate_directive : LexerLib.Directive.t -> (AST.Directive.t * AST.location) = fun d ->
  let lm, loc = match d with Linemarker lm -> r_split lm in
  let linenum, file_path, flag_opt = lm in
  let flag_opt = Utils.Option.apply (fun flag ->
    match flag with
    | LexerLib.Directive.Push -> AST.Directive.Push
    | LexerLib.Directive.Pop  -> AST.Directive.Pop
  ) flag_opt in
  AST.Directive.Linemarker (linenum, file_path, flag_opt), loc
